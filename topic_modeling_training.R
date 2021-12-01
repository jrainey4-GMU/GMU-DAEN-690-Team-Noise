library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(caret)

# tuning parameter   ### INPUT
topic_numbers = 5  # number of topics to generate 

################### import ###########################
# set working directory where the files are
setwd('working directory here')  ### INPUT - set working directory

# FAA data that includes verification for true reports
raw <- read.csv('file path to validated date') ### INPUT - path to validated data set
faa.raw <- raw %>%  
    select(X, Summary, hit) 

################### data cleaning ###########################
### divide the reports into separate words and perform some additional text cleaning
faa <- faa.raw %>%
    tidytext::unnest_tokens(word, Summary) %>%
    mutate(word = tm::removePunctuation(word, 
                                        preserve_intra_word_contractions=FALSE,
                                        preserve_intra_word_dashes=FALSE,
                                        ucp=TRUE
                                        ),
           word = tm::removePunctuation(word, 
                                        preserve_intra_word_contractions=FALSE,
                                        preserve_intra_word_dashes=FALSE,
                                        ucp=FALSE
                                        ),
           word= tm::removeNumbers(word),
           
           #cleaning up some words
           word = gsub("rwy", "runway",word),
           word = gsub("pd", "police",word),
           word = gsub("department", "",word),
           word = gsub("county", "",word),
           word = gsub("state", "",word),
           word = gsub("law enforcement", "leo",word)
           ) %>% 
    
    # removing one letter words
    filter(nchar(word) >1) %>% 
    
    # removing reports that are less than 10 words
    group_by(X) %>% 
    mutate(n_tokens = n()) %>% 
    filter(n_tokens>10)

faa$word <- tm::removePunctuation(faa$word) 

# stop word list
stopWords.list <- c(
    # first round top 50    
    "the","a","reported","at","was","of","and","to","no", "evasive","action",
    "notified","advised","on", "while","taken","in","not","off",
    "from","that","for","it", "he", "approximately","his","with",
    #second round top 50
    "were","did","as","by","or","they","an","be",
    #random found in work
    "report", "receive","received","observed","observe","drone","uas", "pilot",
    "aircraft", "about", 'airport', 'feet', 'ft', 'miles', 'mi','mile','him',
    'had', 'acft',"o'clock", 'arpt', 'they', 'them', 'her', 'she', 'he', 'said',
    'is', 'if', 'notification', 'have', 'but','stated','aviation','approx',
    'information','their', 'when', 'called', 'just', 'could', 'has', 'than',
    'described','unknown', 'would','reports','possibly','could', 'like','then',
    'does', 'this', 'which', 'been','any', 'what', 'there',
    'south','s','north','n','east','e','west','w','se','sw','ne','nw',
    'den', 'other','activity','oclock', 'via'
) 

# stop word dataframe and adding an column to use as an indicator
stop.words <- data.frame(word=stopWords.list,
                         stopword=1, 
                         stringsAsFactors=F
                         )

# removing the stop words from the master dataframe
faa.stopped <- faa %>%
    left_join(y=stop.words, by = 'word', match='all') %>% 
    filter(is.na(stopword)) %>%
    group_by(X) %>%
    summarize(Summary_Stopped= paste(word, collapse = ' '))

#joining the collapsed date back to the faa master to capture all 
# the old and newly cleaned data
data <- faa.stopped %>% 
    inner_join(faa.raw %>% select(X, hit), by='X')

# var needed for DTM
report_tokens <- data %>%
    select(X, Summary_Stopped) %>%
    unnest_tokens(word, Summary_Stopped) %>%
    group_by(X) %>%
    ungroup() %>%
    group_by(word) %>% 
    mutate(token_freq=n()) %>%
    #removing words that appear less than 6 times
    filter(token_freq>5)

# var needed for DTM
report_tokens_and_counts <- report_tokens %>% 
    group_by(word) %>% 
    mutate(token_count=n()) %>% 
    #removing words with less than 6 occurances
    filter(token_count>5) %>%
    group_by(X) %>% 
    summarise(Summary_Stopped = str_c(word, collapse = " "))

##################### TRAIN TEST SPLITS  ##############################
### CREATING TRAIN AND TEST SPLIT ###
set.seed(217)
# randomly selecting report IDs that will be in the training set
# this will be used later as well when training the prediction model
train_rows <- sample(unique(data$X), size = length(unique(data$X))*.75, replace = FALSE)

#adding a column to flag training records
data <- data %>% 
    mutate(
        train = case_when(X %in% train_rows ~ 1, TRUE ~ 0 )
    )

# assigning the training inputs to variables  - 
train_x <- report_tokens %>% 
    left_join(y=data, by="X", match="all") %>% 
    filter(train == 1) 
    #(y= report_tokens_and_counts, by= "X", match= "all")

######################  TOPICS  #################################
# creating document term matrix
dtm <- train_x %>% 
    cast_dtm(document = X,term = word,value = token_freq)

#check dimenstions of dtm
cat(paste0('DTM dimensions: Documents (',dim(dtm)[1],') x Tokens (',dim(dtm)[2],')',
           ' (average token frequency: ',round(sum(dtm)/sum(dtm!=0),2),')'))

start_time <- Sys.time()
lda_fit <- LDA(
    dtm,
    k = topic_numbers,
    control = list(nstart=1,best=TRUE, seed=c(217))
)
end_time <- Sys.time()
end_time - start_time

## OUTPUT THE LDA MODEL
saveRDS(lda_fit, 'topic_model.RDS')

# topics from the LDA model
topics <- tidy(lda_fit)


################ PREDICTIVE TOPIC MODEL  ##################

# get reviews, prepare the tokens, and add topic betas to tokens 
# and aggregate to get topic probabilities per review
report_topicprobs <- data %>% 
    select(X,Summary_Stopped) %>% 
    # unnest tokens
    unnest_tokens(token, Summary_Stopped) %>% 
    # add token betas
    left_join(topics,by=c("token"="term")) %>% 
    # aggregate over betas
    group_by(X, topic) %>% summarise(beta_topic = sum(beta,na.rm=T)) %>% 
    # reset beta_topic to sum to 1 over al topics
    group_by(X) %>% mutate(prob_topic = beta_topic / sum(beta_topic,na.rm=T)) %>% 
    select(-beta_topic) %>%  ungroup() %>% 
    # transpose (topic probs in columns) , remove 1 column and 1 observation with NA topic scores
    pivot_wider(names_from = topic, values_from = prob_topic,names_prefix='prob_topic') %>% 
    select(-prob_topicNA) %>% filter(!is.na(prob_topic1))

# this variable is used later to relabel records as hits - 
labels <- data %>% select(X, hit)

#adding some dummy variables for categorical variables for possible use in models
raw$weightSmall <- ifelse((raw$FAA.Weight== "Small" |raw$FAA.Weight== "Small+"), 1, 0)
raw$weightLarge <- ifelse((raw$FAA.Weight== "Large" | raw$FAA.Weight== "Heavy"| raw$FAA.Weight== "Super"), 1, 0)
raw$commercial <- ifelse(raw$Air.Carrier=="X", 1, 0)
raw$Engine.Number <- ifelse(is.na(raw$Engine.Number), 0, raw$Engine.Number)
raw$engineJet <- ifelse(raw$Engine.Type== 'Jet', 1, 0)
raw$enginePiston <- ifelse(raw$Engine.Type== 'Piston', 1, 0)
raw$ClassHeli <- ifelse((raw$Class== 'Gyrocopter' | raw$Class== 'Helicopter' | raw$Class== 'Tiltrotor'), 1, 0)
raw$ClassLand <- ifelse(raw$Class== 'Landplane', 1, 0)

# prepare data voor training and testing
set.seed(217)
# randomly selecting report IDs that will be in the training set

trainids <- data %>% select(X, train)
modelinput <- report_topicprobs %>% 
    # add label and train set indicator
    inner_join(labels,by="X") %>% 
    inner_join(trainids,by="X") %>%
    inner_join(raw, by= "X") %>%
    # set label to factor
    
    mutate(hit=hit.x)

train <- modelinput %>% filter(train==1) %>% select(-train)
test <- modelinput %>% filter(train!=1) %>% select(-train)

# generating the forumla for the model input. It is a string in the form required


#create list of topic probs that can adjust to 
#the tuning param and will be used in the predictive 
#model formula
tops <- c()
for (i in seq(1:topic_numbers)){
    temp <- paste0('prob_topic', i, sep='')
    tops <- c(tops, temp)
}
# the dummy variables created and that wish to be used
# need to be added here, after tops
feat_topics <- c(tops,
                 'commercial', 'ClassLand') # <<<<<
formula <- as.formula(paste('hit', paste(feat_topics, collapse=" + "), sep=" ~ "))


# logistic regression
set.seed(217)

#fitting the logistic regression to the training data set
glm_fit <- glm(formula, data= train, family = binomial)

#convert logit to probability
logProb <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
logProb(coef(glm_fit))

# getting the predictions made on the training set
# these are probabilties
glm_predict <- predict(glm_fit, type="response")

#this is taking the training prediction probability and assigning 
#a threshold where the prediction would be true or false
#currently set at predictions <= 0.01 = noise
train_glm = train %>%
    mutate(predicted.value=as.factor(ifelse(glm_predict<=0.01, 0, 1)))

# finding optimal cutoff probability to classify an actual
# optimizing to max 1 and min 0 class
opti <- optimalCutoff(actuals= as.numeric(paste(train_glm$hit)), 
                      predictedScores= as.numeric(paste(train_glm$predicted.value)),#test_glm_predict,
                      #optimiseFor= "Zeros",
                      returnDiagnostics= TRUE
)
optiCutoff <- opti$optimalCutoff
optiCutoff

saveRDS(glm_fit, 'predictive_model.RDS')

##########################################################
# random forest
##########################################

# rf with caret needs outcome variables not equal to 0 and 1
# converting to n and a
train <- modelinput %>% filter(train==1) %>% select(-train)
train$hit <- gsub(0,"n",train$hit)
train$hit <- gsub(1,"a",train$hit)
test <- modelinput %>% filter(train!=1) %>% select(-train)
test$hit <- gsub(0,"n",test$hit)
test$hit <- gsub(1,"a",test$hit)


tuningGrid = expand.grid(mtry=c(4,5,6,7))
rf_fit <- train(formula,
                data=train,
                method='rf',
                importance= TRUE,
                tuneGrid= tuningGrid,
                #trControl=trainControl(classProbs = TRUE)
)





