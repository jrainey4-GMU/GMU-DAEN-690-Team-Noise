library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(gridExtra)
library(modelplotr)
library(randomForest)
library(caret)
library(LDA)

# tuning parameter
topic_numbers = 4   # number of topics to generate 

################### import ###########################
# set working directory where the files are
setwd('C:\\Users\\jrbrz\\Desktop\\690\\assignments')
#setwd('working directory here')  ### INPUT - set working directory

# FAA data that includes verification for true reports

faa.raw <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split_validated.csv') %>%
#faa.raw <- read.csv('file path to validated date') %>%  ### INPUT - path to validated data set
    select(X, Summary, hit) 

################### data cleaning ###########################
### remove punctation and convert to lowercase and numbers
faa.raw <- faa.raw %>% 
    mutate(Summary = gsub('\\\\n|\\.|\\,|\\;','',tolower(substr(Summary,1,nchar(Summary)))),
           Summary= tm::removeNumbers(Summary))

### divide the reports into separate words
faa <- faa.raw %>%
    tidytext::unnest_tokens(word, Summary)
# remove words that are only 1 letter 
faa <- faa %>% filter(nchar(word) >1)

### dropping reports with counts less than 10 words
# count the reviews that have at least 10 tokens
faa <- faa %>% group_by(X) %>% 
    mutate(n_tokens = n(),report_10tokens_plus = case_when(n_tokens > 10 ~1, TRUE ~ 0)) 

# drop the reports with less than 11 words
faa <- faa %>% filter(n_tokens>10)

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
    'information','their', 'when', 'called', 'just'
)

# stop word dataframe
stop.words <- data.frame(word=stopWords.list, stringsAsFactors=F)

#adding an column to use as an indicator for debugging/exploring later
stop.words <-  stop.words %>% mutate(stopword=1)

# removing the stop words
faa.stopped <- faa %>%
    left_join(y=stop.words, by = 'word', match='all') %>% 
    filter(is.na(stopword))

# joining the stopword-less summaries back to the master dataset
#collapsing the rows of words for each record back into a single record
faa.stopped <- faa.stopped %>%
    group_by(X) %>%
    summarize(Summary_Stopped= paste(word, collapse = ' '))

#joining the collapsed date back to the faa master
data <- faa.raw %>% 
    inner_join(faa.stopped, by='X')


# combining uni-, bi-, tri-grams together
# this only renames data if single word tokens (unigrams) are used. Uncomment the 2nd line if 
# bi- and/or tri- grams are used
report_tokens <- data %>%
    #mutate(Summary = paste0(Summary_Stopped, Summary_Bigrams, Summary_Trigrams)) %>%
    select(X, Summary_Stopped) %>%
    unnest_tokens(word, Summary_Stopped) %>%
    group_by(X) %>%
    ungroup()

report_tokens <- report_tokens %>% 
    group_by(word) %>% mutate(token_freq=n()) %>%  filter(token_freq>=5)

report_tokens_and_counts <- report_tokens %>% 
    group_by(word) %>% 
    mutate(token_count=n()) %>% 
    #removing words with less than 5 occurances
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

# assigning the training input variables  - 
train_x <- report_tokens %>% 
    left_join(y=data, by="X", match="all") %>% 
    filter(train == 1) %>% 
    left_join(y= report_tokens_and_counts, by= "X", match= "all") #%>% select(X, Summary_Stopped.y)

######################  TOPICS  #################################
# creating document term matrix
dtm <- report_tokens %>% 
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
    # combine prepared text including bigrams
    ####mutate(prepared_text = paste(X,bigrams)) %>% 
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

# prepare data voor training and testing
set.seed(217)
# randomly selecting report IDs that will be in the training set

trainids <- data %>% select(X, train)
modelinput <- report_topicprobs %>% 
    # add label and train set indicator
    inner_join(labels,by="X") %>% inner_join(trainids,by="X") %>%
    # set label to factor
    mutate(hit=as.factor(hit))

train <- modelinput %>% filter(train==1) %>% select(-train)
test <- modelinput %>% filter(train!=1) %>% select(-train)

# generating the forumla for the model input. It is a string in the form required
feat_topics <- c('prob_topic1','prob_topic2','prob_topic3','prob_topic4')
formula <- as.formula(paste('hit', paste(feat_topics, collapse=" + "), sep=" ~ "))

# prediction model 
rf_fit <- randomForest(formula, data=train,ntree=500,mtry=3,min_n=50)
predicted <- predict(rf_fit,newdata=test, type="response")

saveRDS(rf_fit, 'randomForest_model.RDS')



