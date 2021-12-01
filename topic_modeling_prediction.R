library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(gridExtra)
library(modelplotr)
library(randomForest)
library(caret)

################### import ###########################

# set working directory where the files are
setwd('working directory here')  ### INPUT - set working directory

new_reports <- read.csv('DATA_FAA_split_validated.csv')
faa.raw  <- new_reports %>%  select(X, Summary)
topic_model <- readRDS('topic_model.RDS')
predictive_model <- readRDS('predictive_model.RDS')

####
#adding dummy variables that were used in the training  
new_reports$commercial <- ifelse(new_reports$Air.Carrier=="X", 1, 0)
new_reports$ClassLand <- ifelse(new_reports$Class== 'Landplane', 1, 0)


################### data cleaning for new reports ###########################
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
    inner_join(new_reports, by='X')



################ PREDICTIVE TOPIC MODEL  ##################
# topics from the LDA model
topics <- tidy(topic_model)

# get reports, prepare the tokens, and add topic betas to tokens 
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

#combining predictions from topic modeling with original UAS report data
modelinput <- report_topicprobs %>%
    left_join(y= new_reports %>% select(X, commercial, ClassLand),
              by= 'X')

#### predict on the new reports ####
predicted <- predict(predictive_model,newdata=modelinput, type="response")

#adding the predictions to the data
modelinput$prediction <- predicted
#removing dummy variables and appending prediction to original report data
new_reports <- new_reports %>%
    left_join(modelinput %>% select(X, prediction), 
              by= 'X'
              )
    select(-c(commercial, ClassLand))
    
#convert the prediction value to actual or noise
#requires threshold used to train 0.01
new_reports$prediction <- if_else(new_reports$prediction >0.01, 
                                  true=1, 
                                  false=0,
                                  missing=0)

write.csv(new_reports, "predictions.csv")
