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
setwd('C:\\Users\\jrbrz\\Desktop\\690\\assignments')
#setwd('working directory here')  ### INPUT - set working directory

new_reports <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split_validated.csv') %>%
    #faa.raw <- read.csv('file path to validated date') %>%  ### INPUT - path to validated data set
    select(X, Summary)
topic_model <- readRDS('topic_model.RDS')
predictive_model <- readRDS('randomForest_model.RDS')


################### data cleaning for new reports ###########################
### remove punctation and convert to lowercase and numbers
faa.raw <- new_reports %>% 
    mutate(Summary = gsub('\\\\n|\\.|\\,|\\;','',tolower(substr(Summary,1,nchar(Summary)))),
           Summary= tm::removeNumbers(Summary))

### divide the reports into separate words
faa <- faa.raw %>%
    tidytext::unnest_tokens(word, Summary)

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
    left_join(y=stop.words, by = 'word', match='all')

# joining the stopword-less summaries back to the master dataset
#collapsing the rows of words for each record back into a single record
faa.stopped <- faa.stopped %>%
    group_by(X) %>%
    summarize(Summary_Stopped= paste(word, collapse = ' '))

#joining the collapsed date back to the faa master
data <- faa.raw %>% 
    inner_join(faa.stopped, by='X')


################ PREDICTIVE TOPIC MODEL  ##################
# topics from the LDA model
topics <- tidy(topic_model)

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

modelinput <- report_topicprobs
predicted <- predict(predictive_model,newdata=modelinput, type="response")


modelinput$prediction <- predicted
new_reports <- new_reports  %>%
    left_join(y=modelinput, by="X")

write.csv(new_reports, "noise_predictions.csv")
