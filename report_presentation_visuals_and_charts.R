library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(gridExtra)
library(modelplotr)
library(randomForest)
library(caret)
library(LDAvis)
library(RColorBrewer)
library(wordcloud2)
library(caret)
library(InformationValue)
library(ROCR)

################### import ###########################
# set working directory where the files are
setwd('C:\\Users\\jrbrz\\Desktop\\690\\assignments')
# FAA data that includes verification for true reports
faa.raw <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split_validated.csv') %>% 
    select(X, Summary, hit) 


################### data cleaning ###########################
### remove punctation and convert to lowercase and numbers
#faa.raw$Summary <- tolower(faa.raw$Summary)
faa.raw <- faa.raw %>% 
    mutate(#Summary = gsub('\\\\n|\\.|\\,|\\;','',tolower(substr(Summary,1,nchar(Summary)))),
        Summary = gsub("[^[:alnum:][:space:]]", "", Summary)) #gsub("[[:punct:]]", "",Summary)) %>%
    

### divide the reports into separate words
faa <- faa.raw %>%
    tidytext::unnest_tokens(word, Summary)
faa <- faa %>% 
    mutate(
        word = tm::removePunctuation(word, 
                                     preserve_intra_word_contractions=FALSE,
                                     preserve_intra_word_dashes=FALSE,
                                     ucp=TRUE)) %>%
    mutate(
        word = tm::removePunctuation(word, 
                                     preserve_intra_word_contractions=FALSE,
                                     preserve_intra_word_dashes=FALSE,
                                     ucp=FALSE)) %>%
    mutate(
        word= tm::removeNumbers(word)) %>%
    mutate(
        word = gsub("rwy", "runway",word)) %>%
    mutate(
        word = gsub("pd", "police",word)) %>%
    mutate(
        word = gsub("department", "",word)) %>%
    mutate(
        word = gsub("county", "",word)) %>%
    mutate(
        word = gsub("state", "",word)) %>%
    mutate(
        word = gsub("law enforcement", "leo",word))

faa <- faa %>%
    mutate(word= gsub('",','',word)) %>%
    mutate(word= gsub('")','',word))

    

# remove words that are only 1 letter 
faa <- faa %>% filter(nchar(word) >1)

### dropping reports with counts less than 10 words
# count the reviews that have at least 10 tokens
faa <- faa %>% group_by(X) %>% 
    mutate(n_tokens = n()) %>% 
    filter(n_tokens>10)
faa$word <- removePunctuation(faa$word) 

################### removing stop words ###########################
# calculating the top 50 words before removing any
top50words <- faa %>%
    group_by(word) %>%
    summarize(word_count = n()) %>%
    arrange(desc(word_count)) %>%
    head(50)

ggplot(top50words, aes(x=reorder(word,word_count), y=word_count)) + 
    geom_col(fill='blue') + 
    coord_flip() + 
    labs(x="Words", y= "Count", title= "Top 50 Words Used in Reports - Before Stop Words Removed") +
    theme_minimal() 

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

# stop word dataframe
stop.words <- data.frame(word=stopWords.list, stringsAsFactors=F)

#adding an column to use as an indicator for debugging/exploring later
stop.words <-  stop.words %>% mutate(stopword=1)

# removing the stop words
faa.stopped <- faa %>%
    left_join(y=stop.words, by = 'word', match='all') %>% 
    filter(is.na(stopword))

# calculating the top 50 words after stop word drop
top50words_postDrop <- faa.stopped %>%
    group_by(word) %>%
    summarize(word_count = n()) %>%
    arrange(desc(word_count)) %>%
    head(50)

ggplot(top50words_postDrop, aes(x=reorder(word,word_count), y=word_count)) + 
    geom_col(fill='blue') + 
    coord_flip() + 
    labs(x="Words", y= "Count", title= "Top 50 Words Used in Reports - After Stop Words Removed") +
    theme_minimal() 

# joining the stopword-less summaries back to the master dataset
#collapsing the rows of words for each record back into a single record
faa.stopped <- faa.stopped %>%
    group_by(X) %>%
    summarize(Summary_Stopped= paste(word, collapse = ' '))

#joining the collapsed date back to the faa master
data <- faa.stopped %>% 
    inner_join(faa.raw %>% select(X, hit), by='X') %>%
    mutate(Summary_Stopped= gsub('",','',Summary_Stopped))

data$Summary_Stopped <- removePunctuation(data$Summary_Stopped)



################### creating bigrams and trigrams ###########################
#grouping the words into bigrams
bigrams <- faa %>%
    group_by(X)  %>% 
    summarize(Summary=paste(word,collapse=' ')) %>%
    unnest_tokens(bigram, token = "ngrams",n = 2, Summary)

#spliting the bigrams into sep. cols.
bigrams_split <- bigrams %>%
    separate(bigram, c('w1', 'w2'), sep=" ")

    group_by(bigram) %>% 
    summarize(n=n()) %>%     top_n(10,wt=n) %>% 
    select(bigram) %>% pll()
#removing bigrams that contain stop words
bigrams_noStop <- bigrams_split %>%
    filter(!w1 %in% stop.words$word & !w2 %in% stop.words$word)

#combining the surviving bigrams back together
bigrams_join <- bigrams_noStop %>%
    unite(bigram, w1, w2, sep = '_')

###### showing the top 10 bigrams
top10_bigrams = bigrams_join %>% 
    print(paste0('Top 10 bigrams: ',paste(top10_bigrams,collapse=", ")))

#grouping the words into trigrams
trigrams <- faa %>%
    group_by(X)  %>% 
    summarize(Summary=paste(word,collapse=' ')) %>%
    unnest_tokens(trigram, token = "ngrams",n = 3, Summary)

#spliting the trigrams into sep. cols.
trigrams_split <- trigrams %>%
    separate(trigram, c('w1', 'w2',  'w3'), sep=" ")

#removing trigrams that contain stop words
trigrams_noStop <- trigrams_split %>%
    filter(!w1 %in% stop.words$word & !w2 %in% stop.words$word & !w3 %in% stop.words$word)

#combining the surviving trigrams back together
trigrams_join <- trigrams_noStop %>%
    unite(trigram, w1, w2, w3, sep = '_')

###### showing the top 10 bigrams
top10_trigrams = trigrams_join %>% 
    group_by(trigram) %>% 
    summarize(n=n()) %>% 
    top_n(10,wt=n) %>% 
    select(trigram) %>% pull()

print(paste0('Top 10 trigrams: ',paste(top10_trigrams,collapse=", ")))

## joining the bigrams and trigrams back to the master dataset
#collapsing the data into 1 row per report
bigrams.stopped <- bigrams_join %>% 
    group_by(X) %>% 
    summarize(Summary_Bigrams=paste(bigram,collapse=' ')) 
trigrams.stopped <- trigrams_join %>% 
    group_by(X) %>% 
    summarize(Summary_Trigrams=paste(trigram,collapse=' ')) 

## joining bigram and trigrams back to master faa
data <- data %>%
    left_join(bigrams.stopped, by= 'X')
data <- data %>%
    left_join(trigrams.stopped, by= 'X')
#################### a ################################
## combining uni-, bi-, tri-grams together
report_tokens <- data %>%
    #mutate(Summary = paste0(Summary_Stopped, Summary_Bigrams, Summary_Trigrams)) %>%
    select(X, Summary_Stopped) %>%
    unnest_tokens(word, Summary_Stopped) %>%
    group_by(X) %>%
    ungroup()

###### plotting ratios
report_tokens %>% 
    group_by(word) %>% summarize(token_freq=n()) %>% 
    mutate(token_freq_binned = case_when(token_freq>20~20,TRUE~as.numeric(token_freq))) %>% 
    group_by(token_freq_binned) %>% summarise(n_tokens = n()) %>% 
    mutate(pct_tokens = n_tokens/sum(n_tokens),
           cumpct_tokens = cumsum(n_tokens)/sum(n_tokens)) %>% 
    ggplot(aes(x=token_freq_binned)) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_bar(aes(y=pct_tokens),stat='identity',fill='blue') +  
    geom_line(aes(y=cumpct_tokens),stat='identity',color='orange',linetype='dashed') + 
    geom_text(aes(y=cumpct_tokens,label=scales::percent(cumpct_tokens,accuracy=1)),
              size=3) + theme_minimal() + 
    ggtitle("Frequency of token in Corpus (all reports)") + xlab("token frequency") +
    ylab("% of all tokens")

###### display ratios
report_tokens %>% 
    group_by(word) %>% summarize(token_freq=n()) %>% 
    mutate(min_6_freq = case_when(token_freq<6~'token frequency: <6',
                                  TRUE~'token frequency: >=6')) %>% 
    group_by(min_6_freq) %>% summarise(n_unique_tokens = n(),n_tokens=sum(token_freq)) %>% 
    mutate(pct_unique_tokens = scales::percent(n_unique_tokens / sum(n_unique_tokens)),
           pct_all_tokens=scales::percent(n_tokens / sum(n_tokens))) 


report_tokens <- report_tokens %>% 
    group_by(word) %>% mutate(token_freq=n()) %>%  filter(token_freq>=5)

report_tokens_and_counts <- report_tokens %>% 
    group_by(word) %>% 
    mutate(token_count=n()) %>% 
    #removing words with less than 6 occurances
    filter(token_count>5) %>%
    group_by(X) %>% 
    summarise(Summary_Stopped = str_c(word, collapse = " "))


###

labels <- data %>% select(X, hit)

################ TRAIN TEST SPLITS  ##############################

### CREATING TRAIN AND TEST SPLIT ###
set.seed(217)
# randomly selecting report IDs that will be in the training set
train_rows <- sample(unique(data$X), size = length(unique(data$X))*.75, replace = FALSE)

#adding a column to flag training records
data <- data %>% 
    mutate(
        train = case_when(
            X %in% train_rows ~ 1, TRUE ~ 0
        )
    )

# assigning the training input variables
train_x <- report_tokens %>% 
    left_join(y=data, by="X", match="all") %>% 
    filter(train == 1) %>% 
    left_join(y= report_tokens_and_counts, by= "X", match= "all")

################ TOPICS  ##############################
# creating document term matrix
dtm <- report_tokens %>% 
    cast_dtm(document = X,term = word,value = token_freq)

#check dimenstions of dtm
cat(paste0('DTM dimensions: Documents (',dim(dtm)[1],') x Tokens (',dim(dtm)[2],')',
           ' (average token frequency: ',round(sum(dtm)/sum(dtm!=0),2),')'))

start_time <- Sys.time()
lda_fit <- LDA(
    dtm,
    k = 5,
    control = list(nstart=1,best=TRUE, seed=c(217))
)
end_time <- Sys.time()
end_time - start_time

############################# SAMPLE LDA MODEL OUTPUT  #####################
# phi (topic - token distribution matrix) -  topics in rows, tokens in columns:
phi <- posterior(lda_fit)$terms %>% as.matrix
cat(paste0('Dimensions of phi (topic-token-matrix): ',paste(dim(phi),collapse=' x '),'\n'))
cat(paste0('phi examples (9 tokens): ','\n'))
phi[,1:9] %>% as_tibble() %>% mutate_if(is.numeric, round, 5) %>% print()

# theta (document - topic distribution matrix) -  documents in rows, topic probs in columns:
theta <- posterior(lda_fit)$topics %>% as.matrix
cat(paste0('\n\n','Dimensions of theta (document-topic-matrix): ',
           paste(dim(theta),collapse=' x '),'\n'))

cat(paste0('theta examples (9 documents): ','\n'))
theta[1:9,] %>% as_tibble() %>% mutate_if(is.numeric, round, 5) %>% 
    setNames(paste0('Topic', names(.))) %>% print()



topics <- tidy(lda_fit)

# only select top-8 terms per topic based on token probability within a topic
topn <- 5
plotinput <- topics %>%
    mutate(topic = as.factor(paste0('Topic',topic))) %>%
    group_by(topic) %>%
    top_n(topn, beta) %>% 
    ungroup() %>%
    arrange(topic, -beta)

# plot highest probability terms per topic
names <- levels(unique(plotinput$topic))
colors <- RColorBrewer::brewer.pal(n=length(names),name="Set2")

plist <- list()

for (i in 1:length(names)) {
    d <- subset(plotinput,topic == names[i])[1:topn,]
    d$term <- factor(d$term, levels=d[order(d$beta),]$term)
    
    p1 <- ggplot(d, aes(x = term, y = beta, width=0.75)) + 
        labs(y = NULL, x = NULL, fill = NULL) +
        geom_bar(stat = "identity",fill=colors[i]) +
        facet_wrap(~topic) +
        coord_flip() +
        guides(fill=FALSE) +
        theme_bw() + theme(strip.background  = element_blank(),
                           panel.grid.major = element_line(colour = "grey80"),
                           panel.border = element_blank(),
                           axis.ticks = element_line(size = 0),
                           panel.grid.minor.y = element_blank(),
                           panel.grid.major.y = element_blank() ) +
        theme(legend.position="bottom") 
    
    plist[[names[i]]] = p1
}


do.call("grid.arrange", c(plist, ncol=2))


##################### LDAvis visual #######################
# using absolute term probability and relative term probabilty
# phi (topic - token distribution matrix) - tokens in rows, topic scores in columns:
phi <- posterior(lda_fit)$terms %>% as.matrix 

# theta (document - topic distribution matrix) - documents in rows, topic probs in columns:
theta <- posterior(lda_fit)$topics %>% as.matrix 

# number of tokens per document
doc_length <- report_tokens %>% group_by(X) %>% 
    summarize(doc_length=n()) %>% select(doc_length) %>% pull() 

# vocabulary: unique tokens
vocab <- colnames(phi) 

# overall token frequency
term_frequency <- report_tokens %>% group_by(word) %>% 
    summarise(n=n()) %>% arrange(match(word, vocab)) %>% select(n) %>% pull() 


# create JSON containing all needed elements
json <- createJSON(phi, theta, doc_length, vocab, term_frequency)
LDAvis::serVis(json)



################ PREDICTIVE TOPIC MODEL ##################

# training and testing error dataframe
records <- matrix(NA, nrow=3, ncol=2)
colnames(records) <- c("train_error", "test_error")
rownames(records) <- c("Logistic","RandonForest","SVM")

error_rates <- function(predicted.value, true.value){
    return(mean(true.value!=predicted.value))
}



# topic probabilities per review
report_topicprobs <- data %>% 
    # combine prepared text including bigrams
    ####mutate(prepared_text = paste(reviewTextClean,bigrams)) %>% 
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


report_topicprobs %>% 
    # rearrange: topic probabilities to rows
    pivot_longer(-X,names_to='topic',values_to='probability') %>% 
    mutate(topic=str_replace(topic,'prob_','')) %>% 
    # add topic labels and add prediction labels %>% 
    inner_join(labels,by='X') %>% 
    mutate(reportType=as.factor(case_when(hit==1~'actual',TRUE~'noise'))) %>%
    # create density plots per topic
    ggplot(aes(x=probability,group=reportType,fill=reportType)) + 
    geom_density(alpha=0.6) + facet_wrap(~topic,ncol = 4) +   
    ggtitle('Topic probability distribution for Actual/Noise') + 
    theme_minimal()  + 
    theme(legend.position=c(0.9, 0.2),legend.text = element_text(size=8),
          legend.title=element_blank(),plot.title = element_text(hjust = 0.5,size=12),
          axis.title = element_text(size=8)) + ylim(c(0,20))


# prepare data for training and testing
set.seed(217)
# randomly selecting report IDs that will be in the training set

raw <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split_validated.csv')
raw$weightSmall <- ifelse((raw$FAA.Weight== "Small" |raw$FAA.Weight== "Small+"), 1, 0)
raw$weightLarge <- ifelse((raw$FAA.Weight== "Large" | raw$FAA.Weight== "Heavy"| raw$FAA.Weight== "Super"), 1, 0)
raw$commercial <- ifelse(raw$Air.Carrier=="X", 1, 0)
raw$Engine.Number <- ifelse(is.na(raw$Engine.Number), 0, raw$Engine.Number)
raw$engineJet <- ifelse(raw$Engine.Type== 'Jet', 1, 0)
raw$enginePiston <- ifelse(raw$Engine.Type== 'Piston', 1, 0)
raw$ClassHeli <- ifelse((raw$Class== 'Gyrocopter' | raw$Class== 'Helicopter' | raw$Class== 'Tiltrotor'), 1, 0)
raw$ClassLand <- ifelse(raw$Class== 'Landplane', 1, 0)


trainids <- data %>% select(X, train)
modelinput <- report_topicprobs %>% 
    # add label and train set indicator
    inner_join(labels,by="X") %>% 
    inner_join(trainids,by="X") %>%
    inner_join(raw, by= "X") %>%
    # set label to factor
    select(X, prob_topic1,prob_topic2,prob_topic3,prob_topic4,prob_topic5,
           weightSmall, weightLarge, commercial, #weightSmallplus, weightHeavy, weightSuper,
           Engine.Number, engineJet, enginePiston, #engineTurboProp, engineTurboShaft,
           ClassHeli, ClassLand, hit.x, train) %>%  #ClassSea,)
            mutate(hit=hit.x) 


train <- modelinput %>% filter(train==1) %>% select(-train)
test <- modelinput %>% filter(train!=1) %>% select(-train)

for (i in topic)

feat_topics <- c('prob_topic1','prob_topic2','prob_topic3','prob_topic4','prob_topic5',
                 'commercial', 'ClassLand'# 'ClassHeli', 
                 #'weightSmall','weightLarge', 'weightSmallplus',  , 'weightHeavy', 'weightSuper', 
                 # 'Engine.Number', 'engineJet', 'enginePiston', 'engineTurboProp', 'engineTurboShaft',
                 #, 'ClassSea'
                 )
formula <- as.formula(paste('hit', paste(feat_topics, collapse=" + "), sep=" ~ "))

#####################################################################################
### logistic
set.seed(217)
glm_fit <- glm(formula, data= train, family = binomial)

#convert logit to probability
logProb <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
logProb(coef(glm_fit))

glm_predict <- predict(glm_fit, type="response")

train_glm = train %>%
    mutate(predicted.value=as.factor(ifelse(glm_predict<=0.01, 0, 1))) 

#logit_traing_error <-  error_rates(predicted.value=as.numeric(paste(train_glm$predicted.value)),  
#                                       true.value= as.numeric(paste(train$hit)))


# finding optimal cutoff probability to classify an actual
opti <- optimalCutoff(actuals= as.numeric(paste(train_glm$hit)), 
                      predictedScores= as.numeric(paste(train_glm$predicted.value)),#test_glm_predict,
                      #optimiseFor= "Zeros",
                      returnDiagnostics= TRUE
)
optiCutoff <- opti$optimalCutoff
optiCutoff

test_glm_predict = predict(glm_fit, type="response", newdata = test)
test_glm = test %>% 
    mutate(predicted.value2=as.factor(ifelse(test_glm_predict<=.009, 0, 1))) 

#logit_test_error <-  error_rates(predicted.value=as.numeric(paste(test_glm$predicted.value2)),  
#                                   true.value= as.numeric(paste(test$hit)))

#records[1,] <- c(logit_traing_error, logit_test_error)



pred <- prediction(as.numeric(paste(test_glm$predicted.value2)), as.numeric(paste(test_glm$hit)))

perf <- performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, lwd = 2, col='blue', main= "Logistic Regression ROC Curve")
abline(a = 0, b = 1) 


auc = performance(pred, measure = "auc")
print(auc@y.values)


confusionMatrix(actuals= as.numeric(paste(test_glm$hit)), 
                predictedScores= as.numeric(paste(test_glm$predicted.value2)) #test_glm_predict,#, 
              #threshold = optiCutoff
              )

sensitivity(as.numeric(paste(test_glm$hit)), 
                as.numeric(paste(test_glm$predicted.value2)))
specificity(as.numeric(paste(test_glm$hit)), 
                as.numeric(paste(test_glm$predicted.value2)))
misClassError(as.numeric(paste(test_glm$hit)), 
                as.numeric(paste(test_glm$predicted.value2)))




# confusion matrix: actual vs predicted counts
actual <- as.numeric(paste(test_glm$hit))
glm_predicted <- as.numeric(paste(test_glm$predicted.value2))
confmat   <- table(glm_predicted, actual)
print(confmat)

actual_classification <- factor(c(0, 0, 1, 1))
predicted_classification <- factor(c(0, 1, 0, 1))
Y      <- c(confmat[1,1], confmat[2,1],
            confmat[1,2], confmat[2,2])
df <- data.frame(actual_classification, predicted_classification, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = actual_classification, y = predicted_classification)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "lightblue") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Logistic Regression Confusion Matrix")


TP <- confmat[2,2]; FP <- confmat[2,1]; FN <- confmat[1,2]; TN <- confmat[1,1]
Accuracy = (TP+TN)/(TP+FP+FN+TN)
Precision = (TP)/(TP+FP)
Recall = (TP)/(TP+FN)
F1.Score = 2*(Recall * Precision) / (Recall + Precision)




###### random forest
#########################################################################
set.seed(217)

trainids <- data %>% select(X, train)
modelinput <- report_topicprobs %>% 
    # add label and train set indicator
    inner_join(labels,by="X") %>% 
    inner_join(trainids,by="X") %>%
    inner_join(raw, by= "X") %>%
    # set label to factor
    mutate(hit=as.factor(case_when(hit.x==1~"actual",TRUE~"noise") )) %>%
    select(X, prob_topic1,prob_topic2,prob_topic3,prob_topic4,prob_topic5,
           weightSmall, weightLarge, commercial, #weightSmallplus, weightHeavy, weightSuper,
           Engine.Number, engineJet, enginePiston, #engineTurboProp, engineTurboShaft,
           ClassHeli, ClassLand, hit, train)  #ClassSea,)
    


train <- modelinput %>% filter(train==1) %>% select(-train)
train$hit <- gsub(0,"n",train$hit)
train$hit <- gsub(1,"a",train$hit)
test <- modelinput %>% filter(train!=1) %>% select(-train)
test$hit <- gsub(0,"n",test$hit)
test$hit <- gsub(1,"a",test$hit)

rf_fit <- train(formula,
                data=train,
                method='rf',
                importance= TRUE,
                tuneGrid= expand.grid(mtry=c(4,5,6,7))#,
                #trControl=trainControl(classProbs = TRUE)
          )


rf_pred <- predict(rf_fit, test, type='prob')

test_rf = test %>% 
    mutate(predicted.value=as.factor(ifelse(rf_pred$actual>0.0001,#rf_pred$actual > rf_pred$noise, 
                                            1, 0)))
test_rf$hit <- gsub("noise",0,test_rf$hit)
test_rf$hit <- gsub("actual",1,test_rf$hit)
pred <- prediction(as.numeric(paste(test_rf$predicted.value)), 
                   as.numeric(paste(test_rf$hit)))

perf <- performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, lwd = 2, col='blue', main= "Random Forest ROC Curve")
abline(a = 0, b = 1) 


auc = performance(pred, measure = "auc")
print(auc@y.values)


confusionMatrix(actuals= as.numeric(paste(test_rf$hit)), 
                predictedScores= as.numeric(paste(test_rf$predicted.value)) #test_glm_predict,#, 
                #threshold = optiCutoff
)

sensitivity(as.numeric(paste(test_rf$hit)), 
            as.numeric(paste(test_rf$predicted.value)))
specificity(as.numeric(paste(test_rf$hit)), 
            as.numeric(paste(test_rf$predicted.value)))
misClassError(as.numeric(paste(test_rf$hit)), 
              as.numeric(paste(test_rf$predicted.value)))




# confusion matrix: actual vs predicted counts
actual <- as.numeric(paste(test_rf$hit))
rf_predicted <- as.numeric(paste(test_rf$predicted.value))
confmat   <- table(rf_predicted, actual)
print(confmat)

actual_classification <- factor(c(0, 0, 1, 1))
predicted_classification <- factor(c(0, 1, 0, 1))
Y      <- c(confmat[1,1], confmat[2,1],
            confmat[1,2], confmat[2,2])
df <- data.frame(actual_classification, predicted_classification, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = actual_classification, y = predicted_classification)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "lightblue") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Random Forest Confusion Matrix")


TP <- confmat[2,2]; FP <- confmat[2,1]; FN <- confmat[1,2]; TN <- confmat[1,1]
Accuracy = (TP+TN)/(TP+FP+FN+TN)
Precision = (TP)/(TP+FP)
Recall = (TP)/(TP+FN)
F1.Score = 2*(Recall * Precision) / (Recall + Precision)
