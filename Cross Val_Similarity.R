library(dplyr)
library(tidyverse)
#NLP processing 
library(text2vec)

#set working directory where the files are
setwd('C:\\Users\\jrbrz\\Desktop\\690\\assignments')
#FAA data
faa <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split.csv') %>% 
    select("X", "Summary", 'Year', 'Month', 'State' )
#ASRS data
asrs <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_ASRS_filtered.csv') %>% 
    select("Narrative", "Synopsis", 'Year', 'Month', 'State') 

##### filtering the data set will screw up merging in the end. 
#adds a column for the number of characters in a report
###faa['length'] <- apply(faa['Summary'],2,nchar)
#filters out the reports that have less than 30 characters
###faa <- faa %>% filter(length > 30)

###########################################################################
### SIMILARITY TESTING ### 

###### creating our working vocabulary based off all FAA reports + ASRS narrative and synopsis 
###### removing stop words and infrequent words

#combining faa and ASRS text summaries to be used to create vocabulary below
# vocabulary will be used in the similarity model
a <- data.frame(asrs['Narrative'])
names(a) <- 'Summary'
vocab_df <- bind_rows(faa['Summary'], a['Summary'])

a <- data.frame(asrs['Synopsis'])
names(a) <- 'Summary'
vocab_df<- bind_rows(vocab_df, a['Summary'])


stopWords <- c("REPORTED", "FEET", "NOTIFIED", "NOTIFIED.", "ADVISED", 
               "APPROXIMATELY","INFORMATION", "APPROX", "INCIDENT", "TYPE", 
               "AVIATION","UNKNOWN", "UNK", "UNKN", "SAID", "NOTIFICATION", 
               'AT', 'AND','TO', 'A', 'THE', 'OF', 'WAS', "I", "ON", "IN")#, "uas")

# creating the vocabulary in model form
it <- text2vec::itoken(vocab_df[,'Summary'], progressbar = FALSE)
v  <-  text2vec::create_vocabulary(it, stopwords = stopWords)
v  <-  text2vec::prune_vocabulary(v, term_count_min = 5)
vectorizer  <-  text2vec::vocab_vectorizer(v)

# variables for looping
years  <-  seq(2014, 2021)
months <-  seq(1,12)

#used to keep row count on the original FAA data set so that the matches can be 
#updated.
rowA  <-  0
rowB <-  0
# similarity testing data will be held in this dataframe
hits <-  data.frame()
#testing similarity for all the records
for (y in years){
    #filtering the data to process the models in groups.
    #ASRS data does not provide the day of month so it has to be processed as months
    if (y == 2014) {
        for (m in seq(11,12)) {
            #temp data frames for the specific year and month
            tempFAA <- faa %>% filter(Year == y & Month == m)
            tempASRS <- asrs %>% filter(Year == y & Month == m)
            states <- intersect(tempFAA$State, tempASRS$State)
            if (length(states) > 0) {
                for (s in states) {
                    tempFAA <- faa %>% filter(Year == y & Month == m)
                    tempASRS <- asrs %>% filter(Year == y & Month == m)
                    for (i in 1:nrow(tempFAA)){
                        for(j in 2:ncol(tempFAA)){
                            if (tempFAA[i,5]!=s){
                                tempFAA[i,j]= ""
                            }
                        }
                    }
                    
                    for (i in 1:nrow(tempASRS)){
                        for(j in 1:ncol(tempASRS)){
                            if (tempASRS[i,5]!=s){
                                tempASRS[i,j]= ""
                            }
                        }
                    }
                    
                    #tempFAA <- tempFAA %>% filter(State == s)
                    #tempASRS <- tempASRS %>% filter(State == s)
                    #if (dim(tempASRS)[1] > 0 & dim(tempFAA)[1] > 0) {
                    # creating the text 3(FAA-Summary and ASRS-Narrative and Synopsis) documents
                    it1 <- text2vec::itoken(tempFAA$Summary,progressbar= FALSE)
                    dtm1 <- text2vec::create_dtm(it1, vectorizer)
                    it2 <- text2vec::itoken(tempASRS$Synopsis,progressbar= FALSE)
                    dtm2 <- text2vec::create_dtm(it2, vectorizer)
                    it3 <- text2vec::itoken(tempASRS$Narrative,progressbar= FALSE)
                    dtm3 <- text2vec::create_dtm(it3, vectorizer)
                    
                    # testing similarity for FAA summary/ASRS Narrative and FAA summary/ASRS Synopsis
                    similarity1 <- text2vec::sim2(dtm1, dtm2, method = "cosine", norm = "l2")
                    similarity2 <- text2vec::sim2(dtm1, dtm3, method = "cosine", norm = "l2")
                    
                    #returns the data on from the similarity tests for the FAA/ASRS synopsis check
                    for (i in 1:ncol(similarity1)){
                        if (max(similarity1[,i])>0){
                            val1 <- match(max(similarity1[,i]), similarity1[,i]) + rowA
                            hits[val1, 'X'] = val1 - 1
                            #hits[val1, 'faa'] = tempFAA[val1 ,"Summary"]
                            hits[val1, 'faa'] = faa[val1 ,"Summary"]
                            hits[val1, 'asrsSynopsis'] = tempASRS[i, "Synopsis"]
                            hits[val1, 'synopsisHit'] = 1
                            hits[val1, 'synopsisPct'] = max(similarity1[,i])
                            hits[val1,'year'] = y
                            hits[val1,'month'] = m
                        } else {
                            hits[val1, 'X'] = val1 - 1
                        }
                    }
                    #returns the data on from the similarity tests for the FAA/ASRS narrative check
                    for (i in 1:ncol(similarity2)){
                        if (max(similarity2[,i])>0){
                            val2 <- match(max(similarity2[,i]), similarity2[,i]) + rowB
                            hits[val2, 'X'] = val2 - 1
                            #hits[val2, 'faa'] = tempFAA[val2,"Summary"]
                            hits[val2, 'faa'] = faa[val2,"Summary"]
                            hits[val2, 'asrsNarrative'] = tempASRS[i,"Narrative"]
                            hits[val2, 'narrativeHit'] = 1
                            hits[val2, 'narrativeHitPct'] = max(similarity2[,i])
                            hits[val2,'year'] = y
                            hits[val2,'month'] = m
                        } else {
                            hits[val2, 'X'] = val2 - 1
                        }
                    }
                }
            }
                #rowA <- rowA + nrow(similarity1)
                #rowB <- rowB + nrow(similarity2)
                rowA <- rowA + nrow(tempFAA)
                rowB <- rowB + nrow(tempFAA)
            
        }
    } else if (y == 2021) {
        for (m in seq(1,6)) {
            #temp data frames for the specific year and month
            tempFAA <- faa %>% filter(Year == y & Month == m)
            tempASRS <- asrs %>% filter(Year == y & Month == m)
            states <- intersect(tempFAA$State, tempASRS$State)
            if (length(states) > 0) {
            for (s in states) {
                tempFAA <- faa %>% filter(Year == y & Month == m)
                tempASRS <- asrs %>% filter(Year == y & Month == m)
                for (i in 1:nrow(tempFAA)){
                    for(j in 2:ncol(tempFAA)){
                        if (tempFAA[i,5]!=s){
                            tempFAA[i,j]= ""
                        }
                    }
                }
                
                for (i in 1:nrow(tempASRS)){
                    for(j in 1:ncol(tempASRS)){
                        if (tempASRS[i,5]!=s){
                            tempASRS[i,j]= ""
                        }
                    }
                }
                
                #tempFAA <- tempFAA %>% filter(State == s)
                #tempASRS <- tempASRS %>% filter(State == s)
                #if (dim(tempASRS)[1] > 0 & dim(tempFAA)[1] > 0) {
                # creating the text 3(FAA-Summary and ASRS-Narrative and Synopsis) documents
                it1 <- text2vec::itoken(tempFAA$Summary,progressbar= FALSE)
                dtm1 <- text2vec::create_dtm(it1, vectorizer)
                it2 <- text2vec::itoken(tempASRS$Synopsis,progressbar= FALSE)
                dtm2 <- text2vec::create_dtm(it2, vectorizer)
                it3 <- text2vec::itoken(tempASRS$Narrative,progressbar= FALSE)
                dtm3 <- text2vec::create_dtm(it3, vectorizer)
                
                # testing similarity for FAA summary/ASRS Narrative and FAA summary/ASRS Synopsis
                similarity1 <- text2vec::sim2(dtm1, dtm2, method = "cosine", norm = "l2")
                similarity2 <- text2vec::sim2(dtm1, dtm3, method = "cosine", norm = "l2")
                
                #returns the data on from the similarity tests for the FAA/ASRS synopsis check
                for (i in 1:ncol(similarity1)){
                    if (max(similarity1[,i])>0){
                        val1 <- match(max(similarity1[,i]), similarity1[,i]) + rowA
                        hits[val1, 'X'] = val1 - 1
                        #hits[val1, 'faa'] = tempFAA[val1 ,"Summary"]
                        hits[val1, 'faa'] = faa[val1 ,"Summary"]
                        hits[val1, 'asrsSynopsis'] = tempASRS[i, "Synopsis"]
                        hits[val1, 'synopsisHit'] = 1
                        hits[val1, 'synopsisPct'] = max(similarity1[,i])
                        hits[val1,'year'] = y
                        hits[val1,'month'] = m
                    } else {
                        hits[val1, 'X'] = val1 - 1
                    }
                }
                #returns the data on from the similarity tests for the FAA/ASRS narrative check
                for (i in 1:ncol(similarity2)){
                    if (max(similarity2[,i])>0){
                        val2 <- match(max(similarity2[,i]), similarity2[,i]) + rowB
                        hits[val2, 'X'] = val2 - 1
                        #hits[val2, 'faa'] = tempFAA[val2,"Summary"]
                        hits[val2, 'faa'] = faa[val2,"Summary"]
                        hits[val2, 'asrsNarrative'] = tempASRS[i,"Narrative"]
                        hits[val2, 'narrativeHit'] = 1
                        hits[val2, 'narrativeHitPct'] = max(similarity2[,i])
                        hits[val2,'year'] = y
                        hits[val2,'month'] = m
                    } else {
                        hits[val2, 'X'] = val2 - 1
                    }
                }
            }
            }
            #rowA <- rowA + nrow(similarity1)
            #rowB <- rowB + nrow(similarity2)
            rowA <- rowA + nrow(tempFAA)
            rowB <- rowB + nrow(tempFAA)
        }
            
    } else {
        for (m in seq(1,12)) {
            #temp data frames for the specific year and month
            tempFAA <- faa %>% filter(Year == y & Month == m)
            tempASRS <- asrs %>% filter(Year == y & Month == m)
            states <- intersect(tempFAA$State, tempASRS$State)
            if (length(states) > 0) {
            for (s in states) {
                tempFAA <- faa %>% filter(Year == y & Month == m)
                tempASRS <- asrs %>% filter(Year == y & Month == m)
                for (i in 1:nrow(tempFAA)){
                    for(j in 2:ncol(tempFAA)){
                        if (tempFAA[i,5]!=s){
                            tempFAA[i,j]= ""
                        }
                    }
                }
                
                for (i in 1:nrow(tempASRS)){
                    for(j in 1:ncol(tempASRS)){
                        if (tempASRS[i,5]!=s){
                            tempASRS[i,j]= ""
                        }
                    }
                }
                
                #tempFAA <- tempFAA %>% filter(State == s)
                #tempASRS <- tempASRS %>% filter(State == s)
                #if (dim(tempASRS)[1] > 0 & dim(tempFAA)[1] > 0) {
                # creating the text 3(FAA-Summary and ASRS-Narrative and Synopsis) documents
                it1 <- text2vec::itoken(tempFAA$Summary,progressbar= FALSE)
                dtm1 <- text2vec::create_dtm(it1, vectorizer)
                it2 <- text2vec::itoken(tempASRS$Synopsis,progressbar= FALSE)
                dtm2 <- text2vec::create_dtm(it2, vectorizer)
                it3 <- text2vec::itoken(tempASRS$Narrative,progressbar= FALSE)
                dtm3 <- text2vec::create_dtm(it3, vectorizer)
                
                # testing similarity for FAA summary/ASRS Narrative and FAA summary/ASRS Synopsis
                similarity1 <- text2vec::sim2(dtm1, dtm2, method = "cosine", norm = "l2")
                similarity2 <- text2vec::sim2(dtm1, dtm3, method = "cosine", norm = "l2")
                
                #returns the data on from the similarity tests for the FAA/ASRS synopsis check
                for (i in 1:ncol(similarity1)){
                    if (max(similarity1[,i])>0){
                        val1 <- match(max(similarity1[,i]), similarity1[,i]) + rowA
                        hits[val1, 'X'] = val1 - 1
                        #hits[val1, 'faa'] = tempFAA[val1 ,"Summary"]
                        hits[val1, 'faa'] = faa[val1 ,"Summary"]
                        hits[val1, 'asrsSynopsis'] = tempASRS[i, "Synopsis"]
                        hits[val1, 'synopsisHit'] = 1
                        hits[val1, 'synopsisPct'] = max(similarity1[,i])
                        hits[val1,'year'] = y
                        hits[val1,'month'] = m
                    } else {
                        hits[val1, 'X'] = val1 - 1
                    }
                }
                #returns the data on from the similarity tests for the FAA/ASRS narrative check
                for (i in 1:ncol(similarity2)){
                    if (max(similarity2[,i])>0){
                        val2 <- match(max(similarity2[,i]), similarity2[,i]) + rowB
                        hits[val2, 'X'] = val2 - 1
                        #hits[val2, 'faa'] = tempFAA[val2,"Summary"]
                        hits[val2, 'faa'] = faa[val2,"Summary"]
                        hits[val2, 'asrsNarrative'] = tempASRS[i,"Narrative"]
                        hits[val2, 'v'] = 1
                        hits[val2, 'narrativeHitPct'] = max(similarity2[,i])
                        hits[val2,'year'] = y
                        hits[val2,'month'] = m
                    } else {
                        hits[val2, 'X'] = val2 - 1
                    }
                }
            }
            }
            #rowA <- rowA + nrow(similarity1)
            #rowB <- rowB + nrow(similarity2)
            rowA <- rowA + nrow(tempFAA)
            rowB <- rowB + nrow(tempFAA)
        }
    }
}
hits <- hits %>%
    mutate(hit = if_else(narrativeHit == 1 | synopsisHit == 1, 1, 0, missing= 0))
write.csv(hits, file='DATA_crossVal_hits.csv',row.names = FALSE)

###merging the htis back to the FAA_split dataset 

#adding single validated HIT column for predicitve modeling
master <- read.csv('C:\\Users\\jrbrz\\Desktop\\690\\assignments\\DATA_FAA_split.csv')
faa <- hits %>%
    select(X, hit)
faa <- merge(y=faa, x=master, by="X", all.x = TRUE) %>%
    mutate(hit = if_else(hit == 1, 1, 0, missing= 0))
write.csv(faa, file='DATA_FAA_split_validated.csv', row.names = FALSE)


