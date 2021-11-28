# Federal Aviation Administration (FAA) Data source Combination and Text Mining
The project aims to form the mechanism of data interaction and sharing between heterogeneous data sources in an elaborate system environment. The known issues that need to be addressed to accomplish the project’s objective are the data quality issues, duplicate data, scalability, multiple copies of record from several data sources that leads to unreliable data integration at the end. Therefore, the project study will puzzle out the major challenges of data merging, and therefore basically merge the data from disparate data sources into a single unified view in order to conduct the centralized analysis of the new data set to uncover the valuable insight. In addtion, the project will explore the insight extraction by using Structural Topic Modelling (STM) which is one of the Natural Language Processing (NLP) to highlight the key information from text corpus which is the remark column. 

# Problem Statement 
The Federal Aviation Administration uses numerous data repositories to document aviation related events such as aircraft service difficulty reports, aircraft incidents and accidents reports, and emergency operations network reports, among many others. Since these data are collected and entered by subject matter experts in various lines of business across various times, they are collected and saved in different databases. The databases are not connected and a complete and detailed picture in a unified space does not exist for aviation safety investigation. Having all this data in a unified system is crucial for prompt investigation to identify patterns and hidden cause-effect links. The FAA wishes to combine and merge these disparate data to form a unified view of aviation-related events, and prioritize hazards to prevent any future aviation-related failures, accidents, and incidents in order to maintain high standards of aviation safety.

# Alogorithm: Fuzzy matching alogorithm and Structural Topic Modeling (STM) 
Fuzzy join also known as  similarity join is a binary operation that takes two sets of elements as input and computes a set of similar element-pairs as output . Instead of marking out records as a ‘match’ or ‘non-match’, fuzzy matching identifies the probability that two records actually match based on whether they agree or disagree on the various identifiers. This is different compared to exact joins where records are matched based on the common keys. Fuzzy matching allows us to identify non-exact matches of target items. Generally, fuzzy matching is an algorithm for linking text to similar text.

Structural Topic Modelling (STM) is a model which is used to estimate a topic model and determine its relationship to document-level metadata. ISTM provides a comprehensive understanding of which variables are linked with which features of the text. The document-topic and topic-word distributions yield documents that have metadata associated with them. A topic is a combination of words that probably belong to the associated topic and a document is a mixture of multiple topics. The topic proportions for all topics within a document sum to one and the sum of word probabilities for each topic is also one. Topical prevalence helps determine how much of the given document is associated with a particular topic. On the other hand, topical content refers to the words that make up a topic.

# STM parallel workflow diagram
The diagram below represents the parallel workflow in the STM algorithm. The STM algorithm ingests the text or description column along with the associated metadata and it will pass it to textProcessor and then to a utility function (prepDocuments). This function automatically removes infrequent terms based on the set parameters. The output documents and metadata will be used for analysis and the results will be estimated for both topical prevalence and topical contents. The next step as shown in the STM parallel workflow diagram, will be the model selection and search, and understanding the topics and labeling them. Finally the results can be visualized based on topics’ prevalence
![STM diagram](https://user-images.githubusercontent.com/61568065/116450855-cbb9a980-a829-11eb-98da-c36dd07dd2f2.PNG)

# Top topics by prevalence in SDR, AID and EON
The visulizations to understand the topic prevalence in the text corpus of each dataset, and which words contribute to each topic.
![SDR vis](https://user-images.githubusercontent.com/61568065/116951025-d2468780-ac54-11eb-85f0-f94351bb28b8.png)
![AID vis](https://user-images.githubusercontent.com/61568065/116952322-60703d00-ac58-11eb-9819-a93b71272524.png)
![EON vis](https://user-images.githubusercontent.com/61568065/116952420-a1685180-ac58-11eb-880c-7c360e54c9ec.png)

# Requirements
Rerquired Packages:

Python
* Python 2.7+
* NumPy 1.10+
* Fuzzy matcher

R 
* STM
* TM
* Stminsights
* Lubridate
* wordclod

# The Process of Execution: Python and R
* (1) Run the 'Required Libraries' section
* (2) Upload 'AID-a_file', 'AID-e_file', 'SDR', 'EON' year-wise datasets
* (3) Run 'AID Data Analysis' section
* (4) Run 'E-File AID Data Analysis' section
* (5) Run 'SDR Data Analysis' section
* (6) Run 'EON Data Analysis' section
* (7) Run 'STM.csv to be used in R' section and upload the individual csv files of AID, SDR and EON to the working directory of R Code
* (8) After running the R code, upload the csv files obtained for each dataset AID, SDR and EON and then run 'Merging of Keywords Column from R'
* (9) Run 'Fuzzy Merging' section