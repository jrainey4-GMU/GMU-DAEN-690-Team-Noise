# Federal Aviation Administration (FAA) Data Source Cross-Validation & Natural Language Processing (NLP)
The project aims to provide a repeatable analytic product that allows the FAA to better understand the Unmanned Aircraft System (UAS) data available. The known issues that need to be addressed to accomplish the projects objective are the data quality issues, inconsistent reporting, and lack of cross-validation data (ASRS database). Before the data can be used for cross validation or NLP, the large "Summary" text blog must be cleaned and distributed into usable variables. In addition, we will join in other lookup tables to provide additional information present in the original data source such as supporting aircraft information. After the data is cleaned and curated, the project will explore cross validation to test whether data records appear true or false and NLP methods (Fuzzy Method Algorithm, RegEx, Word Embedding) to find further insights in the data. 

# Importance of problem 
Being able to determine the extent of noise over actuals from the database will allow FAA to better understand the restrictions that they must govern to improve risk safety. Validation across different databases will help narrow down the understanding of reports that may be considered noise.

# Problem Statement 
The goal of our problem is to validate the information from the FAA UAS Sightings compared to other databases. A credible database that will be evaluated is the NASA Aviation Safety Reporting System (ASRS) which reports confidential reports of aviation traffic. This will help the FAA establish a mechanism to ensure that FAAS management of small UAS safety risks is able to be applied better to the agency’s policies.

# Alogorithm: Fuzzy matching alogorithm
**Placeholder if we use Fuzzy Matching, this blurp was taken from another project so will need word smithing if used**
Fuzzy join also known as  similarity join is a binary operation that takes two sets of elements as input and computes a set of similar element-pairs as output . Instead of marking out records as a ‘match’ or ‘non-match’, fuzzy matching identifies the probability that two records actually match based on whether they agree or disagree on the various identifiers. This is different compared to exact joins where records are matched based on the common keys. Fuzzy matching allows us to identify non-exact matches of target items. Generally, fuzzy matching is an algorithm for linking text to similar text.

# Data workflow diagram
**Could probably use some more fluff**
The diagram below represents the data workflow we used from ingesting the raw data through outputing the final product.

![Workflow Diagram](https://github.com/jrainey4-GMU/GMU-DAEN-690-Team-Noise/blob/0a43aa42314a6dd60c0a38e644e42275d545184e/Workflow%20Diagram.png)


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