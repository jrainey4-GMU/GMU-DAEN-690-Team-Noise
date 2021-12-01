# Federal Aviation Administration (FAA) Data Source Cross-Validation & Natural Language Processing (NLP)
The project aims to provide a repeatable analytic product that allows the FAA to better understand the Unmanned Aircraft System (UAS) data available. The known issues that need to be addressed to accomplish the projects’ objective are the data quality issues, inconsistent reporting, and lack of cross-validation data (ASRS database). Before the data can be used for cross validation or NLP, the large "Summary" text blog must be cleaned and distributed into usable variables. In addition, we will join in other lookup tables to provide additional information present in the original data source such as supporting aircraft information. After the data is cleaned and curated, the project will explore cross validation to test whether data records appear true or false and NLP methods (Fuzzy Method Algorithm, RegEx, Word Embedding) to find further insights in the data. 

# Importance of problem 
Being able to determine the extent of noise over actuals from the database will allow FAA to better understand the restrictions that they must govern to improve risk safety. Validation across different databases will help narrow down the understanding of reports that may be considered noise.

# Problem Statement 
The goal of our problem is to validate the information from the FAA UAS Sightings compared to other databases. A credible database that will be evaluated is the NASA Aviation Safety Reporting System (ASRS) which reports confidential reports of aviation traffic. This will help the FAA establish a mechanism to ensure that FAAS management of small UAS safety risks is able to be applied better to the agency’s policies.

# Data workflow diagram
The diagram below represents the data workflow we used from ingesting the raw data through outputting the final product.

![Workflow Diagram](https://github.com/jrainey4-GMU/GMU-DAEN-690-Team-Noise/blob/0a43aa42314a6dd60c0a38e644e42275d545184e/Workflow%20Diagram.png)


# Requirements
Required Packages:

 Python
* Python 3.8
* pandas
* NumPy
* re
* datetime
* os
* load_workbook from openpyxl
* openpyxl

R 
* tidyverse
* dplyr
* tidytext
* topicmodels
* tm
* gridExtra
* modelplotr
* randomForest
* caret
* text2vec
* InformationValue
* ROCR

# Execution Options
## 1) Cleaning UAS reports for data analysis and reporting
* use historical_data_merge_FAA.py to merge FAA data reports if necessary
* input data to FAA_data processing.py; output will be a cleaned dataset that can be used for analysis and reporting

## 2) Prediction Execution Processes
### a) Starting with validated data and using existing Model to predict
* use historical_data_merge_FAA.py to merge FAA data reports if necessary
* input data to FAA_data processing.py for cleaned and formated data set
* use cleaned data set with topic_model.RDS and predictive_model.RDS to run topic_modeling_prediction.R; this will out put predictions on your new UAS reports

### b) Starting with validated data and no trained model to predict
* use historical_data_merge_FAA.py to merge FAA data reports if necessary
* input data to FAA_data processing.py
* use output from FAA_data processing.py to train a predictive topic model in topic_modeling_training.R
* using topic_model.RDS and predictive_model.RDS from topic_modeling_training.R and the validated dataset, run topic_modeling_prediction.R; this will out put predictions on your new UAS reports

### c) Starting with no data and no trained models to predict
* download desired FAA files from https://www.faa.gov/uas/resources/public_records/uas_sightings_report/ and matching ASRS data from https://akama.arc.nasa.gov/ASRSDBOnline/QueryWizard_Filter.aspx
* save both sets to their own folders
* run their historical merge scripts historical_data_merge_FAA.py and historical_data_merge_ASRS.py
* use output from historical_data_merge_FAA.py to run FAA_data processing.py
* use output from FAA_data processing.py and historical_data_merge_ASRS.py to run Cross Val_Similarity.R
* using the validated output from Cross Val_Similarity.R, train a predictive topic model in topic_modeling_training.R
* using topic_model.RDS and predictive_model.RDS from topic_modeling_training.R and the validated dataset from Cross Val_Similarity.R, run topic_modeling_prediction.R; this will out put predictions on your new UAS reports



