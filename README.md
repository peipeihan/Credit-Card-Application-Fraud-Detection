# Credit-Card-Application-Fraud-Detection
The purpose of this project is to identify unusual items by giving a fraud score to each record in product applications using unsupervised learning models. The dataset was downloaded from the blackboard showing the application’s detail information including date, applicant’s name, SSN, address, date of birth and phone number of 100,000 records. Anomalous records per fraud model are more likely to be a signal of identity theft due to the limitation of the size of our data.


To identify potential fraud, time flow is critical and only the observations in the past can be utilized to build the model. Variables are built based on a time window. The appearance of frivolous values, which will be meaningless noise in the model, is checked and replaced with neutral values. The fix time window of 3 and 7 days together with all past period are applied in this project. Identifier variables which include both full name and date of birth, different combinations of counting for different time window and unique variables with the same combinations of other variables are created. By adding these additional variables in the original dataset, we tried to ferret out some 

hidden information and relationship among each variable.
