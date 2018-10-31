# Date 14-Oct-2018 Dev: Yogesh Shinde
# https://www.linkedin.com/in/yogesh-sql-msbi-datascience/
# Problem Statement: We have to identify high-risk diabetic patients through risk stratification. 
#                    This will help the payer to decide what are the right intervention programs for these patients.

# We need to perform 3 tasks.
# A. Data preparation
#                  1. Remove redundant variables.
#                  2. Check for missing values and treat them accordingly.
#                  3. Scale numeric attributes and create dummy variables for categorical ones.
#                  4. Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
#                  5. Create the derived metric 'comorbidity', according to the following scheme -

# B. Data Exploration
#                 1. Perform basic data exploration for some categorical attributes
#                 2. Perform basic data exploration for some numerical attributes

# C. Model Building
#                 1.  Divide your data into training and testing dataset
#                 2.  Compare the performance of at least two algorithms and decide which one to use for predicting risk of readmission for the patient
#                 3.  Stratify your population into 3 risk buckets:
#                           High risk (Probability of readmission >0.7)
#                           Medium risk (0.3 < Probability of readmission < 0.7)
#                           Low risk (Probability of readmission < 0.3)


# Set the path
setwd("F:/UpGrad/Course6/Assignment")


# call the libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
# library(doParallel)
library(car)
# library(tidyverse)

# Import the CSV file into DataFrame
Risk_Stratification_DF <- read.csv("diabetic_data.csv",stringsAsFactors = TRUE)


# Check if upload looks good
head(Risk_Stratification_DF)
# Observed question mark in below columns
# weight / payer_code / medical_specialty / diag_2 / 

#Check the structure
str(Risk_Stratification_DF)
# DF contians 101766 obs. of  50 variables


# **************************************************************************************
#                                 A. Data preparation
# **************************************************************************************


# Check for duplicate values
which(duplicated(Risk_Stratification_DF))
# integer(0)
# OR
# sum(duplicated(Risk_Stratification_DF$encounter_id))
# [1] 0
# OR
length(unique(tolower(Risk_Stratification_DF$encounter_id)))  
# 101766 count comes same as total number of rows hence no ducplicate
# OR
nrow(Risk_Stratification_DF) == length(unique(tolower(Risk_Stratification_DF$encounter_id)))
# TRUE

# #No duplicate records found in DF.

# ----------------------------------------------------------------------------------------------------------------


# Check for NA values
sapply(Risk_Stratification_DF, function(x) sum(is.na(x)))
# Here you can see NO NA values by each column
# OR
# sum(is.na(Risk_Stratification_DF)) -- Here you will get total count of NA values i.e. 0
# 0


# ----------------------------------------------------------------------------------------------------------------

# Task: Remove redundant variables 

# As we seen the below columns contains question marks; we will check how many records are there for ?
# weight / payer_code / medical_specialty / diag_2 / 

# install.packages("sqldf")
library(sqldf)


count_weight <- sqldf("select count(*) from Risk_Stratification_DF where weight = '?'")
count_weight
# 98569
# this is almost 97%
# OR
# round(prop.table(table(Risk_Stratification_DF$weight)),4)*100

count_payer_code <- sqldf("select count(*) from Risk_Stratification_DF where payer_code = '?'")
count_payer_code
# 40256
# this is almost 40%
# OR
# round(prop.table(table(Risk_Stratification_DF$payer_code)),4)*100

count_medical_specialty <- sqldf("select count(*) from Risk_Stratification_DF where medical_specialty = '?'")
count_medical_specialty
# 40256
# this is almost 50%
# OR
# round(prop.table(table(Risk_Stratification_DF$medical_specialty)),4)*100

count_diag_2 <- sqldf("select count(*) from Risk_Stratification_DF where diag_2 = '?'")
count_diag_2
# 358
# this is very small 0.35%
# OR
# round(prop.table(table(Risk_Stratification_DF$diag_2)),4)*100


# 24 features for medications
# see the pattern of the data of all medications

round(prop.table(table(Risk_Stratification_DF$metformin)),4)*100
# Down     No Steady     Up 
# 0.57  80.36  18.03   1.05 


round(prop.table(table(Risk_Stratification_DF$repaglinide)),4)*100
# Down     No Steady     Up 
# 0.04  98.49   1.36   0.11 

round(prop.table(table(Risk_Stratification_DF$nateglinide)),4)*100
# Down     No Steady     Up 
# 0.01  99.31   0.66   0.02 


round(prop.table(table(Risk_Stratification_DF$chlorpropamide)),4)*100
# Down     No Steady     Up 
# 0.00  99.92   0.08   0.01 


round(prop.table(table(Risk_Stratification_DF$glimepiride)),4)*100
# Down     No Steady     Up 
# 0.19  94.90   4.59   0.32 


round(prop.table(table(Risk_Stratification_DF$acetohexamide)),4)*100
# No Steady 
# 100      0 


round(prop.table(table(Risk_Stratification_DF$glipizide)),4)*100
# Down     No Steady     Up 
# 0.55  87.53  11.16   0.76 


round(prop.table(table(Risk_Stratification_DF$glyburide)),4)*100
# Down     No Steady     Up 
# 0.55  89.53   9.11   0.80


round(prop.table(table(Risk_Stratification_DF$tolbutamide)),4)*100
# No Steady 
# 99.98   0.02 


round(prop.table(table(Risk_Stratification_DF$pioglitazone)),4)*100
# Down     No Steady     Up 
# 0.12  92.80   6.85   0.23 


round(prop.table(table(Risk_Stratification_DF$rosiglitazone)),4)*100
# Down     No Steady     Up 
# 0.09  93.75   5.99   0.17

round(prop.table(table(Risk_Stratification_DF$acarbose)),4)*100
# Down     No Steady     Up 
# 0.00  99.70   0.29   0.01 


round(prop.table(table(Risk_Stratification_DF$miglitol)),4)*100
# Down     No Steady     Up 
# 0.00  99.96   0.03   0.00


round(prop.table(table(Risk_Stratification_DF$troglitazone)),4)*100
# No Steady 
# 100      0


round(prop.table(table(Risk_Stratification_DF$tolazamide)),4)*100
# No Steady     Up 
# 99.96   0.04   0.00 


round(prop.table(table(Risk_Stratification_DF$examide)),4)*100
# No 
# 100 


round(prop.table(table(Risk_Stratification_DF$citoglipton)),4)*100
# No 
# 100 


round(prop.table(table(Risk_Stratification_DF$insulin)),4)*100
# Down     No Steady     Up 
# 12.01  46.56  30.31  11.12 


round(prop.table(table(Risk_Stratification_DF$glyburide.metformin)),4)*100
# Down     No Steady     Up 
# 0.01  99.31   0.68   0.01 


round(prop.table(table(Risk_Stratification_DF$glipizide.metformin)),4)*100
# No Steady 
# 99.99   0.01 


round(prop.table(table(Risk_Stratification_DF$glimepiride.pioglitazone)),4)*100
# No Steady 
# 100      0 

round(prop.table(table(Risk_Stratification_DF$metformin.rosiglitazone)),4)*100
# No Steady 
# 100      0 


round(prop.table(table(Risk_Stratification_DF$metformin.pioglitazone)),4)*100
# No Steady 
# 100      0 


# From above data it is clear only insulin is scattered data others are having values only for one option.
# hence we can remove those columns except insulin



# also below columns are having data particularly for single value and we can remove those

round(prop.table(table(Risk_Stratification_DF$max_glu_serum)),4)*100
# 200  >300  None  Norm 
# 1.46  1.24 94.75  2.55 

round(prop.table(table(Risk_Stratification_DF$A1Cresult)),4)*100
# >7    >8  None  Norm 
# 3.75  8.07 83.28  4.90 


.
# As we have seen above that columns are having same data or unwanted data hence we can remove redundant columns
Risk_Stratification_DF[,c("weight", "payer_code", "medical_specialty", "metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton", "glyburide.metformin", "glipizide.metformin", "glimepiride.pioglitazone", "metformin.rosiglitazone", "metformin.pioglitazone", "max_glu_serum", "A1Cresult")] <- list(NULL)

head(Risk_Stratification_DF)
# ----------------------------------------------------------------------------------------------------------------


# Task: Check for missing values and treat them accordingly.

colnames(Risk_Stratification_DF)

count_encounter_id <- sqldf("select count(*) from Risk_Stratification_DF where encounter_id = '?'")
count_encounter_id
# No question mark present


count_patient_nbr <- sqldf("select count(*) from Risk_Stratification_DF where patient_nbr = '?'")
count_patient_nbr
# No question mark present

count_race <- sqldf("select count(*) from Risk_Stratification_DF where race = '?'")
count_race
# 2273 question mark present

count_gender <- sqldf("select count(*) from Risk_Stratification_DF where gender = '?'")
count_gender
# No question mark present


count_age <- sqldf("select count(*) from Risk_Stratification_DF where age = '?'")
count_age
# No question mark present


count_admission_type_id <- sqldf("select count(*) from Risk_Stratification_DF where admission_type_id = '?'")
count_admission_type_id
# No question mark present

count_discharge_disposition_id <- sqldf("select count(*) from Risk_Stratification_DF where discharge_disposition_id = '?'")
count_discharge_disposition_id
# No question mark present

count_admission_source_id <- sqldf("select count(*) from Risk_Stratification_DF where admission_source_id = '?'")
count_admission_source_id
# No question mark present

count_num_lab_procedures <- sqldf("select count(*) from Risk_Stratification_DF where num_lab_procedures = '?'")
count_num_lab_procedures
# No question mark present

count_num_procedures <- sqldf("select count(*) from Risk_Stratification_DF where num_procedures = '?'")
count_num_procedures
# No question mark present

count_num_medications <- sqldf("select count(*) from Risk_Stratification_DF where num_medications = '?'")
count_num_medications
# No question mark present


count_number_outpatient <- sqldf("select count(*) from Risk_Stratification_DF where number_outpatient = '?'")
count_number_outpatient
# No question mark present

count_number_emergency <- sqldf("select count(*) from Risk_Stratification_DF where number_emergency = '?'")
count_number_emergency
# No question mark present

count_number_inpatient <- sqldf("select count(*) from Risk_Stratification_DF where number_inpatient = '?'")
count_number_inpatient
# No question mark present


count_diag_1 <- sqldf("select count(*) from Risk_Stratification_DF where diag_1 = '?'")
count_diag_1
# 21 question mark present

count_diag_2 <- sqldf("select count(*) from Risk_Stratification_DF where diag_2 = '?'")
count_diag_2
# 358 question mark present

count_diag_3 <- sqldf("select count(*) from Risk_Stratification_DF where diag_3 = '?'")
count_diag_3
# 1423 question mark present


count_number_diagnoses <- sqldf("select count(*) from Risk_Stratification_DF where number_diagnoses = '?'")
count_number_diagnoses
# No question mark present


count_number_insulin <- sqldf("select count(*) from Risk_Stratification_DF where insulin = '?'")
count_number_insulin
# No question mark present

count_number_change <- sqldf("select count(*) from Risk_Stratification_DF where change = '?'")
count_number_change
# No question mark present


count_number_diabetesMed <- sqldf("select count(*) from Risk_Stratification_DF where diabetesMed = '?'")
count_number_diabetesMed
# No question mark present

count_number_readmitted <- sqldf("select count(*) from Risk_Stratification_DF where readmitted = '?'")
count_number_readmitted
# No question mark present



# We can delete records with ?
# race, diag_1, diag_2, diag_3
nrow(Risk_Stratification_DF)
# 101766

Risk_Stratification_DF <- Risk_Stratification_DF[which(!(Risk_Stratification_DF$race %in% "?")),]
nrow(Risk_Stratification_DF)
# 99493

Risk_Stratification_DF <- Risk_Stratification_DF[which(!(Risk_Stratification_DF$diag_1 %in% "?")),]
nrow(Risk_Stratification_DF)
# 99474

Risk_Stratification_DF <- Risk_Stratification_DF[which(!(Risk_Stratification_DF$diag_2 %in% "?")),]
nrow(Risk_Stratification_DF)
# 99139

Risk_Stratification_DF <- Risk_Stratification_DF[which(!(Risk_Stratification_DF$diag_3 %in% "?")),]
nrow(Risk_Stratification_DF)
# 98053

# you can use query like this as well
# sqldf(c("Delete from Risk_Stratification_DF where race = '?'", "select * from main.Risk_Stratification_DF"))

head(Risk_Stratification_DF)


# Invalid Gender type Unknown/Invalid 
round(prop.table(table(Risk_Stratification_DF$gender)),4)*100
# Female            Male Unknown/Invalid 
# 53.88           46.12            0.00 

# Remove Invalid Gender records
Risk_Stratification_DF <- Risk_Stratification_DF[which(!(Risk_Stratification_DF$gender %in% "Unknown/Invalid")),]
nrow(Risk_Stratification_DF)
# 98052

# ----------------------------------------------------------------------------------------------------------------

# Task: Scale numeric attributes and create dummy variables for categorical ones.

# convert factors with 2 levels to numerical variables
Risk_Stratification_DF$gender <- factor(Risk_Stratification_DF$gender)
levels(Risk_Stratification_DF$gender)<-c(1,0)
Risk_Stratification_DF$gender<- as.numeric(levels(Risk_Stratification_DF$gender))[Risk_Stratification_DF$gender]



Risk_Stratification_DF$change <- factor(Risk_Stratification_DF$change)
levels(Risk_Stratification_DF$change)<-c(1,0)
Risk_Stratification_DF$change<- as.numeric(levels(Risk_Stratification_DF$change))[Risk_Stratification_DF$change]



Risk_Stratification_DF$diabetesMed <- factor(Risk_Stratification_DF$diabetesMed)
levels(Risk_Stratification_DF$diabetesMed)<-c(1,0)
Risk_Stratification_DF$diabetesMed<- as.numeric(levels(Risk_Stratification_DF$diabetesMed))[Risk_Stratification_DF$diabetesMed]

# colnames(Risk_Stratification_DF)


dummy_1 <- data.frame(model.matrix( ~race, data = Risk_Stratification_DF))
dummy_1 <- dummy_1[,-1]
#Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,-3], dummy_1)
Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,], dummy_1)
# colnames(Risk_Stratification_DF)


dummy_2 <- data.frame(model.matrix( ~age, data = Risk_Stratification_DF))
dummy_2 <- dummy_2[,-1]
# Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,-4], dummy_2)
Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,], dummy_2)
# colnames(Risk_Stratification_DF)



dummy_3 <- data.frame(model.matrix( ~insulin, data = Risk_Stratification_DF))
dummy_3 <- dummy_3[,-1]
# Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,-18], dummy_3)
Risk_Stratification_DF <- cbind(Risk_Stratification_DF[,], dummy_3)
# colnames(Risk_Stratification_DF)


Risk_Stratification_DF_2 <- Risk_Stratification_DF
# Risk_Stratification_DF <- Risk_Stratification_DF_2

# ----------------------------------------------------------------------------------------------------------------

# Task: Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".

Risk_Stratification_DF$readmitted <- ifelse(Risk_Stratification_DF$readmitted %in% c("<30",">30"),'yes','no')
# View(Risk_Stratification_DF$readmitted)


Risk_Stratification_DF$readmitted <- factor(Risk_Stratification_DF$readmitted)
levels(Risk_Stratification_DF$readmitted)<-c(1,0)
Risk_Stratification_DF$readmitted<- as.numeric(levels(Risk_Stratification_DF$readmitted))[Risk_Stratification_DF$readmitted]

# View(Risk_Stratification_DF)
# colnames(Risk_Stratification_DF)

# ----------------------------------------------------------------------------------------------------------------

# Task: Create the derived metric 'comorbidity', according to the following scheme -
str(Risk_Stratification_DF)


Risk_Stratification_DF$diag_1 <- as.character(Risk_Stratification_DF$diag_1)
Risk_Stratification_DF$diag_2 <- as.character(Risk_Stratification_DF$diag_2)
Risk_Stratification_DF$diag_3 <- as.character(Risk_Stratification_DF$diag_3)



Risk_Stratification_DF$Comm1 <-
1*(
  1*((as.numeric(Risk_Stratification_DF$diag_1) > 250) &  (as.numeric(Risk_Stratification_DF$diag_1) < 251)) 
|
  1*((as.numeric(Risk_Stratification_DF$diag_2) > 250) &  (as.numeric(Risk_Stratification_DF$diag_2) < 251))
|
  1*((as.numeric(Risk_Stratification_DF$diag_3) > 250) &  (as.numeric(Risk_Stratification_DF$diag_3) < 251))
)

Risk_Stratification_DF$Comm1[is.na(Risk_Stratification_DF$Comm1)] <- 0


Risk_Stratification_DF$Comm2 <-
  1*(
    1*((as.numeric(Risk_Stratification_DF$diag_1) > 389) &  (as.numeric(Risk_Stratification_DF$diag_1) < 460)) 
    |
      1*((as.numeric(Risk_Stratification_DF$diag_2) > 389) &  (as.numeric(Risk_Stratification_DF$diag_2) < 460))
    |
      1*((as.numeric(Risk_Stratification_DF$diag_3) > 389) &  (as.numeric(Risk_Stratification_DF$diag_3) < 460))
  )

Risk_Stratification_DF$Comm2[is.na(Risk_Stratification_DF$Comm2)] <- 0

# View(Risk_Stratification_DF)

# Comorbidity Matrix
# if Diabetes = False and Circulatory Disease = false then Comorbidity = 0
# if Diabetes = True  and Circulatory Disease = false then Comorbidity = 1
# if Diabetes = Fasle and Circulatory Disease = True  then Comorbidity = 2
# if Diabetes = True  and Circulatory Disease = True  then Comorbidity = 3

Risk_Stratification_DF$comorbidity <- ifelse(Risk_Stratification_DF$Comm1 ==0 & Risk_Stratification_DF$Comm2 == 0, 0, 
                                               ifelse(Risk_Stratification_DF$Comm1 == 1 & Risk_Stratification_DF$Comm2 == 0, 1, 
                                                      ifelse(Risk_Stratification_DF$Comm1 ==0 & Risk_Stratification_DF$Comm2 == 1, 2, 3))
                                               )

# colnames(Risk_Stratification_DF)

# Now remove columns diag_1, diag_2, diag_3, Comm1 & Comm2
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "diag_1")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "diag_2")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "diag_3")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "Comm1")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "Comm2")]

# View(Risk_Stratification_DF)

# colnames(Risk_Stratification_DF)

# ----------------------------------------------------------------------------------------------------------------

# Data Exploration


# Let's summarise the data
summary(Risk_Stratification_DF)
colnames(Risk_Stratification_DF)

str(Risk_Stratification_DF)
View(Risk_Stratification_DF)

#see the effect of categorical variables on Readmission 
ggplot(data = Risk_Stratification_DF,aes(race,fill = readmitted)) + geom_bar() + 
  ggtitle("Race wise Readmission")

ggplot(data = Risk_Stratification_DF,aes(gender,fill = readmitted)) + geom_bar() +  
  ggtitle("Gemder wise Readmission")
# 0= Female & 1 = Male

ggplot(data = Risk_Stratification_DF,aes(age,fill = readmitted)) + geom_bar() + 
  ggtitle("Age wise Readmission")


ggplot(data = Risk_Stratification_DF,aes(admission_type_id,fill = readmitted)) + geom_bar() + 
  ggtitle("Admission Type ID wise Readmission")

ggplot(data = Risk_Stratification_DF,aes(insulin,fill = readmitted)) + geom_bar() + 
  ggtitle("Insulin wise Readmission")


ggplot(data = Risk_Stratification_DF,aes(comorbidity,fill = readmitted)) + geom_bar() + 
  ggtitle("Comorbidity wise Readmission")


ggplot(data = Risk_Stratification_DF,aes(change,fill = readmitted)) + geom_bar() + 
  ggtitle("Change wise Readmission")

ggplot(data = Risk_Stratification_DF,aes(insulin,fill = comorbidity)) + geom_bar() + 
  ggtitle("Insulin wise Comorbidity")



#see the effect of numerical variables on Readmission 
ggplot(data = Risk_Stratification_DF,aes(num_lab_procedures,fill = readmitted)) + geom_bar() + 
  ggtitle("Number Lab procedures wise Readmission")



ggplot(data = Risk_Stratification_DF,aes(num_procedures,fill = readmitted)) + geom_bar() + 
  ggtitle("Number Other procedures wise Readmission")


ggplot(data = Risk_Stratification_DF,aes(num_medications,fill = readmitted)) + geom_bar() + 
  ggtitle("Number of Medications wise Readmission")


# colnames(Risk_Stratification_DF)
# Risk_Stratification_DF <- Risk_Stratification_DF_3

# Now remove columns race, age

Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "race")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "age")]
Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "insulin")]

# colnames(Risk_Stratification_DF)

# ----------------------------------------------------------------------------------------------------------------

library(caTools)

# did not perfrom this as it is throwing an error
# Risk_Stratification_DF <- cbind(Risk_Stratification_DF$encounter_id, Risk_Stratification_DF$patient_nbr, 
#                              scale(dplyr::select_if(Risk_Stratification_DF[, -c(1:2)],is.numeric)), 
#                              dplyr::select_if(Risk_Stratification_DF[, -c(1:2)],is.factor), 
#                              dplyr::select_if(Risk_Stratification_DF[, -c(1:2)],is.character))

Risk_Stratification_DF <- Risk_Stratification_DF[,-which(names(Risk_Stratification_DF) == "encounter_id")]

Risk_Stratification_DF_3 <- Risk_Stratification_DF

# colnames(Risk_Stratification_DF)

# Model Building

# Task: Divide your data into training and testing dataset

#Create Test and Train Datasets
set.seed(100)


train <- which(sample.split(Risk_Stratification_DF$readmitted,SplitRatio = 0.7))

test <- which(!c(1:nrow(Risk_Stratification_DF) %in% train))

Risk_Stratification_DF_Train <- Risk_Stratification_DF[train,]
Risk_Stratification_DF_Test <- Risk_Stratification_DF[test,]


# colnames(Risk_Stratification_DF)

# Build model
model_1 <- glm(readmitted~., data=Risk_Stratification_DF_Train, family="binomial")

summary(model_1)

step <- stepAIC(model_1, direction="both")


model_2 <- glm(readmitted ~ patient_nbr + gender + admission_type_id + discharge_disposition_id + 
                 admission_source_id + time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + raceAfricanAmerican + raceCaucasian + 
                 raceHispanic + age.10.20. + age.30.40. + age.40.50. + age.50.60. + 
                 age.60.70. + age.70.80. + age.80.90. + insulinNo + insulinSteady + 
                 insulinUp + comorbidity,
data=Risk_Stratification_DF_Train, family="binomial")

sort(vif(model_2), decreasing = TRUE)

summary(model_2)


# base on VIF & P-Value following columns are removed.
# raceHispanic  
# raceCaucasian
# raceAfricanAmerican
# age.10.20
# age.70.80
# age.60.70
# age.80.90
# age.50.60
# insulinNo
# insulinUp

model_3 <- glm(readmitted ~ patient_nbr + gender + admission_type_id + discharge_disposition_id + 
                 admission_source_id + time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + age.10.20. + age.30.40. + age.40.50. + insulinSteady + 
                 comorbidity,
               data=Risk_Stratification_DF_Train, family="binomial")

sort(vif(model_3), decreasing = TRUE)

summary(model_3)

# base on P-Value following columns are removed. VIF is fine for all


model_4 <- glm(readmitted ~ patient_nbr + gender + admission_type_id + discharge_disposition_id + 
                 admission_source_id + time_in_hospital + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + insulinSteady + 
                 comorbidity,
               data=Risk_Stratification_DF_Train, family="binomial")

summary(model_4)

# P value looks fine for all columns

Final_Model <- model_4




## Evaluate Model on TEST data


# Lets see what are the predictions made
Risk_Stratification_DF_Predicted <- predict(Final_Model, type="response", newdata = dplyr::select(Risk_Stratification_DF_Test, -readmitted))

Risk_Stratification_DF_Test$Readmitted_Predicted <- Risk_Stratification_DF_Predicted

View(Risk_Stratification_DF_Test)

# Maximum predicted probability
max(Risk_Stratification_DF_Test$Readmitted_Predicted)
# 0.8152061

# Minimum predicted probability
min(Risk_Stratification_DF_Test$Readmitted_Predicted)
# 0.0003413073

# Let's use the probability cutoff of 50%.
Risk_Stratification_Test_pred_attr <- factor(ifelse(Risk_Stratification_DF_Predicted >= 0.50, "Yes", "No"))
Risk_Stratification_Test_actual_attr <- factor(ifelse(Risk_Stratification_DF_Test$readmitted == 1,"Yes","No"))

table(Risk_Stratification_Test_actual_attr, Risk_Stratification_Test_pred_attr)

#                                        Risk_Stratification_Test_pred_attr
# Risk_Stratification_Test_actual_attr    No   Yes
# No                                      5798  7917
# Yes                                     3322 12379

library(caret)

Risk_Stratification_Test_conf <- confusionMatrix(Risk_Stratification_Test_pred_attr, Risk_Stratification_Test_actual_attr, positive = "Yes")

Risk_Stratification_Test_conf



# Confusion Matrix and Statistics

#               Reference
# Prediction    No   Yes
# No            5798  3322
# Yes           7917 12379

# Accuracy : 0.6179          
# 95% CI   : (0.6123, 0.6235)
# No Information Rate : 0.5338          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.2157          
# Mcnemar's Test P-Value : < 2.2e-16       

# Sensitivity : 0.7884          
# Specificity : 0.4227          
# Pos Pred Value : 0.6099          
# Neg Pred Value : 0.6357          
# Prevalence : 0.5338          
# Detection Rate : 0.4208          
# Detection Prevalence : 0.6900          
# Balanced Accuracy : 0.6056          

# 'Positive' Class : Yes             



## Find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(Risk_Stratification_DF_Predicted >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, Risk_Stratification_Test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(Risk_Stratification_DF_Predicted)

# Creating cutoff values from 0003413 to 0.8152061 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.81,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# Plotting ROC Curve having Sensitivity,Specificity and Accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.41,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
View(cutoff)

# Let's choose a cutoff value = 0.5594949 at the point where sensitivity & specificity are nearest to eachother
Risk_Stratification_Test_cutoff_attr <- factor(ifelse(Risk_Stratification_DF_Predicted >=0.5594949, "Yes", "No"))

conf_final <- confusionMatrix(Risk_Stratification_Test_cutoff_attr, Risk_Stratification_Test_actual_attr, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

#Accuracy :: 0.6101441 
#Sensitivity :: 0.6014267 
#Specificity :: 0.620124




## KS Statistic - Test Data

Risk_Stratification_Test_cutoff_attr <- ifelse(Risk_Stratification_Test_cutoff_attr == "Yes", 1, 0)
Risk_Stratification_Test_actual_attr <- ifelse(Risk_Stratification_Test_actual_attr == "Yes", 1, 0)

#on testing  data
library(ROCR)
pred_object_test<- prediction(Risk_Stratification_Test_cutoff_attr, Risk_Stratification_Test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.2215506



# ----------------------------------------------------------------------------------------------------------------

# Random Forest Model

# install.packages("randomForest")

library(randomForest)

model_rf <- randomForest(readmitted ~ ., data=Risk_Stratification_DF_Train, proximity=FALSE,
                        ntree=50, mtry=5, do.trace=TRUE, na.action=na.omit)


#     |      Out-of-bag   |
#Tree |      MSE  %Var(y) |
#   1 |   0.3724   149.63 |
#   2 |     0.36   144.66 |
#   3 |   0.3413   137.14 |
#   4 |    0.327   131.39 |
#   5 |   0.3149   126.52 |
#   6 |   0.3038   122.06 |
#   7 |   0.2938   118.06 |
#   8 |   0.2857   114.81 |
#   9 |    0.279   112.10 |
#  10 |   0.2728   109.64 |
#  11 |   0.2671   107.32 |
#  12 |   0.2627   105.57 |
#  13 |   0.2591   104.13 |
#  14 |   0.2561   102.89 |
#  15 |   0.2531   101.72 |
#  16 |   0.2505   100.67 |
#  17 |   0.2485    99.84 |
#  18 |   0.2468    99.17 |
#  19 |   0.2449    98.40 |
#  20 |   0.2435    97.85 |
#  21 |   0.2423    97.36 |
#  22 |   0.2413    96.94 |
#  23 |   0.2399    96.40 |
#  24 |   0.2391    96.06 |
#  25 |   0.2382    95.70 |
#  26 |   0.2373    95.36 |
#  27 |   0.2365    95.02 |
#  28 |   0.2359    94.80 |
#  29 |   0.2353    94.56 |
#  30 |   0.2347    94.31 |
#  31 |   0.2342    94.09 |
#  32 |   0.2336    93.88 |
#  33 |   0.2332    93.70 |
#  34 |   0.2327    93.51 |
#  35 |   0.2323    93.33 |
#  36 |   0.2319    93.19 |
#  37 |   0.2315    93.04 |
#  38 |   0.2311    92.88 |
#  39 |   0.2308    92.75 |
#  40 |   0.2306    92.65 |
#  41 |   0.2302    92.51 |
#  42 |   0.2299    92.38 |
#  43 |   0.2296    92.27 |
#  44 |   0.2294    92.17 |
#  45 |    0.229    92.03 |
#  46 |   0.2287    91.91 |
#  47 |   0.2285    91.82 |
#  48 |   0.2282    91.69 |
#  49 |   0.2281    91.65 |
#  50 |   0.2279    91.58 |



# Lets see what are the predictions made
Risk_Stratification_DF_Predicted_RF <- predict(model_rf, type="response", newdata = dplyr::select(Risk_Stratification_DF_Test, -readmitted))

Risk_Stratification_DF_Test$Readmitted_Predicted_RF <- Risk_Stratification_DF_Predicted_RF

View(Risk_Stratification_DF_Test)

# Maximum predicted probability
max(Risk_Stratification_DF_Test$Readmitted_Predicted_RF)
# 0.9671633

# Minimum predicted probability
min(Risk_Stratification_DF_Test$Readmitted_Predicted_RF)
# 0.01032051

# Let's use the probability cutoff of 50%.
Risk_Stratification_Test_pred_attr_RF <- factor(ifelse(Risk_Stratification_DF_Predicted_RF >= 0.50, "Yes", "No"))
Risk_Stratification_Test_actual_attr_RF <- factor(ifelse(Risk_Stratification_DF_Test$readmitted == 1,"Yes","No"))

table(Risk_Stratification_Test_actual_attr_RF, Risk_Stratification_Test_pred_attr_RF)

#                                           Risk_Stratification_Test_pred_attr_RF
# Risk_Stratification_Test_actual_attr_RF    No   Yes
# No                                        7706  6009
# Yes                                       4620 11081


Risk_Stratification_Test_conf_RF <- confusionMatrix(Risk_Stratification_Test_pred_attr_RF, Risk_Stratification_Test_actual_attr_RF, positive = "Yes")

Risk_Stratification_Test_conf_RF

# Confusion Matrix and Statistics

# Reference
# Prediction    No   Yes
# No            7706  4620
# Yes           6009 11081

# Accuracy : 0.6387          
# 95% CI : (0.6331, 0.6442)
# No Information Rate : 0.5338          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.2693          
# Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.7058          
#             Specificity : 0.5619          
#          Pos Pred Value : 0.6484          
#          Neg Pred Value : 0.6252          
#              Prevalence : 0.5338          
#          Detection Rate : 0.3767          
#    Detection Prevalence : 0.5810          
#       Balanced Accuracy : 0.6338          
                                          
#        'Positive' Class : Yes         


# View(Risk_Stratification_DF_Test)
Risk_Stratification_DF_Test$Stratificaiton <-  ifelse(Risk_Stratification_DF_Test$Readmitted_Predicted_RF > 0.7,
                                        yes = "High risk", 
                                        no = ifelse(Risk_Stratification_DF_Test$Readmitted_Predicted_RF < 0.3,
                                                    yes = "Low Risk",
                                                    no = "Medium Risk"))

# View(Risk_Stratification_DF_Test)
# colnames(Risk_Stratification_DF_Test)
