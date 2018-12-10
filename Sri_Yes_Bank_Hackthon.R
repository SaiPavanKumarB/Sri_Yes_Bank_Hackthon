# Sri - Yes Bank Hackthon
library("ggplot2")
# Set the working directory

setwd("D:/Sri_Yes_Bank_Hackthon")

# Read the train & test files

train = read.csv("Yes_Bank_Training.csv")
test = read.csv("Yes_Bank_Test.csv")

# Summary of train file
NROW(train) # 31649
str(train)

# Check for data set imbalance
table(train$outcome) # 29809 NO 1840 YES # Imbalanced data set

# Check each independent variable for missing values & distribution
NROW(train[is.na(train$serial_number),])

NROW(train[is.na(train$age_in_years),])
hist(train$age_in_years, labels = TRUE, col = "dark red", main = "Age distribution", xlab = "Age")

NROW(train[is.na(train$job_description),])
ggplot(data.frame(train$job_description), aes(x=train$job_description), colour = "dark red")+geom_bar(stat = "count") # 221 Missing values
NROW(train[which(train$job_description=="unknown"),])

NROW(train[is.na(train$marital_status),])
NROW(train[which(train$marital_status=="unknown"),])
ggplot(data.frame(train$marital_status), aes(x=train$marital_status), colour = "dark red")+geom_bar(stat = "count") # No Missing values


NROW(train[is.na(train$education_details),])
NROW(train[which(train$education_details=="unknown"),])
ggplot(data.frame(train$education_details), aes(x=train$education_details), colour = "dark red")+geom_bar(stat = "count") # 1272 Missing values


NROW(train[is.na(train$has_default),])
NROW(train[which(train$has_default=="unknown"),])
ggplot(data.frame(train$has_default), aes(x=train$has_default), colour = "dark red")+geom_bar(stat = "count") 

NROW(train[is.na(train$balance_in_account),])
View(train[which(train$balance_in_account<0),])
hist(train$balance_in_account, labels = TRUE, col = "dark red", main = "Balance_in_account distribution", xlab = "Balance_in_Acct", xlim = range(min(train$balance_in_account), max(train$balance_in_account)))


NROW(train[is.na(train$housing_status),])
NROW(train[which(train$housing_status=="unknown"),])
ggplot(data.frame(train$housing_status), aes(x=train$housing_status), colour = "dark red")+geom_bar(stat = "count") 


NROW(train[is.na(train$previous_loan),])
NROW(train[which(train$previous_loan=="unknown"),])
ggplot(data.frame(train$previous_loan), aes(x=train$previous_loan), colour = "dark red")+geom_bar(stat = "count") 


NROW(train[is.na(train$phone_type),])
NROW(train[which(train$phone_type=="unknown"),]) #12765 
ggplot(data.frame(train$phone_type), aes(x=train$phone_type), colour = "dark red")+geom_bar(stat = "count") 


NROW(train[is.na(train$date),])
View(train[which(train$balance_in_account<0),])
hist(train$date, labels = TRUE, col = "dark red", main = "Date Contacted distribution", xlab = "Date Contacted", xlim = range(min(train$date), max(train$date)))





NROW(train[is.na(train$call_duration),])
View(train[which(train$call_duration==0),])
hist(train$call_duration, labels = TRUE, col = "dark red", main = "Call Duration distribution", xlab = "Call Duration", xlim = range(min(train$call_duration), max(train$call_duration)))


NROW(train[is.na(train$campaign_contacts),])
View(train[which(train$campaign_contacts==-1),])
hist(train$campaign_contacts, labels = TRUE, col = "dark red", main = "No_of_days_gap_after_previous_campaign_&_contact", xlab = "No_of_days_gap_after_previous_campaign_&_contact", xlim = range(min(train$campaign_contacts), max(train$campaign_contacts)))

NROW(train[is.na(train$days_passed),])
View(train[which(train$days_passed==0),])
hist(train$days_passed, labels = TRUE, col = "dark red", main = "days_passed", xlab = "days_passed", xlim = range(min(train$days_passed), max(train$days_passed)))


NROW(train[is.na(train$previous_contact),])
View(train[which(train$previous_contact==0),])
hist(train$previous_contact, labels = TRUE, col = "dark red", main = "previous_contact", xlab = "previous_contact", xlim = range(min(train$previous_contact), max(train$previous_contact)))


NROW(train[is.na(train$poutcome_of_campaign),])
NROW(train[which(train$poutcome_of_campaign=="other"),]) 
ggplot(data.frame(train$poutcome_of_campaign), aes(x=train$poutcome_of_campaign), colour = "dark red")+geom_bar(stat = "count") 


# Handling missing values 
# Job Description
# Check bank balance, previous loan status & housing loan status of records with unknown

View(train[which(train$job_description=="unknown"),])


# Replace unknown values with mode i.e. blue-collar
train$job_description = ifelse(train$job_description=="unknown","blue-collar",as.character(train$job_description))


# Education Details
View(train[which(train$education_details=="unknown"),])

# 50% of the unknown records have previous housing loan which can be assumed that they have
# good educational qualification

# Replacing unknown with tertiary

train$education_details = ifelse(train$education_details=="unknown","tertiary", as.character(train$education_details))



