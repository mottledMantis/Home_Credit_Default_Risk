library(tidyverse)
library(fastDummies)

#at visualization time, make histogram for numerical features
#bar plot for the categorical data


application_test <- read_csv("data/application_test.csv")
application_train <- read_csv("data/application_train.csv")
bureau <- read_csv("data/bureau.csv")
bureau_balance <- read_csv("data/bureau_balance.csv")
credit_card_balance <- read_csv("data/credit_card_balance.csv")
installments_payments <- read_csv("data/installments_payments.csv")
POS_CASH_balance <- read_csv("data/POS_CASH_balance.csv")
previous_application <- read_csv("data/previous_application.csv")
sample_submission <- read_csv("data/sample_submission.csv")

View(application_test)
View(application_train)
View(bureau)
View(bureau_balance)
View(credit_card_balance)
View(installments_payments)
View(POS_CASH_balance)
View(previous_application)
View(sample_submission)
