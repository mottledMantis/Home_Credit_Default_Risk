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
HomeCredit_columns_description <- read_csv("data/HomeCredit_columns_description.csv")

# View(application_test)
# View(application_train)
# View(bureau)
# View(bureau_balance)
# View(credit_card_balance)
# View(installments_payments)
# View(POS_CASH_balance)
# View(previous_application)
# View(sample_submission)
# View(HomeCredit_columns_description)

# to try: aggregate(table[cols to mean, eg 2:5]), list(table$categorical), mean)

#combine application test and train into a single df called application_all, with TARGET as Column 2

application_all <- merge(application_train, application_test, all = TRUE)
application_all <- application_all[ , c(1, 122, 2:121)]

#create df based on "bureau" grouped by SK_ID_CURR called bureau_grouped_ID

bureau_grouped_ID <- group_by(bureau, SK_ID_CURR) %>% 
  summarise("MAX_DAYS_LATE_BUREAU" = max(CREDIT_DAY_OVERDUE))

# another way of doing the above: bureau_grouped_ID <- group_by(bureau, SK_ID_CURR)
# bureau_grouped_ID <- summarise(bureau_grouped_ID, "max" = max(CREDIT_DAY_OVERDUE))

#create a new "results" df that includes only our relevant info, one row per client

results <- data.frame("SK_ID_CURR" = application_all$SK_ID_CURR,
                      "TARGET" = application_all$TARGET,
                      "ALL_INCOME" = application_all$AMT_INCOME_TOTAL)
results <- merge(results, bureau_grouped_ID, by.x = 1, by.y = 1, all = TRUE)

# Since NA values mean no information and we are looking for a positive flag
#for overdue payments, might as well make NA values in "max" <- 0
results$MAX_DAYS_LATE_BUREAU[is.na(results$MAX_DAYS_LATE_BUREAU)] <- 0

#plot something
# ggplot(results, aes(x = as.factor(results$TARGET), y = results$MAX_DAYS_LATE_BUREAU)) +
#   geom_point()

#results <- add_column(summarise(bureau_grouped_ID, max(CREDIT_DAY_OVERDUE)))

# finalData<-subset(data,!(is.na(data["mmul"]) | is.na(data["rnor"]))) - use to remove rows with no TARGET value
# use merge to combine the test and train sets?

