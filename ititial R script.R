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
                      "LOAN_TYPE" = application_all$NAME_CONTRACT_TYPE,
                      "AGE" = abs(application_all$DAYS_BIRTH / 365.25),
                      "GENDER" = application_all$CODE_GENDER,
                      "OWNS_CAR" = application_all$FLAG_OWN_CAR,
                      "AGE_OF_CAR" = application_all$OWN_CAR_AGE,
                      "OWNS_REALTY" = application_all$FLAG_OWN_REALTY,
                      "CHILDREN" = application_all$CNT_CHILDREN,
                      "TOTAL_INCOME" = application_all$AMT_INCOME_TOTAL,
                      "ALL_INCOME" = application_all$AMT_INCOME_TOTAL,
                      "LOAN_AMOUNT" = application_all$AMT_CREDIT,
                      "ANNUITY_AMOUNT" = application_all$AMT_ANNUITY,
                      "PURCHASE_PRICE_OF_GOODS" = application_all$AMT_GOODS_PRICE,
                      "RATIO_LOAN_TO_ANNUITY" = (application_all$AMT_CREDIT / application_all$AMT_ANNUITY),
                      "INCOME_TYPE" = application_all$NAME_INCOME_TYPE,
                      "EDUCATION" = application_all$NAME_EDUCATION_TYPE,
                      "MARITAL_STATUS" = application_all$NAME_FAMILY_STATUS,
                      "HOUSING_STATUS" = application_all$NAME_HOUSING_TYPE,
                      "YEARS_AT_CURRENT_JOB" = abs(application_all$DAYS_EMPLOYED / 365.25),
                      "EMPLOYER_TYPE" = application_all$ORGANIZATION_TYPE,
                      "YEARS_SINCE_GETTING_IDENTITY_DOCUMENT" = abs(application_all$DAYS_REGISTRATION / 365.25),
                      "REGION_AND_CITY_RATING" = application_all$REGION_RATING_CLIENT_W_CITY
                      )
results <- merge(results, bureau_grouped_ID, by.x = 1, by.y = 1, all = TRUE)



# Since NA values mean no information and we are looking for a positive flag
#for overdue payments, might as well make NA values in "max" <- 0
results$MAX_DAYS_LATE_BUREAU[is.na(results$MAX_DAYS_LATE_BUREAU)] <- 0


# create results_train byt removing samples with TARGET == NA
results_train <- filter(results, is.na(results$TARGET) == FALSE)
View(results_train)

#Look for NAs
na_count <- data.frame("Num_NAs" = sapply(application_all, function(y) sum(length(which(is.na(y))))))
na_count$percent_NAs <- as.integer(100 * na_count$Num_NAs / nrow(application_all))
View(na_count)


#create a function for converting days to years - use 365.25 to account for leap years
d_to_y <- function (days) {
  days / 365.25
}

#convert days values to years and add to "results"
results$AGE <- -(d_to_y(application_all$DAYS_BIRTH))
results$YRS_EMPLOYED <- -(d_to_y(application_all$DAYS_EMPLOYED))

#The following creates a df that shows that all major outliers for
#days_employed are pensioners, and a few unemplyed
outliers_emp <- data.frame(filter(application_all, DAYS_EMPLOYED > 10000))
View(outliers_emp)


#plot something
# ggplot(results, aes(x = as.factor(results$TARGET), y = results$MAX_DAYS_LATE_BUREAU)) +
#   geom_point()

#results <- add_column(summarise(bureau_grouped_ID, max(CREDIT_DAY_OVERDUE)))

# finalData<-subset(data,!(is.na(data["mmul"]) | is.na(data["rnor"]))) - use to remove rows with no TARGET value
# use merge to combine the test and train sets?

