library(tidyverse)
library(fastDummies)
library(GGally)
library(gridExtra)
library(fastDummies)

#LOAD ALL OF THE DATA

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

#CREATE RESULTS DATA FRAME THAT SELECTS FEATURES TO BE ANAYLZED

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
                      "LOAN_AMOUNT" = application_all$AMT_CREDIT,
                      "PAYMENT_AMOUNT" = application_all$AMT_ANNUITY,
                      "PURCHASE_PRICE_OF_GOODS" = application_all$AMT_GOODS_PRICE,
                      "RATIO_LOAN_TO_ANNUITY" = (application_all$AMT_CREDIT / application_all$AMT_ANNUITY),
                      "INCOME_TYPE" = application_all$NAME_INCOME_TYPE,
                      "EDUCATION" = application_all$NAME_EDUCATION_TYPE,
                      "MARITAL_STATUS" = application_all$NAME_FAMILY_STATUS,
                      "HOUSING_STATUS" = application_all$NAME_HOUSING_TYPE,
                      "YEARS_AT_CURRENT_JOB" = abs(application_all$DAYS_EMPLOYED / 365.25),
                      "EMPLOYER_TYPE" = application_all$ORGANIZATION_TYPE,
                      "YEARS_SINCE_GETTING_IDENTITY_DOCUMENT" = abs(application_all$DAYS_REGISTRATION / 365.25),
                      "REGION_AND_CITY_RATING" = as.factor(application_all$REGION_RATING_CLIENT_W_CITY)
                      )

#add the bureau infomation to the results df
results <- merge(results, bureau_grouped_ID, by.x = 1, by.y = 1, all = TRUE)


#group chilren amounts >4
results$CHILDREN <- as.factor(ifelse(results$CHILDREN>4, '5+', results$CHILDREN))

#copy income type to a variable for grouping and remove the original

results$INCOME_TYPE_GROUPED <- results$INCOME_TYPE
results$INCOME_TYPE <- NULL

# combine factor levels to combine low counts
results$INCOME_TYPE_GROUPED <- as.factor(
  ifelse(results$INCOME_TYPE_GROUPED %in% c('Businessman','Maternity leave', 'Student', 'Unemployed'), 'Other', as.character(results$INCOME_TYPE_GROUPED)
  )
)

# All original levels:
# "Advertising", "Agriculture", "Bank", "Business Entity Type 1", "Business Entity Type 2", "Business Entity Type 3", "Cleaning", "Construction", "Culture", "Electricity", "Emergency", "Government", "Hotel", "Housing", "Industry: type 1", "Industry: type 10", "Industry: type 11", "Industry: type 12", "Industry: type 13", "Industry: type 2", "Industry: type 3", "Industry: type 4", "Industry: type 5", "Industry: type 6", "Industry: type 7", "Industry: type 8", "Industry: type 9", "Insurance", "Kindergarten", "Legal Services", "Medicine", "Military", "Mobile", "Postal", "Realtor", "Religion", "Restaurant", "School", "Security", "Security Ministries", "Self-employed", "Services", "Telecom", "Trade: type 1", "Trade: type 2", "Trade: type 3", "Trade: type 4", "Trade: type 5", "Trade: type 6", "Trade: type 7", "Transport: type 1", "Transport: type 2", "Transport: type 3", "Transport: type 4", "University", "XNA" 

#group employer type into fewer categories
results$EMPLOYER_TYPE_GROUPED <- results$EMPLOYER_TYPE

results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Business Entity Type 1", "Business Entity Type 2", "Business Entity Type 3"), 'Business Entity', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)

results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Industry: type 1", "Industry: type 10", "Industry: type 11", "Industry: type 12", "Industry: type 13", "Industry: type 2", "Industry: type 3", "Industry: type 4", "Industry: type 5", "Industry: type 6", "Industry: type 7", "Industry: type 8", "Industry: type 9"), 'Industry', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)

results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Trade: type 1", "Trade: type 2", "Trade: type 3", "Trade: type 4", "Trade: type 5", "Trade: type 6", "Trade: type 7"), 'Trade', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)

results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Transport: type 1", "Transport: type 2", "Transport: type 3", "Transport: type 4"), 'Transport', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)

#... and remove the original column
results$EMPLOYER_TYPE <- NULL


# results$EMPLOYER_TYPE_GROUPED <- results$EMPLOYER_TYPE
# levels(results$EMPLOYER_TYPE_GROUPED) <- c("Advertising", "Agriculture", "Bank", "Business Entity", "Business Entity", "Business Entity", "Cleaning", "Construction", "Culture", "Electricity", "Emergency", "Government", "Hotel", "Housing", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Industry", "Insurance", "Kindergarten", "Legal Services", "Medicine", "Military", "Mobile", "Other", "Police", "Postal", "Realtor", "Religion", "Restaurant", "School", "Security", "Security Ministries", "Self-employed", "Services", "Telecom", "Trade", "Trade", "Trade", "Trade", "Trade", "Trade", "Trade", "Transport", "Transport", "Transport", "Transport", "University", "XNA")


# # ACTUALLY, lEAVE THIS OUT FOR NOW: Since NA values mean no information and we are looking for a positive flag
# #for overdue payments, might as well make NA values in "max" <- 0
# results$MAX_DAYS_LATE_BUREAU[is.na(results$MAX_DAYS_LATE_BUREAU)] <- 0

#create dummy columns

results <- cbind(results, as.data.frame(model.matrix(~results$CHILDREN)))
results <- cbind(results, as.data.frame(model.matrix(~results$REGION_AND_CITY_RATING)))
results <- cbind(results, as.data.frame(model.matrix(~results$EMPLOYER_TYPE_GROUPED)))
results <- cbind(results, as.data.frame(model.matrix(~results$HOUSING_STATUS)))
results <- cbind(results, as.data.frame(model.matrix(~results$INCOME_TYPE)))
results <- cbind(results, as.data.frame(model.matrix(~results$LOAN_TYPE)))

#remove intercept columns
results$'(Intercept)' <- NULL
results$'(Intercept)' <- NULL
results$'(Intercept)' <- NULL
results$'(Intercept)' <- NULL
results$'(Intercept)' <- NULL
results$'(Intercept)' <- NULL

#remove original columns

# results$CHILDREN <- NULL
# results$REGION_AND_CITY_RATING <- NULL
# results$EMPLOYER_TYPE_GROUPED <- NULL
# results$HOUSING_STATUS <- NULL
# results$INCOME_TYPE <- NULL
# results$LOAN_TYPE <- NULL

#clean the column names of the dummy columns
colnames(results) <- gsub("results\\$+", "", colnames(results))




# create results_train and results_test by removing samples with TARGET == NA
results_train <- filter(results, is.na(results$TARGET) == FALSE)
results_test <- filter(results, is.na(results$TARGET) == TRUE)
View(results_train)
View(results_train)


#write the results_train  and results_test data to csv files
write_csv(results_train, "results_train.csv")
write_csv(results_test, "results_test.csv")



##OTHER FUNCTIONS

# #Look for NAs
# na_count <- data.frame("Num_NAs" = sapply(application_all, function(y) sum(length(which(is.na(y))))))
# na_count$percent_NAs <- as.integer(100 * na_count$Num_NAs / nrow(application_all))
# View(na_count)

# 
# #The following creates a df that shows that all major outliers for
# #days_employed are pensioners, and a few unemplyed
# outliers_emp <- data.frame(filter(application_all, DAYS_EMPLOYED > 10000))
# View(outliers_emp)
# 
# 
# #plot something
# ggplot(results_train, aes(x = factor(TARGET))) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = LOAN_TYPE )) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = AGE)) +
#   geom_histogram(stat = "bin", bins = 20)
# ggplot(results_train, aes(x = GENDER)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = OWNS_CAR)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = AGE_OF_CAR)) +
#   geom_histogram(stat = "bin", binwidth = 2.5)
# ggplot(results_train, aes(x = OWNS_REALTY)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = factor(CHILDREN))) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = TOTAL_INCOME)) +
#   scale_x_log10(breaks=c(1e+5,1e+6)) +
#   xlim(0, 1e+6) +
#   geom_histogram(bins = 20)
# # add limit
# ggplot(results_train, aes(x = LOAN_AMOUNT)) +
#   geom_histogram(stat = "bin")
# 
# ggplot(results_train, aes(x = PAYMENT_AMOUNT)) +
#   geom_histogram(stat = "bin")
# 
# # add limit
# ggplot(results_train, aes(x = PURCHASE_PRICE_OF_GOODS)) +
#   geom_histogram(stat = "bin")
# ggplot(results_train, aes(x = RATIO_LOAN_TO_ANNUITY)) +
#   geom_histogram()
# ggplot(results_train, aes(x = INCOME_TYPE)) +
#   geom_bar(stat = "count") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# # ADD OTHER CATEGORY FOR LOW SAMPLES
# ggplot(results_train, aes(x = EDUCATION)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = MARITAL_STATUS)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = HOUSING_STATUS)) +
#   geom_bar(stat = "count")
# ggplot(results_train, aes(x = YEARS_AT_CURRENT_JOB)) +
#   geom_histogram(binwidth = 2)
# #MAKE THE OUTLIERS = 0
# ggplot(results_train, aes(x = EMPLOYER_TYPE)) +
#   geom_bar(stat = "count")
# #SORT ACCORDING TO NUMBER OF SAMPLES, AND THEN GROUP LOW-OCCURRING CATEGORIES
# ggplot(results_train, aes(x = YEARS_SINCE_GETTING_IDENTITY_DOCUMENT)) +
#   geom_histogram()
# ggplot(results_train, aes(x = REGION_AND_CITY_RATING)) +
#   geom_bar(stat = "count")
# 
# 
# #results <- add_column(summarise(bureau_grouped_ID, max(CREDIT_DAY_OVERDUE)))
# 
# # finalData<-subset(data,!(is.na(data["mmul"]) | is.na(data["rnor"]))) - use to remove rows with no TARGET value
# # use merge to combine the test and train sets?
# ggplot(results_train, aes(factor(TARGET),
#            AGE)) +
#   geom_jitter( alpha = .05)  +
#   geom_boxplot( alpha = .5, color = "blue")+
#   stat_summary(fun.y = "mean",
#                geom = "point",
#                color = "red",
#                shape = 8,
#                size = 4)
# 
# ggplot(results_train) + 
#   geom_bar(aes(GENDER, TARGET), 
#            position = "dodge", stat = "summary", fun.y = "mean")

# test <- data.frame(a = as.factor(1:3), b = as.factor(4:6))
# test <- cbind(test, as.data.frame(model.matrix(~test$a)))
# test <- cbind(test, as.data.frame(model.matrix(~test$b)))
# test$`(Intercept)` <- NULL
# test$`(Intercept)` <- NULL
# test$a <- NULL
# test$b <- NULL
# test
