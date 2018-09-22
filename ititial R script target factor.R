library(tidyverse)
library(GGally)
library(gridExtra)


#LOAD ALL OF THE DATA
setwd("C:/Users/Jim/Google Drive/Documents/gits/Home_Credit_Default_Risk")

application_test <- read_csv("data/application_test.csv")
application_train <- read_csv("data/application_train.csv")
bureau <- read_csv("data/bureau.csv")
# bureau_balance <- read_csv("data/bureau_balance.csv")
# credit_card_balance <- read_csv("data/credit_card_balance.csv")
# installments_payments <- read_csv("data/installments_payments.csv")
# POS_CASH_balance <- read_csv("data/POS_CASH_balance.csv")
# previous_application <- read_csv("data/previous_application.csv")
# sample_submission <- read_csv("data/sample_submission.csv")
# HomeCredit_columns_description <- read_csv("data/HomeCredit_columns_description.csv")

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
remove(application_train)
remove(application_test)

#create df based on "bureau" grouped by SK_ID_CURR called bureau_grouped_ID

bureau_grouped_ID <- group_by(bureau, SK_ID_CURR) %>% 
  summarise("MAX_DAYS_LATE_BUREAU" = max(CREDIT_DAY_OVERDUE))
remove(bureau)

# another way of doing the above: bureau_grouped_ID <- group_by(bureau, SK_ID_CURR)
# bureau_grouped_ID <- summarise(bureau_grouped_ID, "max" = max(CREDIT_DAY_OVERDUE))

#CREATE RESULTS DATA FRAME THAT SELECTS FEATURES TO BE ANAYLZED

results <- data.frame("SK_ID_CURR" = application_all$SK_ID_CURR,
                      "TARGET" = as.factor(application_all$TARGET),
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
remove(application_all)

# replace low-count "XNA" factor from GENDER with NA and make factor
# results$GENDER <- gsub("XNA", NA, results$GENDER)
# results$GENDER <- as.factor(results$GENDER)

#add the bureau infomation to the results df
results <- merge(results, bureau_grouped_ID, by.x = 1, by.y = 1, all = TRUE)
remove(bureau_grouped_ID)

write.csv(results, "results.csv")

remove(results)

results <- read.csv("results.csv")
results <- results[-1]

#Look for NAs
# nas_in_result <- data.frame("Num_NAs" = sapply(results, function(y) sum(length(which(is.na(y))))))
# nas_in_result$percent_NAs <- as.integer(100 * nas_in_result$Num_NAs / nrow(results))
# View(nas_in_result)

#Fix NAs
results$PAYMENT_AMOUNT[is.na(results$PAYMENT_AMOUNT)] <- 0
results$PURCHASE_PRICE_OF_GOODS[is.na(results$PURCHASE_PRICE_OF_GOODS)] <- 0
results$RATIO_LOAN_TO_ANNUITY[is.na(results$RATIO_LOAN_TO_ANNUITY)] <- 0
results$MAX_DAYS_LATE_BUREAU[is.na(results$MAX_DAYS_LATE_BUREAU)] <- 0


#group chilren amounts >4
results$CHILDREN <- as.factor(ifelse(results$CHILDREN>4, '5Plus', results$CHILDREN))

#copy income type to a variable for grouping and remove the original

results$INCOME_TYPE_GROUPED <- results$INCOME_TYPE
results$INCOME_TYPE <- NULL

# combine INCOME_TYPE factor levels for low counts
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
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Advertising", "Security", "Security Ministries", "Cleaning", "Culture", "Emergency", "Insurance", "Legal Services", "Mobile", "Realtor", "Religion", "Telecom", "Services", "Hotel", "Restaurant"), 'Service', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)

results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Kindergarten", "School", "University"), 'Education', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)


results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Postal", "Police", "Military", "Government"), 'Govt Services', as.character(results$EMPLOYER_TYPE_GROUPED)
  )
)


results$EMPLOYER_TYPE_GROUPED <- as.factor(
  ifelse(results$EMPLOYER_TYPE_GROUPED %in% c("Construction", "Housing"), 'Housing', as.character(results$EMPLOYER_TYPE_GROUPED)
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

#Fix some NAs
results$AGE_OF_CAR[is.na(results$AGE_OF_CAR)] <- as.numeric(0)
results$MAX_DAYS_LATE_BUREAU[is.na(results$AGE_OF_CAR)] <- as.numeric(0)


#create version with dummy columns
results_dummy <- results
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$GENDER)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$OWNS_CAR)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$OWNS_REALTY)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$MARITAL_STATUS)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$INCOME_TYPE_GROUPED)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$EDUCATION)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$CHILDREN)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$REGION_AND_CITY_RATING)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$EMPLOYER_TYPE_GROUPED)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$HOUSING_STATUS)))
# results <- cbind(results, as.data.frame(model.matrix(~results$INCOME_TYPE)))
results_dummy <- cbind(results_dummy, as.data.frame(model.matrix(~results_dummy$LOAN_TYPE)))

#remove intercept columns
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
results_dummy$'(Intercept)' <- NULL
# results$'(Intercept)' <- NULL

#remove original columns

results_dummy$GENDER <- NULL
results_dummy$GENDERXNA <- NULL #very few XNA values, make = 0
results_dummy$OWNS_CAR <- NULL
results_dummy$OWNS_REALTY <- NULL
results_dummy$EDUCATION <- NULL
results_dummy$MARITAL_STATUS <- NULL
results_dummy$INCOME_TYPE_GROUPED <- NULL
results_dummy$CHILDREN <- NULL
results_dummy$REGION_AND_CITY_RATING <- NULL
results_dummy$EMPLOYER_TYPE_GROUPED <- NULL
results_dummy$HOUSING_STATUS <- NULL
# results$INCOME_TYPE <- NULL
results_dummy$LOAN_TYPE <- NULL

#clean the column names of the dummy columns
colnames(results_dummy) <- gsub("results_dummy\\$+", "", colnames(results_dummy))
colnames(results_dummy) <- gsub("\\-", "_", colnames(results_dummy))
colnames(results_dummy) <- gsub("\\ ", "_", colnames(results_dummy))
colnames(results_dummy) <- gsub("\\/", "or", colnames(results_dummy))

#remove ID
results_dummy$SK_ID_CURR <- NULL
results$SK_ID_CURR <- NULL


# colnames(results) <- gsub("EMPLOYER_TYPE_GROUPEDSelf-employed", "EMPLOYER_TYPE_GROUPEDSelf_employed", colnames(results))
# colnames(results) <- gsub("EMPLOYER_TYPE_GROUPEDBusiness Entity", "EMPLOYER_TYPE_GROUPEDBusiness_Entity", colnames(results))
# colnames(results) <- gsub("EMPLOYER_TYPE_GROUPEDGovt Services", "EMPLOYER_TYPE_GROUPEDGovt_Services", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSHouse / apartment", "HOUSING_STATUSHouse_or_apartment", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSMunicipal apartment", "HOUSING_STATUSMunicipal_apartment", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSOffice apartment", "HOUSING_STATUSOffice_apartment", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSRented apartment", "HOUSING_STATUSRented_apartment", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSWith parents", "HOUSING_STATUSWith_parents", colnames(results))
# colnames(results) <- gsub("INCOME_TYPEState servant", "INCOME_TYPEState_servant", colnames(results))
# colnames(results) <- gsub("HOUSING_STATUSWith parents", "HOUSING_STATUSWith_parents", colnames(results))




# create results_train and results_test by removing samples with TARGET == NA
results_train <- filter(results, is.na(results$TARGET) == FALSE)
results_test <- filter(results, is.na(results$TARGET) == TRUE)
results_dummy_train <- filter(results_dummy, is.na(results_dummy$TARGET) == FALSE)
results_dummy_test <- filter(results_dummy, is.na(results_dummy$TARGET) == TRUE)
# View(results_train)
# View(results_test)


#write the results_train  and results_test data to csv files and results_dummy
write_csv(results_train, "results_train.csv")
write_csv(results_test, "results_test.csv")
write_csv(results_dummy_train, "results_dummy_train.csv")
write_csv(results_dummy_test, "results_dummy_test.csv")

remove(results_dummy)
remove(results_dummy_test)
remove(results_dummy_train)

#FIX GENDER AND IF DUMMY INCLUDE ONLY ONE
#FIX EMPLOYER

# CREATE TRAIN AND TEST SETS FROM results_train FOR MODEL CREATION
## here's the library that includes sample.split function
library(caTools)


##shuffle the rows into a temporary df to eliminate any ordering bias
## can use set.seed(number) if you want to make the split reproducible
results_train_shuffle <- results_train[sample(1:nrow(results_train)), ] 

## split the shuffled data 80/20
split <- sample.split(results_train_shuffle$TARGET, SplitRatio = 0.80) 

## make new dfs from the splitted data
results_train_Train <- subset(results_train_shuffle, split == TRUE)
results_train_Test <- subset(results_train_shuffle, split == FALSE)

## remove the temporary shuffle df
remove(results_train_shuffle)
remove(split)

#Create logistic model
# Log.mod <- glm(TARGET ~ AGE + AGE_OF_CAR + TOTAL_INCOME + LOAN_AMOUNT +
#                  PAYMENT_AMOUNT + PURCHASE_PRICE_OF_GOODS + RATIO_LOAN_TO_ANNUITY +
#                  YEARS_AT_CURRENT_JOB + YEARS_SINCE_GETTING_IDENTITY_DOCUMENT +
#                  MAX_DAYS_LATE_BUREAU + GENDERM + GENDERXNA + OWNS_CARY +
#                  OWNS_REALTYY + MARITAL_STATUSMarried + MARITAL_STATUSSeparated +
#                  MARITAL_STATUSSingle_or_not_married + MARITAL_STATUSUnknown +
#                  MARITAL_STATUSWidow + INCOME_TYPE_GROUPEDOther +
#                  INCOME_TYPE_GROUPEDPensioner + INCOME_TYPE_GROUPEDState_servant +
#                  INCOME_TYPE_GROUPEDWorking + EDUCATIONHigher_education +
#                  EDUCATIONIncomplete_higher + EDUCATIONLower_secondary +
#                  EDUCATIONSecondary_or_secondary_special + CHILDREN1 + CHILDREN2 +
#                  CHILDREN3 + CHILDREN4 + CHILDREN5Plus + REGION_AND_CITY_RATING1 +
#                  REGION_AND_CITY_RATING2 + REGION_AND_CITY_RATING3 +
#                  EMPLOYER_TYPE_GROUPEDBank + EMPLOYER_TYPE_GROUPEDBusiness_Entity +
#                  EMPLOYER_TYPE_GROUPEDEducation + EMPLOYER_TYPE_GROUPEDElectricity +
#                  EMPLOYER_TYPE_GROUPEDGovt_Services + EMPLOYER_TYPE_GROUPEDHousing +
#                  EMPLOYER_TYPE_GROUPEDIndustry + EMPLOYER_TYPE_GROUPEDMedicine +
#                  EMPLOYER_TYPE_GROUPEDOther + EMPLOYER_TYPE_GROUPEDSelf_employed +
#                  EMPLOYER_TYPE_GROUPEDService + EMPLOYER_TYPE_GROUPEDTrade +
#                  EMPLOYER_TYPE_GROUPEDTransport + EMPLOYER_TYPE_GROUPEDXNA +
#                  HOUSING_STATUSHouse_or_apartment + HOUSING_STATUSMunicipal_apartment +
#                  HOUSING_STATUSOffice_apartment + HOUSING_STATUSRented_apartment +
#                  HOUSING_STATUSWith_parents + LOAN_TYPERevolving_loans,
#                data = results_train_Train,
#                family = binomial)
# summary(Log.mod)

#Features to keep, based on initial run of glm and coeff corr table

# TARGET ~ AGE + AGE_OF_CAR + LOAN_AMOUNT +
#   PAYMENT_AMOUNT+ YEARS_AT_CURRENT_JOB +
#   YEARS_SINCE_GETTING_IDENTITY_DOCUMENT +
#   MAX_DAYS_LATE_BUREAU +
#   GENDERM + OWNS_CARY +
#   OWNS_REALTYY + MARITAL_STATUSMarried +
#   MARITAL_STATUSSingle_or_not_married +
#   MARITAL_STATUSWidow +
#   INCOME_TYPE_GROUPEDWorking +
#   EDUCATIONIncomplete_higher + EDUCATIONLower_secondary +
#   EDUCATIONSecondary_or_secondary_special + CHILDREN1 + CHILDREN2 +
#   REGION_AND_CITY_RATING1 +
#   REGION_AND_CITY_RATING3 +
#   EMPLOYER_TYPE_GROUPEDBank +
#   EMPLOYER_TYPE_GROUPEDEducation + EMPLOYER_TYPE_GROUPEDElectricity +
#   EMPLOYER_TYPE_GROUPEDGovt_Services + LOAN_TYPERevolving_loans
# 
# Remove cols that will break model
# results_train_Test <- results_train_Test[-1]
# results_train_Train <- results_train_Train[-1]
# results_train_Train$SK_ID_CURR <- NULL
# results_train_Test$SK_ID_CURR <- NULL

#Quick model to see which columns to use
Log.mod <- glm(TARGET ~ .,
               data = results_train_Train,
               family = binomial)
summary(Log.mod)

#TARGET to factor
results_train_Test$TARGET <- as.factor(results_train_Test$TARGET)
results_train_Train$TARGET <- as.factor(results_train_Train$TARGET)

#Select columns for model based on above run
results_train_Test <- results_train_Test[,c(1:8, 10:12, 14, 15, 17:22)]
results_train_Train <- results_train_Train[,c(1:8, 10:12, 14, 15, 17:22)]
str(results_train_Train)
str(results_train_Test)


#GLM model
Log.mod <- glm(TARGET ~ .,
               data = results_train_Train,
               family = binomial)
summary(Log.mod)


predictTrainGLM <- predict(Log.mod, type = "response") #tells predict fcnt to give us probailities
summary(predictTrainGLM)
str(predictTrainGLM)
str(results_train_Train)


#to test actual vs. predicted
table(results_train_Train$TARGET, predictTrainGLM > 0.5)
# rows = true outcome, cols = predicted outcome - so 70 correct goodcare, 10 correct poorcare

# sensitivity <- 10/25 # correct positives/total positives
# sensitivity
# specificity <- 70/74 #true negatives/total negatives
# specificity


library(ROCR)
ROCRpred <- prediction(predictTrainGLM, results_train_Train$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))


#Now run prediction on test set
predictTestGLM <- predict(Log.mod, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTestGLM > 0.5)

#Random Forest
##Segment train set into subets for performance comparison - 5 10 20 30 60 80
results_train_Train_05 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.05), ]
results_train_Train_10 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.1), ]
results_train_Train_20 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.2), ]
results_train_Train_30 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.3), ]
results_train_Train_60 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.6), ]
results_train_Train_80 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train) * 0.8), ]
results_train_Train_100 <- results_train_Train[sample(nrow(results_train_Train),
                                                     nrow(results_train_Train)), ]
str(results_train_Train_05)
str(results_train_Train)
remove(results_train_Train)

#load the libraries
library(randomForest)
library(tictoc)
library(ROCR)

#Initialize evaluation df
RFModelEval = data.frame(matrix(ncol = 4, nrow = 0))
colnames(RFModelEval) <- c("percent of data", "train time",
                           "sensitivity: TP/(TP+FN)", "overall accuracy: (TN+TP)/N")

#train the model
#start tictoc timer
tic("05 percent")
#train model
rf_classifier_05 = randomForest(data=results_train_Train_05, TARGET ~ .)
#stop timer
tictoc_05 <- toc()
#write values to our performance evalation table
RFModelEval[1, 1] <- "5"
RFModelEval[1, 2] <- tictoc_05$toc - tictoc_05$tic
#test the model against the data on which it was trained
rf_predict_05 <- predict(rf_classifier_05, results_train_Train_05, type = "response")
#predicition is a fucntion from package ROCR, rf_predict_05 is our prediction fuction
#from above, results_train_Train_05$TARGET is our TRUE OUTCOMES
ROCRpred_05 <- prediction(as.numeric(rf_predict_05), as.numeric(results_train_Train_05$TARGET))
ROCRperf_05 <- performance(ROCRpred_05, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_05, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
#Now run prediction on test set
predictTest_05 <- predict(rf_classifier_05, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_05)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_05 <- table(results_train_Test$TARGET, predictTest_05)
ConfMat_05
#And add performance stats to evlauation table
RFModelEval[1,3] <- ConfMat_05[2,2]/(ConfMat_05[2,2] + ConfMat_05[2,1])
RFModelEval[1,4] <- (ConfMat_05[1,1]+ConfMat_05[2,2]) / sum(ConfMat_05[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_05, "./rf_classifier_05.rds")

tic("10 percent")
rf_classifier_10 = randomForest(data=results_train_Train_10, TARGET ~ .)
tictoc_10 <- toc()
RFModelEval[2, 1] <- "10"
RFModelEval[2, 2] <- tictoc_10$toc - tictoc_10$tic
rf_predict_10 <- predict(rf_classifier_10, results_train_Train_10, type = "response")
ROCRpred_10 <- prediction(as.numeric(rf_predict_10), as.numeric(results_train_Train_10$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_10 <- performance(ROCRpred_10, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_10, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_10 <- predict(rf_classifier_10, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_10)
ConfMat_10 <- table(results_train_Test$TARGET, predictTest_10)
ConfMat_10
RFModelEval[2,3] <- ConfMat_10[2,2]/(ConfMat_10[2,2] + ConfMat_10[2,1])
RFModelEval[2,4] <- (ConfMat_10[1,1]+ConfMat_10[2,2]) / sum(ConfMat_10[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_10, "./rf_classifier_10.rds")


tic("20 percent")
rf_classifier_20 = randomForest(data=results_train_Train_20, TARGET ~ .)
tictoc_20 <- toc()
RFModelEval[3, 1] <- "20"
RFModelEval[3, 2] <- tictoc_20$toc - tictoc_20$tic
rf_predict_20 <- predict(rf_classifier_20, results_train_Train_20, type = "response")
ROCRpred_20 <- prediction(as.numeric(rf_predict_20), as.numeric(results_train_Train_20$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_20 <- performance(ROCRpred_20, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_20, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_20 <- predict(rf_classifier_20, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_20)
ConfMat_20 <- table(results_train_Test$TARGET, predictTest_20)
ConfMat_20
RFModelEval[3,3] <- ConfMat_20[2,2]/(ConfMat_20[2,2] + ConfMat_20[2,1])
RFModelEval[3,4] <- (ConfMat_20[1,1]+ConfMat_20[2,2]) / sum(ConfMat_20[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_20, "./rf_classifier_20.rds")


tic("30 percent")
rf_classifier_30 = randomForest(data=results_train_Train_30, TARGET ~ .)
tictoc_30 <- toc()
RFModelEval[4, 1] <- "30"
RFModelEval[4, 2] <- tictoc_30$toc - tictoc_30$tic
rf_predict_30 <- predict(rf_classifier_30, results_train_Train_30, type = "response")
ROCRpred_30 <- prediction(as.numeric(rf_predict_30), as.numeric(results_train_Train_30$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_30 <- performance(ROCRpred_30, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_30, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_30 <- predict(rf_classifier_30, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_30)
ConfMat_30 <- table(results_train_Test$TARGET, predictTest_30)
ConfMat_30
RFModelEval[4,3] <- ConfMat_30[2,2]/(ConfMat_30[2,2] + ConfMat_30[2,1])
RFModelEval[4,4] <- (ConfMat_30[1,1]+ConfMat_30[2,2]) / sum(ConfMat_30[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_30, "./rf_classifier_30.rds")


tic("60 percent")
rf_classifier_60 = randomForest(data=results_train_Train_60, TARGET ~ .)
tictoc_60 <- toc()
RFModelEval[5, 1] <- "60"
RFModelEval[5, 2] <- tictoc_60$toc - tictoc_60$tic
rf_predict_60 <- predict(rf_classifier_60, results_train_Train_60, type = "response")
ROCRpred_60 <- prediction(as.numeric(rf_predict_60), as.numeric(results_train_Train_60$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_60 <- performance(ROCRpred_60, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_60, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_60 <- predict(rf_classifier_60, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_60)
ConfMat_60 <- table(results_train_Test$TARGET, predictTest_60)
ConfMat_60
RFModelEval[5,3] <- ConfMat_60[2,2]/(ConfMat_60[2,2] + ConfMat_60[2,1])
RFModelEval[5,4] <- (ConfMat_60[1,1]+ConfMat_60[2,2]) / sum(ConfMat_60[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_60, "./rf_classifier_60.rds")

tic("80 percent")
rf_classifier_80 = randomForest(data=results_train_Train_80, TARGET ~ .)
tictoc_80 <- toc()
RFModelEval[6, 1] <- "80"
RFModelEval[6, 2] <- tictoc_80$toc - tictoc_80$tic
rf_predict_80 <- predict(rf_classifier_80, results_train_Train_80, type = "response")
ROCRpred_80 <- prediction(as.numeric(rf_predict_80), as.numeric(results_train_Train_80$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_80 <- performance(ROCRpred_80, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_80, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_80 <- predict(rf_classifier_80, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_80)
ConfMat_80 <- table(results_train_Test$TARGET, predictTest_80)
ConfMat_80
RFModelEval[6,3] <- ConfMat_80[2,2]/(ConfMat_80[2,2] + ConfMat_80[2,1])
RFModelEval[6,4] <- (ConfMat_80[1,1]+ConfMat_80[2,2]) / sum(ConfMat_80[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_80, "./rf_classifier_80.rds")

tic("100 percent")
rf_classifier_100 = randomForest(data=results_train_Train_100, TARGET ~ .)
tictoc_100 <- toc()
RFModelEval[7, 1] <- "100"
RFModelEval[7, 2] <- tictoc_100$toc - tictoc_100$tic
rf_predict_100 <- predict(rf_classifier_100, results_train_Train_100, type = "response")
ROCRpred_100 <- prediction(as.numeric(rf_predict_100), as.numeric(results_train_Train_100$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_100 <- performance(ROCRpred_100, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_100, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_100 <- predict(rf_classifier_100, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_100)
ConfMat_100 <- table(results_train_Test$TARGET, predictTest_100)
ConfMat_100
RFModelEval[7,3] <- ConfMat_100[2,2]/(ConfMat_100[2,2] + ConfMat_100[2,1])
RFModelEval[7,4] <- (ConfMat_100[1,1]+ConfMat_100[2,2]) / sum(ConfMat_100[1:2, 1:2])
View(RFModelEval)
saveRDS(rf_classifier_100, "./rf_classifier_100.rds")




#naive bayes


#k nearest neighboor


#support vector machine
library(e1071)
mySvm <- svm(TARGET ~ ., data = results_train_Train_60, kernel = "linear")
plot(mySvm, results_train_Test)
print(mySvm)
saveRDS(mySvm, "./mySvm.rds")
             

svm_predict_60 <- predict(mySvm, results_train_Train_60, type = "response")
ROCRpred_svm60 <- prediction(as.numeric(svm_predict_60), as.numeric(results_train_Train_100$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_svm60 <- performance(ROCRpred_svm60, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf_svm60, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))
predictTest_svm60 <- predict(mySvm, type = "response", newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_svm60)
ConfMat_svm60 <- table(results_train_Test$TARGET, predictTest_svm60)
ConfMat_svm60
RFModelEval[8,3] <- ConfMat_svm60[2,2]/(ConfMat_svm60[2,2] + ConfMat_svm60[2,1])
RFModelEval[8,4] <- (ConfMat_svm60[1,1]+ConfMat_svm60[2,2]) / sum(ConfMat_svm60[1:2, 1:2])
View(RFModelEval)
saveRDS(mySvm, "./mySvm.rds")










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
