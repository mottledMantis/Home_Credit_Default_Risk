###RUNNING MODELS BEGIN HERE
#remember to set working directory
setwd("C:/Users/Jim/Google Drive/Documents/gits/Home_Credit_Default_Risk")
library(tidyverse)
library(GGally)
library(gridExtra)
library(tictoc)
library(ROCR)
library(randomForest)
library(xgboost)
library(caret)
library(e1071)
RFModelEval <-
  data.frame(as.matrix(read.csv("RFModelEval.csv")[-1]),
             stringsAsFactors = FALSE)


results_train_Train <-
  data.frame(read.csv("results_train_Train.csv"), check.names = TRUE)
results_train_Train <- results_train_Train[-1]
results_train_Train_numeric <-
  data.frame(read.csv("results_train_Train.csv"), check.names = TRUE)
results_train_Train_numeric <- results_train_Train_numeric[-1]
results_train_Train$TARGET <- as.factor(results_train_Train$TARGET)
results_train_Test <-
  data.frame(read.csv("results_train_Test.csv"), check.names = TRUE)
results_train_Test <- results_train_Test[-1]
results_train_Test_numeric <-
  data.frame(read.csv("results_train_Test.csv"), check.names = TRUE)
results_train_Test_numeric <- results_train_Train_numeric[-1]
results_train_Test$TARGET <- as.factor(results_train_Test$TARGET)



#Initialize evaluation df
# RFModelEval = data.frame(matrix(ncol = 5, nrow = 0))
# colnames(RFModelEval) <- c("percent of data", "train time",
#                            "sensitivity: TP/(TP+FN)", "overall accuracy: (TN+TP)/N", "AUC Score")
View(RFModelEval)


#GLM model
tic("GLM")
Log.mod <- glm(TARGET ~ .,
               data = results_train_Train,
               family = binomial)
tictoc_GLM <- toc()
saveRDS(Log.mod, "./Log-mod.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# Log.mod <- readRDS("./Log-mod.rds")

summary(Log.mod)

predictTrainGLM <-
  predict(Log.mod) #tells predict fcnt to give us probailities
summary(predictTrainGLM)


#to test actual vs. predicted
GLMCONF <- table(results_train_Train$TARGET, predictTrainGLM > .5)
GLMCONF
RFModelEval[10, 1] <- "100% GLM"
RFModelEval[10, 2] <- tictoc_GLM$toc - tictoc_GLM$tic
RFModelEval[10, 3] <- GLMCONF[2, 2] / (GLMCONF[2, 2] + GLMCONF[2, 1])
RFModelEval[10, 4] <-
  (GLMCONF[1, 1] + GLMCONF[2, 2]) / sum(GLMCONF[1:2, 1:2])
# rows = true outcome, cols = predicted outcome
# sensitivity <- 10/25 # correct positives/total positives
# sensitivity
# specificity <- 70/74 #true negatives/total negatives
# specificity
ROCRpred <- prediction(predictTrainGLM, results_train_Train$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred, "auc")
auc1 <- as.numeric(auc.tmp@y.values)
RFModelEval[10, 5] <- auc1

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "GLM 100%"
)


#Now run prediction on test set
predictTestGLM <- predict(Log.mod, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTestGLM > 0.5)
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(Log.mod)

#Simple Tree
library(rpart)
tic("simple tree")
myTree = rpart(TARGET ~ ., data = results_train_Train, method = "class")
tictoc_Tree = toc()
saveRDS(myTree, "./myTree.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# myTree <- readRDS("./myTree.rds")


predictTrainTree <-
  predict(myTree) #tells predict fcnt to give us probailities
summary(predictTrainTree)

#to test actual vs. predicted
TreeCONF <-
  table(results_train_Train$TARGET, predictTrainTree[, 2] > .5)
TreeCONF

RFModelEval[13, 1] <- "100% Simple Tree"
RFModelEval[13, 2] <- tictoc_Tree$toc - tictoc_Tree$tic
RFModelEval[13, 3] <- TreeCONF[2, 2] / (TreeCONF[2, 2] + TreeCONF[2, 1])
RFModelEval[13, 4] <-
  (TreeCONF[1, 1] + TreeCONF[2, 2]) / sum(TreeCONF[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(myTree)

#naive bayes
library(e1071)
tic("Naive Bayes")
myNaiveBayes <- naiveBayes(TARGET ~ ., results_train_Train)
tictoc_NB = toc()
saveRDS(myNaiveBayes, "./myNaiveBayes.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# myNaiveBayes <- readRDS("./myNaiveBayes.rds")
nb_predict_100 <- predict(myNaiveBayes, results_train_Train)
ROCRpred_nb100 <-
  prediction(as.numeric(nb_predict_100), results_train_Train$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_nb100 <- performance(ROCRpred_nb100, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_nb100, "auc")
auc2 <- as.numeric(auc.tmp@y.values)
RFModelEval[14, 5] <- auc2
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_nb100,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "Naive Bayes 100%"
)
predictTest_nb100 <-
  predict(myNaiveBayes, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_nb100)
ConfMat_nb100 <- table(results_train_Test$TARGET, predictTest_nb100)
ConfMat_nb100
RFModelEval[14, 1] <- "100% Naive Bayes"
RFModelEval[14, 2] <- tictoc_NB$toc - tictoc_NB$tic
RFModelEval[14, 3] <-
  ConfMat_nb100[2, 2] / (ConfMat_nb100[2, 2] + ConfMat_nb100[2, 1])
RFModelEval[14, 4] <-
  (ConfMat_nb100[1, 1] + ConfMat_nb100[2, 2]) / sum(ConfMat_nb100[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(myNaiveBayes)

#k nearest neighboor
#MIGHT NEED TO REDO DUMMIES WITH N DUMMIES INSTEAD OF N-1
#Create dummy variables out of a categorical variable and include
#them instead of original categorical variable. Unlike regression,
#create k dummies instead of (k-1). For example, a categorical variable
#named "Department" has 5 unique levels / categories. So we will create 5
#dummy variables. Each dummy variable has 1 against its department and else 0.



library(caret)
tic("K nearest neighbor")
trctrl <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    classProbs = TRUE
  )
myKnn <-
  train(
    TARGET ~ . ,
    data = results_train_Train,
    method = "knn",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC" #,
    # tuneLength = tunel
  )
tictoc_Knn <- toc()
saveRDS(myKnn, "./myKnn.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# myKnn <- readRDS("./myKnn.rds")

# Summary of model
myKnn
plot(myKnn, main = "myKnn")

# Validation
myKnn_pred <- predict(myKnn, results_train_Test, type = "prob")

#Storing Model Performance Scores
knn_pred_val <- prediction(myKnn_pred[, 2], results_train_Test$TARGET)

# Calculating Area under Curve (AUC)
knn_perf_val <- performance(knn_pred_val, "auc")
auc.tmp <-
  performance(knn_pred_val, "auc")
auc3 <- as.numeric(auc.tmp@y.values)
RFModelEval[11, 5] <- auc3
knn_perf_val

# Plot AUC
knn_perf <- performance(knn_pred_val, "tpr", "fpr")
plot(
  knn_perf,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "KNN 100%"
)


#Calculating KS statistics
ks <-
  max(attr(knn_perf_val, "y.values")[[1]] - (attr(knn_perf_val, "x.values")[[1]]))
ks
ConfMat_knn <- table(results_train_Test$TARGET, myKnn_pred)
ConfMat_knn

RFModelEval[11, 1] <- "100% KNN"
RFModelEval[11, 2] <- tictoc_Knn$tic - tictoc_Knn$toc
RFModelEval[11, 3] <-
  ConfMat_knn[2, 2] / (ConfMat_knn[2, 2] + ConfMat_knn[2, 1])
RFModelEval[11, 4] <-
  (ConfMat_knn[1, 1] + ConfMat_knn[2, 2]) / sum(ConfMat_knn[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(myKnn)

#support vector machine
library(e1071)
tic("SVM")
mySvm <-
  svm(TARGET ~ ., data = results_train_Train, kernel = "linear")
tictoc_SVM <- toc()

saveRDS(mySvm, "./mySvm.rds")
# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# mySvm <- readRDS("./mySvm.rds")

plot(mySvm, results_train_Test, main = "Svm")
print(mySvm)

# TARGET is factor
svm_predict_100 <- predict(mySvm, results_train_Train)
ROCRpred_svm100 <-
  prediction(as.numeric(svm_predict_100), results_train_Train$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf_svm100 <- performance(ROCRpred_svm100, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_svm100, "auc")
auc4 <- as.numeric(auc.tmp@y.values)
RFModelEval[8, 5] <- auc4
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_svm100,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "Svm 100% Train test on self"
)
predictTest_svm100 <- predict(mySvm, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_svm100)
ConfMat_svm100 <-
  table(results_train_Test$TARGET, predictTest_svm100)
ConfMat_svm100
RFModelEval[8, 1] <- "100% SVM"
RFModelEval[8, 2] <- tictoc_SVM$tic - tictoc_SVM$toc
RFModelEval[8, 3] <-
  ConfMat_svm60[2, 2] / (ConfMat_svm60[2, 2] + ConfMat_svm60[2, 1])
RFModelEval[8, 4] <-
  (ConfMat_svm60[1, 1] + ConfMat_svm60[2, 2]) / sum(ConfMat_svm60[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(mySvm)


#Gradient Boosting trainer - using NUMERIC TARGET
library(xgboost)
dtrain <-
  xgb.DMatrix(
    data = as.matrix(results_train_Train_numeric[, 2.]),
    label = as.matrix(results_train_Train_numeric$TARGET)
  )
tic("XGBoost")
myXGB <-
  xgboost(
    data = dtrain,
    objective = "binary:logistic",
    nthread = 4,
    nrounds = 500,
    verbose = 2,
    max_depth = 10
  )
tictoc_XGB <- toc()

saveRDS(myXGB, "./myXGB.rds")
# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# mySvm <- readRDS("./myXGB.rds")

plot(myXGB, results_train_Test_numeric, main = "100% Xtreme Gradient Boost")
print(myXGB)

XGB_predict_100 <-
  predict(myXGB, as.matrix(results_train_Train_numeric[, 2.]))
ROCRpred_XGB100 <-
  prediction(as.numeric(XGB_predict_100),
             results_train_Train_numeric$TARGET)
ROCRperf_XGB100 <- performance(ROCRpred_XGB100, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_XGB100, "auc")
auc14 <- as.numeric(auc.tmp@y.values)
RFModelEval[15, 5] <- auc14
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_XGB100,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "XGB 100% Train test on self"
)
predictTest_XGB100 <-
  predict(myXGB, newdata = as.matrix(results_train_Test_numeric[, 2.]))


predictTest_Pred_XGB100 <-
  prediction(as.numeric(predictTest_XGB100),
             results_train_Test_numeric$TARGET)
predictTest_Perf_XGB100 <-
  performance(predictTest_Pred_XGB100 , "tpr", "fpr")

plot(
  predictTest_Perf_XGB100,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "XGB 100% Train test on TEST"
)
table(results_train_Test_numeric$TARGET, predictTest_XGB100 > .5)
ConfMat_XGB100 <-
  table(results_train_Test_numeric$TARGET, predictTest_XGB100 > .5)
ConfMat_XGB100
RFModelEval[15, 1] <- "100% XGB"
RFModelEval[15, 2] <- tictoc_SVM$tic - tictoc_SVM$toc
RFModelEval[15, 3] <-
  ConfMat_XGB100[2, 2] / (ConfMat_XGB100[2, 2] + ConfMat_XGB100[2, 1])
RFModelEval[15, 4] <-
  (ConfMat_XGB100[1, 1] + ConfMat_XGB100[2, 2]) / sum(ConfMat_XGB100[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(myXGB)
remove(dtrain)


#Random Forest
##Segment train set into subets for performance comparison - 5 10 20 30 60 80
results_train_Train_05 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.05),],
             row.names = NULL)

write.csv(results_train_Train_05, "results_train_Train_05.csv")

results_train_Train_10 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.1),],
             row.names = NULL)

write.csv(results_train_Train_10, "results_train_Train_10.csv")
results_train_Train_20 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.2),],
             row.names = NULL)

write.csv(results_train_Train_20, "results_train_Train_20.csv")
results_train_Train_30 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.3),],
             row.names = NULL)

write.csv(results_train_Train_30, "results_train_Train_30.csv")
results_train_Train_60 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.6),],
             row.names = NULL)

write.csv(results_train_Train_60, "results_train_Train_60.csv")
results_train_Train_80 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.8),],
             row.names = NULL)

write.csv(results_train_Train_80, "results_train_Train_80.csv")
results_train_Train_100 <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train)),],
             row.names = NULL)



remove(results_train_Train_05)
remove(results_train_Train_10)
remove(results_train_Train_20)
remove(results_train_Train_30)
remove(results_train_Train_60)
remove(results_train_Train_80)
remove(results_train_Train_100)
remove(results_train_Train)

#load the libraries
library(randomForest)
library(tictoc)
library(ROCR)

#train the model

#train with 5% of the 80% of original train data
#start tictoc timer
results_train_Train_05 <- read.csv("results_train_Train_05.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_05$TARGET <- as.factor(results_train_Train_05$TARGET)
tic("05 percent")
#train model
rf_classifier_05 = randomForest(TARGET ~ ., data = results_train_Train_05, importance = TRUE)
#stop timer
tictoc_05 <- toc()
saveRDS(rf_classifier_05, "./rf_classifier_05.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_05 <- readRDS("./rf_classifier_05.rds")

#write values to our performance evalation table
RFModelEval[1, 1] <- "5% Random Forest"
RFModelEval[1, 2] <- tictoc_05$toc - tictoc_05$tic
#test the model against the data on which it was trained
rf_predict_05 <- predict(rf_classifier_05, results_train_Train_05)
#predicition is a fucntion from package ROCR, rf_predict_05 is our prediction fuction
#from above, results_train_Train_05$TARGET is our TRUE OUTCOMES
ROCRpred_05 <-
  prediction(as.numeric(rf_predict_05),
             as.numeric(results_train_Train_05$TARGET))
ROCRperf_05 <- performance(ROCRpred_05, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_05,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 5% Train test on self"
)
#Now run prediction on test set
predictTest_05 <-
  predict(rf_classifier_05, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_05 > .5)
ROCRpred_05_Test <-
  prediction(as.numeric(predictTest_05),
             as.numeric(results_train_Test$TARGET))
ROCRperf_05_Test <- performance(ROCRpred_05_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_05_Test, "auc")
auc5 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_05 <- table(results_train_Test$TARGET, predictTest_05 > .5)
ConfMat_05
#Plot it
plot(
  ROCRperf_05_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 5% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[1, 5] <- auc5
RFModelEval[1, 3] <-
  ConfMat_05[2, 2] / (ConfMat_05[2, 2] + ConfMat_05[2, 1])
RFModelEval[1, 4] <-
  (ConfMat_05[1, 1] + ConfMat_05[2, 2]) / sum(ConfMat_05[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_05)
remove(rf_classifier_05)



#train with 10% of the 80% of original train data
#start tictoc timer
results_train_Train_10 <- read.csv("results_train_Train_10.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_10$TARGET <- as.factor(results_train_Train_10$TARGET)
tic("10 percent")
#train model
rf_classifier_10 = randomForest(TARGET ~ ., data = results_train_Train_10, importance = TRUE)
#stop timer
tictoc_10 <- toc()
saveRDS(rf_classifier_10, "./rf_classifier_10.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_10 <- readRDS("./rf_classifier_10.rds")

#write values to our performance evalation table
RFModelEval[2, 1] <- "10% Random Forest"
RFModelEval[2, 2] <- tictoc_10$toc - tictoc_10$tic
#test the model against the data on which it was trained
rf_predict_10 <- predict(rf_classifier_10, results_train_Train_10)
#predicition is a fucntion from package ROCR, rf_predict_10 is our prediction fuction
#from above, results_train_Train_10$TARGET is our TRUE OUTCOMES
ROCRpred_10 <-
  prediction(as.numeric(rf_predict_10),
             as.numeric(results_train_Train_10$TARGET))
ROCRperf_10 <- performance(ROCRpred_10, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_10,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 10% Train test on self"
)
#Now run prediction on test set
predictTest_10 <-
  predict(rf_classifier_10, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_10 > .5)
ROCRpred_10_Test <-
  prediction(as.numeric(predictTest_10),
             as.numeric(results_train_Test$TARGET))
ROCRperf_10_Test <- performance(ROCRpred_10_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_10_Test, "auc")
auc6 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_10 <- table(results_train_Test$TARGET, predictTest_10 > .5)
ConfMat_10
#Plot it
plot(
  ROCRperf_10_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 10% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[2, 5] <- auc6
RFModelEval[2, 3] <-
  ConfMat_10[2, 2] / (ConfMat_10[2, 2] + ConfMat_10[2, 1])
RFModelEval[2, 4] <-
  (ConfMat_10[1, 1] + ConfMat_10[2, 2]) / sum(ConfMat_10[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_10)
remove(rf_classifier_10)




#train with 10% of the 80% of original train data TARGET AS FACTOR
#start tictoc timer
results_train_Train_10 <- read.csv("results_train_Train_10.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_10$TARGET <- as.factor(results_train_Train_10$TARGET)
tic("10 percent FACTOR")
#train model
rf_classifier_10_factor = randomForest(TARGET ~ ., data = results_train_Train_10, importance = TRUE)
#stop timer
tictoc_10 <- toc()
saveRDS(rf_classifier_10_factor, "./rf_classifier_10_factor.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_10_factor <- readRDS("./rf_classifier_10_factor.rds")

#write values to our performance evalation table
RFModelEval[16, 1] <- "10% Random Forest"
RFModelEval[16, 2] <- tictoc_10$toc - tictoc_10$tic
#test the model against the data on which it was trained
rf_predict_10 <-
  predict(rf_classifier_10_factor, results_train_Train_10)
#predicition is a fucntion from package ROCR, rf_predict_10 is our prediction fuction
#from above, results_train_Train_10$TARGET is our TRUE OUTCOMES
ROCRpred_10 <-
  prediction(as.numeric(rf_predict_10),
             as.numeric(results_train_Train_10$TARGET))
ROCRperf_10 <- performance(ROCRpred_10, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_10,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 10% Train test on self FACTOR"
)
#Now run prediction on test set
predictTest_10 <-
  predict(rf_classifier_10_factor, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_10 > .5)
ROCRpred_10_Test <-
  prediction(as.numeric(predictTest_10),
             as.numeric(results_train_Test$TARGET))
ROCRperf_10_Test <- performance(ROCRpred_10_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_10_Test, "auc")
auc6 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_10 <- table(results_train_Test$TARGET, predictTest_10 > .5)
ConfMat_10
#Plot it
plot(
  ROCRperf_10_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 10% Train on TEST FACTOR"
)
#And add performance stats to evlauation table
RFModelEval[16, 5] <- auc6
RFModelEval[16, 3] <-
  ConfMat_10[2, 2] / (ConfMat_10[2, 2] + ConfMat_10[2, 1])
RFModelEval[16, 4] <-
  (ConfMat_10[1, 1] + ConfMat_10[2, 2]) / sum(ConfMat_10[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_10)
remove(rf_classifier_10_factor)





#train with 20% of the 80% of original train data
#start tictoc timer
results_train_Train_20 <- read.csv("results_train_Train_20.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
results_train_Train_20$TARGET <-
  as.factor(results_train_Train_20$TARGET)
tic("20 percent")
#train model
rf_classifier_20 = randomForest(TARGET ~ ., data = results_train_Train_20, importance = TRUE)
#stop timer
tictoc_20 <- toc()
saveRDS(rf_classifier_20, "./rf_classifier_20.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_20 <- readRDS("./rf_classifier_20.rds")

#write values to our performance evalation table
RFModelEval[3, 1] <- "20% Random Forest"
RFModelEval[3, 2] <- tictoc_20$toc - tictoc_20$tic
#test the model against the data on which it was trained
rf_predict_20 <- predict(rf_classifier_20, results_train_Train_20)
#predicition is a fucntion from package ROCR, rf_predict_20 is our prediction fuction
#from above, results_train_Train_20$TARGET is our TRUE OUTCOMES
ROCRpred_20 <-
  prediction(as.numeric(rf_predict_20),
             as.numeric(results_train_Train_20$TARGET))
ROCRperf_20 <- performance(ROCRpred_20, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_20,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 20% Train test on self"
)
#Now run prediction on test set
predictTest_20 <-
  predict(rf_classifier_20, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_20 > .5)
ROCRpred_20_Test <-
  prediction(as.numeric(predictTest_20),
             as.numeric(results_train_Test$TARGET))
ROCRperf_20_Test <- performance(ROCRpred_20_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_20_Test, "auc")
auc7 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_20 <- table(results_train_Test$TARGET, predictTest_20 > .5)
ConfMat_20
#Plot it
plot(
  ROCRperf_20_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 20% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[3, 5] <- auc7
RFModelEval[3, 3] <-
  ConfMat_20[2, 2] / (ConfMat_20[2, 2] + ConfMat_20[2, 1])
RFModelEval[3, 4] <-
  (ConfMat_20[1, 1] + ConfMat_20[2, 2]) / sum(ConfMat_20[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_20)
remove(rf_classifier_20)



#train with 30% of the 80% of original train data
#start tictoc timer
results_train_Train_30 <- read.csv("results_train_Train_30.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_30$TARGET <- as.factor(results_train_Train_30$TARGET)
tic("30 percent")
#train model
rf_classifier_30 = randomForest(TARGET ~ ., data = results_train_Train_30, importance = TRUE)
#stop timer
tictoc_30 <- toc()
saveRDS(rf_classifier_30, "./rf_classifier_30.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_30 <- readRDS("./rf_classifier_30.rds")

#write values to our performance evalation table
RFModelEval[4, 1] <- "30% Random Forest"
RFModelEval[4, 2] <- tictoc_30$toc - tictoc_30$tic
#test the model against the data on which it was trained
rf_predict_30 <- predict(rf_classifier_30, results_train_Train_30)
#predicition is a fucntion from package ROCR, rf_predict_30 is our prediction fuction
#from above, results_train_Train_30$TARGET is our TRUE OUTCOMES
ROCRpred_30 <-
  prediction(as.numeric(rf_predict_30),
             as.numeric(results_train_Train_30$TARGET))
ROCRperf_30 <- performance(ROCRpred_30, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_30,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 30% Train test on self"
)
#Now run prediction on test set
predictTest_30 <-
  predict(rf_classifier_30, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_30 > .5)
ROCRpred_30_Test <-
  prediction(as.numeric(predictTest_30),
             as.numeric(results_train_Test$TARGET))
ROCRperf_30_Test <- performance(ROCRpred_30_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_30_Test, "auc")
auc8 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_30 <- table(results_train_Test$TARGET, predictTest_30 > .5)
ConfMat_30
#Plot it
plot(
  ROCRperf_30_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 30% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[4, 5] <- auc8
RFModelEval[4, 3] <-
  ConfMat_30[2, 2] / (ConfMat_30[2, 2] + ConfMat_30[2, 1])
RFModelEval[4, 4] <-
  (ConfMat_30[1, 1] + ConfMat_30[2, 2]) / sum(ConfMat_30[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_30)
remove(rf_classifier_30)




#train with 60% of the 80% of original train data
#start tictoc timer
results_train_Train_60 <- read.csv("results_train_Train_60.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_60$TARGET <- as.factor(results_train_Train_60$TARGET)
tic("60 percent")
#train model
rf_classifier_60 = randomForest(TARGET ~ ., data = results_train_Train_60, importance = TRUE)
#stop timer
tictoc_60 <- toc()
saveRDS(rf_classifier_60, "./rf_classifier_60.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_60 <- readRDS("./rf_classifier_60.rds")

#write values to our performance evalation table
RFModelEval[5, 1] <- "60% Random Forest"
RFModelEval[5, 2] <- tictoc_60$toc - tictoc_60$tic
#test the model against the data on which it was trained
rf_predict_60 <- predict(rf_classifier_60, results_train_Train_60)
#predicition is a fucntion from package ROCR, rf_predict_60 is our prediction fuction
#from above, results_train_Train_60$TARGET is our TRUE OUTCOMES
ROCRpred_60 <-
  prediction(as.numeric(rf_predict_60),
             as.numeric(results_train_Train_60$TARGET))
ROCRperf_60 <- performance(ROCRpred_60, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_60,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 60% Train test on self"
)
#Now run prediction on test set
predictTest_60 <-
  predict(rf_classifier_60, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_60 > .5)
ROCRpred_60_Test <-
  prediction(as.numeric(predictTest_60),
             as.numeric(results_train_Test$TARGET))
ROCRperf_60_Test <- performance(ROCRpred_60_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_60_Test, "auc")
auc9 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_60 <- table(results_train_Test$TARGET, predictTest_60 > .5)
ConfMat_60
#Plot it
plot(
  ROCRperf_60_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 60% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[5, 5] <- auc9
RFModelEval[5, 3] <-
  ConfMat_60[2, 2] / (ConfMat_60[2, 2] + ConfMat_60[2, 1])
RFModelEval[5, 4] <-
  (ConfMat_60[1, 1] + ConfMat_60[2, 2]) / sum(ConfMat_60[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_60)
remove(rf_classifier_60)




#train with 80% of the 80% of original train data
#start tictoc timer
results_train_Train_80 <- read.csv("results_train_Train_80.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_80$TARGET <- as.factor(results_train_Train_80$TARGET)
tic("80 percent")
#train model
rf_classifier_80 = randomForest(TARGET ~ ., data = results_train_Train_80, importance = TRUE)
#stop timer
tictoc_80 <- toc()
saveRDS(rf_classifier_80, "./rf_classifier_80.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_80 <- readRDS("./rf_classifier_80.rds")

#write values to our performance evalation table
RFModelEval[6, 1] <- "80% Random Forest"
RFModelEval[6, 2] <- tictoc_80$toc - tictoc_80$tic
#test the model against the data on which it was trained
rf_predict_80 <- predict(rf_classifier_80, results_train_Train_80)
#predicition is a fucntion from package ROCR, rf_predict_80 is our prediction fuction
#from above, results_train_Train_80$TARGET is our TRUE OUTCOMES
ROCRpred_80 <-
  prediction(as.numeric(rf_predict_80),
             as.numeric(results_train_Train_80$TARGET))
ROCRperf_80 <- performance(ROCRpred_80, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_80,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 80% Train test on self"
)
#Now run prediction on test set
predictTest_80 <-
  predict(rf_classifier_80, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_80 > .5)
ROCRpred_80_Test <-
  prediction(as.numeric(predictTest_80),
             as.numeric(results_train_Test$TARGET))
ROCRperf_80_Test <- performance(ROCRpred_80_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_80_Test, "auc")
auc10 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_80 <- table(results_train_Test$TARGET, predictTest_80 > .5)
ConfMat_80
#Plot it
plot(
  ROCRperf_80_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 80% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[6, 5] <- auc10
RFModelEval[6, 3] <-
  ConfMat_80[2, 2] / (ConfMat_80[2, 2] + ConfMat_80[2, 1])
RFModelEval[6, 4] <-
  (ConfMat_80[1, 1] + ConfMat_80[2, 2]) / sum(ConfMat_80[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_80)
remove(rf_classifier_80)



#train with 100% of the 80% of original train data
#start tictoc timer
results_train_Train_100 <- read.csv("results_train_Train.csv")[-1]
#Comment the next line to run with TARGET as NUMERIC
# results_train_Train_100$TARGET <- as.factor(results_train_Train_100$TARGET)
tic("100 percent")
#train model
rf_classifier_100 = randomForest(TARGET ~ ., data = results_train_Train_100, importance = TRUE)
#stop timer
tictoc_100 <- toc()
saveRDS(rf_classifier_100, "./rf_classifier_100.rds")

# READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# rf_classifier_100 <- readRDS("./rf_classifier_100.rds")

#write values to our performance evalation table
RFModelEval[7, 1] <- "100% Random Forest"
RFModelEval[7, 2] <- tictoc_100$toc - tictoc_100$tic
#test the model against the data on which it was trained
rf_predict_100 <-
  predict(rf_classifier_100, results_train_Train_100)
#predicition is a fucntion from package ROCR, rf_predict_100 is our prediction fuction
#from above, results_train_Train_100$TARGET is our TRUE OUTCOMES
ROCRpred_100 <-
  prediction(as.numeric(rf_predict_100),
             as.numeric(results_train_Train_100$TARGET))
ROCRperf_100 <- performance(ROCRpred_100, "tpr", "fpr")

#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(
  ROCRperf_100,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 100% Train test on self"
)
#Now run prediction on test set
predictTest_100 <-
  predict(rf_classifier_100, newdata = results_train_Test)
table(results_train_Test$TARGET, predictTest_100 > .5)
ROCRpred_100_Test <-
  prediction(as.numeric(predictTest_100),
             as.numeric(results_train_Test$TARGET))
ROCRperf_100_Test <- performance(ROCRpred_100_Test, "tpr", "fpr")
auc.tmp <-
  performance(ROCRpred_100_Test, "auc")
auc11 <- as.numeric(auc.tmp@y.values)
#make a confusion matrix to compare predicted vs. actual on the TEST set
ConfMat_100 <-
  table(results_train_Test$TARGET, predictTest_100 > .5)
ConfMat_100
#Plot it
plot(
  ROCRperf_100_Test,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, .1),
  text.adj = c(-0.2, 1.7),
  main = "RF 100% Train on TEST"
)
#And add performance stats to evlauation table
RFModelEval[7, 5] <- auc11
RFModelEval[7, 3] <-
  ConfMat_100[2, 2] / (ConfMat_100[2, 2] + ConfMat_100[2, 1])
RFModelEval[7, 4] <-
  (ConfMat_100[1, 1] + ConfMat_100[2, 2]) / sum(ConfMat_100[1:2, 1:2])
View(RFModelEval)
write.csv(RFModelEval, "RFModelEval.csv")
remove(results_train_Train_100)
remove(rf_classifier_100)
















#
# ####OLD CODE, DELETE IF OTHER CODE WORKS
# #train with 10% of the 80% of original train data
# results_train_Train_10 <- read.csv("results_train_Train_10.csv")[-1]
# results_train_Train_10$TARGET <- as.factor(results_train_Train_10$TARGET)
#
# tic("10 percent")
# rf_classifier_10 = randomForest(data=results_train_Train_10, TARGET ~ .)
# tictoc_10 <- toc()
# saveRDS(rf_classifier_10, "./rf_classifier_10.rds")
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_10 <- readRDS("./rf_classifier_10.rds")
#
# RFModelEval[2, 1] <- "10% Random Forest"
# RFModelEval[2, 2] <- tictoc_10$toc - tictoc_10$tic
# rf_predict_10 <- predict(rf_classifier_10, results_train_Train_10)
# ROCRpred_10 <- prediction(as.numeric(rf_predict_10), as.numeric(results_train_Train_10$TARGET))
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, qualityTrain$Poorcare is our TRUE OUTCOMES
# ROCRperf_10 <- performance(ROCRpred_10, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_10,"auc"); auc6 <- as.numeric(auc.tmp@y.values)
# RFModelEval[2, 5] <- auc6
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_10, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 10% Train test on self")
# predictTest_10 <- predict(rf_classifier_10,
#                           newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_10)
# ConfMat_10 <- table(results_train_Test$TARGET, predictTest_10)
# ConfMat_10
# RFModelEval[2,3] <- ConfMat_10[2,2]/(ConfMat_10[2,2] + ConfMat_10[2,1])
# RFModelEval[2,4] <- (ConfMat_10[1,1]+ConfMat_10[2,2]) / sum(ConfMat_10[1:2, 1:2])
# View(RFModelEval)
# write.csv(RFModelEval, "RFModelEval.csv")
# remove(results_train_Train_10)
# remove(rf_classifier_10)
#
#
# #train with 20% of the 80% of original train data
# results_train_Train_20 <- read.csv("results_train_Train_20.csv")[-1]
# results_train_Train_20$TARGET <- as.factor(results_train_Train_20$TARGET)
# tic("20 percent")
# rf_classifier_20 = randomForest(data=results_train_Train_20, TARGET ~ .)
# tictoc_20 <- toc()
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_20 <- readRDS("./rf_classifier_20.rds")
#
# RFModelEval[3, 1] <- "20% Random Forest"
# RFModelEval[3, 2] <- tictoc_20$toc - tictoc_20$tic
# rf_predict_20 <- predict(rf_classifier_20, results_train_Train_20)
# ROCRpred_20 <- prediction(as.numeric(rf_predict_20), results_train_Train_20$TARGET)
# ROCRperf_20 <- performance(ROCRpred_20, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_20,"auc"); auc7 <- as.numeric(auc.tmp@y.values)
# RFModelEval[3, 5] <- auc7
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_20, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 20% Train test on self")
# predictTest_20 <- predict(rf_classifier_20, newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_20)
# ConfMat_20 <- table(results_train_Test$TARGET, predictTest_20)
# ConfMat_20
# remove(results_train_Train_20)
# remove(rf_classifier_20)
#
#
# #train with 30% of the 80% of original train data
# results_train_Train_30 <- read.csv("results_train_Train_30.csv")[-1]
# results_train_Train_30$TARGET <- as.factor(results_train_Train_30$TARGET)
# tic("30 percent")
# rf_classifier_30 = randomForest(data=results_train_Train_30, TARGET ~ .)
# tictoc_30 <- toc()
# saveRDS(rf_classifier_30, "./rf_classifier_30.rds")
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_30 <- readRDS("./rf_classifier_30.rds")
#
# RFModelEval[4, 1] <- "30% Random Forest"
# RFModelEval[4, 2] <- tictoc_30$toc - tictoc_30$tic
# rf_predict_30 <- predict(rf_classifier_30, results_train_Train_30)
# ROCRpred_30 <- prediction(as.numeric(rf_predict_30), as.numeric(results_train_Train_30$TARGET))
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, qualityTrain$Poorcare is our TRUE OUTCOMES
# ROCRperf_30 <- performance(ROCRpred_30, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_30,"auc"); auc8 <- as.numeric(auc.tmp@y.values)
# RFModelEval[4, 5] <- auc8
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_30, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 30% Train test on self")
# predictTest_30 <- predict(rf_classifier_30, newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_30)
# ConfMat_30 <- table(results_train_Test$TARGET, predictTest_30)
# ConfMat_30
# RFModelEval[4,3] <- ConfMat_30[2,2]/(ConfMat_30[2,2] + ConfMat_30[2,1])
# RFModelEval[4,4] <- (ConfMat_30[1,1]+ConfMat_30[2,2]) / sum(ConfMat_30[1:2, 1:2])
# View(RFModelEval)
# write.csv(RFModelEval, "RFModelEval.csv")
# remove(results_train_Train_30)
# remove(rf_classifier_30)
#
#
# #train with 60% of the 80% of original train data
# results_train_Train_60 <- read.csv("results_train_Train_60.csv")[-1]
# results_train_Train_60$TARGET <- as.factor(results_train_Train_60$TARGET)
# tic("60 percent")
# rf_classifier_60 = randomForest(data=results_train_Train_60, TARGET ~ .)
# tictoc_60 <- toc()
# saveRDS(rf_classifier_60, "./rf_classifier_60.rds")
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_60 <- readRDS("./rf_classifier_60.rds")
#
# RFModelEval[5, 1] <- "60% Random Forest"
# RFModelEval[5, 2] <- tictoc_60$toc - tictoc_60$tic
# rf_predict_60 <- predict(rf_classifier_60, results_train_Train_60)
# ROCRpred_60 <- prediction(as.numeric(rf_predict_60), as.numeric(results_train_Train_60$TARGET))
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, qualityTrain$Poorcare is our TRUE OUTCOMES
# ROCRperf_60 <- performance(ROCRpred_60, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_60,"auc"); auc9 <- as.numeric(auc.tmp@y.values)
# RFModelEval[5, 5] <- auc9
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_60, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 60% Train test on self")
# predictTest_60 <- predict(rf_classifier_60, newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_60)
# ConfMat_60 <- table(results_train_Test$TARGET, predictTest_60)
# ConfMat_60
# RFModelEval[5,3] <- ConfMat_60[2,2]/(ConfMat_60[2,2] + ConfMat_60[2,1])
# RFModelEval[5,4] <- (ConfMat_60[1,1]+ConfMat_60[2,2]) / sum(ConfMat_60[1:2, 1:2])
# View(RFModelEval)
# write.csv(RFModelEval, "RFModelEval.csv")
# remove(results_train_Train_60)
# remove(rf_classifier_60)
#
#
# #train with 80% of the 80% of original train data
# tic("80 percent")
# results_train_Train_80 <- read.csv("results_train_Train_80.csv")[-1]
# results_train_Train_80$TARGET <- as.factor(results_train_Train_80$TARGET)
# rf_classifier_80 = randomForest(data=results_train_Train_80, TARGET ~ .)
# tictoc_80 <- toc()
# saveRDS(rf_classifier_80, "./rf_classifier_80.rds")
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_80 <- readRDS("./rf_classifier_80.rds")
#
# RFModelEval[6, 1] <- "80% Random Forest"
# RFModelEval[6, 2] <- tictoc_80$toc - tictoc_80$tic
# rf_predict_80 <- predict(rf_classifier_80, results_train_Train_80)
# ROCRpred_80 <- prediction(as.numeric(rf_predict_80), as.numeric(results_train_Train_80$TARGET))
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, qualityTrain$Poorcare is our TRUE OUTCOMES
# ROCRperf_80 <- performance(ROCRpred_80, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_80,"auc"); auc10 <- as.numeric(auc.tmp@y.values)
# RFModelEval[6, 5] <- auc10
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_80, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 80% Train test on self")
# predictTest_80 <- predict(rf_classifier_80, newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_80)
# ConfMat_80 <- table(results_train_Test$TARGET, predictTest_80)
# ConfMat_80
# RFModelEval[6,3] <- ConfMat_80[2,2]/(ConfMat_80[2,2] + ConfMat_80[2,1])
# RFModelEval[6,4] <- (ConfMat_80[1,1]+ConfMat_80[2,2]) / sum(ConfMat_80[1:2, 1:2])
# View(RFModelEval)
# write.csv(RFModelEval, "RFModelEval.csv")
# remove(results_train_Train_80)
# remove(rf_classifier_80)
#
#
# #train with 100% of the 80% of original train data
# results_train_Train_100 <- read.csv("results_train_Train_100.csv")[-1]
# results_train_Train$TARGET <- as.factor(results_train_Train$TARGET)
# tic("100 percent")
# rf_classifier_100 = randomForest(data=results_train_Train_100, TARGET ~ .)
# tictoc_100 <- toc()
# saveRDS(rf_classifier_100, "./rf_classifier_100.rds")
#
#
# # READ THE TRAINER IF IT WAS PREVIOUSLY RUN AND SAVED
# # rf_classifier_100 <- readRDS("./rf_classifier_100.rds")
#
# RFModelEval[7, 1] <- "100% Random Forest"
# RFModelEval[7, 2] <- tictoc_100$toc - tictoc_100$tic
# rf_predict_100 <- predict(rf_classifier_100, results_train_Train_100)
# ROCRpred_100 <- prediction(as.numeric(rf_predict_100), as.numeric(results_train_Train_100$TARGET))
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, qualityTrain$Poorcare is our TRUE OUTCOMES
# ROCRperf_100 <- performance(ROCRpred_100, "tpr", "fpr")
# auc.tmp <- performance(ROCRpred_100,"auc"); auc11 <- as.numeric(auc.tmp@y.values)
# RFModelEval[7, 5] <- auc11
# #tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
# plot(ROCRperf_100, colorize = TRUE, print.cutoffs.at = seq(0,1,.1),
#      text.adj = c(-0.2, 1.7), main = "RF 100% Train test on self")
# predictTest_100 <- predict(rf_classifier_100, newdata = results_train_Test)
# table(results_train_Test$TARGET, predictTest_100)
# ConfMat_100 <- table(results_train_Test$TARGET, predictTest_100)
# ConfMat_100
# RFModelEval[7,3] <- ConfMat_100[2,2]/(ConfMat_100[2,2] + ConfMat_100[2,1])
# RFModelEval[7,4] <- (ConfMat_100[1,1]+ConfMat_100[2,2]) / sum(ConfMat_100[1:2, 1:2])
# View(RFModelEval)
# write.csv(RFModelEval, "RFModelEval.csv")
# remove(results_train_Train)
# remove(rf_classifier_100)