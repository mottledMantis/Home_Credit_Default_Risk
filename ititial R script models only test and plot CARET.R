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
library(png)
library(imager)

results_train_Test <-
  data.frame(read.csv("results_train_Test.csv"), check.names = TRUE)
results_train_Test <- results_train_Test[-1]
results_train_Test$TARGET <- as.factor(results_train_Test$TARGET)

names(results_train_Test$TARGET) <- make.names(results_train_Test$TARGET)

#CARET REQUIRES DIFFERENT NAMES
feature.names <- names(results_train_Test)

for (f in feature.names) {
  if (class(results_train_Test[[f]]) == "factor") {
    levels <- unique(c(results_train_Test[[f]]))
    results_train_Test[[f]] <- factor(results_train_Test[[f]],
                             labels = make.names(levels))
  }
}

# feature.names = names(results_train_Test)
# 
# for (f in feature.names) {
#   if (class(results_train_Test[[f]]) == "factor") {
#     levels <- unique(c(results_train_Test[[f]]))
#     results_train_Test[[f]] <- factor(results_train_Test[[f]],
#                                       labels = make.names(levels))
#   }
# }

r <- 1

RFModelEval <-
  as.data.frame(read.csv("./assets/RFModelEval2.csv", stringsAsFactors = FALSE))

RFModelEval$X <- NULL

names(RFModelEval) <- c(
  "Pct of train data used and model type",
  "Training time in seconds",
  "Sensitivity: TP/(TP+FN)",
  "Accuracy: (TP+TN)/N",
  "AUC Score",
  "Specificity: (TN/N)",
  "Precision: TP/(TP+FP)",
  "ntree",
  "mtry",
  "Comment"
)






#GLM model
# READ THE TRAINER
this.Model <- readRDS("./assets/myGLM.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest$X2 > .5)
confMat <- confMat[c(2,1) , c(2,1)]

write.csv(confMat, "./assets/confGLM.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(as.data.frame(predictTest$X2), results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_GLM_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(perfMin,
                         perfMax,
                         (perfMax - perfMin)/50),
  text.adj = c(-0.2, 1.7),
  main = "GLM 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1
# 
# #Simple Tree
# library(rpart)
# 
# this.Model <- readRDS("./assets/myTree.rds")
# RFModelEvalIndex = r
# 
# predictTest <-
#   predict(this.Model, newdata = results_train_Test, type = "class")
# 
# confMat <- table(results_train_Test$TARGET, predictTest)
# confMat <- confMat[c(2,1) , c(2,1)]
# write.csv(confMat, "./assets/confTree.csv")
# 
# RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
# RFModelEval[RFModelEvalIndex, 4] <-
#   (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
# RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
# RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
# 
# #TP FN
# #FP TN
# 
# #Now run prediction on test set
# ROCRpred_TEST <- prediction(as.numeric(predictTest), results_train_Test$TARGET)
# #predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
# #from above, results_train_Train$TARGET is our TRUE OUTCOMES
# ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
# attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1
# 
# auc.tmp <-
#   performance(ROCRpred_TEST, "auc")
# RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)
# 
# perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
# perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
# png('./assets/rf_Tree_TEST.png', width = 600, height = 600)
# ROCR::plot(
#   ROCRperf_TEST,
#   colorize = TRUE,
#   lwd = 3,
#   print.cutoffs.at = seq(perfMin,
#                          perfMax,
#                          (perfMax - perfMin)/50),
#   text.adj = c(-0.2, 1.7),
#   main = "Simple Tree 100% on TEST"
# )
# abline(a = 0, b = 1, lwd = 3, col = "black")
# dev.off()
# write.csv(RFModelEval, "./assets/RFModelEval2.csv")
# remove(this.Model)
# 
# r <- r + 1


#naive bayes
library(e1071)

this.Model <- readRDS("./assets/myNaiveBayes.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest$X1 > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confNB.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(as.numeric(predictTest), results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_NB_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(perfMin,
                         perfMax,
                         (perfMax - perfMin)/50),
  text.adj = c(-0.2, 1.7),
  main = "Naive Bayes 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1



#k nearest neighboor
library(caret)

this.Model <- readRDS("./assets/myKnn.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")
levels(predictTest) <- c(0, 1)

confMat <- table(results_train_Test$TARGET, predictTest$X1 > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confKnn.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest$X2, results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_Knn_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(perfMin,
                         perfMax,
                         (perfMax - perfMin)/10),
  text.adj = c(-0.2, 1.7),
  main = "K Nearest Neighbor 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1

#support vector machine
library(e1071)

this.Model <- readRDS("./assets/mySvm.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "class")

confMat <- table(results_train_Test$TARGET, predictTest)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confSvm.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(as.numeric(predictTest), (results_train_Test$TARGET))
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
# attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

# perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
# perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_svm_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         0.1),
  text.adj = c(-0.2, 1.7),
  main = "Support Vector Machine 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1

#XGBoost
this.Model <- readRDS("./assets/myXGB.rds")
RFModelEvalIndex = r
data.temp <- results_train_Test
data.temp$TARGET <- as.integer(data.temp$TARGET)

predictTest <-
  predict(this.Model, newdata = xgb.DMatrix(as.matrix(data.temp)))

confMat <- table(results_train_Test$TARGET, c(predictTest > .5))
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confXGB.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(as.numeric(predictTest), results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_XGB_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(perfMin,
                         perfMax,
                         (perfMax - perfMin)/10),
  text.adj = c(-0.2, 1.7),
  main = "Xtreme Gradient Boost 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)


r <- r + 1

#Random Forest


#load the libraries
library(randomForest)
library(tictoc)
library(ROCR)


#Random Forest Trained on 5% of train data
this.Model <- readRDS("./assets/rf_classifier_05.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF05.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF05_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 05% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1


#Random Forest Trained on 10% of train data
this.Model <- readRDS("./assets/rf_classifier_10.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF10.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF10_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 10% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1

#Random Forest Trained on 20% of train data
this.Model <- readRDS("./assets/rf_classifier_20.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF20.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF20_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 20% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1

#Random Forest Trained on 30% of train data
this.Model <- readRDS("./assets/rf_classifier_30.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF30.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF30_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 30% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1



#Random Forest Trained on 60% of train data
this.Model <- readRDS("./assets/rf_classifier_60.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF60.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF60_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 60% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1



#Random Forest Trained on 80% of train data
this.Model <- readRDS("./assets/rf_classifier_80.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF80.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF80_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 80% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1


#Random Forest Trained on 100% of train data
this.Model <- readRDS("./assets/rf_classifier_100.rds")
RFModelEvalIndex = r

predictTest <-
  predict(this.Model, newdata = results_train_Test, type = "prob")

confMat <- table(results_train_Test$TARGET, predictTest[,2] > .5)
confMat <- confMat[c(2,1) , c(2,1)]
write.csv(confMat, "./assets/confRF100.csv")

RFModelEval[RFModelEvalIndex, 3] <- confMat[1, 1] / (confMat[1, 1] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 4] <-
  (confMat[1, 1] + confMat[2, 2]) / sum(confMat[1:2, 1:2])
RFModelEval[RFModelEvalIndex, 6] <- confMat[2, 2] / (confMat[2, 2] + confMat[1, 2])
RFModelEval[RFModelEvalIndex, 7] <- confMat[1, 1] / (confMat[1, 1] + confMat[2, 1])
#TP FN
#FP TN

#Now run prediction on test set
ROCRpred_TEST <- prediction(predictTest[,2], results_train_Test$TARGET)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, results_train_Train$TARGET is our TRUE OUTCOMES
ROCRperf_TEST <- performance(ROCRpred_TEST, "tpr", "fpr")
attributes(ROCRperf_TEST)$alpha.values[[1]][1] <- 1

auc.tmp <-
  performance(ROCRpred_TEST, "auc")
RFModelEval[RFModelEvalIndex, 5] <- as.numeric(auc.tmp@y.values)

perfMin <- min(unlist(ROCRperf_TEST@alpha.values))
perfMax <- max(unlist(ROCRperf_TEST@alpha.values))
png('./assets/rf_RF100_TEST.png', width = 600, height = 600)
ROCR::plot(
  ROCRperf_TEST,
  colorize = TRUE,
  lwd = 3,
  print.cutoffs.at = seq(0,
                         1,
                         .1),
  text.adj = c(-0.2, 1.7),
  main = "Random Forest 100% on TEST"
)
abline(a = 0, b = 1, lwd = 3, col = "black")
dev.off()
write.csv(RFModelEval, "./assets/RFModelEval2.csv")
remove(this.Model)

r <- r + 1