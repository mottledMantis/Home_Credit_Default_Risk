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
library(jpeg)
library(imager)
library(kknn)
library(klaR)
library(ranger)
library(doParallel)


cl <- makePSOCKcluster(4)
registerDoParallel(cl)


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


#IF CHANGE START HERE
results_train_Train <-
  data.frame(read.csv("results_train_Train.csv"), check.names = TRUE)
results_train_Train <- results_train_Train[-1]
results_train_Train$TARGET <- as.factor(results_train_Train$TARGET)


###PARAMETERS FOR THE MODELS###
#set value for r - index for use in RFModelEval
r <- 1

#add a comment to the EvalModel?
comm <- "Downsampled training set"

#sampling method and other info for caret trainControl models
samp <- "repeatedCV"
resamp <- "down"
num <- 10
reps <- 3
# n = default percentage of data used for training (as a 2 place decimal)
#note - if this changes, must run code starting at IF CHANGE START HERE above
n <- 1

###END PARAMETERS FOR MODELS


results_train_Train <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * n),],
             row.names = NULL)

# make sure the column names are compliant
names(results_train_Train$TARGET) <-
  make.names(results_train_Train$TARGET)
# 
# #create a duplicate dataset with BALANCED TARGET values for non-caret
# down_train <- downSample(x = trainData[, -ncol(results_train_Train) + 1],
#                          y = trainData$TARGET)
# down_train[, ncol(down_train)] <- NULL
# table(down_train$TARGET)
# 

# feature.names <- names(results_train_Train)
#
# for (f in feature.names) {
#   if (class(results_train_Train[[f]]) == "factor") {
#     levels <- unique(c(results_train_Train[[f]]))
#     results_train_Train[[f]] <- factor(results_train_Train[[f]],
#                                        labels = make.names(levels))
#   }
# }


#SHUFFLE THE TRAIN DATA
results_train_Train <-
  results_train_Train[sample(nrow(results_train_Train)),]

#CARET REQUIRES DIFFERENT NAMES
feature.names <- names(results_train_Train)

for (f in feature.names) {
  if (class(results_train_Train[[f]]) == "factor") {
    levels <- unique(c(results_train_Train[[f]]))
    results_train_Train[[f]] <- factor(results_train_Train[[f]],
                             labels = make.names(levels))
  }
}

#GLM model
trainData = results_train_Train
tic("GLM")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

myGLM <-
  train(
    TARGET ~ . ,
    data = trainData,
    method = "glm",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC",
    tuneLength = 10
  )
tictoc_temp <- toc()
saveRDS(myGLM, "./assets/myGLM.rds")
RFModelEval[r, 1] <- "100% GLM"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1

# 
# #Simple Tree
# library(rpart)
# trainData = results_train_Train
# 
# tic("simple tree")
# myTree = rpart(TARGET ~ ., data = trainData, method = "class")
# tictoc_temp = toc()
# saveRDS(myTree, "./assets/myTree.rds")
# RFModelEval[r, 1] <- "100% Simple Tree"
# RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
# RFModelEval[r, 10] <- comm
# write.csv(RFModelEval, "./assets/RFModelEval2.csv")
# 
# r <- r + 1

#naive bayes
library(e1071)
library(klaR)
trainData = results_train_Train

tic("Naive Bayes")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

myNaiveBayes <-
  train(
    TARGET ~ . ,
    data = trainData,
    method = "nb",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC",
    tuneLength = 10
  )
tictoc_temp <- toc()
saveRDS(myNaiveBayes, "./assets/myNaiveBayes.rds")
RFModelEval[r, 1] <- "100% Naive Bayes"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1

#k nearest neighboor
library(caret)
library(kknn)
trainData = results_train_Train

tic("K nearest neighbor")
trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )


myKnn <-
  train(
    TARGET ~ . ,
    data = trainData,
    method = "kknn",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC",
    tuneLength = 10
  )
tictoc_temp <- toc()
saveRDS(myKnn, "./assets/myKnn.rds")
RFModelEval[r, 1] <- "100% KNN"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#support vector machine
library(e1071)
trainData = results_train_Train

tic("SVM")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

mySvm <-
  train(
    TARGET ~ . ,
    data = trainData,
    method = "svmLinearWeights",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC",
    tuneLength = 10
  )
tictoc_temp <- toc()

saveRDS(mySvm, "./assets/mySvm.rds")
RFModelEval[r, 1] <- "100% SVM"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#Gradient Boosting trainer - using NUMERIC TARGET
library(xgboost)
trainData = results_train_Train
# trainData$TARGET <- as.numeric(trainData$TARGET) - 1

dtrain <-
  xgb.DMatrix(data = as.matrix(trainData[, 2.]),
              label = as.matrix(trainData$TARGET))
tic("XGBoost")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

myXGB <-
  train(
    TARGET ~ . ,
    data = trainData,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trctrl,
    metric = "ROC",
    tuneLength = 10
  )
tictoc_temp <- toc()

saveRDS(myXGB, "./assets/myXGB.rds")
RFModelEval[r, 1] <- "100% Gradient Boosting"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")

r <- r + 1

#Random Forest
# ##Segment train set into subets for performance comparison - 5 10 20 30 60 80
# results_train_Train_05 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.05),],
#              row.names = NULL)
#
# write.csv(results_train_Train_05, "results_train_Train_05.csv")
#
# results_train_Train_10 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.1),],
#              row.names = NULL)
#
# write.csv(results_train_Train_10, "results_train_Train_10.csv")
# results_train_Train_20 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.2),],
#              row.names = NULL)
#
# write.csv(results_train_Train_20, "results_train_Train_20.csv")
# results_train_Train_30 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.3),],
#              row.names = NULL)
#
# write.csv(results_train_Train_30, "results_train_Train_30.csv")
# results_train_Train_60 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.6),],
#              row.names = NULL)
#
# write.csv(results_train_Train_60, "results_train_Train_60.csv")
# results_train_Train_80 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train) * 0.8),],
#              row.names = NULL)
#
# write.csv(results_train_Train_80, "results_train_Train_80.csv")
# results_train_Train_100 <-
#   data.frame(results_train_Train[sample(nrow(results_train_Train),
#                                         nrow(results_train_Train)),],
#              row.names = NULL)
#
#
#
# remove(results_train_Train_05)
# remove(results_train_Train_10)
# remove(results_train_Train_20)
# remove(results_train_Train_30)
# remove(results_train_Train_60)
# remove(results_train_Train_80)
# remove(results_train_Train_100)
# remove(results_train_Train)



#train the model

#train with 5% of the 80% of original train data

trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.05),],
             row.names = NULL)

#start tictoc timer
tic("05 percent")


trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_05 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)
#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_05, "./assets/rf_classifier_05.rds")
RFModelEval[r, 1] <- "5% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#train with 10% of the 80% of original train data

trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.1),],
             row.names = NULL)

#start tictoc timer
tic("10 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_10 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_10, "./assets/rf_classifier_10.rds")
RFModelEval[r, 1] <- "10% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#train with 20% of the 80% of original train data

trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.2),],
             row.names = NULL)
#start tictoc timer
tic("20 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_20 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_20, "./assets/rf_classifier_20.rds")
RFModelEval[r, 1] <- "20% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#train with 30% of the 80% of original train data

trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.3),],
             row.names = NULL)
#start tictoc timer
tic("30 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_30 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_30, "./assets/rf_classifier_30.rds")
RFModelEval[r, 1] <- "30% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")

r <- r + 1

#train with 60% of the 80% of original train data
trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.6),],
             row.names = NULL)

#start tictoc timer
tic("60 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_60 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_60, "./assets/rf_classifier_60.rds")
RFModelEval[r, 1] <- "60% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#train with 80% of the 80% of original train data
trainData <-
  data.frame(results_train_Train[sample(nrow(results_train_Train),
                                        nrow(results_train_Train) * 0.8),],
             row.names = NULL)

#start tictoc timer
tic("80 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_80 = train(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_80, "./assets/rf_classifier_80.rds")
RFModelEval[r, 1] <- "80% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1


#train with 100% of the 80% of original train data
trainData <- results_train_Train

#start tictoc timer
tic("100 percent")

trctrl <-
  trainControl(
    method = samp,
    number = num,
    classProbs = TRUE,
    sampling = resamp,
    repeats = reps
  )

#train model
rf_classifier_100 = randomForesttrain(
  TARGET ~ . ,
  data = trainData,
  method = "ranger",
  preProcess = c("center", "scale"),
  trControl = trctrl,
  metric = "ROC",
  tuneLength = 10
)

#stop timer
tictoc_temp <- toc()
saveRDS(rf_classifier_100, "./assets/rf_classifier_100.rds")
RFModelEval[r, 1] <- "100% Random Forest"
RFModelEval[r, 2] <- tictoc_temp$toc - tictoc_temp$tic
RFModelEval[r, 10] <- comm
write.csv(RFModelEval, "./assets/RFModelEval2.csv")


r <- r + 1

stopCluster(cl)
