library(caret)
library(tidyverse)
library(randomForest)
library(xgboost)
library(ggplot2)
library(klaR)
library(jsonlite)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in dataset
test<-read.csv("Data/test_models_linear_interp.csv")
train<-read.csv("Data/train_models_linear_interp.csv")

#make outcome a factor
test<-test %>% mutate(age_group=as.factor(age_group)) %>% select(-c('subject'))
train<-train %>% mutate(age_group=as.factor(age_group)) %>% select(-c('subject'))

#SVM - Accuracy=.4371    Kappa=.3245
train_control <- trainControl(method="repeatedcv", number=5, repeats=1, verboseIter=TRUE)
svm1 <- train(age_group~., data=train, method = "svmLinear", trControl = train_control)
preds <- predict(svm1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]
#It's not horrible, but I'd definitely like to be better

#random forest - Accuracy=.5143    Kappa=.4171
rf1 <- randomForest(age_group~., data=train, do.trace=T, ntree=1000, mtry=80)
preds <- predict(rf1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]


#XGB - Accuracy=.4729    Kappa=.3674
tune_grid <- expand.grid(
  nrounds=seq(50,1000,50),
  eta=c(.025,.05,.1,.2),
  max_depth=c(5),
  gamma=0,
  colsample_bytree=1,
  min_child_weight=1,
  subsample=1
)
train_control <- trainControl(method="cv", number=2, verboseIter=TRUE, allowParallel = TRUE)
xgb1 <- train(age_group~., data=train, method = "xgbTree", trControl = train_control, tuneGrid=tune_grid, verbose=TRUE)
preds <- predict(xgb1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]

tuneplot <- function(x, probs = 1) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

tuneplot(xgb1)


#Naive bayes Accuracy=.2629   Kappa=.1149
train_control <- trainControl(method="cv", number=2, verboseIter=TRUE, allowParallel = TRUE)
nb1 <- train(age_group~., data=train, method = "nb", trControl = train_control, verbose=TRUE)
preds <- predict(nb1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]

#svm is NOT good?
train_control <- trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)
svm1 <- train(age_group~., data=train, method = "svmLinear", trControl = train_control, verbose=T)
preds <- predict(svm1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]



#try with spline regression dataset
#read in dataset
raw_test<-read.csv("Data/test_models_spline.csv")
raw_train<-read.csv("Data/train_models_spline.csv")

#make outcome a factor
test<-raw_test %>% mutate(age_group=as.factor(age_group)) %>% dplyr::select(-c("subject"))
train<-raw_train %>% mutate(age_group=as.factor(age_group)) %>% dplyr::select(-c("subject"))

#random forest - Accuracy=.7486    Kappa=.6983
set.seed(100)
rf1 <- randomForest(age_group~., data=train, do.trace=T)
preds <- predict(rf1,test)
confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')]

#export the predictions as a csv and JSON
export<-c()
export$subject<-raw_test$subject
export$predicted_age_group<-preds
export<-as.data.frame(export)
write.csv(export,'Output/predictions/RF_no_fgen_predictions.csv',row.names=F)

json_out<-toJSON(as.list(setNames(as.integer(as.character(preds)),raw_test$subject)), auto_unbox = T, pretty = T)
write(json_out,'Output/predictions/RF_no_fgen_predictions.json')
