library(randomForest)
library(tidyverse)
library(jsonlite)
  
#try with spline regression dataset
#read in dataset
raw_test<-read.csv("Data/final/test_models_spline.csv")
raw_train<-read.csv("Data/final/train_models_spline.csv")

#make outcome a factor
test<-raw_test %>% dplyr::select(-c("subject"))
train<-raw_train %>% mutate(age_group=as.factor(age_group)) %>% dplyr::select(-c("subject"))

#random forest - Accuracy=.7486    Kappa=.6983
set.seed(100)
rf1 <- randomForest(age_group~., data=train, do.trace=T,ntree=500)
preds <- predict(rf1,test)

#export the predictions as a JSON
json_out<-toJSON(as.list(setNames(as.integer(as.character(preds)),raw_test$subject)), auto_unbox = T, pretty = T)
write(json_out,'Output/predictions/final/model_1_predictions.json')
