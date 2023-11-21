library(randomForest)
library(tidyverse)
library(jsonlite)
library(caret)
library(Metrics)

result_list=list()
for (i in c(1,2,3,4,5)) {
  
  #try with spline regression dataset
  #read in dataset
  raw_test<-read.csv(paste0("Data/five_fold_spline/test",i,".csv"))
  raw_train<-read.csv(paste0("Data/five_fold_spline/train",i,".csv"))
  
  #make outcome a factor
  test<-raw_test %>% mutate(age_group=as.factor(age_group)) %>% dplyr::select(-c("subject"))
  train<-raw_train %>% mutate(age_group=as.factor(age_group)) %>% dplyr::select(-c("subject"))
  
  #random forest - Accuracy=.7486    Kappa=.6983
  set.seed(100)
  rf1 <- randomForest(age_group~., data=train, do.trace=T,ntree=500)
  preds <- predict(rf1,test)
  print(confusionMatrix(preds,test$age_group)$overall[c('Accuracy','Kappa')])
  
  results<-data.frame('fold'=i,
                      'accuracy'=confusionMatrix(preds,test$age_group)$overall[c('Accuracy')],
                      'rmse'=rmse(as.integer(as.character(preds)),as.integer(as.character(test$age_group))),
                      'mae'=mae(as.integer(as.character(preds)),as.integer(as.character(test$age_group))))
  result_list[[i]]<-results
}

master_results<-do.call(rbind,result_list)

# #export the predictions as a csv and JSON
# export<-c()
# export$subject<-raw_test$subject
# export$predicted_age_group<-preds
# export<-as.data.frame(export)
# write.csv(export,'Output/predictions/RF_no_fgen_predictions.csv',row.names=F)
# 
# json_out<-toJSON(as.list(setNames(as.integer(as.character(preds)),raw_test$subject)), auto_unbox = T, pretty = T)
# write(json_out,'Output/predictions/RF_no_fgen_predictions.json')
