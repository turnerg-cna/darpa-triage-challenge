library(tidyverse)
library(ggplot2)
library(Metrics)
library(jsonlite)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in test data for comparison
test_data<-read.csv('Data/test_models_spline.csv') %>% 
  dplyr::select(c('subject','age_group')) %>% 
  mutate(age_group=as.factor(age_group),
         int_actual=as.numeric(as.character(age_group)))

### Random forest spline data ###
model_name='CNN with linear interpolation'

#read in preds as file
# preds<-read.csv('Output/predictions/cnn_predictions_51p.csv') %>%
#   mutate(prediction=as.factor(predicted_age_group),
#          int_prediction=as.numeric(as.character(prediction)))

#read in preds as a JSON file
json<-fromJSON("Output/predictions/111523_Mlloyd_tested_sample.json",flatten=T)
df_json<-as.data.frame(t(as.data.frame(json)))
preds<-df_json %>% mutate(subject=substr(rownames(df_json),2,10),
                          int_prediction=as.numeric(V1),
                          prediction=as.factor(int_prediction))

#generate accuracy metrics
cm<-confusionMatrix(preds$prediction,test_data$age_group)
prop_cm<-as.data.frame(round(prop.table(cm$table,margin=2)*100,0))

precise_accuracy<-round(cm$overall[c('Accuracy')]*100,2)
accuracy<-round(cm$overall[c('Accuracy')]*100,0)
rmse<-rmse(preds$int_prediction,test_data$int_actual)

ggplot(prop_cm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=paste0(Freq,"%"))) +
  scale_fill_gradient(low="white", high="#009194", name="Percent by row") + 
  ggtitle(paste0("Confusion matrix for ",model_name," (",accuracy, "% accuracy)"))

precise_accuracy
rmse

#ggsave(paste0("Output/confusion matrices/Confusion matrix for ",model_name,".png"),width=8,height=6)

