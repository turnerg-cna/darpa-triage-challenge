library(tidyverse)
library(reshape2)
library(caTools)
library(zoo)
library(splines)
library(Metrics)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in training datasets
brach_data<-read.csv("data/raw/brachP_train_data.csv")
aorta_data<-read.csv("data/raw/aortaP_train_data.csv")

###make an unmodified dataset to get column order correct below###
merged<-merge(brach_data,aorta_data, by=c('X','target')) %>% arrange(X) %>% rename(age_group="target",subject="X")

###linear interpolation of NA values###
#Let's reshape and rename the data to make it easier to work with
reshaped_brach_data<-brach_data %>% gather("time","value",2:337) %>%
  rename(subject=X, age_group=target) %>% mutate(dataset="brach")%>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$")))

reshaped_aorta_data<-aorta_data %>% gather("time","value",2:337) %>% 
  rename(subject=X, age_group=target) %>% mutate(dataset="aorta")%>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$")))

#put the data together for now
master_data<-rbind(reshaped_aorta_data,reshaped_brach_data)

###SPLINES to replace data###
spline_data<-master_data %>% group_by(subject, dataset) %>% mutate(spline=predict(lm(value~bs(time,knots=c(100,150,200))),as.data.frame(time)))

spline_no_na<-spline_data %>% filter(!is.na(value))
spline_mse<- mse(spline_no_na$value,spline_no_na$spline)

###now, let's export the spline data
export_spline<-spline_data %>% dplyr::select(-c("value"))

#make it wide so models like it
export_spline_wide_data<-export_spline %>% mutate(record=paste0(dataset,"_t_",as.character(time)))%>%
  dcast(subject+age_group ~record, value.var="spline")

#set up the data for modeling
export_spline_final<-export_spline_wide_data %>% dplyr::select(c(names(merged)))

#export the interpolated data for modeling
write.csv(export_spline_final,"Data/final/train_models_spline.csv",row.names=F)
