library(tidyverse)
library(reshape2)
library(caTools)
library(zoo)
library(splines)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in training datasets
brach_data<-read.csv("data/raw/brachP_train_data.csv")
aorta_data<-read.csv("data/raw/aortaP_train_data.csv")



###first, let's export a dataset that still has NAs just in case people want it that way###
#merged<-merge(brach_data,aorta_data, by=c('X','target')) %>% arrange(X) %>% rename(age_group="target",subject="X")

#export the subject labels for people to use just in case
#sample <-sample.split(merged$age_group, SplitRatio = 0.8)
#train  <- subset(merged, sample == TRUE)
#test   <- subset(merged, sample == FALSE)
#df_partition_train<-data.frame(train$subject) %>% mutate(category='train') %>% rename(subject="train.subject")
#df_partition_test<-data.frame(test$subject) %>% mutate(category='test')%>% rename(subject="test.subject")
#df_partition<-rbind(df_partition_train,df_partition_test)
#write.csv(df_partition,"Data/partition_labels.csv",row.names=F)

#export pre-split data with NA values
#partitions<-read.csv('Data/partition_labels.csv')
#test<-merge(merged,partitions) %>% filter(category=='test') %>% select(-c("category"))
#train<-merge(merged,partitions) %>% filter(category=='train') %>% select(-c("category"))

#export the un-interpolated data for modeling
#write.csv(test,"Data/test_models_with_na.csv",row.names=F)
#write.csv(train,"Data/train_models_with_na.csv",row.names=F)


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

#linear interpolation of missing values for non-end NAs
interior_interp<-master_data %>% group_by(subject,dataset) %>% mutate(interior_interp = na.approx(value, na.rm=FALSE)) %>%ungroup()

#now predict missing end values using lm
#n_obs is the number of end values used to form the linear models
# n_obs<-40
# first_obs<-interior_interp %>% filter(time<n_obs)
# last_obs<-interior_interp %>% filter(time>(max(time)-n_obs))
# 
# extended_first_obs<- first_obs %>% group_by(subject, dataset) %>% mutate(extrap=predict(lm(value[1:n_obs]~time),as.data.frame(time)))
# extended_last_obs<- last_obs %>% group_by(subject, dataset) %>% mutate(extrap=predict(lm(value[1:n_obs]~time),as.data.frame(time)))
# combined_interp_extrap<- rbind(extended_first_obs,extended_last_obs) %>% merge(interior_interp,all.y=T)
# 
# filled_data<-combined_interp_extrap %>% 
#   mutate(filled_value=case_when(
#     is.na(interior_interp) ~ extrap,
#     !is.na(interior_interp) ~ interior_interp))
# 
# #look at subjects 1 and 1495
# plotme<- filled_data %>% filter(subject==1,dataset=="brach") %>% gather("dataset","value",c(4,8)) %>% arrange(dataset) 
# ggplot(plotme, aes(x=time,y=value)) + geom_point(aes(colour=dataset))
# 
# ###now, let's export the filled data
# export_filled<-filled_data %>% select(-c("value","interior_interp","extrap"))
# 
# #make it wide so models like it
# export_filled_wide_data<-export_filled %>% mutate(record=paste0(dataset,"_t_",as.character(time)))%>%
#   dcast(subject+age_group ~record, value.var="filled_value")
# 
# #set up the data for modeling
# export_filled_final<-export_filled_wide_data %>% merge(partitions) %>% select(c(names(test),'category'))
# train  <- export_filled_final%>% filter(category=='train') %>% select(-c("category"))
# test   <- export_filled_final%>% filter(category=='test') %>% select(-c("category"))

#export the interpolated data for modeling
#write.csv(test,"Data/test_models_linear_interp.csv",row.names=F)
#write.csv(train,"Data/train_models_linear_interp.csv",row.names=F)



###SPLINES to replace data###
spline_data<-master_data %>% group_by(subject, dataset) %>% mutate(spline=predict(lm(value~bs(time,knots=c(100,150,200))),as.data.frame(time)))

plotme<-spline_data %>% filter(subject>100, 
                               subject<110,
                               dataset=='aorta') 

ggplot(plotme) + geom_point(aes(colour=dataset, x=time, y=value)) + geom_point(aes(x=time,y=spline))+
  facet_wrap(~subject) + labs(y="Pressure",x="Time",title="splines with knots @ 100, 150, 200")
#ggsave("Output/splines_brach.png",width=12,height=9)

###now, let's export the spline data
export_spline<-spline_data %>% select(-c("value"))

#make it wide so models like it
export_spline_wide_data<-export_spline %>% mutate(record=paste0(dataset,"_t_",as.character(time)))%>%
  dcast(subject+age_group ~record, value.var="spline")

#set up the data for modeling
export_spline_final<-export_spline_wide_data %>% merge(partitions) %>% select(c(names(test),'category'))
train  <- export_spline_final%>% filter(category=='train') %>% select(-c("category"))
test   <- export_spline_final%>% filter(category=='test') %>% select(-c("category"))

#export the interpolated data for modeling
#write.csv(test,"Data/test_models_spline.csv",row.names=F)
#write.csv(train,"Data/train_models_spline.csv",row.names=F)
#write.csv(spline_data,"Data/long_format_spline.csv",row.names=F)
