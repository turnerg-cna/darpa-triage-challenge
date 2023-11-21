library(tidyverse)
library(reshape2)
library(caTools)
library(zoo)
library(splines)
library(dplyr)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in training datasets
brach_data<-read.csv("data/raw/brachP_train_data.csv")
aorta_data<-read.csv("data/raw/aortaP_train_data.csv")

###merge the datasets together###
merged<-merge(brach_data,aorta_data, by=c('X','target')) %>% arrange(X) %>% rename(age_group="target",subject="X")

###create 5 folds to test models with###
#for some reason I could not find a package function which could actually do this... Several were close but most were not exclusive samples
set.seed(100)
fold1 <- sample.split(merged$age_group, SplitRatio = 0.8)
fold1_test_subjects <- merged %>% filter(fold1==FALSE) %>% dplyr::select(subject)
minus_fold1<-merged %>% filter(fold1==TRUE)

fold2<-sample.split(minus_fold1$age_group, SplitRatio = 0.75)
fold2_test_subjects <- minus_fold1 %>% filter(fold2==FALSE) %>% dplyr::select(subject)
minus_fold2<-minus_fold1 %>% filter(fold2==TRUE)

fold3<-sample.split(minus_fold2$age_group, SplitRatio = 0.66666666)
fold3_test_subjects <- minus_fold2 %>% filter(fold3==FALSE) %>% dplyr::select(subject)
minus_fold3<-minus_fold2 %>% filter(fold3==TRUE)

fold4<-sample.split(minus_fold3$age_group, SplitRatio = 0.5)
fold4_test_subjects <- minus_fold3 %>% filter(fold4==FALSE) %>% dplyr::select(subject)
fold5_test_subjects <- minus_fold3 %>% filter(fold4==TRUE) %>% dplyr::select(subject)


#check and see if the folding worked... looks good!
fold1_data<-merged %>% filter(subject %in% fold1_test_subjects$subject)
table(fold1_data$age_group)

fold2_data<-merged %>% filter(subject %in% fold2_test_subjects$subject)
table(fold2_data$age_group)

fold3_data<-merged %>% filter(subject %in% fold2_test_subjects$subject)
table(fold3_data$age_group)


###now reshape the data to make it easier to work with###
#Let's reshape and rename the data to make it easier to work with
reshaped_brach_data<-brach_data %>% gather("time","value",2:337) %>%
  rename(subject=X, age_group=target) %>% mutate(dataset="brach")%>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$")))

reshaped_aorta_data<-aorta_data %>% gather("time","value",2:337) %>% 
  rename(subject=X, age_group=target) %>% mutate(dataset="aorta")%>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$")))

#put the data together for now
master_data<-rbind(reshaped_aorta_data,reshaped_brach_data)


###Now fit splines to the data and predict a smooth curve for each subject/dataset
spline_data<-master_data %>% group_by(subject, dataset) %>% mutate(spline=predict(lm(value~bs(time,knots=c(100,150,200))),as.data.frame(time)))

plotme<-spline_data %>% filter(subject %in% c(1,5,89,345,1241,2009))

ggplot(plotme) + geom_point(aes(colour=dataset, x=time, y=value)) + geom_point(aes(x=time,y=spline))+
  facet_wrap(~subject+dataset) + labs(y="Pressure",x="Time",title="splines with knots @ 100, 150, 200")

###now, let's export the spline data
export_spline<-spline_data %>% dplyr::select(-c("value"))

#make it wide so models like it
export_spline_wide_data<-export_spline %>% mutate(record=paste0(dataset,"_t_",as.character(time)))%>%
  dcast(subject+age_group ~record, value.var="spline")

#set up the data for modeling
export_spline_final<-export_spline_wide_data %>% dplyr::select(c(names(merged)))

iter=0
for (fold in c(fold1_test_subjects,fold2_test_subjects,fold3_test_subjects,fold4_test_subjects,fold5_test_subjects)) {
  iter<-iter+1
  test<-export_spline_final %>% filter(subject %in% fold)
  train<-export_spline_final %>% filter(!subject %in% fold)
  write.csv(test,paste0("Data/five_fold_spline/test",iter,".csv"),row.names=F)
  write.csv(train,paste0("Data/five_fold_spline/train",iter,".csv"),row.names=F)
}



