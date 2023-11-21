library(tidyverse)
library(ggplot2)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in training datasets
train<-read.csv("Data/train_models_spline.csv")
test<-read.csv("Data/test_models_spline.csv")

combined<-rbind(train,test)

#Let's reshape and rename the data to make it easier to work with
master_data<-combined %>% mutate(subject=seq(1:n())) %>%
  gather("variable","value",2:673) %>% 
  mutate(time=as.numeric(str_extract(variable,"[^_]+$")),
         dataset=str_extract(variable,"^[a-z]*"),
         dataset=if_else(dataset=="brach","brachial","aortic")) %>% 
  select(-c("variable"))

#Now let's see if there are any obvious differences between the average age group data
mean_by_age<-master_data %>% group_by(age_group,time,dataset) %>% summarize(value=mean(na.omit(value)))
ggplot(mean_by_age, aes(x=time,y=value)) + geom_point(aes(colour=dataset)) + 
  facet_wrap(~age_group) + labs(y="Pressure",x="Time",title="Mean pressure by age group")

max_by_subject<-master_data %>% group_by(subject,dataset,age_group) %>% 
  summarize(value=max(na.omit(value))) %>% mutate(age_group=as.character(age_group))
ggplot(max_by_subject, aes(x=age_group,y=value,fill=dataset)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="Max pressure by age group")

#what about the diference between these two maxes?
max_aortic<-max_by_subject %>% filter(dataset=="aortic")
max_brachial<-max_by_subject %>% filter(dataset=="brachial")
difference_in_max_by_subject<-merge(max_aortic, max_brachial, by=c("subject","age_group")) %>% 
  mutate(age_group=as.character(age_group),
         difference_in_max=value.y-value.x)
ggplot(difference_in_max_by_subject, aes(x=age_group,y=difference_in_max)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="Max brachial - max aortic by age group")

#how do we turn that dip around 150 into a feature?
key_times<-master_data %>% filter(time==150 | time==200,
                                  dataset=='brachial') %>% dcast(subject+age_group+dataset ~time, value.var="value") %>% 
  mutate(delta=`150`-`200`,
         age_group=as.character(age_group))
ggplot(key_times, aes(x=age_group,y=delta)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="brachial at 150-200")

#when does aortic peak?
key_times<-master_data %>% filter(dataset=='aortic') %>% arrange(subject, desc(value))%>%distinct(subject,.keep_all=T)%>%
  mutate(age_group=as.character(age_group))
ggplot(key_times, aes(x=age_group,y=time)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="time of peak aortic")
