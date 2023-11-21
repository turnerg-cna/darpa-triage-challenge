library(tidyverse)
library(ggplot2)

#set the working directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge")

#read in training datasets
brach_data<-read.csv("data/raw/brachP_train_data.csv")
aorta_data<-read.csv("data/raw/aortaP_train_data.csv")

#There are a significant number of NA values... 
sum(is.na(brach_data))/(ncol(brach_data)*nrow(brach_data)) #~12% missing from brach
sum(is.na(aorta_data))/(ncol(aorta_data)*nrow(aorta_data)) #~12% missing from aorta

#A pretty even distribution of target variable; that's convenient
table(aorta_data$target)

#Let's reshape and rename the data to make it easier to work with
reshaped_brach_data<-brach_data %>% gather("time","value",2:337) %>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$"))) %>% 
  rename(subject=X, age_group=target) %>% mutate(dataset="brachial")

reshaped_aorta_data<-aorta_data %>% gather("time","value",2:337) %>% 
  mutate(time=as.numeric(str_extract(time,"[^_]+$"))) %>% 
  rename(subject=X, age_group=target) %>% mutate(dataset="aortic")

#put the data together for now
master_data<-rbind(reshaped_aorta_data,reshaped_brach_data)

#let's look at the first subject's data
first_subject_data<-master_data %>% filter(subject==0)
ggplot(first_subject_data, aes(x=time,y=value)) + geom_point(aes(colour=dataset))
#I'm a little surprised the data seems to jump around so much... but that's okay I guess

#Now let's see if there are any obvious differences between the average age group data
mean_by_age<-master_data %>% group_by(age_group,time,dataset) %>% summarize(value=mean(na.omit(value)))
ggplot(mean_by_age, aes(x=time,y=value)) + geom_point(aes(colour=dataset)) + 
  facet_wrap(~age_group) + labs(y="Pressure",x="Time",title="Mean pressure by age group")
#ggsave("test.png",width=8,height=6)



#There are a few things I see right away...
# 1. peak pressure seems to increase with age in both datasets, but especially aortic
# 2. there's a pronounced "bounce" in brachial pressure which diminishes with age
# 3. there seems to be a larger aortic-brachial pressure diference in younger age groups as well



#Let's look first at peak pressure
max_by_subject<-master_data %>% group_by(subject,dataset,age_group) %>% 
  summarize(value=max(na.omit(value))) %>% mutate(age_group=as.character(age_group))
ggplot(max_by_subject, aes(x=age_group,y=value,fill=dataset)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="Max pressure by age group")
#ggsave("max pressure by age group.png",width=8,height=6)



#what about the diference between these two maxes?
max_aortic<-max_by_subject %>% filter(dataset=="aortic")
max_brachial<-max_by_subject %>% filter(dataset=="brachial")
difference_in_max_by_subject<-merge(max_aortic, max_brachial, by=c("subject","age_group")) %>% 
  mutate(age_group=as.character(age_group),
         difference_in_max=value.y-value.x)
ggplot(difference_in_max_by_subject, aes(x=age_group,y=difference_in_max)) + geom_boxplot() + 
  labs(y="Pressure",x="Age Group",title="Max brachial - max aortic by age group")
#ggsave("max brachial-max aortic by age group.png",width=8,height=6)



#what if we scatter these two variables?
ggplot(difference_in_max_by_subject, aes(x=difference_in_max,y=value.x)) + 
  geom_point(aes(colour=age_group))+labs(x="Max brachial-max aortic",y="Max aortic",title="multiple variable clusters")
#ggsave("max brachial-max aortic vs. max aortic.png",width=8,height=6)





###outliers?###
spline_data<-read.csv("data/long_format_spline.csv")

plotme<-spline_data %>% filter(dataset=='brach') 

plot<-ggplot(plotme) + geom_point(aes(x=time,y=spline),alpha=.01,size=.1)+
  facet_wrap(~age_group) + labs(y="Pressure",x="Time",title="variability by age group (Brach)")

#ggsave("output/brach spline variation.png",width=8,height=6,plot=plot)

plotme<-spline_data %>% filter(dataset=='aorta') 

plot<-ggplot(plotme) + geom_point(aes(x=time,y=spline),alpha=.01,size=.1)+
  facet_wrap(~age_group) + labs(y="Pressure",x="Time",title="variability by age group (aorta)")

#ggsave("output/aorta spline variation.png",width=8,height=6,plot=plot)
