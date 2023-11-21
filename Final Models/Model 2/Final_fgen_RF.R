---
title: "DARPA Challenge"
author: "Marwan Lloyd"
date: "2023-11-06"
output: html_document
---

```{R libraries}
library(TSstudio)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(party)
library(randomForest)
library(caret)
library(jsonlite)
```

```{r Directory}
#Change to proper directory
setwd("P:/Data Science Team/Projects/DARPA Triage Challenge/Data")
```



```{r read in datasets}
test_models_with_na <- read.csv("test_models_with_na.csv")
test_models_linear_interp <- read.csv("test_models_linear_interp.csv")
train_models_with_na <- read.csv("train_models_with_na.csv")
train_models_linear_interp <- read.csv("train_models_linear_interp.csv")
train_models_spline <- read.csv("train_models_spline.csv")
test_models_spline <- read.csv("test_models_spline.csv")
final_test_models_spline <- read.csv("final/test_models_spline.csv")
final_train_models_spline <- read.csv("final/train_models_spline.csv")

#Raw data
raw_aorta_test <- read.csv("raw/aortaP_test_data.csv")
raw_aorta_train <- read.csv("raw/aortaP_train_data.csv")
raw_brach_test <- read.csv("raw/brachP_test_data.csv")
raw_brach_train  <- read.csv("raw/brachP_train_data.csv")

```

```{r preliminary EDA}
#if I want to plot some to take a look
#Plot just only one
#NOTE: 2:673 so it doesn't include the age group variable 
single_plot <- ts_plot(ts(t(train_models_with_na[1,3:673])))
single_plot_brach <-ts_plot(ts(t(train_models_with_na[1,3:338]))) 
single_plot_aorta <- ts_plot(ts(t(train_models_with_na[1,339:674]))) 

#Plot first 10 
#NOTE: 2:673 so it doesn't include the age group variable 
group_plot <- ts_plot(ts(t(train_models_with_na[1:10,3:673])))
group_plot_brach <-ts_plot(ts(t(train_models_with_na[1:10, 3:336]))) 
group_plot_aorta <- ts_plot(ts(t(train_models_with_na[1:10 ,337:673]))) 
```


```{r Glenn EDA & expanded}
#read in training datasets
brach_data<-read.csv("raw/brachP_train_data.csv")
aorta_data<-read.csv("raw/aortaP_train_data.csv")

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

```

```{r Glenn EDA 2}
#Now let's see if there are any obvious differences between the average age group data
mean_by_age<-master_data %>% group_by(age_group,time,dataset) %>% summarize(value=mean(na.omit(value)))
ggplot(mean_by_age, aes(x=time,y=value)) + geom_point(aes(colour=dataset)) + 
  facet_wrap(~age_group) + labs(y="Pressure",x="Time",title="Mean pressure by age group")
#ggsave("test.png",width=8,height=6)

```

```{r Turning data into ratios}
#A_B_ratio <- train_models_linear_interp[,339:674] / train_models_linear_interp[,3:338]
#A_B_ratio$subject <- train_models_linear_interp$subject
#A_B_ratio$age_group <- train_models_linear_interp$age_group


#Spline version instead of lin interp version
#NOTE: as.factor() is used to turn the age group into a factor (since that's the outcome)
A_B_ratio <- train_models_spline[,339:674] / train_models_spline[,3:338]
A_B_ratio$age_group <- as.factor(train_models_spline$age_group)
A_B_product <- train_models_spline[,339:674] * train_models_spline[,3:338]
A_B_product$age_group <- as.factor(train_models_spline$age_group)

test_A_B_ratio <- test_models_spline[,339:674] / test_models_spline[,3:338]
test_A_B_ratio$age_group <- as.factor(test_models_spline$age_group)
test_A_B_product <- test_models_spline[,339:674] * test_models_spline[,3:338]
test_A_B_product$age_group <- as.factor(test_models_spline$age_group)
```

```{r BASIC linear regression model using ratios}
round(predict( lm(age_group ~ . , data = A_B_ratio) , newdata = A_B_ratio[-337]), 0) 
table(round(predict( glm(age_group ~ . , data = A_B_ratio) , newdata = A_B_ratio[-337]), 0) == A_B_ratio$age_group)
```

```{r Checking a model with ratio extremes}
A_B_ratio_ends <- c()
A_B_ratio_ends$max_ratio <- apply(A_B_ratio[-337], 1, max, na.rm=TRUE)
A_B_ratio_ends$min_ratio <- apply(A_B_ratio[-337], 1, min, na.rm=TRUE)
A_B_ratio_ends$age_group <- as.factor(train_models_spline$age_group)
A_B_ratio_ends <- data.frame(A_B_ratio_ends)

test_A_B_ratio_ends <- c()
test_A_B_ratio_ends$max_ratio <- apply(test_A_B_ratio[-337], 1, max, na.rm=TRUE)
test_A_B_ratio_ends$min_ratio <- apply(test_A_B_ratio[-337], 1, min, na.rm=TRUE)
test_A_B_ratio_ends$age_group <- as.factor(test_models_spline$age_group)
test_A_B_ratio_ends <- data.frame(test_A_B_ratio_ends)
```

```{r random forest try}

rf_model_ratio <- randomForest(age_group ~., data = A_B_ratio, do.trace = T)

```

```{r random forest for ratio ends}

rf_ends_model_ratio <- randomForest(age_group ~., data = A_B_ratio_ends)

```

```{r}
colnames(A_B_ratio) <- paste("Ratio", colnames(A_B_ratio), sep = "_")
colnames(test_A_B_ratio) <- paste("Ratio", colnames(test_A_B_ratio), sep = "_")
colnames(A_B_product) <- paste("Product", colnames(A_B_product), sep = "_")
colnames(test_A_B_product) <- paste("Product", colnames(test_A_B_product), sep = "_")
#raw_data_and_ratios <- cbind(train_models_spline, A_B_ratio[-337], A_B_ratio_ends[-3])
raw_data_and_ratios <- cbind(train_models_spline, A_B_product[-337])
raw_data_and_ratios$age_group <- as.factor(raw_data_and_ratios$age_group)
test_raw_data_and_ratios <- cbind(test_models_spline, test_A_B_ratio[-337], test_A_B_ratio_ends[-3])
test_raw_data_and_ratios$age_group <- as.factor(test_raw_data_and_ratios$age_group)
```

```{r big random forest model}
big_rf_ratio_model <- randomForest(age_group ~., data = raw_data_and_ratios[-1], do.trace = T)
```

```{r additional feature generation beyond ratios}
#Get maximum pressure 
raw_data_with_features <- train_models_spline
raw_data_with_features$age_group <- as.factor(raw_data_with_features$age_group)
raw_data_with_features$max_pressure <-  apply(train_models_spline[2:674], 1, max, na.rm=TRUE)
raw_data_with_features$max_brach <-  apply(train_models_spline[2:338], 1, max, na.rm=TRUE)
raw_data_with_features$max_aortic <-  apply(train_models_spline[339:674], 1, max, na.rm=TRUE)
raw_data_with_features$brach_dip_period_min <- apply(train_models_spline[128:178], 1, min, na.rm=TRUE)

#raw_data_with_features$min_pressure <-  apply(train_models_spline[3:674], 1, min, na.rm=TRUE)

test_raw_data_with_features <- test_models_spline
test_raw_data_with_features$age_group <- as.factor(test_raw_data_with_features$age_group)
test_raw_data_with_features$max_pressure <-  apply(test_models_spline[2:674], 1, max, na.rm=TRUE)
test_raw_data_with_features$max_brach <-  apply(test_models_spline[2:338], 1, max, na.rm=TRUE)
test_raw_data_with_features$max_aortic <-  apply(test_models_spline[339:674], 1, max, na.rm=TRUE)
test_raw_data_with_features$brach_dip_period_min <- apply(test_models_spline[128:178], 1, min, na.rm=TRUE)
#raw_data_with_features$min_pressure <-  apply(train_models_spline[3:674], 1, min, na.rm=TRUE)


```

```{r}

# Tune the number of variables tried at each split (mtry)
#tuned_mtry <- tuneRF(raw_data_with_features[c(-1,-2)], raw_data_with_features$age_group, ntreeTry = 500, stepFactor = 1.5)
#get smallest OOB error 
#tuned_mtry <- tuned_mtry[order(data.frame(tuned_mtry)$OOBError)]

big_features_model <- randomForest(age_group ~., data = raw_data_with_features[-1], do.trace = T,  importance=TRUE)
```

```{r get most important variables of the model above}
#apply(varImp(big_features_model),1,mean, na.rm = TRUE)[order(apply(varImp(big_features_model),1,mean, na.rm = TRUE), decreasing = TRUE)]
# Compute mean variable importance for each feature
mean_var_imp <- apply(varImp(big_features_model), 1, mean, na.rm = TRUE)

# Identify features where mean variable importance is greater than 3
selected_features <- names(mean_var_imp[mean_var_imp > 3])

# Filter rows in raw_data_with_features based on selected features
filtered_data <- raw_data_with_features[, selected_features]
filtered_data$age_group <- raw_data_with_features$age_group
```

```{r model using only the filtered columns}

# Tune the number of variables tried at each split (mtry)
#filtered_tuned_mtry <- tuneRF(filtered_data[-244], filtered_data$age_group, ntreeTry = 500, stepFactor = 1.5)
#get smallest OOB error 
#filtered_tuned_mtry <- filtered_tuned_mtry[order(data.frame(filtered_tuned_mtry)$OOBError)]


filtered_model <- randomForest(age_group ~., data = filtered_data, do.trace = T)
```

```{r}
final_raw_data_with_features <- final_train_models_spline
final_raw_data_with_features$age_group <- as.factor(final_raw_data_with_features$age_group)
final_raw_data_with_features$max_pressure <-  apply(final_train_models_spline[2:674], 1, max, na.rm=TRUE)
final_raw_data_with_features$max_brach <-  apply(final_train_models_spline[2:338], 1, max, na.rm=TRUE)
final_raw_data_with_features$max_aortic <-  apply(final_train_models_spline[339:674], 1, max, na.rm=TRUE)
final_raw_data_with_features$brach_dip_period_min <- apply(final_train_models_spline[128:178], 1, min, na.rm=TRUE)

#raw_data_with_features$min_pressure <-  apply(train_models_spline[3:674], 1, min, na.rm=TRUE)

final_test_raw_data_with_features <- final_test_models_spline
final_test_raw_data_with_features$max_pressure <-  apply(final_test_models_spline[2:673], 1, max, na.rm=TRUE)
final_test_raw_data_with_features$max_brach <-  apply(final_test_models_spline[2:337], 1, max, na.rm=TRUE)
final_test_raw_data_with_features$max_aortic <-  apply(final_test_models_spline[338:673], 1, max, na.rm=TRUE)
final_test_raw_data_with_features$brach_dip_period_min <- apply(final_test_models_spline[128:178], 1, min, na.rm=TRUE)
#raw_data_with_features$min_pressure <-  apply(train_models_spline[3:674], 1, min, na.rm=TRUE)


```

```{r}
final_big_features_model <- randomForest(age_group ~., data = final_raw_data_with_features[-1], do.trace = T,  importance=TRUE)
```

```{r get most important variables of the model above}
#apply(varImp(big_features_model),1,mean, na.rm = TRUE)[order(apply(varImp(big_features_model),1,mean, na.rm = TRUE), decreasing = TRUE)]
# Compute mean variable importance for each feature
final_mean_var_imp <- apply(varImp(final_big_features_model), 1, mean, na.rm = TRUE)

# Identify features where mean variable importance is greater than 3
final_selected_features <- names(final_mean_var_imp[final_mean_var_imp > 3])

# Filter rows in raw_data_with_features based on selected features
final_filtered_data <- final_raw_data_with_features[, selected_features]
final_filtered_data$age_group <- final_raw_data_with_features$age_group
```

```{r model using only the filtered columns}

# Tune the number of variables tried at each split (mtry)
#filtered_tuned_mtry <- tuneRF(filtered_data[-244], filtered_data$age_group, ntreeTry = 500, stepFactor = 1.5)
#get smallest OOB error 
#filtered_tuned_mtry <- filtered_tuned_mtry[order(data.frame(filtered_tuned_mtry)$OOBError)]


final_filtered_model <- randomForest(age_group ~., data = final_filtered_data, do.trace = T)
```

```{r output of using the filtered model}
output_df <- c()
output_df$subject <- final_test_raw_data_with_features$subject
output_df$predicted_age_group <- predict(final_filtered_model, newdata = final_test_raw_data_with_features) 
output_df <- as.data.frame(output_df)
```

```{r}
json_out<-toJSON(setNames(as.list(as.numeric(output_df$predicted_age_group)-1), output_df$subject), auto_unbox = T, pretty = T)
write(json_out,'112123_test_predictions_method_2.json')
```

```{r confusion matrix}
cm <- confusionMatrix(predict(filtered_model, newdata = test_raw_data_with_features), test_raw_data_with_features$age_group)
```



```{r}
big_whole_features_data <- cbind(raw_data_with_features, A_B_product[-337], A_B_ratio[-337])
test_big_whole_features_data <- cbind(test_raw_data_with_features, test_A_B_product[-337], test_A_B_ratio[-337])
```

```{r}
giant_model <- randomForest(age_group ~., data = big_whole_features_data[-1], do.trace = T,  importance=TRUE)
```

```{r}
giant_mean_var_imp <- apply(varImp(giant_model), 1, mean, na.rm = TRUE)

# Identify features where mean variable importance is greater than 2.8
giant_selected_features <- names(giant_mean_var_imp[giant_mean_var_imp > 3])

# Filter rows in big_whole_features_data based on selected features
giant_filtered_data <- big_whole_features_data[, giant_selected_features]
giant_filtered_data$age_group <- big_whole_features_data$age_group
```

```{r model using only the filtered columns}


giant_filtered_model <- randomForest(age_group ~., data = giant_filtered_data, do.trace = T)
```
