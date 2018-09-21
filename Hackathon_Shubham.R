#Reading the required libraries
library(MASS)
library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(ROCR)
library(dummies)
library(car)

# setting working directory
setwd("~/Downloads")
train_data<-read.csv("train_LZdllcl.csv",header=TRUE,stringsAsFactors=FALSE, na.strings = c("","NA","na","-"))

#Bird's eye view of data
View(train_data)
str(train_data)
sum(is.na(train_data))
summary(train_data)

#Missing value treatment
train_data$previous_year_rating[is.na(train_data$previous_year_rating)] <- 0

#Outlier Treatment

n<-boxplot(train_data$no_of_trainings)
quantile(train_data$no_of_trainings,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,1))
train_data[(which(train_data$no_of_trainings>4)),]$no_of_trainings <- 4

m<-boxplot(train_data$age)
quantile(train_data$age,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
train_data[(which(train_data$age>51)),]$age <- 51

o<-boxplot(train_data$previous_year_rating)
quantile(train_data$previous_year_rating,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

q<-boxplot(train_data$length_of_service)
quantile(train_data$length_of_service,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
train_data[(which(train_data$length_of_service > 15)),]$length_of_service <- 15

p<-boxplot(train_data$avg_training_score)
quantile(train_data$avg_training_score,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

# dummy variable creation
train_data$department <- as.factor(train_data$department)
train_data$education <- as.factor(train_data$education)
train_data$gender <- as.factor(train_data$gender)
train_data$recruitment_channel <- as.factor(train_data$recruitment_channel)
train_data$KPIs_met..80. <- as.factor(train_data$KPIs_met..80.)
train_data$awards_won. <- as.factor(train_data$awards_won.)
train_data$is_promoted <- as.factor(train_data$is_promoted)

train_data <- dummy.data.frame(train_data, names=c("department", "education", "gender","recruitment_channel","KPIs_met..80.","awards_won."), sep="_")
colnames(train_data)

#split for train data to regress the new train data, and perform in-sample testing in the new test data

set.seed(10)

split_indices <- sample.split(train_data, SplitRatio = 0.70)

train <- train_data[split_indices, ]

test <- train_data[!split_indices, ]

#Modelling
#generic step : understand basic regressors
logistic_1 <- glm(is_promoted ~ ., family = "binomial", data = train[,-1])
summary(logistic_1)
#identify and pick up the required predictor variables, by choosing the model with the lower AIC
logistic_2 <- stepAIC(logistic_1, direction = "both",k=5)
#Realise the model in terms of better p value variables and VIF value less than 2
logistic_3 <- glm(is_promoted ~ department_Analytics + department_Finance + department_HR + 
                    department_Legal + department_Operations + department_Procurement + 
                    `department_R&D` + `department_Sales & Marketing` + #region + 
                    `education_Master's & above` + no_of_trainings + age + previous_year_rating + 
                    length_of_service + KPIs_met..80._0 + awards_won._0 + avg_training_score, family = "binomial", data = train[, -1])
summary(logistic_3)
vif(logistic_3)
#Since the significance of all the variables is high, we can use all the identified predictor variables in the chosen model

#Check dimensions before executing the prediction
dim(train)
#Predict the response for the target variable
prob_q <- predict(logistic_3, type = 'response', newdata = test[,-c(1,30)])
pred_q <- ifelse(prob_q > 0.5,1,0)
cm <- table(test[,30],pred_q > 0.5)

#F1 Score Evaluation
precision2 = 14992/(14992 +374)
recall2 = 14992/(14992+69)
FScore_rev2 <- (2 * precision2 *recall2)/sum(precision2,recall2)

#F1 Score of the Model = 0.9854406

############################################################################################################################################
#Outside Sample Test Data

test_data<-read.csv("test_2umaH9m.csv",header=TRUE,na.strings = c("","NA","na","-"),stringsAsFactors=FALSE)

#Bird's eye view for Outer sample test data
View(test_data)
test_data$previous_year_rating[is.na(test_data$previous_year_rating)] <- 0

#Outlier treatment of Outer sample test data
t<-boxplot(test_data$no_of_trainings)
quantile(test_data$no_of_trainings,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,1))
test_data[(which(test_data$no_of_trainings>4)),]$no_of_trainings <- 4

k<-boxplot(test_data$age)
quantile(test_data$age,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
test_data[(which(test_data$age>51)),]$age <- 51

w<-boxplot(test_data$previous_year_rating)
quantile(test_data$previous_year_rating,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

o<-boxplot(test_data$length_of_service)
quantile(test_data$length_of_service,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
test_data[(which(test_data$length_of_service > 15)),]$length_of_service <- 15

i<-boxplot(test_data$avg_training_score)
quantile(test_data$avg_training_score,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

#dummy variables for Outer sample test data

test_data$department <- as.factor(test_data$department)
test_data$education <- as.factor(test_data$education)
test_data$gender <- as.factor(test_data$gender)
test_data$recruitment_channel <- as.factor(test_data$recruitment_channel)
test_data$KPIs_met..80. <- as.factor(test_data$KPIs_met..80.)
test_data$awards_won. <- as.factor(test_data$awards_won.)

str(test_data)

test_data <- dummy.data.frame(test_data, names=c("department", "education", "gender","recruitment_channel","KPIs_met..80.","awards_won."), sep="_")
colnames(test_data)

#Predictions on Outer sample test data
prob <- predict(logistic_3, type = 'response', newdata = test_data)
pred <- ifelse(prob > 0.5, 1, 0)
y_pred <- factor(pred, levels=c(0, 1))
y_act <- train_data$is_promoted
ispromoted <- y_act
test_data$is_promoted <- y_pred
#Checking the accuracy of prediction made on Outer sample test data
mean(y_pred == y_act)
#Accuracy on outer sample = 0.8930813
write.csv(test_data[c("employee_id","is_promoted")] ,"Output_WNS_Hackathon_Shubham.csv",row.names = FALSE)