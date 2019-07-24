rm(list=ls())

setwd('C:/UsersHp/Desktop/Edwisor/Bike rental project')


#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

days= read.csv('day.csv', header = TRUE)

head(days)

summary(days)

#renaming the columns

names(days)= c('instant', 'date', 'season', 'year', 'month', 'holiday', 'weekday', 'workingday', 
               'weather', 'temp', 'atemp', 'humidity', 'windspeed', 'casual', 'registered', 'count')

str(days)

#Exploratory data analysis

days$date = as.Date(days$date)
days$season= as.factor(days$season)
days$year = as.factor(days$year)
days$month= as.factor(days$month)
days$holiday= as.factor(days$holiday)
days$weekday= as.factor(days$weekday)
days$workingday= as.factor(days$workingday)
days$weather= as.factor(days$weather)


library(ggplot2)
library(dplyr)
library(relaimpo)
library(RColorBrewer)

#bifurcation of bike hired during 2011 and 2012

ggplot(days, aes(year, count)) +
  geom_boxplot(fill= 'grey') +
  theme_classic()+
  labs(title = 'Rental of bikes per year')+
  scale_x_discrete(labels= c('2011', '2012'))

#bifurcation based on seasons

col= brewer.pal(4, 'Set3')
ggplot(days, aes(season, count)) +
  geom_boxplot(fill= col)+
  theme_classic()+
  labs(title = 'bikes rented per season')+
  scale_x_discrete(labels= c('Spring', 'Summer', 'Fall', 'Winter'))

#bifurcation based on months

col= brewer.pal(12, 'Set3')
ggplot(days, aes(month, count))+
  geom_boxplot(fill= col)+
  theme_classic()+
  labs(title= 'Bikes rented per month')+
  scale_x_discrete(labels= c('Jan', 'Feb', 'March', 'April', 'May', 'June', 'July',
                             'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))

#holiday bifurcation

ggplot(days, aes(holiday, count))+
  geom_boxplot(fill= 'grey')+
  theme_classic()+
  labs(title = 'bikes rented on holiday')+
  scale_x_discrete(labels= c('no', 'yes'))


#bifurcation of weekday

col= brewer.pal(7,'Set3')
ggplot(days, aes(weekday, count))+
  geom_boxplot(fill= col)+
  labs(title = 'Bike rented as per weekday')+
  theme_classic()+
  scale_x_discrete(labels= c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun'))

#bifurcation on workingday

ggplot(days, aes(workingday, count))+
  geom_boxplot(fill= 'red')+
  theme_classic()+
  labs(title= 'Bike rented as working and non-working days')+
  scale_x_discrete(labels= c('no', 'yes'))

#bifurcation on weather

col= brewer.pal(3, 'Set3')
ggplot(days, aes(weather, count))+
  geom_boxplot(fill= col)+
  theme_classic()+
  labs(title= 'Bike rental count during various weahters')

#Missing value analysis

missing_val= data.frame(apply(days, 2, function(x){sum(is.na(x))}))

#write the output

write.csv(missing_val, 'Missing_valuesR.csv', row.names = F)

#Outlier Analysis


cnames= c('temp', 'atemp', 'humidity', 'windspeed', 'count')

#creating a variable to store row id's to be removed

outliers= c()

#loop through columns

for(i in cnames){
  
  #getting min/max values
  
  Tmax= quantile(days[,i], 0.75, na.rm= TRUE) + (IQR(days[,i], na.rm= TRUE)*1.5)
  Tmin= quantile(days[,i], 0.25, na.rm= TRUE) - (IQR(days[,i], na.rm= TRUE)*1.5)
  
  #getting id's using which
  
  idx= which(days[,i]< Tmin | days[,i]> Tmax)
  
  #shows number of outliers in each columns
  
  print(paste(i, length(idx), sep= ''))
  
  #append the list
  
  outliers= c(outliers, idx)
  
}
  
days= days[-outliers,]

#Feature selection

#correlation plot

corrgram(days[,10:16], order= F, upper.panel = panel.pie, text.panel= panel.txt, 
         main='Correlation plot')

## Chi-squared Test of Independence
factor_index = sapply(days,is.factor)
factor_data = days[,factor_index]

for (i in 1:7) {
  
  print(chisq.test(table(days$count, factor_data[,i])))
  
}

#dimension reduction

days = subset(days, select= -c(atemp))

#variance inflation factor 

library(usdm)

vifcor(days[,11:14], th= 0.9)

rmExcept('days')

#Model development

cat_variables= c('season', 'year', 'holiday', 'workingday', 'weather')
 
#num_variales= c('temp', 'humiditity', 'windspeed', 'month', 'weekday')
 
days_model= data.frame(days$count)
names(days_model)[1]= 'count'
 
days_cat= days[,cat_variables]

install.packages("fastDummies")
library(fastDummies)

days_cat= fastDummies::dummy_cols(days_cat)

days_cat= subset(days_cat, select= -c(season, year, holiday, workingday, weather))

model_data= cbind(days_cat, days_model)

model_data= cbind(model_data, days[,10:12])

install.packages('magrittr')
install.packages('dplyr')
library(magrittr)
library(dplyr)


model_data= model_data%>%select(count, everything())


#data partitioning 

library(DataCombine)
rmExcept('model_data')

train_index= sample(1:nrow(model_data), 0.7 * nrow(model_data))

train= model_data[train_index,]
test= model_data[-train_index,]

#Decision tree

library(rpart)
library(MASS)

tree= rpart(count~., data= train, method= 'anova')

#predict

predict_tree= predict(tree, test[,-1])

#MAPE

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}


MAPE(test[,1], predict_tree)

#error= 21.90
#accuracy= 78.1

#Random Forest

library(randomForest)

rf_tree= randomForest(count ~., model_data, importance= TRUE, ntree= 300)

rf_predict= predict(rf_tree, test[,-1])

MAPE(test[,1], rf_predict)

#With 200 trees
#error= 8.61 & accuracy= 91.4

#taking ntree as 300
#error= 8.66 & accuracy= 91.34

#Linear Regression

library(usdm)

lin_reg= lm(count~., data= model_data)

lin_predict= predict(lin_reg, test[,-1])

MAPE(test[,1], lin_predict)

#error= 16.34
#accuracy= 83.66

#Ridge, Lasso and Elastic net regressions

install.packages('glmnet')
library(glmnet)

#split into test and train for development

train_1= model_data%>%sample_frac(0.7)

test_1= model_data%>%setdiff(train_1)

x_train = model.matrix(count~., train_1)[,-1]
x_test = model.matrix(count~., test_1)[,-1]

library(Select)
y_train = train_1 %>% dplyr::select(count)%>% unlist() %>% as.numeric()
y_test = test_1%>%dplyr::select(count)%>%unlist()%>%as.numeric()


#Ridge regression

lambda = 10^seq(10, -2, length= 100)

ridge_reg= glmnet(x_train, y_train, alpha = 0, lambda = lambda)
summary(ridge_reg)

#finding best lambda via cross validation

ridge_reg1= cv.glmnet(x_train, y_train, alpha= 0)

bestlam= ridge_reg1$lambda.min

ridge_pred= predict(ridge_reg, s= bestlam, newx= x_test)

MAPE(y_test, ridge_pred)

#Lasso regression

lasso_reg= glmnet(x_train, y_train, alpha= 1, lambda= lambda)

lasso_pred= predict(lasso_reg, s= bestlam, newx= x_test)

MAPE(y_test, lasso_pred)

