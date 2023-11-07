# Simple linear regression on Boston Housing
library(tidyverse)
ds =read.csv('Boston_House_Prices.csv')

#explore 
head(ds)

glimpse(ds)
names(ds)
summary(ds)

#Missing data?
colSums(is.na(ds))

#rooms
ggplot(data=ds,aes(Rooms))+geom_density()

#impute the missing value using mean
rooms_mean=mean(ds$Rooms,na.rm=TRUE)
ds$Rooms=ifelse(is.na(ds$Rooms),
                rooms_mean,
                ds$Rooms)

#distance
ggplot(data=ds,aes(Distance))+geom_density()

#impute the missing value using mean
Distance_median=median(ds$Distance,na.rm=TRUE)
ds$Distance=ifelse(is.na(ds$Distance),
                Distance_median,
                ds$Distance)
colSums(is.na(ds))

#Simple Linear REgression between rows and values
ds_new=ds[c(1,3)]

#splitting the data into training and test set

#library(caTools)

split=sample.split(ds_new$Value, SplitRatio=2/3)
#2/3 data for training and 1/3 for testing
training_set=subset(ds_new,split==TRUE)
test_set=subset(ds_new, split==FALSE)

#training the model using training set
regressor_SLR=lm(formula=Value ~Rooms, training_set)
summary(regressor_SLR)

#Accuracy on test set
y_pred=predict(regressor_SLR, newdata = test_set)
result=data.frame(test_set$Value, y_pred)
head(result)

#accuracy check by MAE, MSE, RMSE
library(Metrics)
mae(test_set$Value,y_pred)
mse(test_set$Value,y_pred)
rmse(test_set$Value,y_pred)

#single prediction
new=data.frame(Rooms=10)
#value of the house
predict(regressor_SLR, newdata=new)

#plot the test set results
ggplot()+geom_point(aes(x=test_set$Rooms,y=test_set$Value),color='darkgreen')+
  geom_line(aes(x=test_set$Rooms,y=y_pred),color='darkred')+
  xlab('Rooms')+ylab('Value')
