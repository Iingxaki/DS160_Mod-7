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

#split the data
library(caTools)
set.seed(40)
split=sample.split(dataset$Value,SplitRatio=2/3)
training_set=subset(ds,split==TRUE)
test_set=subset(ds,split==FALSE)

#training the model
regressor_MLR=lm(formula= Value~Rooms+Distance, training_set)
summary(regressor_MLR)
y_pred=predict=predict(regressor_SLR, newdata = test_set)
result=data.frame(test_set$Value, y_pred)
head(result)

#accuracy check by MAE, MSE, RMSE
library(Metrics)
mae(test_set$Value,y_pred)
mse(test_set$Value,y_pred)
rmse(test_set$Value,y_pred)

#single prediction
new=data.frame(Rooms=10,Distance=12)

#value of the house
predict(regressor_SLR, newdata=new)
