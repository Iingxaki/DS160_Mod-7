#Multiple Linear Regression
library(tidyverse)

#load the dataset
dataset=read.csv("insurance.csv")

#basic EDA
glimpse(dataset)

#number of variables
length(dataset)
names(dataset)

summary(dataset[sapply(dataset,is.numeric)])

#categorical data
table(dataset$sex)
table(dataset$region)
table(dataset$smoker)

#missing values imputation
colSums(is.na(dataset))

ggplot(data=dataset,aes(x=bmi))+geom_density()
ggplot(data=dataset,aes(x=charges))+geom_density()

#uniform distribution use mean to impute
#skewed data use median/most frequent for imputation

bmi_median=median(dataset$bmi,na.rm=TRUE)
paste("median",bmi_median)
dataset$bmi=ifelse(is.na(dataset$bmi),
                   bmi_median,
                   dataset$bmi)
colSums(is.na(dataset))

#splitting the dataset into training and test set
install.packages('caTools')
library(caTools)
#reproduce same result
set.seed(123)
split=sample.split(dataset$charges,SplitRatio=.80)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#training/fitting MLR
regressor=lm(formula=charges~., data=training_set)
summary(regressor)

#MLR equation
#charges= -12678.97+266.38*age-257.44*sexmale+350*bmi+547*children+24755.21*smokeryes-357.5*regionnorthwest-901.64*regionsouteast-1067.01*regionsouthwest

#model performance on Test set
y_pred=predict(regressor, newdata=test_set)
data=data.frame(y_pred,test_set$charges)
head(data)

install.packages("Metrics")
library(Metrics)
#actual =test_set$charges
#predicted=y_pred
paste("MAE",mae(test_set$charges, y_pred))
paste("MSE",mse(test_set$charges, y_pred))
paste("RMSE",rmse(test_set$charges, y_pred))

#Validation dataset
new=read.csv('Validation.csv')
head(new)
new_x=new[c(1:6)]
data.frame(new$charges.actual.,predict(regressor,newdata=new_x))
