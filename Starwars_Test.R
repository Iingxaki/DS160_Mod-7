# install package
install.packages("tidyverse")
library(tidyverse)
library(readr)

#load the dataset
data = read.csv("characters.csv", show_col_type=FALSE)

#Explore the data
view(data)
head(data) #top 6 rows
tail(data) #last 6 rows
length(data) #of columns
names(data) #column names
str(data) #checking the data types
summary(data)

#unique eye color
unique(data$eye_color)
unique(data$hair_color)
unique(data$species)

#pipe operator
#take the output of one function and pass it

#select variables
data %>%
  select('name','height','gender') %>%
  view()

#select first three column
data %>%
  select(1:3)
  
#display height and mass
data %>%
  select(2:3)

#ends with "color
data %>%
  select(ends_with("color")) %>%
  view()

#contains "hair"
data %>%
  select(contains("hair")) %>%
  view()
  
#rename
data %>%
  rename(characters=name) %>%
  view()

#filter rows
data %>%
  select(height,mass) %>%
  filter(height>150 & mass<50)

#filter out the ows where mass is <60 and gender is male 
data %>%
  select(mass, gender) %>%
  filter(mass<60 & gender =="male")

#update
data %>%
  select(gender)%>%
  mutate(gender=recode(gender,"male"="M","female"="F"))
  
#missing data imputation
paste("Number of missing data",sum(is.na(data)))

#column specific missing data
colSums(is.na(data))

#drop/remove rows / entries having missing data
no_missing_data=drop_na(data)
paste("number of missing values", sum(is.na(no_missing_data)))
paste("number of rows",nrow(no_missing_data))
paste("Original data",nrow(data))

#impute height with mean
#na.rm means do not use the missing values
height_mean=mean(data$height, na.rm=TRUE)
paste(height_mean)

data$height=ifelse(is.na(data$height),height_mean,data$height)

paste("Number of missing values in height column", sum(is.na(data$height)))

#impute gender using unknown
data$gender=ifelse(is.na(data$gender),"unknown",data$gender)
paste("Number of missing value in gender",sum(is.na(data$gender)))
colSums(is.na(data))

#impute hair_color, skin_color, eye_color, species, homeworld, birthyear with unknown