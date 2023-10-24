# install package
install.packages("tidyverse")
library(tidyverse)

#load the dataset
data = read.csv("characters.csv")

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

#impute hair_color, skin_color, eye_color, species, homeworld, birth_year with unknown
data$hair_color=ifelse(is.na(data$hair_color),"unknown",data$hair_color)
data$skin_color=ifelse(is.na(data$skin_color),"unknown",data$skin_color)
data$eye_color=ifelse(is.na(data$eye_color),"unknown",data$eye_color)
data$species=ifelse(is.na(data$species),"unknown",data$species)
data$homeworld=ifelse(is.na(data$homeworld),"unknown",data$homeworld)
data$birth_year=ifelse(is.na(data$birth_year),"unknown",data$birth_year)

mass_med=median(data$mass, na.rm=TRUE)
paste(mass_med)
data$mass=ifelse(is.na(data$mass),mass_med,data$mass)
paste("Number of missing values in mass column", sum(is.na(data$mass)))


#Mutate (add a column/ variable to the dataset)
data_new=data%>%
  mutate(height_ft=height/30.48) %>% #height cm converted
  select(height,height_ft, mass, species, gender) %>%
  mutate(tallness= ifelse(height_ft<4,'short', 'tall'))
head(data_new)  
  
#Describe the data_new
#height_ft
paste("min height in ft", min(data_new$height_ft))
paste("max height in ft", max(data_new$height_ft))

#mass
paste("Range",range(data_new$mass))
paste('IQR',IQR(data_new$mass))

#centrality
#mean and median for height_ft
paste("mean",mean(data_new$height_ft))
paste("median",median(data_new$height_ft))
#variance and SD
paste("variance",var(data_new$height_ft))
paste("SD",sd(data_new$height_ft))

summary(data_new)
summary(data_new$height_ft)

#install.packages("psych")
library(psych)
describe(data_new)

summary_datanew=describe(data_new)
view(summary_datanew)
write.csv(summary_datanew,"summary_datanew.csv")

#calculating frequency of categorical data

table(data_new$species)
table(data_new$gender)
table(data_new$tallness)
table(data$eye_color)

#visualization using ggplot

#Barplot
#aes=aesthetics
#geom=geometric objects
#ggplot to create graphics
ggplot(data=data_new,aes(x=height_ft))+geom_bar()

# barplot of mass
ggplot(data=data_new,aes(x=mass))+geom_bar()

#histogram of height
ggplot(data=data_new,aes(x=height))+geom_histogram()

#boxplot for height
ggplot(data=data_new,aes(x=height))+geom_boxplot(fill='black')

#save the result
png('boxplot_height.png')

ggplot(data=data_new,aes(x=height))+geom_boxplot(fill='black')+labs(title='Boxplot of height',x='Height of characters')
dev.off() #shutdown the device

#Density plot of height
ggplot(data=data_new,aes(x=height,
                         color=gender))+geom_density(alpa=.3)

#plot only male and female using density plot
data_new %>%
  filter(gender %in% c('male','female')) %>%
  ggplot(aes(x=height,color=gender, fill=gender))+geom_density(alpha=.4)

#scatter plot height vs mass
ggplot(data=data_new,aes(x=height,y=mass,color=gender))+geom_point()
