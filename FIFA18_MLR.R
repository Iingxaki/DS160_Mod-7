library('tidyverse')
fifa=read.csv('complete.csv')
length(fifa)
names(fifa)

data= fifa %>%
  select('name','club','age','league','height_cm','weight_kg','eur_wage','overall',
         'potential','international_reputation','preferred_foot','skill_moves','weak_foot',
         'strength','stamina')
head(data)
unique(data$league)
unique(data$club)
summary(data[sapply(data, is.numeric)])

#missing data?
colSums(is.na(data))

#Q1. Identify clubs and players of "overall" skill is above 80
clubdata=data %>% 
  filter(overall>80) %>% 
  group_by(league,club) %>% 
# create new columns to make graphs of them
  summarise(count=n(),
            mean_overall=round(mean(overall)),
            mean_age=round(mean(age)),
            mean_potential =round(mean(potential)),
            total_wage=round(sum(eur_wage)))
  
head(clubdata)
unique(clubdata$club)
unique(clubdata$league)

#average overall skill score
clubdata %>% 
  filter(club %in% c('Manchester United','Liverpool','Real Madrid CF','Chelsea','Milan')) %>%
  ggplot(aes(x=club,y=mean_overall, color=club,fill=club))+ geom_col(position='dodge')+
  labs(y='overall skill')

#average overall age
clubdata %>% 
  filter(club %in% c('Manchester United','Liverpool','Real Madrid CF','Chelsea','Milan')) %>%
  ggplot(aes(x=club,y=mean_age, color=club,fill=club))+ geom_col(position='dodge')+
  labs(y='Average Age')

#average potential
clubdata %>% 
  filter(club %in% c('Manchester United','Liverpool','Real Madrid CF','Chelsea','Milan')) %>%
  ggplot(aes(x=club,y=mean_potential, color=club,fill=club))+ geom_col(position='dodge')+
  labs(y='Average Potential')

#average potential for different leagues
unique(clubdata$league)
clubdata %>% 
  filter(league %in% c('Italian Serie A','English Premier League','German Bundesliga',
         'French Ligue 1')) %>% 
  arrange(mean_potential) %>% 
  ggplot(aes(x=factor(club,levels =rev(club)),y=mean_potential,color=league,fill=league))+
  geom_col(position='dodge')+labs(y='Age Potential',x='Clubs')+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#total wage
unique(clubdata$league)
clubdata %>% 
  filter(league %in% c('Italian Serie A','English Premier League','German Bundesliga',
                       'French Ligue 1')) %>% 
  arrange(total_wage) %>% 
  ggplot(aes(x=factor(club,levels =rev(club)),y=total_wage,color=league,fill=league))+
  geom_col(position='dodge')+labs(y='Total wage',x='Clubs')+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#MLR
names(data)
dataset=data[sapply(data,is.numeric)]

names(dataset)
library(caTools)
split=sample.split(dataset$potential,SplitRatio=.80)

training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
MLR=lm(formula=potential~.,
       data=training_set)
summary(MLR)

#prediction on test set
y_pred=predict(MLR,newdata=test_set)
library(Metrics)

rmse(test_set$potential, y_pred)
mae(test_set$potential, y_pred)

saved=data.frame(test_set$potential, y_pred)
head(saved)

#Training to find out the euro wage
rmse(test_set$eur_wage, y_pred)
mae(test_set$eur_wage, y_pred)

saved=data.frame(test_set$eur_wage, y_pred)
head(saved)
summary(data[sapply(data,is.numeric)])
