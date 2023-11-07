library('tidyverse')
fifa=read.csv('complete.csv')
length(complete_fifa)
names(complete_fifa)

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
  summarise(count=n(),
            mean_overall=round(mean(overall)),
            mean_age=round(mean(age)),
            mean_potential =round(mean(potential)),
            total_wage=round(sum(eur_wage)))
  
head(clubdata)

