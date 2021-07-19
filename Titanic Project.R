setwd("c:/Users/Work Station/Desktop/Data Analysis Projects/Datasets/Datasets-master")

## Installiing packages
install.packages("plyr")
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("plotly")

## Installing the libraries

library(tidyverse)
library(lubridate)
library(ggplot)
library(plyr)
library(dplyr)
library(here)
library(skimr)
library(janitor)
library(readr)
library(tidyr)
library(maps)
library(ggmap)
library(ggthemes)
library(plotly)



## Collect data

titanic <- read.csv("titanic_data_V2.csv")

## Inspecting the data 
colnames(titanic)
glimpse(titanic)
str((titanic))
head(titanic)
dim(titanic)
nrow(titanic)
summary(titanic)

## Summary of the fare
summary(titanic$Fare)


## Dropping the 'Cabin',.'SibSp','Parch','Ticket' as they won't be necessary for this analysis

titanic <- titanic %>%  
  select(-c( Cabin,SibSp,Parch,Ticket))

## Dropping the NA values in the Age column so as to make visualization better

na.omit(titanic$Age)

## Changing to the 'Survived', 'Pclass', 'Sex', 'Embarked' columns to factors to make the analysis easier
sapply(titanic, class)
SurvivalRate <- as.factor(titanic$Survived)
Passengerclass <- as.factor(titanic$Pclass)
Sex <- as.factor(titanic$Sex)
CityEmbarked <- (titanic$Embarked)


#Visualization

## Passengers sex by Passenger Class
sexPclassPlot <- titanic %>% 
  ggplot()+
  geom_bar(aes(Sex, fill =Passengerclass))+
   labs(title = "Passengers by Sex")
ggplotly(sexPclass)

 ## There were more male passengers on-board. Almost double the number of female passengers. Most of the male passengers consisting of the economy passenger class.

## Passengers by Passenger class 
PclassPlot <- titanic %>% 
  ggplot()+
  geom_bar(aes(x= Passengerclass, fill = Passengerclass))+
  labs(title = "Passengers by Passenger Class")
ggplotly(Pclass) 

 ## A bulk of the passengers on-board were in economy class. Considering the price difference between the fare for First Class and Economy, its safe to say economy class consisted of the lower class.

## Survival rate by Passenger Class

SurvivalPlot <- titanic %>% 
  ggplot()+
  geom_bar(aes(x= SurvivalRate, fill = Passengerclass))+
  labs(title =  "Passenger Survival by Passenger Class")
ggplotly(Survival)

count(titanic$Survived) #Confirming the exact number passengers for each factor 

  ## A total of 549 passengers died, while 342 survived. With the largest percentage of deaths consisting of passengers in economy class. This confirms my hypothesis that low income passengers suffered the most loss on the Titanic.


## Histogram of Passenger Age by Passenger Class

titanic %>% 
  ggplot()+
  geom_histogram(aes(x = Age, fill = SurvivalRate))+
  labs(title = "Passengers by Age") 

 
## Histogram of Passenger Age by Survival Rate

titanic %>% 
  ggplot()+
  geom_histogram(aes(x = Age, fill = SurvivalRate))+
  labs(title = "Passengers by Age", caption = "0 - Died;  1 - Survived")
geom_count(x = Age, fill = Passengerclass)




## Most of the passengers were between the age of 20-35 and where passengers of the economy class. A passenger class that suffered the most deaths, further proving the hypothesis that sinking of the Titanic was most catastrophic for the lower income young men.

#Cities Passengers embarked from 

titanic %>% 
  ggplot()+
  geom_bar(aes(x = CityEmbarked, fill =Passengerclass))+
  labs(title = "City Embarked")
 
## Most of the passengers embarked the Titanic in Southampton, with the city of Cherbourg coming in 2nd and Queenstown at 3rd. 
  
