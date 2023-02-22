#load libraries
library(tidyverse)
library(lubridate)

#read data
restdata = read_csv("restaurant_inspections.csv")

#view the data and column names
View(restdata)
names(restdata)

#The column data has a different name, so I changed it to make it easier to use
restdata = rename(restdata,dates="DATE_")

#checking for missing data before creating a histogram
sum(is.na(restdata$"SCORE"))

#create histogram for the score column of the data
ggplot(restdata, aes(x=SCORE)) +
  geom_histogram()

#the restaurantopendate column was in the character class, so I changed it to read them as dates
restdata$RESTAURANTOPENDATE = ymd_hms(restdata$RESTAURANTOPENDATE)

#I created a scatter plot to determine a correlation between score and restaurant open date and added a 
#line of best fit
library(ggplot2)
ggplot(restdata, aes(x=RESTAURANTOPENDATE, y=SCORE)) + geom_point(shape=23) +
  geom_smooth(method=lm, se=FALSE)

#There is missing data in the restaurant open date column, so I checked the numbr of NA's to determine 
#how big the effect of the missing data would be. 
sum(is.na(restdata$RESTAURANTOPENDATE))

#Although the plot is mostly obvious, I want to make sure the r^2 value is similar. 
Model<-lm(SCORE~RESTAURANTOPENDATE,data=restdata)
summary(Model)$r.squared

#I need to determine all of the city names on the list so I can combine the ones that are spelled differently.
unique(restdata$CITY)

#I am recoding all of the city names to be upper case and the same spelling.
restdata$CITY=recode(restdata$CITY,"Cary"="CARY","RESEARCH TRIANGLE PARK"= "RTP","Apex"="APEX","Raleigh"="RALEIGH",
                     "FUQUAY VARINA"="FUQUAY-VARINA","Zebulon"="ZEBULON","Morrisville"="MORRISVILLE",
                     "MORRISVILE"="MORRISVILLE", "Wake Forest"="WAKE FOREST","Holly Springs"="HOLLY SPRINGS",
                     "Fuquay Varina"="FUQUAY-VARINA","Fuquay-Varina"="FUQUAY-VARINA","HOLLY SPRING"="HOLLY SPRINGS",
                     "Garner"="GARNER")

#I notice there is an NA column, so I want to determine how many are missing so I know how big the effect could 
#possibly be.
sum(is.na(restdata$"CITY"))
library(dplyr)
city_ss=restdata %>%
  group_by(CITY) %>%
  summarise(n = n())
print(city_ss)

#I am checking my work to determine if I missed any duplicates/misspellings.
unique(restdata$CITY)

#I am grouping the data by city and summarizing the mean scores because we want the mean score not the sum.
city_data=group_by(restdata, CITY) %>%
  summarize(mean_city_scores=mean(SCORE))

#I created a bar chart of the cities vs the mean scores to see if there was a significant trend while omitting the NA and 
#North Carolina group.
city_data %>%
  filter(CITY != "NORTH CAROLINA") %>%
  filter(CITY != "NA") %>%
  ggplot(aes(x = CITY, y=mean_city_scores)) +
  geom_bar(stat="identity")

#I wanted to make sure all of the inspector names were documented under the same unique name.
unique(restdata$INSPECTOR)

#checking for any missing data 
sum(is.na(restdata$"INSPECTOR"))

#I did the same process as above, grouping the scores with the inspector names. 
inspect_data=group_by(restdata, INSPECTOR) %>%
  summarize(mean_inspect_scores=mean(SCORE))

#I created a bar chart with the inspector name on the x axis and their mean scores on the y axis.
ggplot(inspect_data, aes(x = INSPECTOR, y=mean_inspect_scores)) +
  geom_bar(stat="identity")

#I created groups for both inspector and city separately, and asked it to summarize the number (n)
#instead of mean or sum like before.
library(dplyr)
city_ss=restdata %>%
  group_by(CITY) %>%
  summarise(n = n())

inspect_ss=restdata %>%
  group_by(INSPECTOR) %>%
  summarise(n = n())

#i want to see the full tibbles
print(city_ss)
print(inspect_ss, n=39)

#Checking for facility type missing data
sum(is.na(restdata$"FACILITYTYPE"))
all(is.na(restdata$CITY) == is.na(restdata$FACILITYTYPE))

#grouping data by facility and the mean scores for each
unique(restdata$FACILITYTYPE)
ft_data=group_by(restdata, FACILITYTYPE) %>%
  summarize(mean_ft_scores=mean(SCORE))

#making a bar plot of the facility type's mean scores
ft_data %>%
  filter(FACILITYTYPE != "NA") %>%
  ggplot(aes(x = FACILITYTYPE, y=mean_ft_scores)) +
  geom_bar(stat="identity")

#viewing the data
View(ft_data)
View(city_data)
View(inspect_data)

#creating a dataset of only restaurant facilities
onlyrest= filter(restdata, FACILITYTYPE == "Restaurant")

#creating a histogram of distribution of scores for restaurants only. 
ggplot(onlyrest, aes(x=SCORE)) +
  geom_histogram()

#creating a scatterplot to see the correlation between res. open date and score for only restaurants
ggplot(onlyrest, aes(x=RESTAURANTOPENDATE, y=SCORE)) + geom_point(shape=23) +
  geom_smooth(method=lm, se=FALSE)

#seeing how many missing data points we have for re. open date for only restaurants
sum(is.na(onlyrest$RESTAURANTOPENDATE))

#creating a model to determine the r^2 for res. open date and score for restaurants
Model<-lm(SCORE~RESTAURANTOPENDATE,data=onlyrest)
summary(Model)$r.squared

#looking at cities for restaurants only
unique(onlyrest$CITY)

#looking at sample sizes for cities for restaurants only
city_ss2=onlyrest %>%
  group_by(CITY) %>%
  summarise(n = n())
print(city_ss2)

#grouping and taking mean of only restaurant scores by city
city_data2=group_by(onlyrest, CITY) %>%
  summarize(mean_city_scores2=mean(SCORE))

#making a bar plot of mean scores of only restaurants by city. 
city_data2 %>%
  ggplot(aes(x = CITY, y=mean_city_scores2)) +
  geom_bar(stat="identity")

unique(onlyrest$INSPECTOR)

#grouping and taking the mean scores of inspectors for restaurants only
inspect_data2=group_by(onlyrest, INSPECTOR) %>%
  summarize(mean_inspect_scores2=mean(SCORE))

#making a bar plot of mean scores of only restaurants by inspector
ggplot(inspect_data2, aes(x = INSPECTOR, y=mean_inspect_scores2)) +
  geom_bar(stat="identity")

#counting sample sizes
city_ss2=onlyrest %>%
  group_by(CITY) %>%
  summarise(n = n())

inspect_ss2=onlyrest %>%
  group_by(INSPECTOR) %>%
  summarise(n = n())

#i want to see the full tibbles
print(city_ss2)
print(inspect_ss2, n=39)

View(city_data2)
View(inspect_data2)



