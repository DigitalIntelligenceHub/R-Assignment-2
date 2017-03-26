#Question 17
library("ggplot2")
library("lubridate")
library("dplyr")
library("tidyr")
library("DataCombine")

bufferedDataFrame <- read.csv("C:/Users/umair.hanif/Desktop/Learning Outcomes/AR/hospitaldata.csv", strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F)
dataf <- tbl_df(bufferedDataFrame)
View(dataf)
glimpse(dataf)

#removing character from age M
dataf$Age <- as.numeric(gsub("[^0-9]",'',dataf$Age))
class(dataf$Age)
unique(dataf$Age)


# Question #1 
#Changing Datatypes from factor to their required formats
names(dataf) <- gsub("\\.",'',names(dataf))

#now lets play with date
dataf$Date <- as.Date(strptime(dataf$Date, "%a, %B %d, %Y"))

visits_by_age <- dataf %>%
  select(id,Age) %>%
  group_by(Age) %>%
  summarize(visits=length(Age)) %>%
  arrange(desc(visits)) %>%
  filter(!is.na(Age)) %>%
  print

ggplot(data=visits_by_age,aes(x=as.numeric(Age),y=visits))+geom_bar(stat='identity',fill='slate blue')+ggtitle("Visits By Age")+labs(x='Age',y='Visits')

