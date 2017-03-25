# Obaid Ur Rehman

#Loading required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(mosaic)


#Loading data set from csv file named "hospitaldata.csv"

hdf <- read.csv("D:\\Inbox Workplace\\R Workspace\\R Learning Assignment 2\\R-Assignment-2\\Obaid_Islamabad_r_Assignment2\\hospitaldata.csv",na.strings=c("","NA"))
dim(hdf)
# 222 observations and 15 columns

#Printing hdf
head(hdf)

# Q1. Cleaning the column names
names(hdf)<-gsub("\\.","",names(hdf))
head(hdf) #dots from column names removed



# Q2. Which day of the week is expected to have most visits?
dayPop <-
  hdf %>%
  mutate(Day=wday(mdy(Date),label=TRUE)) %>%
  group_by(Day) %>%
  summarize(visits=length(Day))

ggplot(dayPop,aes(x=Day,y=visits))+geom_bar(stat="identity",fill="#8E44AD")+ggtitle("Visits by Days")+labs(x="Day",y="Vists")

#The visits on Monday are greater than visits on other days of week, and also the probability of Monday is higher 
# therefore, Monday is expected to have most visits

