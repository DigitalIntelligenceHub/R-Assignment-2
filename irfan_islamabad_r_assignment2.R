# Load the file to Variable MyData
library(dplyr)
library(tidyr)
library(lubridate)
datahosp <- read.csv(file="hospitaldata.csv", header=TRUE, sep=",")
datahosp = tbl_df(datahosp)
# Q1. Please remove the dots in the names, so it may become easier for you to work through it

names(datahosp) <- gsub(x = names(datahosp), pattern = "\\.", replacement = " ")
names(datahosp) <- gsub(x = names(datahosp), pattern = "  ", replacement = " ")
names(datahosp)

# Q2. Which day of the week is expected to have most visits?
datahosp <- separate(datahosp, Date, c('Day', 'Date'), extra = "merge", sep = ",")

day_of_week_most_visits <- slice(count(datahosp, Day), which.max(n))
day_of_week_most_visits

# Q3.What is the average age of patients? 
avg_age_patient <- summarize(datahosp, avg_age = mean(as.integer(as.character(datahosp$Age)), na.rm = TRUE))
avg_age_patient

# Q4. How many children were entertained? (Make a Bracket of Age from 1-12)
# class(as.datahosp$Age) change the age (factor) to integers
datahosp$Age <- as.integer(as.character(datahosp$Age))
number_of_children1 <- sum(datahosp$Age < 12, na.rm = TRUE)
number_of_children1
# or using count and filter on age
number_of_children2 <- count(filter(datahosp, Age < 12))
number_of_children2


# Q5. Which gender type had what kind of procedure in abundance? 
# i.e. Female visit mostly because of Gynae Problem
datahosp$Sex <- as.character(datahosp$Sex)
# convert all lower case to upper so that m would be M and f would F
datahosp$Sex <- toupper(datahosp$Sex)


new <- datahosp %>% group_by(Sex, Specialty) %>%
  summarize(freq = n()) %>%
  slice(which.max(freq))

new[order(new$freq,decreasing=T)[1:2],]
