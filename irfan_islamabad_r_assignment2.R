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
