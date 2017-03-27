library (dplyr)
library(tidyr)
library(lubridate)

# Question 1
# Please remove the dots in the names, so it may become easier for you to work through it.
hospitaldata <- read.csv("C:/Users/albab.ahmad/Desktop/Assessment 2/Albab-Ahmad-Khan_Rawalpindi_R_Assignment2/hospitaldata.csv")
df = hospitaldata
View(df)
#names(df) <- gsub(x= "\\.", " ", names(df), fixed = TRUE)
names(df) <- gsub(x= names(df), pattern = "\\.", replace = "")
names(df)
