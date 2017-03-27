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

# Question 2
# Which day of the week is expected to have most visits?
xx <- tbl_df(df)
df <- separate(df, Date, into = c("Day", "Date"), sep = ",", extra = "merge")
new <- df %>% count(Day) %>%  slice(which.max(n))
new

# Question 3
# What is the average age of patients?
ages <- hospitaldata$Age
gsub("-", "NA", ages) 
df1= as.numeric(ages)
class(df1)
mean(df1, na.rm = TRUE)
