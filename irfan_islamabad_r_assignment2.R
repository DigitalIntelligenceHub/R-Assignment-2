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

# Q6. Which Doctor is earning highest? 
#aggregate(as.numeric(datahosp$`Total  Charges`), by=list(Category=datahosp$`Consulting  Doctor`), FUN=sum)

datahosp$`Total Charges` <- as.numeric(as.character(datahosp$`Total Charges`))
highest_paid_doc <- datahosp %>% 
  group_by(`Consulting Doctor`) %>%
  summarize(Total = sum(`Total Charges`, na.rm = TRUE))

highest_paid_doc[order(highest_paid_doc$Total,decreasing=T)[1],]

# Q7. Which procedure type earns more money

highest_earning_procedure <- datahosp %>% 
  group_by(Procedure) %>%
  summarize(Total = sum(`Total Charges`, na.rm = TRUE))

highest_earning_procedure[order(highest_earning_procedure$Total,decreasing=T)[1],]

# Q8. Which time of the day has highest frequency of visits by hour?
datahosp$Time <- gsub(x = datahosp$Time, pattern = "-", replacement = "")
datahosp$Time <- parse_date_time(datahosp$Time,"%H:%M%p")
datahosp$Time <- format(datahosp$Time, "%H:%M:%S")
datahosp <- separate(datahosp, Time, c('Hour', 'Minute', 'Seconds'),  sep = ":")
higest_freq_vists_by_hour <- slice(count(datahosp, Hour), which.max(n))

higest_freq_vists_by_hour


# 10. How many patients are repeated visitors?

repeated_visitors <- datahosp %>%
  group_by(id) %>% 
  summarize(freq = n()) %>%
  filter(freq > 1) %>%
  count()

repeated_visitors

# 11. Give us the id of repeated visitors. 

repeated_visitors <- datahosp %>%
  group_by(id) %>% 
  summarize(freq = n()) %>%
  filter(freq > 1)

repeated_visitors_id <- repeated_visitors$id
repeated_visitors_id

# 12. Which patients visited again for the same problem? 
repeated_visitors_same_prob <- datahosp %>%
  group_by(id, Specialty) %>% 
  summarize(freq = n()) %>% 
  filter(freq > 1)


repeated_visitors_same_prob$id

# Q 13.What is the median age for Females and Males? 
datahosp$Sex <- gsub(x = datahosp$Sex, pattern = "-", replacement = "")
median_age_female_male <- datahosp%>%
  filter(Sex != "") %>%
  group_by(Sex)%>% 
  summarise(Median=median(Age, na.rm =TRUE))

median_age_female_male

# Q 14. What is the total amount in balance
datahosp$`Amount Balance` <- gsub(x = datahosp$`Amount Balance`, pattern = "-", replacement = "")
datahosp$`Amount Balance` <- as.numeric(as.character(gsub(x = datahosp$`Amount Balance`, pattern = ",", replacement = "")))
total_amount <- sum(as.integer(as.character(datahosp$`Amount Balance`)), na.rm = TRUE)
total_amount
