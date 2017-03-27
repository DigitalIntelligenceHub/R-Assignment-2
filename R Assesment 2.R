library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")

getwd()
setwd("C:/Users/muhammad.ihtsham/Documents")
mydata <- read.csv("hospitaldata.csv")

# 1: Removing dots from column names
names(mydata) <- gsub("\\.", "", names(mydata))

# 2: Most visits in a day
mydata <- separate(mydata, Date, c("Day", "Month", "Year"), sep = ",")
table(mydata$Day)
# Table command shows that Monday has the highest number of patient visits

# 3: Average age of aptients
class(Age)
mydata$Age <- as.numeric(mydata$Age, na.rm = TRUE)
mean(mydata$Age, na.rm = TRUE)

# 4: How many children were entertained (Age: 1-12)
child <- which(mydata$Age > 1 & mydata$Age < 12)
length(child)
# 23 Children between age range 1-12

# 5: Visit in abundance by gender wise


library("dplyr")
library("tidyr")
library("stringr")
# 6: Which doctor is earning highest
mydata$ConsultingDoctor <- as.character(mydata$ConsultingDoctor)
mydata$TotalCharges <- as.numeric(mydata$TotalCharges)

highest_earner <- mydata %>%
  group_by(ConsultingDoctor) %>%
  summarise(sum(TotalCharges), na.rm = T) %>%
  select(ConsultingDoctor, TotalCharges)
