library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")

getwd()
setwd("C:/Users/muhammad.ihtsham/Documents")
mydata <- read.csv("hospitaldata.csv")

# Converting to table format
mydata <- tbl_df(mydata)

# Formatting date & time columns
mydata$Date <- as.Date(strptime(mydata$Date, "%a, %B %d, %Y"))
mydata$Time <- format(strptime(mydata$Time, format='%I:%M %p'), '%I:%M %p')

# Removing characters from age
mydata$Age <- as.numeric(gsub("[^0-9]",'',mydata$Age))

# Changing sex characters to uppercase
mydata$Sex <- toupper(mydata$Sex)

# Remove Cancelled to NA in total charges
mydata$TotalCharges <- as.numeric(gsub("cancelled", NA, ignore.case = T, mydata$TotalCharges))

# Remove Cancelled to NA in procedure
mydata$Procedure <- (gsub("cancelled", NA, ignore.case = T, mydata$Procedure))


# Coverting AmountBalance to numeric
mydata$AmountBalance <- as.numeric(gsub(",",'',mydata$AmountBalance))
class(mydata$AmountBalance)


# 1: Removing dots from column names
names(mydata) <- gsub("\\.", "", names(mydata))


# 2: Most visits in a day
print(paste("Most visited day of the week is",
            weekdays(mydata$Date[which(table(mydata$Date) == max(table(mydata$Date)))])))





