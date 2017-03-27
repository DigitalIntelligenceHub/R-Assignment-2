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

# Question 4
# How many children were entertained? (Make a Bracket of Age from 1-12)
four <- filter(df, df1< 12)
count(four, na.rm = TRUE)

# Question 5
# Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem
df$Sex <- gsub("-", "", df$Sex)
df$Sex <- as.character(Map({function (a) toupper(a)}, df$Sex))
df %>%
  filter(Sex != "") %>% 
  group_by(Sex, Specialty)  %>%  
  summarise(n=n()) %>%  
  slice(which.max(n)) %>%  
  arrange(desc(n))

# Question 6 
# Which Doctor is earning highest?
View(df)
df$TotalCharges <- as.numeric(df$TotalCharges)
df %>%
  filter(ConsultingDoctor != "") %>%
  group_by(ConsultingDoctor)  %>%  
  summarise(sum= sum(TotalCharges, na.rm = TRUE)) %>%  
  slice(which.max(sum)) %>%  
  arrange(desc(sum))

# Question 7
# Which procedure type earns more money?
df %>%
  filter(Procedure != "") %>%
  group_by(Procedure)  %>%  
  summarise(sum= sum(TotalCharges, na.rm = TRUE)) %>%  
  slice(which.max(sum)) %>%  
  arrange(desc(sum))

# Question 8
# Which time of the day has highest frequency of visits by hour?
  gsub(x= df$Time, pattern = "-", replacement  = "")
  #filter(df$Time != "") %>%
  fulltime <- as.POSIXct(parse_date_time(df$Time, "%H:%M%p"))
  hour(fulltime)
  
  df %>% 
    group_by(hour(fulltime)) %>% 
    summarise(cnt = n()) %>% 
    slice(which.max(cnt)) 

#Question 9. 
# Create a bracket of time by Morning, Afternoon, Evening, Night 
# (6am - 12pm - Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night).
  gsub(x= df$Time, pattern = "-", replacement  = "")
  filter(df$Time != "") %>%
  filter(is.na(df$Time)) %>%
  fulltime <- as.POSIXct(parse_date_time(df$Time, "%H:%M%p"))
  a <- hour(fulltime)
  
  check <- function()
  {
    if ( findInterval(a, c(6:12))){ 
      print("Morning")
    }else if ( findInterval(a, c(12:16))) {
      print ("Afternoon")
    } else if ( findInterval(a, c(16:19))) {
      print("Evening")
    } else 
      print("Night")
  }
check(24)
check(1)

# Question 10.
#  How many patients are repeated visitors?
  df %>%
    group_by(id)  %>%  
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    count()

# Question 11.
# Give us the id of repeated visitors.
x <- df %>%
  group_by(id)  %>%  
  summarise(n=n()) %>% 
  filter(n>1) 
  x$id

# Question 12
#  Which patients visited again for the same problem?
  df %>%
    group_by(id, Specialty) %>%
    summarise(n=n()) %>%
    filter(n>1)
View(df)
df$Age <- as.numeric(df$Age)

#Question 13
# What is the median age for Females and Males?
  df %>%
    filter(Sex != "") %>%
    group_by(Sex) %>%        
    summarise(Median = median(Age, na.rm=TRUE))

# Question 14
# What is the total amount in balance?
df$AmountBalance <- as.character (df$AmountBalance)
df$AmountBalance <- gsub(x= df$AmountBalance, pattern = "-", replacement = "")
df$AmountBalance <-  gsub(x= df$AmountBalance, pattern = ",", replacement = "" )
df$AmountBalance <- as.numeric(df$AmountBalance)
sum(df$AmountBalance, na.rm=TRUE)

#Question 15
#How much money was made by Procedure Type "Consultation"?
df$TotalCharges <- as.numeric(df$TotalCharges)
df %>%
    filter(Procedure == "Consultation") %>%
    summarise(sum=sum(TotalCharges, na.rm = TRUE ))

#Question 16
# Is there a relation between Age and Total Charges paid?
df$Age <- as.numeric(df$Age)
df$TotalCharges <- as.numeric(df$TotalCharges)
cor.test(x= df$Age, y= df$TotalCharges)
