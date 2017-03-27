# import Modules and data
library(dplyr)
library(tidyr)
library(lubridate)

raw_data <- read.csv("C:/Users/shahid.ammer/Documents/Shahid_Ammer_ISB_R_Assesment2/hospitaldata.csv")
mydf = tbl_df(raw_data)
mydf

# Please remove the dots in the names, so it may become easier for you to work through it
# Question 1
names(mydf)
names(mydf) <- sub("[.]+$", "", gsub("[.]+", " ", names(mydf)))
names(mydf)

# 2. Which day of the week is expected to have most visits?
#Question 2
mydf <- mydf %>% separate(Date, c('Day', 'Date'), sep="," , extra="merge")
most_expected_day <- mydf %>% count(Day) %>% slice(which.max(n))
most_expected_day

# 3. What is the average age of patients?
#Question 3
mydf$Age <- as.integer(as.character(mydf$Age))
mydf %>% summarize(avg_age = median(mydf$Age, na.rm=TRUE))

# 4. How many children were entertained? (Make a Bracket of Age from 1-12)
# Question 4
#sum(mydf$Age <= 12, na.rm=TRUE)
children_entertained <- mydf %>% filter(Age <= 12) %>% count()
children_entertained

# 5. Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem
#Question 5
mydf$Sex <- gsub("-", "", mydf$Sex)
mydf$Sex <- as.character(Map({function (a) toupper(a)}, mydf$Sex))
mydf %>% 
  filter(Sex != "") %>% 
  group_by(Sex, Specialty)  %>% 
  summarise(n=n()) %>% 
  slice(which.max(n)) %>% 
  arrange(desc(n))

# 6. Which Doctor is earning highest?
#Question 6:
mydf$`Total Charges` <- as.integer(as.character(mydf$`Total Charges`))
mydf %>%
  group_by(`Consulting Doctor`) %>%
  summarise(Sum = sum(`Total Charges`, na.rm=TRUE)) %>%
  slice(which.max(Sum))

# 7. Which procedure type earns more money?
mydf %>%
  group_by(Procedure) %>%
  summarise(Sum = sum(`Total Charges`, na.rm=TRUE)) %>%
  slice(which.max(Sum))

#Question 8
mydf$Time <- gsub(x=mydf$Time, pattern = "-", replacement = "")
Fulltime <- as.POSIXlt(parse_date_time(mydf$Time, "%H:%M%p"))
mydf %>% 
  group_by(hour(Fulltime))  %>% 
  summarise(Freq=n()) %>% 
  slice(which.max(Freq)) 

#Question 9
unique(hour(Fulltime))


# Question 10
mydf %>% 
  group_by(id) %>% 
  summarise(freq=n()) %>% 
  filter(freq > 1) %>% 
  count()  

# Question 11
rep_vis <- mydf %>% 
  group_by(id) %>% 
  summarise(freq=n()) %>% 
  filter(freq > 1) %>%
  arrange(desc(id))

rep_vis$id
