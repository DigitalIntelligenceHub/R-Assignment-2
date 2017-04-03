library(lubridate)
library(dplyr)
library(tidyr)


getwd()
setwd("C:/Users/muhammad.u/Documents/Muhammad_Usman_Lhr_R_assignment2")

# Q#1
# remove dots from header

hsp_df <- read.csv("hospitaldata.csv",sep = ',', header = TRUE)
names(hsp_df)<- gsub("\\.","",names(hsp_df))
names(hsp_df)

# Q#2
# Which day of the week is expected to have most visits ?

hsp_df <- separate(hsp_df,Date,c('day', 'month', 'year'),sep=',')
weekdays<- table(hsp_df$day)
unique(weekdays)
weekdays

# Q#3
# What is the average age of patients?

class(hsp_df$Age)

# convert factor into numeric
hsp_df$Age <- as.numeric(as.character(hsp_df$Age))
class(hsp_df$Age)

# remove na values, then find mean
mean(hsp_df$Age, na.rm = TRUE)

# Q#4
# children from 1 - 12
length(which(hsp_df$Age<12))

#Q#5
# gender type has procedure in abundance
class(hsp_df$Sex)
hsp_df$Sex <- tolower(hsp_df$Sex)
hsp_df$Sex

hsp_df$Sex[hsp_df$Sex=="-" | hsp_df$Sex==""]<-NA
sex_proc <- hsp_df %>%
      group_by(Sex,Procedure) %>%
       summarize(count=n())
View(sex_proc)


#Q#6
# doctor is earning highest
hsp_df$TotalCharges <- as.numeric(as.character(hsp_df$TotalCharges))
hsp_df$ConsultingDoctor <- as.character(hsp_df$ConsultingDoctor)
doc_earn <- hsp_df %>%
  group_by(ConsultingDoctor) %>%
  summarise(total = sum(TotalCharges, na.rm = TRUE))
doc_earn

doc_earn$ConsultingDoctor[which(doc_earn$total==max(doc_earn$total, na.rm = TRUE),arr.ind=TRUE)]



# Q#7
# which procedure earn highest
hsp_df$Procedure <- as.character(hsp_df$Procedure)
proc_earn <- hsp_df %>%
  group_by(Procedure) %>%
  summarise(total = sum(TotalCharges, na.rm = TRUE))
proc_earn

proc_earn$Procedure[which(proc_earn$total==max(proc_earn$total, na.rm = TRUE),arr.ind=TRUE)]


# Q#8
#highest frequency of visits by hour

class(hsp_df$Time)
hsp_df$Time<-as.character(hsp_df$Time)
hsp_df$Time[hsp_df$Time=="-"]<-""
hsp_df$Time[hsp_df$Time==""]<-NA
unique(hsp_df$Time)

conv_time <- hsp_df$Time

#conv_time <- strptime(conv_time,"%H:%M:%S")

conv_time<-strptime(conv_time,"%I:%M %p")
conv_hours<-hour(conv_time)

# conv_hours<-table(conv_hours)
# conv_hours

actual_mode <- table(conv_hours)
names(actual_mode)[actual_mode == max(actual_mode)]

# found the frequency of conv_hours, 13:00 is the most visited hour.


# Q#11
# id of repeated visitors
class(hsp_df$id)
rep_vis <- hsp_df %>%
  group_by(id) %>%
  summarize(count=n())
View(rep_vis)
rep_vis$id[(which(rep_vis$count >1))]

# Q#10
# length of repeated visitors
length(rep_vis$id[(which(rep_vis$count >1))])

# Q#12
# patients visited again for same problem

vis_again <- hsp_df %>%
  group_by(id,Procedure) %>%
  summarize(count=n())
vis_again
vis_again <- vis_again$id[(which(vis_again$count >1))]
vis_again
hsp_df$Sex

# Q#13
# median age female
i<-which(hsp_df$Sex=='f',arr.ind =TRUE) 
median(as.numeric(as.character(hsp_df$Age[i])),na.rm = TRUE)

# median age male
i<-which(hsp_df$Sex=='m',arr.ind =TRUE) 
median(as.numeric(as.character(hsp_df$Age[i])),na.rm = TRUE)

#  Q#16
hsp_df$Age <- as.numeric(as.character(hsp_df$Age))
hsp_df$TotalCharges <- as.numeric(as.character(hsp_df$TotalCharges))
cor.test(hsp_df$Age, hsp_df$TotalCharges)
# as p value > 0.05, its a positive co relation. 
#  p-value = 0.7043

# Q 14
unique(hsp_df$AmountBalance)
hsp_df$AmountBalance <- as.character(hsp_df$AmountBalance)
hsp_df$AmountBalance<- gsub("\\-","",hsp_df$AmountBalance)
hsp_df$AmountBalance<- gsub(",","",hsp_df$AmountBalance)
hsp_df$AmountBalance <- as.numeric(as.character(hsp_df$AmountBalance))
sum(hsp_df$AmountBalance, na.rm=TRUE)

class(hsp_df$AmountBalance)


# Q 15
high_proc<- which(hsp_df$Procedure=='Consultation', arr.ind=TRUE)
sum(hsp_df$TotalCharges[high_proc], na.rm=TRUE)

# Q# 18
x_scal<- which(hsp_df$Procedure=='X Ray' | hsp_df$Procedure=='Scalling' , arr.ind=TRUE)
sum(hsp_df$TotalCharges[x_scal], na.rm=TRUE)

# Q#13
# hsp_df$Age<- as.numeric(as.character(hsp_df$Age))
# max(hsp_df$Age, na.rm = TRUE)
# min(hsp_df$Age, na.rm = TRUE)



