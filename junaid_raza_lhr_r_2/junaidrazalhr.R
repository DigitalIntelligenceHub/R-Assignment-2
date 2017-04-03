#getwd()
getwd()


#setwd('C:/Users/junaid.raza/junaidraza_lhr_r_assignment2',header=TRUE)

library(dplyr)
library(tidyr)
#install.packages("lubridate")
library(lubridate)

#Working with two data sets
#Loading Data into df
df <- read.csv('hospitaldata.csv', header = TRUE)
genpro <- read.csv('hospitaldata.csv', header = TRUE, na.strings=c("","NA"))
#Loading Data into test
test <- read.csv('hospitaldata.csv', header = TRUE)

#Loading Data into test
#rlab <- read.csv('hospitaldata.csv', header = TRUE)


#1
#Removing the Dots 
names(df) <- gsub("\\.", "", names(df))
names(df)

#2
#Which week day have max visits
#Sepate the table to extract three more columns of 
#Day Month and Year
test <- test %>% separate(Date,c('day','month','year'),sep=',')
df <- df %>%  separate(Date,c('day','month','year'),sep=',')
weekdays<-table(df$day)
unique(weekdays)
weekdays
#Monday have 51 visits


#3
#Average age of patients
class(df$Age)
df$Age<-as.numeric(df$Age)
mean(df$Age)
#Mean os 23.33


#4
#Children enteratined(age 1 - 12)
lengthofage<-length(which(df$Age<12))
lengthofage
#58 is length of records



#5
#Which gender have what max procedures
class(df$Sex)
class(df$Procedure)
#spaces replaced with NA of all data frame
df$Sex<-tolower(df$Sex)
df$Sex
genproc <- df %>%
  group_by(Sex,Procedure) %>%
  summarize(count=n())
View(genproc)


#6
#Highest Earning
class(df$TotalCharges)
df$TotalCharges<- as.numeric(as.character(df$TotalCharges))
class(df$TotalCharges)
class(df$ConsultingDoctor)
df$ConsultingDoctor<-as.character(df$ConsultingDoctor)
class(df$ConsultingDoctor)

docmaxpay <- df %>%
  group_by(ConsultingDoctor) %>%
  summarise(total = sum(TotalCharges, na.rm = TRUE))

docmaxpay$ConsultingDoctor[which(docmaxpay$total==max(docmaxpay$total, na.rm = TRUE),arr.ind=TRUE)]
#Dr Alaf Khan


#7
#Procedure type earn max money
class(df$Procedure)
df$Procedure <- as.character(df$Procedure)
maxprocd <- df %>%
  group_by(Procedure) %>%
  summarise(total = sum(TotalCharges, na.rm = TRUE))

maxprocd$Procedure[which(maxprocd$total==max(maxprocd$total, na.rm = TRUE),arr.ind=TRUE)]
#Orthodontics 240,000


#8
#Highest freq of Visits by Hour.
class(df$Time)
df$Time<-as.character(df$Time)
df$Time[df$Time=="-"]<-""
df$Time[df$Time==""]<-NA
unique(df$Time)

get_time<-df$Time

get_time<-strptime(get_time,"%I:%M %p")
get_hours<-hour(get_time)

actual_mode <- table(get_hours)
names(actual_mode)[actual_mode == max(actual_mode)]
#Hence the hour 13:00 is the most visited hour of the day



#9
#Create Bracket of Time.





#10
#patients are Repeated visitors
class(df$id)
vis_num <- df %>%
  group_by(id) %>%
  summarize(count=n())
View(vis_num)
vis_num$id[(which(vis_num$count >1))]

#11
#Info Repeated Visitor
length(vis_num$id[(which(vis_num$count >1))])
#37


#12
#Patient again visit for same problem
vis_two <- df %>%
  group_by(id,Procedure) %>%
  summarize(count=n())
vis_two
vis_two <- vis_two$id[(which(vis_two$count >1))]
vis_two





#13
#Median age for male and females
fmed<-which(df$Sex=='f',arr.ind =TRUE) 
median(as.numeric(as.character(df$Age[fmed])),na.rm = TRUE)
#22
mmed<-which(df$Sex=='m',arr.ind =TRUE) 
median(as.numeric(as.character(df$Age[mmed])),na.rm = TRUE)
#21



#14
#Total Amount Balance



#15
#Procedure consultation Money
consult_money<- which(df$Procedure=='Consultation', arr.ind=TRUE)
sum(df$TotalCharges[consult_money], na.rm=TRUE)
#83950




#16
#Relation age and Total Charges
class(df$Age)
class(df$TotalCharges)
df$TotalCharges<-as.numeric(as.character(df$TotalCharges))
cor.test(df$Age, df$TotalCharges)
#Value is greate then 0.05



#17
#Age with max high visits






#18
#Procedure X-Ray adn Scalling Total cost
total_xray_cost<- which(df$Procedure=='X Ray' | df$Procedure=='Scalling' , arr.ind=TRUE)
sum(df$TotalCharges[total_xray_cost], na.rm=TRUE)
#22300







