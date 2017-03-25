# Obaid Ur Rehman

#Loading required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(mosaic)


#Loading data set from csv file named "hospitaldata.csv"

hdf <- read.csv("D:\\Inbox Workplace\\R Workspace\\R Learning Assignment 2\\R-Assignment-2\\Obaid_Islamabad_r_Assignment2\\hospitaldata.csv",na.strings=c("","NA"))
dim(hdf)
# 222 observations and 15 columns

#Printing hdf
head(hdf)

# Q1. Cleaning the column names
names(hdf)<-gsub("\\.","",names(hdf))
head(hdf) #dots from column names removed



# Q2. Which day of the week is expected to have most visits?
dayPop <-
  hdf %>%
  mutate(Day=wday(mdy(Date),label=TRUE)) %>%
  group_by(Day) %>%
  summarize(visits=length(Day))

ggplot(dayPop,aes(x=Day,y=visits))+geom_bar(stat="identity",fill="#8E44AD")+ggtitle("Visits by Days")+labs(x="Day",y="Vists")

#The visits on Monday are greater than visits on other days of week, and also the probability of Monday is higher 
# therefore, Monday is expected to have most visits

# Q3. What is the average age of patients?
hdfClean<- hdf
hdfClean$Age <-as.numeric(as.character(hdfClean$Age))
mean(hdfClean$Age,na.rm = TRUE) #Average age is 32.7 

# Q4. How many childerns were entertained?
count(filter(hdfClean,Age>=1,Age<=12))  #23 childerns were entertained    #Q to ask, if i use length instead of count it gives 15. y?

# Q5. Which gender type had what kind of procedure in abundance?
hdfClean$Sex <- gsub("f","F",hdfClean$Sex)
hdfClean$Sex<-gsub("\\s|-",NA,hdfClean$Sex)
qplot(data=hdfClean,Sex,fill=Specialty)+ggtitle("Gender Speciality abundance")+labs(x='Gender',y='No of patients')

# As we can see from plot, both Male and Female have Dentist procedure in abundance



# Q6. Which doctor is earning highest?

#Cleaning totalCharges column (we will need in future to summ charges) by Converting them to numeric and replacing NA with 0
hdfClean$TotalCharges <- as.numeric(as.character(hdfClean$TotalCharges))
hdfClean[c('TotalCharges')][is.na(hdfClean[c('TotalCharges')])]<-0  #only chnage NA to 0 in TotalCharges Column
DrEarnings <-
  hdfClean %>%
  group_by(ConsultingDoctor)%>%
  summarize(Earning=sum(TotalCharges)) %>%
  arrange(desc(Earning))

DrEarnings # Dr Alaf Khan has the highest earnings!

#Plottig graph for DoctorEarnings
ggplot(data=DrEarnings,aes(x=ConsultingDoctor,y=Earning))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("ConsultingDoctor Earnings")+labs(x='Consulting DOctor',y='Earnings')


# Q7. Which procedure type earns more money?

#its same as above Question, jut need to group_by with Procedur instead of ConsultingDoctor
# We dont need to clean totalcharges column again

ProcedureEarnings <-
  hdfClean %>%
  group_by(Procedure) %>%
  summarize(Earning=sum(TotalCharges)) %>%
  arrange(desc(Earning))
ProcedureEarnings  #Orthodontics earns more money

#Plotting graph for ProcedureEarnings
ggplot(data=ProcedureEarnings,aes(x=Procedure,y=Earning))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("Earnings by Procedures")+labs(x='Procedures',y="Earnings")




