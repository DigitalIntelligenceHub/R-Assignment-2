library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

Hospital_data <- read.csv("E:/DIH/RWork/Assignment/hospitaldata.csv",header = TRUE, stringsAsFactors = FALSE)
#Q1
#View(Hospital_data)
names(Hospital_data) <- gsub("\\." ," ", names(Hospital_data))
#View(Hospital_data)

#Q3
Hospital_data$Age<-gsub("M" ,"", Hospital_data$Age)
Hospital_data$Age <- as.numeric(Hospital_data$Age)
mean(Hospital_data$Age,na.rm=TRUE)

#Q4
length(which(Hospital_data$Age <= 12 & Hospital_data$Age >=1))

#Q5
Hospital_data$Sex<-gsub("f" ,"F", Hospital_data$Sex)
Hospital_data%>%
  filter(Sex=='M')%>%
  group_by(Procedure)%>%
  summarize(maxi=n())%>%
  filter(maxi==max(maxi))%>%
  print

Hospital_data%>%
  filter(Sex=='F')%>%
  group_by(Procedure)%>%
  summarize(maxi=n())%>%
  filter(maxi==max(maxi))%>%
  print

#Q6
Hospital_data$`Total  Charges` <- as.numeric(Hospital_data$`Total  Charges`)
Hospital_data%>%
  group_by(`Consulting  Doctor`)%>%
  summarize(high=sum(`Total  Charges`, na.rm=TRUE))%>%
  filter(high==max(high))%>%
  print

#Q7
Hospital_data$`Total  Charges` <- as.numeric(Hospital_data$`Total  Charges`)
Hospital_data%>%
  group_by(Procedure)%>%
  summarize(high=sum(`Total  Charges`, na.rm=TRUE))%>%
  filter(high==max(high))%>%
  print

#Q8
Hospital_data$Time<-hour(strptime(Hospital_data$Time, "%I:%M %p" ))
Hospital_data$Time
Hospital_data %>%
  filter(!is.na(Time), Time != '-') %>%
  group_by(Time) %>%
  summarize(timeCount = n()) %>%
  filter(Time != '') %>%
  filter(timeCount == max(timeCount))

  
  

#Q10
Hospital_data%>%
  group_by(id)%>%
  summarize(repeat_id=n())%>%
  filter(repeat_id>1)%>%
  nrow()%>%
  print


#Q11
Hospital_data%>%
  group_by(id)%>%
  summarize(repeat_id=n())%>%
  filter(repeat_id>1)%>%
  print

#Q12
Hospital_data%>%
  group_by(id,Procedure)%>%
  summarize(repeat_id=n())%>%
  filter(repeat_id>1)%>%
  print

#Q13
Hospital_data%>%
  filter(Sex=='M')%>%
  select(Age)%>%
  summarize(med=median(Age,na.rm=TRUE))%>%
  print

Hospital_data%>%
  filter(Sex=='F')%>%
  select(Age)%>%
  summarize(med=median(Age,na.rm=TRUE))%>%
  print

#Q14
Hospital_data$`Amount  Balance`<-gsub("," ,"",Hospital_data$`Amount  Balance` ,fixed=TRUE)
Hospital_data$`Amount  Balance`<-gsub(".00" ,"", Hospital_data$`Amount  Balance`,fixed=TRUE)
Hospital_data$`Amount  Balance` <- as.numeric(Hospital_data$`Amount  Balance`)
Hospital_data%>%
  select(`Amount  Balance`)%>%
  summarize(sum1=sum(`Amount  Balance`, na.rm=TRUE))%>%
  print
  
#Q15
Hospital_data%>%
filter(Procedure=='Consultation')%>%
  select(`Total  Charges`)%>%
  summarize(maxi=sum(`Total  Charges`,na.rm=TRUE) )%>%
  print

#Q17
Hospital_data %>%
  filter(!is.na(Age), Age!='-') %>%
  group_by(Age) %>%
  summarize(AgeCount=n()) %>%
  filter(Age!='') %>%
  filter(AgeCount == max(AgeCount))%>%
  print


#Q18
Hospital_data%>%
  filter(Procedure=='X Ray'| Procedure=='Scalling')%>%
  select(`Total  Charges`)%>%
  summarize(maxi=sum(`Total  Charges`,na.rm=TRUE) )%>%
  print

