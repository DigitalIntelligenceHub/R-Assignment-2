#install.packages("dplyr")
library(dplyr)
mydf <- read.csv("hospitaldata.csv",stringsAsFactors = FALSE)
hos <- tbl_df(mydf)
rm(mydf)

#Question 01  
names(hos) <- gsub(".", "", names(hos), fixed = TRUE)
names(hos)

#Cleaning Dataset and changing column data types
hos$Date <-as.Date(hos$Date,"%a, %B %d, %Y")
library(stringr)
#Some of the data in Age column contain values as month like 6M and 28M, so we have to convert it to years
hos <-
  hos %>%
  mutate(Age= ifelse ( str_detect(Age,"M"),
                       strtoi(gsub("M", "", Age))/12,
                       Age))
hos$Age <-as.numeric(hos$Age)
hos$AmountBalance <-as.numeric(hos$AmountBalance)
hos$TotalCharges <-as.numeric(hos$TotalCharges) 
hos<-
  hos %>%
  mutate(Sex=ifelse(Sex=="" | Sex =="-",NA,Sex))
hos$Sex<-toupper(hos$Sex)
hos$NextApt <-as.Date(hos$NextApt,"%m/%d/%Y")

#Question 01
x<-table(weekdays(hos$Date))
which.max(x)

#Question 02
mean(hos$Age,na.rm = TRUE)

#Question 03
rows <-
  hos %>%
  filter(Age>=1,Age<=12) %>%
  as.data.frame()  
nrow(rows)

#Question 04
hos%>%
  count(Sex, Procedure) %>%
  slice(which.max(n))

#Question 05
hos%>%
  group_by(ConsultingDoctor) %>%
  summarise(TotalCharges=sum(TotalCharges,na.rm=TRUE)) %>%
  filter(TotalCharges==max(TotalCharges))

#Question 06
hos%>%
  group_by(Procedure) %>%
  summarise(TotalCharges=sum(TotalCharges,na.rm=TRUE)) %>%
  filter(TotalCharges==max(TotalCharges))

#Question 07
#install.packages("lubridate")
library(lubridate)

hos$Time <- as.POSIXlt(strptime(hos$Time,"%I:%M%p"))
which.max(table(hour(hos$Time)))

#Question 08
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=6 & hour(Time)<=12,"Morning",NA))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=12 & hour(Time)<=16,"Afternoon",TimeBracket))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=16 & hour(Time)<=19,"Evening",TimeBracket))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=19 & hour(Time)<=6,"Night",TimeBracket))

#Question 09
rv<-
  hos %>%
  group_by(id) %>% 
  filter(n()>1)
nrow(rv)

#Question 10
rv$id

#Question 11
hos%>%
  group_by(id,Specialty)%>%
  filter(n()>1)

#Question 12
hos%>%
  group_by(Sex) %>%
  summarise(Age=median(Age,na.rm=TRUE))

#Question 13
sum(hos$AmountBalance,na.rm=TRUE)

#Question 14
hos%>%
  filter(Procedure=="Consultation")%>%
  group_by(Procedure)%>%
  summarise(SUM=sum(AmountReceived,na.rm=TRUE))

#Question 15
x<-cor(hos$Age[!is.na(hos$Age)],hos$TotalCharges[!is.na(hos$TotalCharges)])