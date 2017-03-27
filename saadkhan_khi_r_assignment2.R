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
hos <-
  hos %>%
  mutate(AmountBalance= ifelse ( str_detect(AmountBalance,","),
                       gsub(",", "", AmountBalance),
                       AmountBalance))


hos$Age <-as.numeric(hos$Age)
hos[is.na(hos$Age),"Age"]<-0
hos$AmountBalance <-as.numeric(hos$AmountBalance)
hos$TotalCharges <-as.numeric(hos$TotalCharges) 
hos[is.na(hos$TotalCharges),"TotalCharges"]<-0


hos<-
  hos %>%
  mutate(Sex=ifelse(Sex=="" | Sex =="-",NA,Sex))
hos$Sex<-toupper(hos$Sex)
hos$NextApt <-as.Date(hos$NextApt,"%m/%d/%Y")

#Question 02
x<-table(weekdays(hos$Date))
names(which.max(x))

#Question 03
mean(hos$Age)

#Question 04
rows <-
  hos %>%
  filter(Age>=1,Age<=12) %>%
  as.data.frame()  
nrow(rows)

#Question 05
hos%>%
  count(Sex, Procedure) %>%
  slice(which.max(n))

#Question 06
hos%>%
  group_by(ConsultingDoctor) %>%
  summarise(TotalCharges=sum(TotalCharges,na.rm=TRUE)) %>%
  filter(TotalCharges==max(TotalCharges))

#Question 07
hos%>%
  group_by(Procedure) %>%
  summarise(TotalCharges=sum(TotalCharges,na.rm=TRUE)) %>%
  filter(TotalCharges==max(TotalCharges))

#Question 08
#install.packages("lubridate")
library(lubridate)

hos$Time <- as.POSIXlt(strptime(hos$Time,"%I:%M%p"))
names(which.max(table(hour(hos$Time))))

#Question 09
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=6 & hour(Time)<12,"Morning",TimeBracket))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=12 & hour(Time)<16,"Afternoon",TimeBracket))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=16 & hour(Time)<19,"Evening",TimeBracket))
hos<-
  hos%>%
  mutate(TimeBracket=ifelse(hour(Time)>=19 & hour(Time)<6,"Night",TimeBracket))

hos$Time<-as.character(hos$Time)
#Question 10

hos$id <- as.numeric(hos$id)
length(table(hos$id)[table(hos$id)>1])
#Question 11
names(table(hos$id)[table(hos$id)>1])

#Question 12
hos %>% 
  select(id, Procedure) %>%
  group_by(id,Procedure) %>%
  tally()%>%
  filter(n>1)%>%
  select(id,Procedure)

  
  




#Question 13
hos %>%
  group_by(Sex)%>%
  summarise(medianAge=median(Age))

#Question 14

sum(hos$AmountBalance,na.rm = TRUE)

#Question 15
hos%>%
  filter(Procedure=="Consultation")%>%
  group_by(Procedure)%>%
  summarise(SUM=sum(AmountReceived,na.rm=TRUE))

#Question 16
cor(hos$Age,hos$TotalCharges)

#Question 17
names(which.max(table(hos$Age[hos$Age!=0])))

#Question 18
sum(hos$AmountReceived[hos$Procedure=="X Ray" | hos$Procedure=="Scalling"])

write.csv(hos, file = "C:/Users/Saad.Khan/Documents/DIH-Assignment_02/saadkhan_khi_r_assignment2/R-Assignment-2/clean_hospitaldata.csv")

