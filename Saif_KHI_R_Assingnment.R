#Question 1)
library("ggplot2")
library("lubridate")
library("dplyr")
library(tidyr)
library("DataCombine")
hospitaldata <- read.csv("hospitaldata.csv")
names(hospitaldata)<-gsub("..","",names(hospitaldata),fixed = TRUE)
names(hospitaldata)<-gsub(".","_",names(hospitaldata),fixed = TRUE)
View(hospitaldata)

#removing character M from age i.e 28M
hospitaldata$Age <- as.numeric(gsub("[^0-9]",'',hospitaldata$Age))

#Question 2)
hospitaldata$Date <- as.Date(strptime(hospitaldata$Date, "%a, %B %d, %Y"))

Max_day <-  hospitaldata %>%
  mutate(Day=weekdays(hospitaldata$Date),label=TRUE) %>%
  group_by(Day) %>%
  summarize(visits=length(Day)) %>%
  print
ggplot(Max_day,aes(x=Day,y=visits))+geom_bar(stat="identity",fill="slateblue")+ggtitle("Visits per Weekday")+labs(x="Day",y="Visits")

#Question 3)
mean(hospitaldata$Age, na.rm = TRUE)

#Question 4)
child <- filter(hospitaldata, Age > 1 & Age < 13) %>%
  select(-(Date:Time)) %>%
  select(-(Sex:Next_Apt)) %>%
  count() %>%
  print
#Question 5)

hospitaldata$Sex <- gsub("f","F",hospitaldata$Sex)
hospitaldata$Sex<-gsub("\\s|-",NA,hospitaldata$Sex)
qplot(data=hospitaldata, Sex, fill=Procedure)+ggtitle("Procedure vs Gender")+labs(x='Gender',y='Procedure')

#Question 6)
qplot(data=hospitaldata, fill=ConsultingDoctor, as.numeric(TotalCharges))+ggtitle("Highest Salary")+labs(x='TotalCharges',y='ConsultingDoctor')

#Question 7)
qplot(data=hospitaldata, as.numeric(TotalCharges), fill=Procedure)+ggtitle("Procedure")+labs(x='TotalCharges',y='Procedure')

#Question 8)
hour_and_visits <-  hospitaldata %>%
  select(Time) %>%
  mutate(Hour = hour(hm(format(strptime(hospitaldata$Time, "%I:%M %p"), "%H:%M")))) %>%
  group_by(Hour) %>%
  summarize(visits=length(Hour)) %>%
  arrange(desc(visits)) %>%
  print     # printing 13 the highest hour, i.e. actaully 1 AM/PM in 12 hour format

#Question 9)


#Question 10)
visitor_repeated <- select(hospitaldata,id) %>%
  group_by(id) %>%
  summarise(visits=length(id)) %>%
  arrange(desc(visits)) %>%
  filter(visits > 1) %>%
  print 

#Question 11)

visitor_repeated <- select(hospitaldata,id) %>%
  group_by(id) %>%
  summarise(visits=length(id)) %>%
  arrange(desc(visits)) %>%
  filter(visits > 1) %>%
  print 

#Question 12)

hospitaldata %>% 
  count(id, Procedure) %>%
  slice(which(n>1))%>%
  filter(!is.na(Procedure))%>%
  select(id,Procedure)

#Question 13)
Age_median <- hospitaldata %>% 
  select(Sex,Age) %>%
  group_by(Sex) %>%
  summarise(median(Age, na.rm = TRUE)) %>%
  print

#Question 14)
hospitaldata$TotalCharges<-as.numeric(as.character(hospitaldata$TotalCharges))
sum_of_Balance <- sum(hospitaldata$TotalCharges, na.rm = TRUE)
print(sum_of_Balance)

#Question 15)
hospitaldata$TotalCharges<-as.numeric(as.character(hospitaldata$TotalCharges))
consult_amount <- hospitaldata%>%
  select(Procedure,TotalCharges) %>%
  group_by(Procedure) %>%
  filter(Procedure == 'Consultation') %>%
  summarise(sum(TotalCharges,na.rm=TRUE)) %>%
  print

#Question 16)
data <- hospitaldata %>%
  select(Age,TotalCharges) %>%
  filter(!is.na(Age),!is.na(TotalCharges))
cor(data$Age,data$TotalCharges)

#Question 17)
Number_of_visits_by_age <- hospitaldata %>%
  select(id,Age) %>%
  group_by(Age) %>%
  summarize(visits=length(Age)) %>%
  arrange(desc(visits)) %>%
  filter(!is.na(Age)) %>%
  print

ggplot(data=Number_of_visits_by_age,aes(x=as.numeric(Age),y=visits))+geom_bar(stat='identity',fill='slate blue')+ggtitle("Number Of Visits By Age")+labs(x='Age',y='Visits')


#Question 18)

sum(hospitaldata$TotalCharges[hospitaldata$Procedure=="X Ray" | hospitaldata$Procedure=="Scalling"],na.rm = TRUE)


write.csv("~/hospitaldata.csv", row.names = FALSE)
View(hospitaldata)
