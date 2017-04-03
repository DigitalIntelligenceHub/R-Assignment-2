library(dplyr)
library(tidyr)
library(lubridate)

#Question 1

data <-read.csv("C:/Users/saba.ali/Desktop/Saba/hospitaldata.csv", strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F, header = TRUE)

data <- rename(data, "Consulting_Doctor"=`Consulting..Doctor`, "Total_Charges"=`Total..Charges`, "Amount_Received"=`Amount..Received.`, "Amount_Balance"=`Amount..Balance`, "Amount_Received_By"=`Amount.Received.By`, "Amount_in_Hospital"=`Amount.in.Hospital`, "Receptionist_Name"=`Receptionist..Name`, "Next_Apt"=`Next.Apt`)


#Question 2


data$Date <- as.Date(strptime(data$Date, "%a, %B %d, %Y"))
weekdays(as.Date(data$Date[which(table(data$Date) == max(table(data$Date)))]))


#Question 3


data$Age <- as.numeric(gsub("[^0-9]",'',data$Age))
mean(data$Age, na.rm = TRUE)


#Question 4


x <- which(data$Age <=12)
length(x)


#Question 5


data$Sex <- toupper(data$Sex)
m <- data%>%
  group_by(Sex,Procedure)%>%
  filter(Sex == 'M')%>%
  tally(sort=T)
m

f <- data%>%
  group_by(Sex,Procedure)%>%
  filter(Sex == 'F')%>%
  tally(sort=T)
f


#Question 6


data$Total_Charges <- as.numeric(data$Total_Charges)

data%>%
  group_by(Consulting_Doctor)%>%
  summarize(sum=sum(Total_Charges))%>%
  arrange(desc(sum))


#Question 7


data%>%
  group_by(Procedure)%>%
  summarize(sum=sum(Total_Charges))%>%
  arrange(desc(sum))


#Question 8


time = hour(strptime(data$Time,'%I:%M %p'))
which.max(table(time))


#Question 9

data %>%
  mutate(time_bracket = ifelse(time >= 6 & time <= 12, "Morning",
       ifelse(time > 12 & time <= 16, "Afternoon",
              ifelse(time > 16 & time <= 19, "Evening",
                     ifelse(time > 19, "Night", NA)))))
  

#Question 10


a <- data%>%
  group_by(id)%>%
  summarize(l = n())%>%
  filter(l>1)
count(a)


#Question 11


a <- data%>%
  group_by(id)%>%
  summarize(l = n())%>%
  filter(l>1)
a


#Question 12


a <- data%>%
  group_by(id,Procedure)%>%
  summarize(l = n())%>%
  filter(l>1)
a


#Question 13


f_age <- data%>%
  filter(Sex == 'F')%>%
  select(Age)%>%
  summarize(median = median(Age,na.rm=T))
f_age

m_age <- data%>%
  filter(Sex == 'M')%>%
  select(Age)%>%
  summarize(median = median(Age,na.rm=T))
m_age


#Question 14

  
data$Amount_Balance <- gsub(",", "", data$Amount_Balance, fixed = TRUE)
data$Amount_Balance <- gsub(".00", "", data$Amount_Balance, fixed = TRUE)
data$Amount_Balance <- as.integer(data$Amount_Balance, na.rm=T)

total <- data%>%
  select(Amount_Balance)%>%
  summarize(sum = sum(Amount_Balance, na.rm=T))
total


#Question 15


money <- data%>%
  filter(Procedure == 'Consultation')%>%
  select(Total_Charges)%>%
  summarize(sum = sum(Total_Charges,na.rm=T))
money


#Question 16


data %>%
  select(Age,Total_Charges)%>%
  filter(!is.na(Age),!is.na(Total_Charges))%>%
  summarize(cor = cor(Age,Total_Charges))


#Question 17


hist(data$Age)


#Question 18


money <- data%>%
  filter(Procedure == 'X Ray'| Procedure == 'Scalling')%>%
  select(Total_Charges)%>%
  summarize(sum = sum(Total_Charges,na.rm=T))
money


write.csv(data,"C:/Users/saba.ali/Desktop/SabaMohammadAli_Khi_r_assignment2/CleanData.csv")

