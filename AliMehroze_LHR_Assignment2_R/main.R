library("dplyr")
library("stringr")
library("tidyr")
library("lubridate")


#This function removes dots from column names 
my_dot_cleanse <- function(df){
  names(df)<-gsub("\\.","",names(df))
  return(df)
  
}


data_buff<-read.csv("hospitaldata.csv")
names(data_buff)

#Q1: Removing dots
data_buff<-my_dot_cleanse(data_buff)
names(data_buff)

#Q2: Day for most visits?
#class(data_buff$Date)
data_buff$Date<-as.character(data_buff$Date)


data_buff<-separate(data_buff,Date,c("Days","Date","Year"),sep=",", remove = TRUE)
data_buff$Days<-as.character(data_buff$Days)
table(data_buff$Days)
#Answer; Monday has the highest frequency

#data_buff$Date<-gsub("\\ ","",data_buff$Date)

#data_buff$Date<-gsub("\\ ","",data_buff$Date)

#Q3 Average age
data_buff$Age<-as.numeric(as.character(data_buff$Age))
#class(data_buff$Age)
mean(data_buff$Age, na.rm=TRUE)

#Q4 Count Children
temp<-which(data_buff$Age>=1 & data_buff$Age<=12)
length(temp)

#Q5 
data_buff$Sex<-tolower(as.character(data_buff$Sex))
data_buff$Sex
data_buff$Sex[data_buff$Sex=="-" | data_buff$Sex==""]<-NA

unique(data_buff$Sex)

check_group <- data_buff %>%
  group_by(Sex,Procedure) %>%
  summarize(count=n())


View(check_group)
#check_group$count==max(check_group$count)

check_group
#Answer: The grouped up data shows that maximum number of Females and Males visited for "Consultation"

#Q6 Highest incomed doctor
class(data_buff$ConsultingDoctor)

#Convert Factor to Numeric
data_buff$ConsultingDoctor<-as.character(data_buff$ConsultingDoctor)
unique(data_buff$ConsultingDoctor)


data_buff$TotalCharges<-as.numeric(as.character(data_buff$TotalCharges))
data_buff$TotalCharges

highest_paid<- data_buff%>%
  group_by(ConsultingDoctor) %>%
  summarize(total=sum(TotalCharges))


View(highest_paid)
#Evaluate the index where highest total charges exist
indexer<- which(highest_paid$total == max(highest_paid$total, na.rm = TRUE), arr.ind = TRUE)
#Reference the index
highest_paid[indexer,"ConsultingDoctor"]


#Q7 Highest paid procedure?

#This uses the same algorithm as the above answer, except for the group_by call on 
#"Procedure" this time
highest_paid2<- data_buff%>%
  group_by(Procedure) %>%
  summarize(total=sum(TotalCharges))

View(highest_paid2)

indexer<- which(highest_paid2$total == max(highest_paid2$total, na.rm = TRUE), arr.ind = TRUE)
highest_paid2[indexer,"Procedure"]

#Q8. Which time of the day has highest frequency of visits by hour? 
class(data_buff$Time)
data_buff$Time<-as.character(data_buff$Time)
data_buff$Time[data_buff$Time=="-"]<-""
data_buff$Time[data_buff$Time==""]<-NA
unique(data_buff$Time)

extract_time<-data_buff$Time

#the following line is copied from:
# http://stackoverflow.com/questions/38228650/convert-12-hour-to-24-hour-format-in-r

extract_time<-strptime(extract_time,"%I:%M %p")
get_hours<-hour(extract_time)

#get_hours<-table(get_hours)
#get_hours

# temp5<-get_hours%>%
#   group_by(get_hours)%>%
#   summarize(total=sum(get_hours))
  

# twelve_hour<-hm(extract_time)
# x<-get_hours[which(get_hours == max(get_hours,na.rm=TRUE),arr.ind = TRUE)]
# x

actual_mode <- table(get_hours)
names(actual_mode)[actual_mode == max(actual_mode)]

#Hence the hour 13:00 is the most visited hour of the day

#Q10 Repeated visitor count
class(data_buff$id)
repeat_visitor <- data_buff %>%
  group_by(id) %>%
  summarize(count=n())
repeat_visitor <- repeat_visitor$id[(which(repeat_visitor$count >1))]
length(repeat_visitor)


#Q11 Repeated visitor count
repeat_visitor

#Q12 Which patients visited again for the same problem? 
same_problem <- data_buff %>%
  group_by(id,Procedure) %>%
  summarize(count=n())
same_problem
same_problem <- same_problem$id[(which(same_problem$count >1))]
same_problem

#Q13 Median for Females and Males

# median age F
indexer_f<-which(data_buff$Sex=='f',arr.ind =TRUE) 
median(as.numeric(as.character(data_buff$Age[indexer_f])),na.rm = TRUE)

# median age M
indexer_m<-which(data_buff$Sex=='m',arr.ind =TRUE) 
median(as.numeric(as.character(data_buff$Age[indexer_m])),na.rm = TRUE)

class(data_buff$AmountBalance)
data_buff$AmountBalance<-as.character(data_buff$AmountBalance)
data_buff$AmountBalance<-gsub(",","", data_buff$AmountBalance)
#Q14 What is the total amount in balance? 
sum(as.numeric(data_buff$AmountBalance), na.rm=TRUE)

#Q15 How much money was made by Procedure Type "Consultation"? 
indexer<- which(data_buff$Procedure=='Consultation', arr.ind=TRUE)
sum(data_buff$TotalCharges[indexer], na.rm=TRUE)

#Q16 Is there a relation between Age and Total Charges paid?
cor.test(data_buff$Age, data_buff$TotalCharges)
#Ans: There is no significant correlation, as val

#Q18. What is the total cost earned by Procedure Type X Ray and Scalling together
indexer<- which(data_buff$Procedure=='X Ray' | data_buff$Procedure=='Scalling' , arr.ind=TRUE)
sum(data_buff$TotalCharges[indexer], na.rm=TRUE)

