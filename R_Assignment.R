library(readr)
hpddata <- read_csv("E:/DIH/R Assignment/Assignment - 22-03-2017/hospitaldata.csv")
# for rename column first should be install deplyr package
library("dplyr")
library(tidyr)
#Please remove the dots in the names, so it may become easier for you to work through it.
#Q.1
names(hpddata) <- gsub("\\.", "", names(hpddata))
View(hpddata)
# Data Cleaning
hpddata<-  hpddata%>%
  mutate(Age=ifelse(Age=="-", NA,Age))

hpddata<-hpddata%>%
  mutate(Age = ifelse(grepl("M", Age), parse_number(Age)/12, ifelse(grepl("-", Age), "",Age)))

hpddata<-  hpddata%>%
  mutate(Sex=replace(Sex,Sex=='f','F'))
hpddata<-  hpddata%>%
  mutate(Sex=replace(Sex,Sex=="-", NA))
hpddata<-  hpddata%>%
  mutate(AmountBalance=replace(AmountBalance,AmountBalance=="-", NA))
hpddata<-mutate(hpddata, Time=format(strptime(hpddata$Time, "%I:%M %p"), format="%H:%M"))
#Q.2

Maximum_days<-
  hpddata%>%
  separate(Date,c("Day"))%>%
  group_by(Day)%>%
  summarize(count_no =n())%>%
  filter(count_no==max(count_no))

View(Maximum_days)

  #Q.3
  
  Avg_age_Patients<-hpddata%>%
    filter(!is.na(Age))%>% 
    summarize(cal_avg = mean(parse_number(Age)))
  View(Avg_age_Patients)
  
  # Q.4
  
   children_visits<- hpddata%>%
  filter(parse_number(Age)>0 , parse_number(Age)<=12)%>%
    summarize(total_children= n())
   View(children_visits)
  
  # Q.5
  
  gender_type<-hpddata%>%
    filter(!is.na(Sex))%>%
    group_by(Sex,Procedure)%>%
    summarize(total_gender= n())%>%
    filter(total_gender==max(total_gender))
  View(gender_type)
  
  # Q.6
  
  high_paid_con_Var<- hpddata%>%
    filter(ConsultingDoctor!='Nursing Staff')%>%
    filter(!is.na(AmountReceived))%>%
    group_by(ConsultingDoctor)%>%
    summarize(high_paid_con = sum(AmountReceived))%>%
    filter(high_paid_con == max(high_paid_con))
  View(high_paid_con_Var)
  
  # Q.7
  
  high_paid_prc_type_Var <-hpddata%>%
    filter(!is.na(AmountReceived))%>%
    group_by(Procedure)%>%
    summarize(high_paid_prc_type = sum(AmountReceived))%>%
    filter(high_paid_prc_type==max(high_paid_prc_type))
  
    View(high_paid_prc_type_Var)

    
  #Q.8
    
    max_freq_time<-hpddata%>%
      filter(!is.na(Time),Time!='-')%>%
      group_by(Time)%>%
      summarize(Total_row_count=n())%>%
      filter(Total_row_count==max(Total_row_count))
    View(max_freq_time)
    
    #Q.9
  slot_hour<-hpddata%>%
    filter(!is.na(Time),Time!='-')%>%
    separate(Time,c("slot_hour"))%>%
    mutate(time_bracket = ifelse(parse_number(slot_hour)>=6&parse_number(slot_hour)<12,'Morning',
           ifelse(parse_number(slot_hour)>=12&parse_number(slot_hour)<16,'Afternoon',
                  ifelse(parse_number(slot_hour)>=16&parse_number(slot_hour)<20,'Evening',
                         ifelse(parse_number(slot_hour)>=20&parse_number(slot_hour)<=24,'Night','Night')))))
  View(slot_hour)  

   #Q.10
  
  repeated_patients<-hpddata%>%
    group_by(id)%>%
    summarize(repeated_count =n())%>%
    filter(repeated_count>1)%>%
    mutate(total_repeated=1)%>%
    group_by(total_repeated)%>% 
    summarize(sum(total_repeated))
  View(repeated_patients)
  #Q.11
  repeated_patients<-hpddata%>%
    group_by(id)%>%
    summarize(repeated_count =n())%>%
    filter(repeated_count>1)%>%
    select(id)
  #Q.12
  repeated_patients<-hpddata%>%
    group_by(id,Procedure)%>%
    summarize(repeated_count =n())%>%
    filter(repeated_count>1)%>%
    group_by(id,Procedure)
  View(repeated_patients)
  #Q.13
  gender_median_age <-hpddata%>%
    filter(!is.na(Age),Age!='-')%>%
    group_by(Sex)%>%
    mutate(Age=parse_number(Age))%>%
    summarize(median_age=median(parse_number(Age)))
  View(gender_median_age)
  
      
  #    Q.14
  total_amount_balance<-hpddata%>%
    filter(!is.na(AmountBalance))%>%
    summarize(Total_balance_amount=sum(parse_number(AmountBalance)))
  View(total_amount_balance)
  #Q.15
  total_cost_Consultation<-hpddata%>%
    filter(Procedure=="Consultation",!is.na(AmountReceived))%>%
    summarize(Total_cost=sum(parse_number(AmountReceived)))      
  View(total_cost_Consultation)
  #Q.17
  age_group_data<-hpddata%>%
    filter(!is.na(Age))%>%
    mutate(Age_group = ifelse(parse_number(Age)<=12,'Children','Elder') )%>%
    group_by(Age_group)%>%
    summarize(no_visits = n())%>%
    filter(no_visits==max(no_visits))
  View(age_group_data)
  #Q.18
  total_xray_scalling <-hpddata%>%
    filter(Procedure=='X Ray'|Procedure=='Scalling')%>%
    summarize(Total_amount= sum(AmountReceived))
  View(total_xray_scalling)
  