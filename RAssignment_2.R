library(tidyr)
library(lubridate)
library(dplyr)
library(plyr)

#hospitaldata <- read.csv("D://DIH//Assignments//FarooqUrRehman_KCI_R_Assignment2//hospitaldata.csv")

## Answer 1
names(hospitaldata) <- gsub("\\.", "", names(hospitaldata))

## Answer 2
day_count <- count(wday(mdy(hospitaldata$Date),label=TRUE))
day_max <- max(day_count$freq)
select(filter(day_count, day_count$freq == day_max), x)

## Answer 3
hospitaldata <- hospitaldata%>%
  mutate(Age=ifelse(Age=='-', NA, Age))%>%
  mutate(Age=ifelse(grepl("M",Age),parse_number(Age)/12,Age))

print(mean(as.numeric(hospitaldata$Age),na.rm = TRUE))

##Answer 4
hospitaldata <- hospitaldata %>%
  mutate(Age = as.numeric(Age)) 

children_data <- hospitaldata %>%
  filter(Age >= 1, Age <=12) %>%
  select()%>%
  count() %>%
  sum() %>%
  print
  
# Answer 5
gen_proc<-hospitaldata %>%
  mutate(Sex = ifelse(Sex == 'M' | Sex =='m', 'M',ifelse(Sex == 'f' | Sex =='F','F',NA))) %>%
  filter(!is.na(Sex), !is.na(Procedure), Sex == 'F') %>%
  select(Procedure) %>%
  count()
gen_proc_1 <- max(gen_proc$freq)
select(filter(gen_proc,gen_proc$freq == gen_proc_1),Procedure,freq)

gen_proc<-hospitaldata %>%
  mutate(Sex = ifelse(Sex == 'M' | Sex =='m', 'M',ifelse(Sex == 'f' | Sex =='F','F',NA))) %>%
  filter(!is.na(Sex), !is.na(Procedure), Sex == 'M') %>%
  select(Procedure) %>%
  count()
gen_proc_1 <- max(gen_proc$freq)
select(filter(gen_proc,gen_proc$freq == gen_proc_1),Procedure,freq)


# Answer 6
doctors_earnings <- hospitaldata %>%
  filter(!is.na(AmountReceived),grepl("^Dr",ConsultingDoctor)) %>%
  group_by(ConsultingDoctor) %>%
  summarise(sum(AmountReceived))

max_earning <- max(doctors_earnings$`sum(AmountReceived)`)
print (filter(doctors_earnings, doctors_earnings$`sum(AmountReceived)` == max_earning))
                   
# Answer 7
proc_earnings <- hospitaldata %>%
  filter(!is.na(AmountReceived),!is.na(Procedure)) %>%
  group_by(Procedure) %>%
  summarise(sum(AmountReceived))

max_proc_earning <- max(proc_earnings$`sum(AmountReceived)`)
print (filter(proc_earnings, proc_earnings$`sum(AmountReceived)` == max_proc_earning))

# Answer 8

cons_time <- hospitaldata %>%
  filter(!is.na(Time),!grepl("-",Time)) %>%
  mutate(am_pm=ifelse(grepl("AM",Time),"AM",ifelse(grepl("PM",Time),"PM","AM"))) %>%
  group_by(cons_time=paste(as.character(hour(hm(Time))),am_pm, sep=":00 ")) %>%
  tally()

cons_max_count <- max(cons_time$n)
print (filter(cons_time, cons_time$n == cons_max_count))

# Answer 9
Time_Bracket <- hospitaldata %>%
  filter(!is.na(Time),!grepl("-",Time)) %>%
  mutate(CHour = as.numeric(hour(hm(Time)))) %>%
  mutate(CHour = ifelse(grepl("AM",Time) & CHour == 12,0,ifelse(grepl("PM",Time) & CHour<12,12+CHour,CHour))) %>%
  mutate(TimeBracket = ifelse(CHour >=6 & CHour<12,"Morning",ifelse(CHour >=12 & CHour<16,"Afternoon",ifelse(CHour >=16 & CHour<19,"Evening","Night"))))%>%
  print(Time,TimeBracket)

# Answer 10 and 11
Repeated_Visitors <- hospitaldata %>%
  filter(grepl("^Dr",ConsultingDoctor)) %>%  
  group_by(id) %>%
  tally()%>%
  filter(n>1) %>%
  print 

# Answer 12
Rep_with_same_problem <- hospitaldata %>%
  filter(grepl("^Dr",ConsultingDoctor)) %>%
  group_by(id, Procedure) %>%
  tally()%>%
  filter(n>1) %>%
  print

# Answer 13
median_age <- hospitaldata %>%
  filter(!is.na(Sex), !grepl("-",Sex), !is.na(Age)) %>%
  group_by(Sex) %>%
  summarise_each(funs(median),Age) %>%
  print

# Answer 14
balance_amount <-hospitaldata %>%
  filter(!is.na(AmountBalance), !grepl("-",AmountBalance)) %>%
  mutate(AmountBalance=parse_number(AmountBalance)) %>%
  summarise(sum(AmountBalance)) %>%
  print

# Answer 15
Consultation_amount <-hospitaldata %>%
  filter(!is.na(AmountReceived), Procedure=='Consultation') %>%
  summarise(sum(AmountReceived)) %>%
  print

# Answer 15
Consultation_amount <-hospitaldata %>%
  filter(!is.na(AmountReceived), Procedure=='Consultation') %>%
  summarise(sum(AmountReceived)) %>%
  print
  
# Answer 16
Age_n_Charges_Relation <- hospitaldata %>%
  filter(!is.na(Age), !is.na(TotalCharges), !grepl("Cancelled",TotalCharges)) %>%
  mutate(TotalCharges = parse_number(TotalCharges)) %>%
  select(TotalCharges, Age) %>%
  cor() %>%
  print
  
# Answer 17
AgeGroup_Visits <-hospitaldata %>%
  filter(!is.na(Age)) %>%
  mutate(Age_Group=ifelse(Age<=10,'0 - 10 yrs',ifelse(Age>10 & Age <= 30,'11 - 30 yrs',ifelse(Age>31 & Age <= 50,'31 - 50 yrs','51 yrs and onwards'))))%>%
  group_by(Age_Group)%>%
  tally() %>%
  print

# Answer 18
XRay_Scalling_Cost <-hospitaldata %>%
  filter(!is.na(AmountReceived), grepl("X Ray",Procedure) | grepl("Scalling",Procedure)) %>%
  summarise(sum(AmountReceived))%>%
  print

# 2nd answer
XRay_Scalling_Cost <-hospitaldata %>%
  filter(!is.na(AmountReceived), Procedure=="X Ray" | Procedure == "Scalling") %>%
  summarise(sum(AmountReceived)) %>%
  print

