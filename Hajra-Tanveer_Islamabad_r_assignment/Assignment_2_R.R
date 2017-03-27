library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
hospitaldata <- read_csv("~/Assignments/R_Assignments/Hajra-Tanveer_Islamabad_r_assignment/hospitaldata.csv")
hospital_tbl <- tbl_df(hospitaldata)
View(hospital_tbl)

#Part 1: Remove "."
names(hospital_tbl) <- gsub(".", " ", names(hospital_tbl), fixed = TRUE)

#Part 2: Which day of the week is expected to have most visits?

hospital_tbl <- hospital_tbl %>%
  separate(Date, into =c("Day", "Date"), ",", extra = "merge")

#Part 3: What is the average age of patients?
#we will replace all month olds and - with NA 
hospital_tbl[hospital_tbl == "-"] <- NA 
#hospital_tbl$Age <- gsub(x = hospital_tbl$Age, pattern = "M | -", replacement = NA)
#why does this ^ not work?!!!
hospital_tbl$Age
Filter_infants <- hospital_tbl%>%
  filter(!grepl('M', Age))
Filter_infants
Non_infants <- as.numeric(Filter_infants$Age)
avg_age <- mean(Non_infants, na.rm = TRUE)
avg_age


#Part4:How many children were entertained? (Make a Bracket of Age from 1-12)
Filter_infants$Age <- as.numeric(Filter_infants$Age)
Filter_infants%>%
  filter(Age <= 12)%>%
  count



#Part5: Which gender type had what kind of procedure in abundance? 
#i.e. Female visit mostly because of Gynae Problem
hospital_tbl %>%
  group_by(Sex, Specialty)%>%
  summarise(count = n())%>%
  arrange(desc(count))


#Part6: Which Doctor is earning highest?
hospital_tbl %>%
  group_by(`Consulting  Doctor`)%>%
  summarise(richest = sum(as.numeric(`Total  Charges`), na.rm = TRUE))%>%
  arrange(desc(richest))

#Part7:Which procedure type earns more money?
hospital_tbl %>%
  group_by(Procedure)%>%
  summarise(richest = sum(as.numeric(`Total  Charges`), na.rm = TRUE))%>%
  arrange(desc(richest))

#Part8:Which time of the day has highest frequency of visits by hour?
Time_tbl<- hospital_tbl
Time_tbl$Time<-hour(strptime(Time_tbl$Time,"%H"))
typeof(Time_tbl$Time)
Time_tbl%>%
  group_by(Time)%>%
  summarise(count = n())
#problem: we dont know if 1 AM or PM


#Part9: Create a bracket of time by Morning, Afternoon, Evening, 
#Night (6am – 12pm – Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 
#7pm – 6 am, Night).


#Part10:How many patients are repeated visitors?
hospital_tbl %>%
  group_by(id)%>%
  summarise(count=n())%>%
  filter(count >1)%>%
  count


#Part11:Give us the id of repeated visitors
hospital_tbl %>%
  group_by(id)%>%
  summarise(count=n())%>%
  filter(count >1)


#Part12: Which patients visited again for the same problem?
hospital_tbl %>%
  group_by(id,Specialty)%>%
  summarise(count=n())%>%
  filter(count >1)


#Part13:What is the median age for Females and Males?
females_ages<- Filter_infants%>%
  filter(Sex == "F")
median_females <- median(females_ages$Age, na.rm = TRUE)
median_females
males_ages<- Filter_infants%>%
  filter(Sex == "M")
median_males <- median(males_ages$Age, na.rm = TRUE)
median_males

#Part14:What is the total amount in balance?
total_balance <- hospital_tbl
total_balance$`Amount  Balance` <- as.numeric(sub(",", "", total_balance$`Amount  Balance`))
sum(total_balance$`Amount  Balance`, na.rm = TRUE)

#Part15:How much money was made by Procedure Type “Consultation”?
hospital_tbl%>%
  group_by(Procedure)%>%
  summarise(total = sum(as.numeric(`Total  Charges`), na.rm = TRUE))%>%
  filter(Procedure == "Consultation")
  
#Part16:Is there a relation between Age and Total Charges paid?
plot(Filter_infants$Age, Filter_infants$`Total  Charges`)
cor.test(as.numeric(Filter_infants$Age), as.numeric(Filter_infants$`Total  Charges`), na.action=na.exclude)
#no correlation


#Part17:Which Age group had highest number of visits?
Filter_infants$Age <- as.numeric(Filter_infants$Age)
Filter_infants$Age <- round(Filter_infants$Age,-1)
Filter_infants$Age
Filter_infants%>%
  group_by(Age)%>%
  summarise(count_age_group = n())%>%
  arrange(desc(count_age_group))%>%
  print
#ppl n their 30s had highest no. of visits


#Part18:What is the total cost earned by Procedure Type X Ray and
#Scalling together?
Filter_infants%>%
  group_by(Procedure)%>%
  summarise(total_procedure= sum(as.numeric(`Total  Charges`),na.rm=TRUE))%>%
  filter(Procedure == "X Ray" | Procedure == "Scalling")%>%
  summarise(sum(total_procedure),na.rm=TRUE)%>%
  print
  
write.csv(hospital_tbl,"~/Assignments/R_Assignments/Hajra-Tanveer_Islamabad_r_assignment/CleanDataset.csv")


































