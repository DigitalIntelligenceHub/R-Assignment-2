library("dplyr")
library("tidyr")
library("lubridate")
library("readr")
hsptldata <- read_csv("D:/DIH/Assigments/R-Assignment-2/ahsentaqi_isb_r_assignment2/hospitaldata.csv" )
View(hsptldata)
hsptldf <- tbl_df(hsptldata)
rm(hsptldata)

install.packages("ggplot2")
library("ggplot2")

## Question 1 

names(hsptldf) <- gsub(".", "", names(hsptldf), fixed = TRUE)
names(hsptldf)

## Question 2 

#separating weekdays, day of month and year column to get the weedays in separate column
hsptldf <-  separate( hsptldf, Date , into = c("day_only" , "month_dayOfMonth", "year"), sep = ",")

#counting weekdays to get total visits on a each day
days_count <- count(hsptldf, day_only)


#getting num of max visits
qplot(data = hsptldf , hsptldf$day_only)
max_visits <- max(days_count$n)
print(days_count)
print(max_visits)


##  Question # 3 

 
# checking the type of column 'Age'
typeof(hsptldf$Age)

# converting type of 'Age' from Character to Integer and calculating Average Age of patients
average_age <- mean(as.integer(hsptldf$Age) , na.rm = TRUE)
print(average_age)
hsptltest <- read_csv("D:/DIH/Assigments/R-Assignment-2/ahsentaqi_isb_r_assignment2/hospitaldata.csv",  na.strings=c("","-"," ", "NA"))

 ## Question # 4
child_count <- filter(hsptldf,   Age <= 12  )
count(child_count)
## Question # 5

hsptldf$Sex <- toupper(hsptldf$Sex)
hsptldf$Sex <- hsptldf$Sex.na.strings("", "-"," ", "NA")
female_patient <- filter(hsptldf, Sex == "F")
qplot(data = female_patient, female_patient$Specialty)
male_patient <- filter(hsptldf, Sex == "M" )
qplot(data = male_patient, male_patient$Specialty)

## Question 6

unique(hsptldf$ConsultingDoctor)
doc_fee <- select(hsptldf, ConsultingDoctor, TotalCharges )
doc_fee$TotalCharges <- as.integer(doc_fee$TotalCharges)
doc_group <- group_by(doc_fee, ConsultingDoctor )
summary <- summarise(doc_group, count = n(), total_pay = sum(TotalCharges, na.rm = TRUE))
highest_earning <- arrange( summary, desc(total_pay))
max(summary$total_pay, na.rm = TRUE)

## Question # 7

unique(hsptldf$Procedure)
hsptldf$TotalCharges <- as.integer(hsptldf$TotalCharges)
procd_table <- select(hsptldf, Procedure, TotalCharges)
procd_group <- group_by(procd_table, Procedure)
prod_sumry <- summarise(procd_group, total_earning = sum(TotalCharges , na.rm = TRUE))
highest_earning_procedure <- arrange(prod_sumry, desc(total_earning))
highest_earning_procedure

## Question # 8  

hsptldf$Time <-  gsub( "-", NA, hsptldf$Time)
hsptldf<-  mutate( hsptldf, hour = hour(hm( format(strptime( hsptldf$Time, "%I:%M %p"), "%H:%M"))))

hour_grp <- group_by(hsptldf, hour)
hour_summary <- summarise(hour_grp, count = n())
qplot(data = hour_summary,factor( hour_summary$hour), hour_summary$count) + geom_bar(stat = "identity")

hour_summary <- arrange(hour_summary, desc(count))
hour_summary
 
## QUestion # 9  
 
label = c("Morning","Afternoon", "Evening", "Night" )
time_frame =  c(6, 12, 16, 19, 23)

hsptldf <-   mutate( hsptldf , time_bracket = cut(hsptldf$hour, time_frame, label))
hsptldf

## Question 10

repeated_vis <-  group_by(hsptldf , id)
vis_sumry <- summarise(repeated_vis , count= n())
total_repeated_vis <- count(filter(vis_sumry, count > 1))
total_repeated_vis

## Question # 11

vis_repeated <- filter(vis_sumry , count > 1)
vis_repeated

## Question # 12(incomplete)

patient_Specialty <- select(hsptldf, id , Specialty)
Specialty_grouped <- group_by(patient_Specialty,id , Specialty)
unique(patient_Specialty$Specialty)
Specialty_summary <- summarise(Specialty_grouped , count= n())
count_id <- filter(Specialty_summary , count > 1)

# Question # 13

gender_group <- group_by(hsptldf , Sex)
summarise( gender_group, median_age = median(gender_group$Age, na.rm = TRUE))

## Question # 14
typeof(hsptldf$AmountBalance)
hsptldf$AmountBalance <- as.numeric(hsptldf$AmountBalance)
total_Balance <- sum(hsptldf$AmountBalance , na.rm = TRUE)
total_Balance

## Question # 15

consulttbl <- select( hsptldf, Procedure, TotalCharges)
consult_grp <- group_by(consulttbl, Procedure)
typeof(consulttbl$TotalCharges)

consulttbl$TotalCharges <- as.numeric(consulttbl$TotalCharges)
typeof(consulttbl$TotalCharges)
consult_summary <- summarise(consult_grp, total_charges = sum(consulttbl$TotalCharges, na.rm = TRUE))
 
consultation_earning <- filter( consult_summary , consult_summary$Procedure == "Consultation")
View(consultation_earning)

## Question # 16
relation <- select(hsptldf, Age, TotalCharges)
relation$Age <- as.numeric(relation$Age)
relation$TotalCharges <- as.numeric(relation$TotalCharges)
cor.test(x = relation$Age, y= relation$TotalCharges , na.rm= TRUE, use = "complete.obs")
# answer ->  there is no corelation 

# Question # 17

age_group <- group_by(relation, Age)
age_summary <-  summarise(age_group, count = n()) 
high_visits <- max(age_summary$count)

## Question # 18
 
consult_summary <- summarise(consult_grp, total_charges = sum(consulttbl$TotalCharges, na.rm = TRUE))
xrayscal_earning <- filter( consult_summary , consult_summary$Procedure == "Scalling" |consult_summary$Procedure == "X Ray")

total_sum <- sum(xrayscal_earning$total_charges, na.rm = TRUE )
View(total_sum)

write.csv(hsptldf, "D:\\DIH\\Assigments\\R-Assignment-2\\ahsentaqi_isb_r_assignment2\\hospital_clean.csv")
