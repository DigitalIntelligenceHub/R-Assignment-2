library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mosaic)
library(data.table)
library(lubridate)
library(stringi)

hosp_data <- read_csv("~/DIH/Assignment/Assignment 2/hospitaldata.csv")
View(hosp_data)

###Q1, remove the dots in the names?
names(hosp_data) <- gsub(".", "", names(hosp_data), fixed = TRUE)

###Q2, Which day of the week is expected to have most visits? Ans = Monday 51
hosp_data <- separate(hosp_data , Date , c("day" , "date") , sep="," , extra = "merge") ##making aseparate column Day which contains the weekday
day_occurances <- table(unlist(hosp_data$day))
max(day_occurances)

    
###Q3, What is the average age of patients? Ans==32.40

hosp_data$Age <- gsub("-", NA , hosp_data$Age , fixed = T)
hosp_data$Age <- gsub("", NA , hosp_data$Age , fixed = T)
hosp_data$Age <- gsub(" ", NA , hosp_data$Age, fixed = T)

month2year <- function(x){
  print(x)
  if(is.na(x) == FALSE & grepl("M" , x)){
    age_year <- gsub("M", "",  x , fixed = T)
    age_year <- as.integer(age_year)
    age_year<-age_year/12
    print(age_year)
    return(age_year)
    }
  else{
    x <- as.integer(x)
    return(x)
    }
}

hosp_data$Age <- Map({function (a)  month2year(a)}, hosp_data$Age)
hosp_data$Age <- as.integer(hosp_data$Age)mean_age <- mean(hosp_data$Age, na.rm = TRUE)
print(mean_age)


###Q4, 4. How many children were entertained? (Make a Bracket of Age from 1-12)  Ans == 24
child_count <- filter(hosp_data , Age <=12 & Age >= 1)
nrow(child_count)


###Q5Which gender type had what kind of procedure in abundance? Dentist for both male and female

hosp_data$Sex <- gsub("-", NA , hosp_data$Sex , fixed = T)
hosp_data$Sex <- gsub("", NA , hosp_data$Sex , fixed = T)
hosp_data$Sex <- gsub("f", "F" , hosp_data$Sex, fixed = T)

sexVspecialty <- hosp_data %>% 
  select(Sex,Specialty) %>% 
###  group_by(Sex, Specialty) %>% 
###  summarize(count = n())

View(sexVspecialty)
qplot(data = sexVspecialty,Sex, fill = Specialty)

###6. Which Doctor is earning highest ? Doctor Alaf
hosp_data$TotalCharges <- gsub("-", NA , hosp_data$TotalCharges , fixed = T)
hosp_data$TotalCharges <- gsub("", NA , hosp_data$TotalCharges , fixed = T)
hosp_data$TotalCharges <- gsub("Cancelled", "NA" , hosp_data$TotalCharges , fixed = T)
hosp_data$TotalCharges <- as.numeric(hosp_data$TotalCharges)

docVearning <- hosp_data %>%
  select(ConsultingDoctor, TotalCharges) %>%
  group_by(ConsultingDoctor) %>%
  summarize( Total = sum(TotalCharges , na.rm = T))


View(docVearning)
qplot(data = docVearning , ConsultingDoctor , Total) + geom_bar(stat = "identity" , fill = "#FF5733")


###7 Which procedure type earns more money? Orthodontics
procedureVearning <- hosp_data %>%
  select(Procedure, TotalCharges) %>%
  group_by(Procedure) %>%
  summarize( Total = sum(TotalCharges , na.rm = T))
View(procedureVearning)
qplot(data = procedureVearning , Procedure , Total) + geom_bar(stat = "identity" , fill = "#FF5733")

###8 Which time of the day has highest frequency of visits by hour? 13th Hour
hosp_data$Time <- gsub("-", NA , hosp_data$Time , fixed = T)
hosp_data$Time <- gsub("", NA , hosp_data$Time , fixed = T)
## hosp_data$Time <- gsub("11:00AMAM", "11:00AM" , hosp_data$Time , fixed = T)


timebracket <- hosp_data %>% 
  select(Time) %>%
  mutate(Hour = hour(hm(format(strptime(Time,"%I:%M %p"),"%H:%M"))))%>%
  group_by(Hour) %>%
  summarize(count= n() )

View(timebracket)

###9. Create a bracket of time by Morning, Afternoon, Evening   :: Afternoon         

timebracketfunc <- function(x){
  print(x)
  for (i in length(x)) {
    
  if(x >= 6 & x < 12){
    return("Morning")
  }
  else if(x >= 12 & x < 16){
    return("Afternoon")
  }
  else if(x >= 16 & x < 19){
    return("Evening")
  }
  else if((x >= 19 & x < 24) | (x >= 0 & x < 6)){
    return("Night")
  }
  else if(is.na(x) == T){
    return(x)
  }
}
}

timebracket1 <- hosp_data %>% 
  select(Time) %>%
  mutate(Hour = hour(hm(format(strptime(hosp_data$Time,"%I:%M %p"),"%H:%M")))) ##  %>%

breaks <- hour(hm(6, 12, 16, 19, 23))
labels <- c("Morning","Afternoon","Evening", "Night")

timebracket1 <- timebracket1 %>%
  mutate('Time_Bracket' = cut(Hour, breaks, labels)) %>%
  group_by(Time_Bracket) %>%
  summarize(count= n() )

hosp_data <- hosp_data %>%
  mutate('Time Bracket' = cut(Hour, breaks, labels))

View(timebracket1)

###10. How many patients are repeated visitors? 37
Repeat_visitor <- hosp_data %>% 
  select(id) %>%
  group_by(id) %>%
  summarize(count= n()) %>%
  filter(count > 1)

nrow(Repeat_visitor)

View(Repeat_visitor)


###11. Give us the id of repeated visitors.
Repeat_visitor <- hosp_data %>% 
  select(id) %>%
  group_by(id) %>%
  summarize(count= n()) %>%
  filter(count > 1)

View(Repeat_visitor)

#12. Which patients visited again for the same problem? Ans= 24
Repeat_visitor_same_prob <- hosp_data %>% 
  select(id, Procedure) %>%
  group_by(id , Procedure) %>%
  summarize(count= n()) %>%
  filter(count > 1)

View(Repeat_visitor_same_prob)

nrow(Repeat_visitor_same_prob)


#13.What is the median age for Females and Males?  Female = 29 , male = 30

hosp_data$Age <- as.numeric(hosp_data$Age)

Median_age_sex <- hosp_data %>% 
  select(Age, Sex) %>%
  group_by(Sex)  %>%
  summarize(Median = median(Age, na.rm = T))

View(Median_age_sex)

#14. What is the total amount in balance? 222500
hosp_data$AmountBalance <- gsub("-", NA , hosp_data$AmountBalance , fixed = T)
hosp_data$AmountBalance <- gsub("", NA , hosp_data$AmountBalance , fixed = T)
hosp_data$AmountBalance <- gsub(",", "" , hosp_data$AmountBalance , fixed = T)
hosp_data$AmountBalance <- as.numeric(hosp_data$AmountBalance)

sum(hosp_data$AmountBalance , na.rm = T)

###Q15. How much money was made by Procedure Type "Consultation" . 83950
consulting_cash <- hosp_data %>% 
  select(Procedure, TotalCharges) %>%
  filter(Procedure == "Consultation")


sum(consulting_cash$TotalCharges , na.rm = T)
View(consulting_cash)


#Q16.  Is there a relation between Age and Total Charges paid? Co-relation = 0.03 , no relation

ageVcash <- hosp_data %>% 
  select(Age, TotalCharges)
  
View(ageVcash)
cor.test(x= ageVcash$Age , y= ageVcash$TotalCharges, na.rm = T , use = "complete.obs")

###Q17 . Which Age group had highest number of visits? Age 30 visits 12

age_visits <- hosp_data %>% 
  select(Age) %>%
  group_by(Age) %>%
  summarize(count= n()) %>%
  filter(count > 1)
View(age_visits)

##Q18. . What is the total cost earned by Procedure Type X Ray and Scalling together 89750

consulting_xray_cash <- hosp_data %>% 
  select(Procedure, TotalCharges) %>%
  filter(Procedure == "Consultation" | Procedure == "X Ray")


sum(consulting_xray_cash$TotalCharges , na.rm = T)
View(consulting_xray_cash)

#####Extracting clean data

write.csv(hosp_data, "C:/Users/shayan.haider/Documents/DIH/Assignment/Assignment_2/hosp_data_clean.csv")
