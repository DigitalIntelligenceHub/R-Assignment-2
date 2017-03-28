library("dplyr")
library("tidyr")
library("lubridate")


setwd('C:/Users/muhammad.shazil/Documents/shazil_lhr_r_Assignment2')
hos_df <- read.csv('hospitaldata.csv', sep = ',', header = TRUE)
names(hos_df) <- gsub("\\.", "", names(hos_df))
hos_df <- hos_df %>% separate(Date, c('Day', 'Month', 'Year'), sep = ',')

hos_1 <- hos_df
hos_df <- hos_1

#Which day of the week is expected to have most visits?
day_df <- select(hos_df, Day )
sep_day <- table(day_df)
print(sep_day)


#What is the average age of patients?
class(hos_df$Age)

hos_df$Age <- as.numeric(as.character(hos_df$Age))
class(hos_df$Age)
mean(hos_df$Age, na.rm = TRUE)
#32.73438


#How many children were entertained? (Make a Bracket of Age from 1-12)
child_in <- which(hos_df$Age > 1 & hos_df$Age < 12)
length(child_in)


#Which gender type had what kind of procedure in abundance? 
#i.e. Female visit mostly because of Gynae Problem
gender_pro <- select(hos_df, Sex, Procedure)

gender_pro$Sex = toupper(gender_pro$Sex)

gender_pro$Sex = ifelse(gender_pro$Sex == "-", NA, gender_pro$Sex)

gender_pro$Sex = ifelse(gender_pro$Sex == "", NA, gender_pro$Sex)

a <- table(gender_pro$Sex, gender_pro$Procedure)


#Doc to revenue
doc_rev <- hos_df %>%  select(ConsultingDoctor,TotalCharges)
View(doc_rev)
doc_rev$TotalCharges <- as.numeric(as.character(doc_rev$TotalCharges))

hi_er <- doc_rev %>% 
  group_by(ConsultingDoctor) %>%
  summarize(total=sum(TotalCharges, na.rm = TRUE))

hi_er <- arrange(hi_er, desc(total))
hi_er



#Procedure to earning 
pro_ty <- hos_df %>% select(Procedure, TotalCharges)

pro_ty$TotalCharges <- as.numeric(as.character(pro_ty$TotalCharges))

pro_ty <- pro_ty %>% group_by(Procedure) %>% 
  summarize(total_sum = sum(TotalCharges, na.rm = TRUE)) 

pro_ty <- arrange(pro_ty, desc(total_sum))
View(pro_ty)

#Which time of the day has highest frequency of visits by hour?
hos_df$Time <- as.character(hos_df$Time)
str(hos_df$Time)
unique(hos_df$Time)

hos_df$Time <- gsub("\\-", NA, hos_df$Time)

class(hos_df$Time)

b <- strptime(hos_df$Time, "%I:%M %p")
hour(b)
hos_df$Time <- substr(strptime(hos_df$Time, "%I:%M %p" ),11,19)
hos_df$Time
ans <- hos_df %>% 
  group_by(Time=hour(b)) %>% 
  summarize(n = n())
View(ans) 


#How many patients are repeated visitors?
rep_vis <- select(hos_df, id)
rep_vis <- rep_vis %>% 
  group_by(id) %>% 
  summarize(total = n()) 

r_vistors1 <-  filter(rep_vis, total > 1)
length(r_vistors1$total)

#Give us the id of repeated visitors.

r_vistors1$id


#Which patients visited again for the same problem?

same_prob <- select(hos_df, id, Procedure)

same_prob1 <- same_prob %>% group_by(id, Procedure) %>% 
  summarize(total = n())

c <- filter(same_prob1, total > 1)


#What is the median age for Females and Males?

age_sex <- select(hos_df, Age, Sex)
age_f <- filter(age_sex, Sex == 'F')
mean(age_f$Age, na.rm = TRUE)

age_m <- filter(age_sex, Sex == 'M')
mean(age_m$Age, na.rm = TRUE)

#What is the total amount in balance?
class(hos_1$AmountBalance)
hos_1$AmountBalance<-as.character(hos_1$AmountBalance)
unique(hos_1$AmountBalance)
hos_1$AmountBalance <- as.numeric(hos_1$AmountBalance)

hos_1$AmountBalance <- gsub("\\-", NA, hos_1$AmountBalance)
hos_1$AmountBalance <- gsub(",", "", hos_1$AmountBalance)

a <- sum(hos_1$AmountBalance, na.rm = TRUE)


#How much money was made by Procedure Type "Consultation"?
money_pro <- select(hos_1, Procedure, TotalCharges)
class(money_pro$TotalCharges)
money_pro$TotalCharges <- as.numeric(as.character(money_pro$TotalCharges))

money_made <- money_pro %>% 
  group_by(Procedure, TotalCharges) %>% 
  summarize(total_sum = sum(TotalCharges))

#Is there a relation between Age and Total Charges paid?
class(hos_1$Age)
hos_1$TotalCharges <-  as.numeric(as.character(hos_1$TotalCharges))
hos_1$Age <-  as.numeric(as.character(hos_1$Age))

cor.test(hos_1$Age, hos_1$TotalCharges)

#What is the total cost earned by Procedure Type X Ray and Scalling together?
hos_1$Procedure <-  as.character(hos_1$Procedure)
two_earning <- filter(hos_1, hos_1$Procedure == 'X Ray' | hos_1$Procedure == 'Scalling')

sum(two_earning$TotalCharges, na.rm = TRUE)
