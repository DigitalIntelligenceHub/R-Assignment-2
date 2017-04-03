library(dplyr)
library(lubridate)
library(readr)

# List all objects in the workspace
#ls()

# Or remove all files from your workspace
#rm(list = ls())

# Load CSV file
hospitaldata <- read.csv('D://diHub//Assessment2_RandPython_Marked//aliejaz1749_khi_r_assignment2//hospitaldata.csv', header = TRUE, stringsAsFactors = F)
str(hospitaldata)

#create copy of dataframe
df <- tbl_df(hospitaldata)
glimpse(df)

# Qus1. Please remove the dots in the names, so it may become easier for you to work through it.
names(df) <- gsub("\\.", "", names(df))

# Qus2. Which day of the week is expected to have most visits?
NameOfDays <- wday(mdy(df$Date), label = TRUE)
which.max(summary(NameOfDays))

# Qus3. What is the average age of patients?
unique(df$Age)
class(df$Age)
p_age_var <- df$Age
p_age <- as.numeric(p_age_var)
mean(p_age, na.rm = TRUE)

# Qus4. How many children were entertained? (Make a Bracket of Age from 1-12)
p_child_age <- as.numeric(df$Age)
p_child_age[is.na(p_child_age)] <- 0
sum(p_child_age >= 12)

# Qus5. Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem
gender_type<-df%>%
filter(!is.na(Sex))%>%
group_by(Procedure,Sex)%>%
summarize(total_gender= n())%>%
filter(total_gender==max(total_gender))
gender_type

# Qus6. Which Doctor is earning highest?
d_high_ern <- select(df, ConsultingDoctor , AmountReceived)
d_high_ern <- filter(df , ConsultingDoctor!='Nursing Staff' , !is.na(AmountReceived))
grp_d_high_ern <- group_by(df, ConsultingDoctor)
summriz_doc_ern <- summarize(grp_d_high_ern, sum(AmountReceived), na.rm = TRUE)
summriz_doc_ern[is.na(summriz_doc_ern)] <- 0
as.numeric(summriz_doc_ern$`sum(AmountReceived)`)
max(summriz_doc_ern$`sum(AmountReceived)`)

# Qus7. Which procedure type earns more money?
p_proc_typ_high <- select(df, Procedure , AmountReceived)
x <- p_proc_typ_high%>%
 filter(!is.na(AmountReceived))%>% 
  group_by(Procedure)%>%
  summarize(Procedure_1 = sum(AmountReceived))%>%
  filter(Procedure_1 == max(Procedure_1)) 
  x

# Qus8. Which time of the day has highest frequency of visits by hour?
x <- df %>%
  filter(!is.na(Time), Time != '-') %>%
  group_by(Time) %>%
  summarize(time_wise_cnt = n()) %>%
  filter(Time != '') %>%
  filter(time_wise_cnt == max(time_wise_cnt))
x

# Qus9. Create a bracket of time by Morning, Afternoon, Evening, Night (6am - 12pm - Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night).


# Qus10. How many patients are repeated visitors?
p_rep_patient_visit <- df %>%
  group_by(id)%>%
  summarize(p_count = n()) %>%
  filter(p_count > 1) %>%
  summarize(tot_rep_vis = n())
p_rep_patient_visit

# Qus11. Give us the id of repeated visitors.
p_rep_vistors <- df %>%
  group_by(id)%>%
  summarize(p_rep_Vist = n()) %>%
  filter(p_rep_Vist > 1) %>%
  arrange(desc(p_rep_Vist))
p_rep_vistors

# Qus12. Which patients visited again for the same problem?
p_p_prob_Vist <- df %>%
  group_by(Procedure, id)%>%
  summarize(p_prob_Vist = n()) %>%
  filter(p_prob_Vist > 1) %>%
  arrange(desc(p_prob_Vist))
p_p_prob_Vist

# Qus13. What is the median age for Females and Males?
p_medi_gender <- df %>%
  group_by(Sex)%>%
  summarize(p_Sex = n()) %>%
  filter(p_Sex > 1) %>%
  arrange(desc(p_Sex))
p_medi_gender

# Qus14. What is the total amount in balance?
p_am_blc <- df$AmountBalance
p_am_blc <- as.numeric(parse_number(p_am_blc))
p_am_blc <- as.numeric(p_am_blc)
p_am_blc[which(is.na(as.numeric(as.character(p_am_blc))))]<-0
p_am_blc = sum(p_am_blc)
p_am_blc

# Qus15. How much money was made by Procedure Type "Consultation"?
p_consultation_max <- df %>%
  filter( Procedure == 'Consultation', !is.na(AmountReceived), AmountReceived!= '-') %>%
  group_by(Procedure) %>% 
  summarize(p_consultation_max = sum(AmountReceived)) 
p_consultation_max

# Qus16. Is there a relation between Age and Total Charges paid?


# Qus17. Which Age group had highest number of visits?
p_max_visit <- df %>%
  filter(Age!= '-', Age!= '', !is.na(Age)) %>%
  group_by(Age) %>% 
  summarize(p_max_visit = n()) %>%
  filter(p_max_visit == max(p_max_visit))
p_max_visit

# Qus18. What is the total cost earned by Procedure Type X Ray and Scalling together?
p_tot_cost <- df %>%
  filter(Procedure == 'X Ray' | Procedure == 'Scalling' , Procedure!= '-', Procedure!= '', !is.na(Procedure)) %>%
  group_by(Procedure) %>% 
  summarize(p_tot_cost = sum(AmountReceived)) %>%
  filter(p_tot_cost == max(p_tot_cost))
p_tot_cost


write.csv(df, file='D:/diHub/Assessment2_RandPython_Marked/aliejaz1749_khi_r_assignment2/updated_hospitaldata.csv')