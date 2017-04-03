library(dplyr)
library(readr)
library(tidyr)

Hosdf <- read.csv('D://Dih//Hunain- Khi_R_Assignment2//hospitaldata.csv',header = TRUE, stringsAsFactors = FALSE)

glimpse(Hosdf)



names(Hosdf) <- gsub("\\.", "", names(Hosdf))
Hosdf
#QS 1
#install.packages("lubridate")
#library(lubridate)



# Q:3
x <- Hosdf$Age
y <- as.numeric(x)
y[which(is.na(as.numeric(as.character(y))))]<-0
mean(y)

# Q:4
x <- Hosdf$Age
y <- as.numeric(x)
y[which(is.na(as.numeric(as.character(y))))]<-0
length(which(y <= 12))


# Qs : 6
x <- Hosdf %>%
  filter(ConsultingDoctor!='Nursing Staff' ,!is.na(AmountReceived)) %>%
  group_by(ConsultingDoctor) %>%
  summarize(tot_con_doc_wise_amt = sum(AmountReceived)) %>%
  filter(tot_con_doc_wise_amt==max(tot_con_doc_wise_amt))


# Qs : 7
x <- Hosdf %>%
  group_by(Procedure) %>%
  summarize(tot_pro_wise_amt = sum(AmountReceived)) %>%
  filter(!is.na(tot_pro_wise_amt)) %>%
  filter(tot_pro_wise_amt == max(tot_pro_wise_amt))


# Qs : 8
x <- Hosdf %>%
  filter(!is.na(Time), Time != '-') %>%
  group_by(Time) %>%
  summarize(time_wise_cnt = n()) %>%
  filter(Time != '') %>%
  filter(time_wise_cnt == max(time_wise_cnt))


# Qs : 10
x <- Hosdf %>%
  group_by(id)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  summarize(tot_rep_vis = n())


# Qs : 11
x <- Hosdf %>%
  group_by(id)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  arrange(desc(pat_wise_cnt))


# Qs : 12
x <- Hosdf %>%
  group_by(id, Procedure)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  #  summarize(tot_rep_vis = n())
  arrange(id)


# Qs : 13
x <- Hosdf %>%
  filter(!is.na(Sex), Sex!='-', !is.na(Age), Age!='-') %>%
  group_by(Sex) %>%
  summarize(M_mean=mean(parse_number(Age)))

# Qs : 14
x <- Hosdf$AmountBalance
x <- as.numeric(parse_number(x))
x <- as.numeric(x)
x[which(is.na(as.numeric(as.character(x))))]<-0
x = sum(x)

x <- Hosdf %>%
  filter(!is.na(AmountBalance), AmountBalance!= '-')  %>%
  summarize(M_mean=sum(parse_number(AmountBalance)))


# Qs : 15
x <- Hosdf %>%
  filter( Procedure == 'Consultation', !is.na(AmountReceived), AmountReceived!= '-') %>%
  group_by(Procedure) %>% 
  summarize(tot_pro_wise_amt = sum(AmountReceived)) 


# Qs : 17
x <- Hosdf %>%
  filter(!is.na(Age), Age!='-') %>%
  group_by(Age) %>%
  summarize(Age_wise_cnt=n()) %>%
  filter(Age!='') %>%
  filter(Age_wise_cnt == max(Age_wise_cnt)) 


# Qs : 18
x <- Hosdf %>%
  filter( Procedure == 'X Ray'|Procedure == 'Scalling', !is.na(AmountReceived), AmountReceived!= '-') %>%
  group_by(Procedure) %>%
  summarize(proc_wise_tot = sum(AmountReceived))


