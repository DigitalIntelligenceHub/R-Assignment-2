library(dplyr)
library(readr)
library(tidyr)



#Q1

Datasethosp <- read.csv("E:\\DIH\\hospitaldata.csv", stringsAsFactors = F, strip.white = T)
names(Datasethosp) <- gsub("\\.", "", names(Datasethosp))
df <- tbl_df(Datasethosp)
names(Datasethosp)
View(df)

df$Age <-as.numeric(df$Age)
df[is.na(df$Age),"Age"]<-0
df$TotalCharges <-as.numeric(df$TotalCharges) 
df[is.na(df$TotalCharges),"TotalCharges"]<-0

#Q2

class(df$Date)
df$Date <- as.Date(strptime(df$Date, "%a, %B %d, %Y"))
weekdays(df$Date[which(table(df$Date) == max(table(df$Date)))])


#Q3

v_age <- as.numeric(df$Age)
v_age[which(is.na(as.numeric(as.character(v_age))))]<-0
v_age
mean(v_age)

#Q4

X <- select(df, Age)
top_counts <- filter(Datasethosp,  X<= 12)



#Q5
df%>%
  count(Sex, Procedure) %>%
  slice(which.max(n))
  

  # Qs : 6
  x <- 
    df %>%
  filter(ConsultingDoctor !='Nursing Staff' ,!is.na(AmountReceived)) %>%
  group_by(ConsultingDoctor) %>%
  summarize(tot_con_doc_wise_amt = sum(AmountReceived)) %>%
  filter(tot_con_doc_wise_amt==max(tot_con_doc_wise_amt))


# Qs : 7
x <- df %>%
  group_by(Procedure) %>%
  summarize(tot_pro_wise_amt = sum(AmountReceived)) %>%
  filter(!is.na(tot_pro_wise_amt)) %>%
  filter(tot_pro_wise_amt == max(tot_pro_wise_amt))


# Qs : 8
x <- df %>%
  filter(!is.na(Time), Time != '-') %>%
  group_by(Time) %>%
  summarize(time_wise_cnt = n()) %>%
  filter(Time != '') %>%
  filter(time_wise_cnt == max(time_wise_cnt))


#Qs : 9

# Qs : 10
x <- df %>%
  group_by(id)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  summarize(tot_rep_vis = n())


# Qs : 11
x <- df %>%
  group_by(id)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  arrange(desc(pat_wise_cnt))


# Qs : 12
x <- df %>%
  group_by(id, Procedure)%>%
  summarize(pat_wise_cnt = n()) %>%
  filter(pat_wise_cnt > 1) %>%
  #  summarize(tot_rep_vis = n())
  arrange(id)


# Qs : 13
x <- df %>%
  filter(!is.na(Sex), Sex!='-', !is.na(Age), Age!='-') %>%
  group_by(Sex) %>%
  summarize(M_mean=mean(parse_number(Age)))

# Qs : 14
x <- df$AmountBalance
x <- as.numeric(parse_number(x))
x <- as.numeric(x)
x[which(is.na(as.numeric(as.character(x))))]<-0
x = sum(x)

x <- df %>%
  filter(!is.na(AmountBalance), AmountBalance!= '-')  %>%
  summarize(M_mean=sum(parse_number(AmountBalance)))


# Qs : 15
x <- df %>%
  filter( Procedure == 'Consultation', !is.na(AmountReceived), AmountReceived!= '-') %>%
  group_by(Procedure) %>% 
  summarize(tot_pro_wise_amt = sum(AmountReceived)) 

# Qs : 16
cor(df$Age,df$TotalCharges)

# Qs : 17
x <- df %>%
  filter(!is.na(Age), Age!='-') %>%
  group_by(Age) %>%
  summarize(Age_wise_cnt=n()) %>%
  filter(Age!='') %>%
  filter(Age_wise_cnt == max(Age_wise_cnt)) 


# Qs : 18
x <- df %>%
  filter( Procedure == 'X Ray'|Procedure == 'Scalling', !is.na(AmountReceived), AmountReceived!= '-') %>%
  group_by(Procedure) %>%
  summarize(proc_wise_tot = sum(AmountReceived))


