df <-read.csv("E://DIH//Assignment2//hospitaldata.csv", strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F)
library("dplyr")
library("lubridate")
library("ggplot2")
View(df)
head(df)
#renaming columns
df<-rename(df, 
       Consulting_Doctor=`Consulting..Doctor`,
       Total_Charges=`Total..Charges`,
       Amount_Received=`Amount..Received.`,
       Amount_Balance=`Amount..Balance`,
       Amount_Received_By=`Amount.Received.By`,
       Amount_in_Hospital=`Amount.in.Hospital`,
       Receptionist_Name=`Receptionist..Name`,
       Next_Apt=`Next.Apt`)

df$Date <- as.Date(strptime(df$Date, "%a, %B %d, %Y"))

df$Time <- format(strptime(df$Time, format='%I:%M %p'), '%I:%M %p')

health_data<-tbl_df(df)

health_data<-mutate(health_data,
                    Age= as.integer(Age),
                    Total_Charges=as.integer(Total_Charges),
                    Amount_in_Hospital=as.integer(Amount_in_Hospital),
                    Time=as.character(Time),
                    Date=as.character(Date))
                    
health_data%>%
select(Date)%>%
transmute(Date=substring(Date, regexpr(' ', Date)+1, nchar(Date)))-> Dates
Dates

health_data%>%
select(Date)%>%
transmute(day= substring(Date,1,regexpr(',', Date)-1))-> Days
Days



#Q2
#Dates<-select(health_data, Date)
#Dates<-mutate(Dates, substring(Date,",")[[1]][1])
df['Date']<-lapply(df['Date'], as.character)

#Q3
summarize(health_data, average=mean(Age))

#Q4
health_data%>%
filter(Age<12)%>%
select(Age)%>%
summarize(total.count=n())

#Q5
health_data%>%
filter(Sex=='M')%>%
group_by(Procedure)%>%
tally(sort=T)

health_data%>%
  filter(Sex=='F' | Sex=='f')%>%
  group_by(Procedure)%>%
  tally(sort=T)

#Q6
health_data%>%
  group_by(Consulting_Doctor)%>%
  summarize(sum=sum(Total_Charges))%>%
  arrange(desc(sum))

#Q7
health_data%>%
  group_by(Procedure)%>%
  summarize(sum=sum(Total_Charges))%>%
  arrange(desc(sum))

#Q8
time<-hour(strptime(health_data$Time, "%I:%M %p"))
which.max(table(time))

#Q9
health_data%>%
  mutate(time_bracket = ifelse(time >= 6 & time <= 12, "Morning",
                        ifelse(time > 12 & time <= 16, "Afternoon",
                        ifelse(time > 16 & time <= 19, "Evening",
                        ifelse(time > 19, "Night", NA)))))



#Q10
health_data%>%
  group_by(id)%>%
  summarize(length=n())%>%
  filter(length>1)%>%
  nrow()

#Q11
health_data%>%
  group_by(id,Procedure)%>%
  summarize(length=n())%>%
  filter(length>1)%>%
  print()

#Q12
health_data%>%
  group_by(id)%>%
  summarize(length=n())%>%
  filter(length>1)%>%
  print()

#Q13
health_data%>%
  filter(Sex=='M')%>%
  select(Age)%>%
  summarize(median=median(Age,na.rm=TRUE))%>%
  print()

health_data%>%
  filter(Sex=='F'| Sex=='f')%>%
  select(Age)%>%
  summarize(median=median(Age,na.rm=TRUE))%>%
  print()

#Q14
health_data$Amount_Balance<-gsub(",", "" ,health_data$Amount_Balance, fixed=TRUE)
health_data$Amount_Balance<-gsub(".00", "" ,health_data$Amount_Balance, fixed=TRUE)

health_data%>%
  mutate(Amount_Balance=as.integer(Amount_Balance))%>%
  select(Amount_Balance)%>%
  summarize(sum=sum(Amount_Balance,na.rm=TRUE))%>%
  print()

#Q15
health_data%>%
  filter(Procedure=="Consultation")%>%
  select(Total_Charges)%>%
  summarize(sum=sum(Total_Charges,na.rm=TRUE))%>%
  print()

#Q16
health_data%>%
  select(Age,Total_Charges) %>%
  filter(!is.na(Age),!is.na(Total_Charges)) ->corr
  
cor(corr$Age,corr$Total_Charges)

#Q17
qplot(health_data$Age, xlab="Age")

#Q18
health_data%>%
  filter(Procedure== 'X Ray' | Procedure== 'Scalling' ) %>%
  select(Total_Charges)%>%
  summarize(sum=sum(Total_Charges,na.rm=TRUE))%>%
  print()

write.csv(health_data, "E://DIH//Assignment2//clean_hospital_data_R.csv")

  