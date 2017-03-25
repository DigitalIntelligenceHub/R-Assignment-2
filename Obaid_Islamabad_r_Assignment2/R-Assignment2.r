# Obaid Ur Rehman

#Loading required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(mosaic)


#Loading data set from csv file named "hospitaldata.csv"

hdf <- read.csv("D:\\Inbox Workplace\\R Workspace\\R Learning Assignment 2\\R-Assignment-2\\Obaid_Islamabad_r_Assignment2\\hospitaldata.csv",na.strings=c("","NA"))
dim(hdf)
# 222 observations and 15 columns

#Printing hdf
head(hdf)

# Q1. Cleaning the column names
names(hdf)<-gsub("\\.","",names(hdf))
head(hdf) #dots from column names removed



# Q2. Which day of the week is expected to have most visits?
dayPop <-
  hdf %>%
  mutate(Day=wday(mdy(Date),label=TRUE)) %>%
  group_by(Day) %>%
  summarize(visits=length(Day))

ggplot(dayPop,aes(x=Day,y=visits))+geom_bar(stat="identity",fill="#8E44AD")+ggtitle("Visits by Days")+labs(x="Day",y="Vists")

#The visits on Monday are greater than visits on other days of week, and also the probability of Monday is higher 
# therefore, Monday is expected to have most visits

# Q3. What is the average age of patients?
hdfClean<- hdf
hdfClean$Age <-as.numeric(as.character(hdfClean$Age))
mean(hdfClean$Age,na.rm = TRUE) #Average age is 32.7 

# Q4. How many childerns were entertained?
count(filter(hdfClean,Age>=1,Age<=12))  #23 childerns were entertained    #Q to ask, if i use length instead of count it gives 15. y?

# Q5. Which gender type had what kind of procedure in abundance?
hdfClean$Sex <- gsub("f","F",hdfClean$Sex)
hdfClean$Sex<-gsub("\\s|-",NA,hdfClean$Sex)
qplot(data=hdfClean,Sex,fill=Specialty)+ggtitle("Gender Speciality abundance")+labs(x='Gender',y='No of patients')

# As we can see from plot, both Male and Female have Dentist procedure in abundance



# Q6. Which doctor is earning highest?

#Cleaning totalCharges column (we will need in future to summ charges) by Converting them to numeric and replacing NA with 0
hdfClean$TotalCharges <- as.numeric(as.character(hdfClean$TotalCharges))
hdfClean[c('TotalCharges')][is.na(hdfClean[c('TotalCharges')])]<-0  #only chnage NA to 0 in TotalCharges Column
DrEarnings <-
  hdfClean %>%
  group_by(ConsultingDoctor)%>%
  summarize(Earning=sum(TotalCharges)) %>%
  arrange(desc(Earning))

DrEarnings # Dr Alaf Khan has the highest earnings!

#Plottig graph for DoctorEarnings
ggplot(data=DrEarnings,aes(x=ConsultingDoctor,y=Earning))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("ConsultingDoctor Earnings")+labs(x='Consulting DOctor',y='Earnings')


# Q7. Which procedure type earns more money?

#its same as above Question, jut need to group_by with Procedur instead of ConsultingDoctor
# We dont need to clean totalcharges column again

ProcedureEarnings <-
  hdfClean %>%
  group_by(Procedure) %>%
  summarize(Earning=sum(TotalCharges)) %>%
  arrange(desc(Earning))
ProcedureEarnings  #Orthodontics earns more money

#Plotting graph for ProcedureEarnings
ggplot(data=ProcedureEarnings,aes(x=Procedure,y=Earning))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("Earnings by Procedures")+labs(x='Procedures',y="Earnings")



# Q8. Which time of day has highest frequency of visits by hours

#Creating a column Hour 
VisitsByHour <-
  hdfClean %>%
  select(Time) %>%
  mutate(Hour = hour(hm(format(strptime(hdfClean$Time, "%I:%M %p"), "%H:%M")))) %>%
  group_by(Hour) %>%
  summarize(Visits=length(Hour)) %>%
  arrange(desc(Visits))%>%
  filter(!is.na(Hour))
VisitsByHour # it seems at 1:00PM (13:00), the visits are maximum. The Hour for 2nd highest is sadly NA

#plotting
ggplot(data=VisitsByHour,aes(x=factor(Hour),y=factor(Visits)))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("Visits By Hour")+labs(x='Hours (Standard Time)',y='Visits')


# Q9. Create a bracket of time

#Create column hour in hdfClean
hdfClean <-
  hdfClean %>%
  mutate(Hour = hour(hm(format(strptime(Time,"%I:%M %p"),format="%H:%M"))))

hdfClean <-
  hdfClean %>%
  mutate( Bracket = derivedFactor(
    "Morning" = (Hour>=6 & Hour<=12),
    "Afternoon" = (Hour>=12 & Hour<=16),
    "Evening" = (Hour>=14 & Hour<=19),
    "Night" =((Hour>=19 & Hour<=23) | (Hour>=0 & Hour<=6) ),
    .method = "first",
    .default = 0
  ))
select(hdfClean,Time,Hour,Bracket)


# Q10.  How many patients are repeated visitor?
repPat <- 
  select(hdfClean,id) %>%
  group_by(id) %>%
  summarize(visits=length(id)) %>%
  arrange(desc(visits)) %>%
  filter(visits >1)
dim(repPat)  #37 Patients have more than one visits. Paient with id= 1 is very unfortunate, with 12 visits, i guess.

#plotting
ggplot(data=repPat,aes(x=factor(id),y=factor(visits)))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("Patients with Repeated vists and their number of vists")+labs(x='Patient IDs',y='No. Of Vists')


# Q11.  Give the id of repeated visitors
ids<-
  repPat %>%
  select(id)
ids #Shows the id(s) of repeated patients


# Q12.  Which patients visited again for the same problem?
samep <-
  hdfClean %>%
  select(id,Specialty) %>%
  group_by(id) %>%
  summarize(problems=n_distinct(Specialty), visits=length(Specialty))%>%
  filter(visits>problems)
samep  
# The above tabel sgow the id, and no of distinct problems that patient have and no of visits patient made
# so if, no of visits is greater than no of problems patient have this means patient have  come more than 1 for atleast one of the problems
# he got

#Q13. What os median age for female and male?
medianAge<-
  hdfClean %>%
  select(Sex,Age) %>%
  group_by(Sex) %>%
  summarize(MedianAge = median(Age,na.rm=TRUE))
medianAge # Shows the median age for Female(F) and Male(M)


# Q14.  What is the total amount in balance?
hdfClean$AmountBalance <-gsub("\\.00|,","",hdfClean$AmountBalance)
hdfClean$AmountBalance <-as.numeric(as.character(hdfClean$AmountBalance))
sum(hdfClean$AmountBalance,na.rm=TRUE) #222500


# Q15.  How much money was made by Procedure Type "Consultation"?

#cleaning TotalCharges column
hdfClean$TotalCharges <- as.numeric(as.character(hdfClean$TotalCharges))
consult <-
  hdfClean %>%
  select(Procedure,TotalCharges) %>%
  group_by(Procedure) %>%
  filter(Procedure =='Consultation') %>%
  summarize(TotalMoney= sum(TotalCharges,na.rm=TRUE)) 
consult #83950 


# Q16.  Is there any relation between Age and Total charges paid?
cor<-cor(hdfClean$Age,hdfClean$AmountReceived, use='complete.obs')  #use is to ignore NA values
cor

# The answer is 0.13 which tell us that there is a weak positive (uphill) relation.
ggplot(data=hdfClean,aes(x=Age,y=AmountReceived))+geom_smooth()
# As we can see for plot, There exists a relation but is very weak linear relationship. We usually dnt consider cor values less than 0.05


# Q17.  Which age group had highest number of visits?
ageVisits<-
  hdfClean %>%
  select(id,Age) %>%
  group_by(Age) %>%
  summarize(Visits=length(Age)) %>%
  arrange(desc(Visits)) %>%
  filter(!is.na(Age))
ageVisits
#Plotting
ggplot(data=ageVisits,aes(x=factor(Age),y=Visits))+geom_bar(stat='identity',fill='#8E44AD')+ggtitle("Vists By Age")+labs(x='Age',y='Visits')
#As we can see, Most no of vists are 30 but the Age is NA so we dont include that. After that, patient of age 30 have more visits than other.


# Q18.  What is the total cost earned by Procedure Type X Ray and Scalling together?
earning <-
  hdfClean %>%
  select(Procedure,TotalCharges) %>%
  filter(Procedure=='X Ray'|Procedure=='Scalling') %>%
  group_by(Procedure) %>%
  summarize(Occurance=n(), Earning=sum(TotalCharges)) 
earning
# Scalling = 16500, X Ray = 5800. 
# As we can see from results, x Ray occured more than Scalling and still earned less than scalling. One thing we can conclude from this is
#that the XRay fee is less than Scalling fee.

#BUTTTTTT!!!, there are procedures in which xray was done along with some other procedure :same for scalling
# now for better results, we dig deep

earning2 <-
  hdfClean %>%  
  select(Procedure,TotalCharges) %>%
  filter(  grepl("X Ray",Procedure) | grepl("Scalling",Procedure),nchar(as.character(Procedure))>8) %>%
  mutate(Procedure= derivedFactor(
    "X Ray" = (grepl("X Ray",Procedure)==TRUE),
    "Scalling" = (grepl("Scalling",Procedure)==TRUE),
    .method = "first",
    .default = 0
  ),
  TotalCharges=derivedFactor(
    "300" = (Procedure =='X Ray'),
    "3000" = (Procedure =='Scalling'),
    .method = "first",
    .default = 0
  ))  %>%
  group_by(Procedure) %>%
  summarize(Occurance=n(),Earning=sum(as.numeric(as.character(TotalCharges))))


totalEarnings <-
  rbind(earning,earning2) %>%
  group_by(Procedure) %>%
  summarize(Occurance=sum(Occurance),Earning=sum(as.numeric(as.character(Earning))))
totalEarnings
# So that totalEarnings Show the actual earning by X Ray and Scalling , and their occurance in the whole data set

#lets Plot this
ggplot(data=totalEarnings,aes(x='',y=Earning,fill=Procedure))+geom_bar(width=1,stat='identity')+coord_polar('y',start=0)


#Generating csv file from cleaned data
write.csv(hdfClean, 'D:/Inbox Workplace/R Workspace/R Learning Assignment 2/R-Assignment-2/Obaid_Islamabad_r_Assignment2/Clean-HospitalData.csv', row.names=T)


