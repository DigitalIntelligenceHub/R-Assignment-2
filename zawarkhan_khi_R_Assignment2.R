#install.packages("lubridate")
#install.packages("varhandle")
#install.packages("data.table")
#install.packages("plyr")
#install.packages("reshape2")
#install.packages("dplyr")

library(lubridate)
library(varhandle)
library(data.table)
library(reshape2)
library(plyr)
library(dplyr)


ds <- read.csv("D:/Python_R_Ass2/hospitaldata.csv",header=TRUE, sep=",")

#Q1
names(ds) <- gsub( ".",  "", names(ds), fixed = TRUE)
summary(ds)
#=======================================================
#Q2
date_in<-as.character(ds$Date)
date_in<-strsplit(date_in,",")
week<-list()
for(i in 1:length(date_in)){
  week[i]<-date_in[[i]][1]
}
week<-unlist(week,recursive=FALSE)
max_week<-max(table(week))
max_week
#=======================================================
#Q3 Average age of Patient
ds$Age<- as.character(ds$Age)
ds$Age<-gsub("-","0",ds$Age)
ds$Age<-gsub("M","",ds$Age)
ds$Age[ds$Age==""]<-"0"
ds$Age<- as.double(ds$Age)
ds$Age[is.na(ds$Age)] <- 0
mean(as.numeric(ds$Age),na.rm=TRUE)
#ds$Age[ds$Age==" "] <- as.numeric(NA)
#=======================================================
#Q4 No of Children entertained
sum(ds$Age >= 1 & ds$Age <= 12,na.rm = TRUE)
#=========================================================
#Q6 which doctor is earning the highest
ds$ConsultingDoctor <- as.character(ds$ConsultingDoctor)
ds$TotalCharges <- as.character(ds$TotalCharges)
ds$TotalCharges[ds$TotalCharges=="Cancelled"]<-"0"
ds$TotalCharges[ds$TotalCharges==""]<-"0"
ds$TotalCharges[ds$TotalCharges=="nan"]<-"0"
ds$TotalCharges<-as.double(ds$TotalCharges)
aggregate(ds$TotalCharges, by=list(Category=ds$ConsultingDoctor), FUN=sum)
#===========================================================================
#Q5 which gender have what kind of procedure in abundance
ds$Sex <- as.character(ds$Sex)
ds$Sex <- gsub( "-",  "NA", ds$Sex)
ds$Sex <- gsub( " ",  "NA", ds$Sex)
ds$Sex <- gsub( ",",  "NA", ds$Sex)
ds$Sex<-gsub("f","F",ds$Sex)
ds$Procedure <- as.character(ds$Procedure)
ds$Sex <- gsub( "-",  "NA", ds$Sex)
ds$Sex <- gsub( " ",  "NA", ds$Sex)
ds$Sex <- gsub( ",",  "NA", ds$Sex)
counts1 <- ddply(ds, .(ds$Procedure[ds$Sex=='M'], ds$Procedure), nrow)
counts2 <- ddply(ds, .(ds$Procedure[ds$Sex=='F'], ds$Procedure), nrow)
names(counts1) <- c("Gender", "Procedure", "Freq")
names(counts2) <- c("Gender", "Procedure", "Freq")
counts1
counts2
counts11 <- ds$Procedure[ds$Sex=='M']
counts22 <- ds$Procedure[ds$Sex=='F']
cat("Male:","Consultaion has",max(table(counts11)))
cat("Female:","Consultaion has",max(table(counts22)))
#===========================================================================
#Q7
#ds$Procedure <- unfactor(ds$Procedure)
#ds$TotalCharges <- unfactor(ds$TotalCharges)
#ds$TotalCharges[is.na(ds$TotalCharges)] <- 0
k<-aggregate(ds$TotalCharges, by=list(Category=ds$Procedure), FUN=sum)
k
#==========================================================================
#Q8 Which time of the day has highest frequency of visits by hour
ds$Time<-as.POSIXct(ds$Time,format="%H:%M %p")
time_hr<-hour(ds$Time)
table(time_hr)
#============================================================
#Q9Create a bracket of time by Morning, Afternoon, Evening, Night 
time_hr<-hour(ds$Time)
List1 <- assign("list", NULL, envir = .GlobalEnv)
List1[time_hr >=06 & time_hr <= 12] <- "Morning"
List1[time_hr > 12 & time_hr <= 16] <- "Afternoon"
List1[time_hr > 16 & time_hr <= 19] <- "Evening"
List1[time_hr > 19 & time_hr < 06] <- "Night "
ds <- mutate(ds, List1)
ds$List1
#==========================================================
#Q10
ds$id <- as.double(ds$id)
ds$id
num<-table(ds$id)
num=num[num>1]
length(num)
#========================================================================
#Q11
ds$id <- as.double(ds$id)
ds$id
num<-table(ds$id)
num=num[num>1]
names(num)
#=======================================================================
#Q12//
countering1<-count(ds,id,Procedure)
countering1<-countering1$id[countering1$n>1]
countering1
#========================================================================
#Q13
Male<-median(ds$Age[ds$Sex=="M"],na.rm=TRUE)
Female<-median(ds$Age[ds$Sex=="F"],na.rm=TRUE)
Male
Female
#=========================================================================
#Q14
ds$AmountBalance<- as.character(ds$AmountBalance)
ds$AmountBalance<-gsub("-","0",ds$AmountBalance)
ds$AmountBalance<-gsub(",","",ds$AmountBalance)
ds$AmountBalance<-gsub(" ","",ds$AmountBalance)
ds$AmountBalance[ds$AmountBalance==""]<-"0"
ds$AmountBalance<- as.double(ds$AmountBalance)
sum<-sum(ds$AmountBalance)
sum
#==================================================
#Q15
#ds$Procedure <- unfactor(ds$Procedure)
#ds$TotalCharges <- unfactor(ds$TotalCharges)
money<-sum(ds$TotalCharges[ds$Procedure=="Consultation"],na.rm=TRUE)
money
#==================================================
#Q16
#ds$TotalCharges <-as.double(ds$TotalCharges)
ds$Age<-as.double(ds$Age)
ds$Age[is.na(ds$Age)] <- 0
ds$TotalCharges[is.na(ds$TotalCharges)] <- 0
unique(ds$Age)
print(cor(ds$Age,ds$TotalCharges))
#===================================================
#Q17  Age group had highest number of visits
#ds$Age<-as.double(ds$Age)
a<-names(which.max(table(ds$Age[ds$Age!=0])))
a
hist(ds$Age)
#====================================================
#Q18 total cost earned by Procedure Type X Ray and Scalling together
#ds$TotalCharges<-unfactor(ds$TotalCharges)
#ds$TotalCharges[is.na(ds$TotalCharges)] <- 0
ds$Procedure<-as.character(ds$Procedure)
sum(ds$TotalCharges[ds$Procedure=="X Ray"|ds$Procedure=="Scalling"])
#==================================================
write.csv(ds,file = "update_hospitaldata.csv")
#==================================================
