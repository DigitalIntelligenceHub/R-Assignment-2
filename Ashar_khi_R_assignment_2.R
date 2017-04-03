
library("dplyr")
library("lubridate")
library("plotly")

mydata<-read.csv("C:/Users/ashar.burney/Downloads/hospitaldata.csv",header=TRUE,sep=",")

#Q1 Please remove the dots in the names, so it may become easier for you to work through it
colnames(mydata)<-gsub("\\.","",colnames(mydata))

#Q2 Which day of the week is expected to have most visits? 
strDate <-as.character(mydata$Date)
strDate<-strsplit(strDate,",")
strWeekday<-list()
for(i in 1:length(strDate)){
  strWeekday[i]<-strDate[[i]][1]
}
strWeekday<-unlist(strWeekday,recursive=FALSE)
#print(table(strWeekday))
max(table(strWeekday))

#Q3 What is the average age of patients?
mydata$Age<- as.character(mydata$Age)
mydata$Age<-gsub("M","",mydata$Age)
mydata$Age<-gsub("-","0",mydata$Age)
mydata$Age[mydata$Age==""]<-"0"
mydata$Age<- as.double(mydata$Age)
avg<-mean(mydata$Age)
print(avg)

#Q4 How many children were entertained?
count<-0
for(i in 1:length(mydata$Age)){
  if(mydata$Age[i]>=1 & mydata$Age[i]<= 12){
    count<-count+1
  }}
print(count)

#Q5 Which gender type had what kind of procedure in abundance?
mydata$Sex[mydata$Sex==""]<-"NA"
mydata$Sex[mydata$Sex=="-"]<-"NA"
mydata$Sex<-gsub("f","F",mydata$Sex)
Male<-mydata$Procedure[mydata$Sex=='M']
max(table(Male))
Female<-mydata$Procedure[mydata$Sex=='F']
max(table(Female))

#Q6 Which Doctor is earning highest?
mydata$TotalCharges<-as.character(mydata$TotalCharges)
mydata$TotalCharges[mydata$TotalCharges=="Cancelled"]<-"0"
mydata$TotalCharges[mydata$TotalCharges==""]<-"0"
mydata$TotalCharges[mydata$TotalCharges=="nan"]<-"0"
mydata$TotalCharges<-as.double(mydata$TotalCharges)
total<-aggregate(mydata$TotalCharges,by=list(category=mydata$ConsultingDoctor),FUN=sum)
print(total)

#Q7 Which procedure type earns more money?
total<-aggregate(mydata$TotalCharges,by=list(category=mydata$Procedure),FUN=sum)
print(total)

#Q8 Which time of the day has highest frequency of visits by hour? 
mydata$Time<-as.POSIXct(mydata$Time,format="%I:%M %p")
hr<-hour(mydata$Time)
table(hr)
print(table(hr))

#Q9 Create a bracket of time by Morning, Afternoon, Evening, Night
timeBracket <- assign("list", NULL, envir = .GlobalEnv)
timeBracket[hr >= 6 & hr <= 12] <- "Morning"
timeBracket[hr > 12 & hr <= 16] <- "Afternoon"
timeBracket[hr > 16 & hr <= 19] <- "Evening"
timeBracket[hr > 19] <- "Night"
mydata <- mutate(mydata, timeBracket)

#Q10 How many patients are repeated visitors?
visitors<-count(mydata,id)
visitors<-visitors$id[visitors$n>1]
print(length(visitors))

#Q11 Give us the id of repeated visitors.
print(visitors)

#Q12 Which patients visited again for the same problem?
sameProb<-count(mydata,id,Procedure)
sameProb<-sameProb$id[sameProb$n>1]
print(sameProb)

#Q13 What is the median age for Females and Males? 
med_m<-median(mydata$Age[mydata$Sex=="M"],na.rm = TRUE)
med_Fmale<-median(mydata$Age[mydata$Sex=="F"],na.rm = TRUE)
print(med_m)
print(med_Fmale)

#Q14 What is the total amount in balance?
mydata$AmountBalance<- as.character(mydata$AmountBalance)
mydata$AmountBalance<-gsub(",","",mydata$AmountBalance)
mydata$AmountBalance<-gsub(" ","",mydata$AmountBalance)
mydata$AmountBalance<-gsub("-","0",mydata$AmountBalance)
mydata$AmountBalance[mydata$AmountBalance==""]<-"0"
mydata$AmountBalance<- as.double(mydata$AmountBalance)
maxv<-sum(mydata$AmountBalance)
print(maxv)

#Q15 How much money was made by Procedure Type "Consultation"? 
print(sum(mydata$TotalCharges[mydata$Procedure=="Consultation"],na.rm = TRUE))

#Q16 Is there a relation between Age and Total Charges paid? 
print(cor(mydata$Age,mydata$TotalCharges))

#Q17 Which Age group had highest number of visits? 
hist(mydata$Age)

#Q18 What is the total cost earned by Procedure Type X Ray and Scalling together?
print(sum(mydata$TotalCharges[mydata$Procedure=="X Ray" | mydata$Procedure=="Scalling"]))
write.csv(mydata,file="C:/Users/ashar.burney/Documents/GitHub/R-Assignment-2/updated_hospital_dataset.csv")
