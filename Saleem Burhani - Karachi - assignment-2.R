# R Assignment 2
# 22 March 2017
# Completed 27 march 2017
# Saleem Burhani

# load libraries
library(swirl)
#onetime usage
#swirl::install_course("Getting and Cleaning Data")
#library(datasets)
library(dplyr)
#library(sparklyr)
#library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)

# load dataset
path2csv = "C:\\Users\\biuser\\Desktop\\DIH\\22mar17-R&P\\hospitaldata.csv"
mydfCSV <- read.csv(path2csv,stringsAsFactors=FALSE)



"Task ........1"
"Please remove the dots in the names, so it may become easier for you to work through it."
" "
# Name column is 
mydfcolnames <- colnames(mydfCSV)
mydfcolnames <- gsub(".."," ",mydfcolnames,fixed=TRUE)
mydfcolnames <- gsub("."," ",mydfcolnames,fixed=TRUE)
colnames(mydfCSV) <- mydfcolnames
head(mydfCSV)

# Remove all "Nursing Staff" records which are not required
mydf <- filter(mydfCSV,mydfCSV$`Consulting Doctor` != "Nursing Staff")

"Task 2"
"Which day of the week is expected to have most visits?"
" "

# create column have week day
mydf <- mutate(mydf,Week_Day = substr(mydf$Date,0,str_locate(mydf$Date,",")-1))
# count records group by week_day 
patient_by_week_day <- aggregate(rep(1, length(mydf$Week_Day)),by=list(mydf$Week_Day), sum)
# get the day having max records
max_patient_in_a_week_day <- max(patient_by_week_day$x, na.rm = TRUE)
filter(patient_by_week_day, x == max_patient_in_a_week_day)

"Task 3"
"What is the average age of patients?"
" "
newdf <- mydf
newdf$age <- as.numeric(mydf$Age, na.rm=TRUE)
mean(newdf$age, na.rm=TRUE)


"Task 4"
"How many children were entertained? (Make a Bracket of Age from 1-12)"
" "
newdf <- mutate(newdf, adult_child = ifelse (age>=1 & age<=12,"Child","Adult"))
count(newdf,adult_child)

"Task 5"
"Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem"
" "
#filter(newdf, Sex == "M")
temp <- count(newdf,Sex,Specialty,sort=TRUE)
tempM <- filter(temp,Sex == "M")
tempF <- filter(temp,Sex == "F")
tempM[1,]
tempF[1,]
#max(tempM$n, na.rm=TRUE)
#max(temp$n, na.rm = TRUE)

"Task 6"
"Which Doctor is earning highest?"
" "
newdf$`Amount Received ` <- as.numeric(mydf$`Amount Received `, na.rm=TRUE)
doctors <- aggregate(newdf$`Amount Received `, by=list(Doctor=newdf$`Consulting Doctor`), FUN=sum)
filter(doctors, x == max(doctors$x, na.rm=TRUE))

"Task 7"
"Which procedure type earns more money?"
" "
Procedures <- aggregate(newdf$`Amount Received `, by=list(Procedures=newdf$Procedure), FUN=sum)
filter(Procedures, x == max(Procedures$x, na.rm=TRUE))

"Task 8"
"Which time of the day has highest frequency of visits by hour?"
" "
# Correct HH:MM:SS XM"
Times <- newdf$Time
Times <- gsub("AM",":00 AM",Times,fixed=TRUE)
Times <- gsub("PM",":00 PM",Times,fixed=TRUE)
for (i in 1:NROW(Times)){
  if ((is.na(Times))==FALSE){
  if (substr(Times,6,6) == "") {
    Times[i] = paste(Times[i],":00 AM",sep ="")
  }
  }# is.na
}
# add new column with all NA
newdf <- mutate(newdf,time24 = hm(Time))
# add 12 hours to PM value and update the new mutated column
Times2 = 0
Times2 <- hms(Times, na.rm=TRUE)
for (i in 1:NROW(newdf)){
  if (grepl("P", Times[i])==TRUE){
   Times2[i] <- hms(Times[i]) + hours(12)
   #cntPM = cntPM + 1
  }
  if (grepl("A", Times[i])==TRUE){
    Times2[i] <- hms(Times[i])
    #cntAM = cntAM +1
  }
  # Update value in data fram
  newdf$time24[i] = Times2[i]
}
cnt = count(newdf,hour(time24),sort=TRUE)
cnt[1,]


"Task 9"
"Create a bracket of time by Morning, Afternoon, Evening, Night (6am - 12pm - Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night)."
" "

newdf <- mutate(newdf,TimeBracket = Time)
#TB <- newdf$TimeBracket
for (i in 1:NROW(newdf)){
  if ((is.na(newdf$time24[i]))==FALSE){
    newdf$TimeBracket[i] = paste("NA")
  # Morning
  if ((hour(newdf$time24[i]) >= 6)==TRUE){
    if ((hour(newdf$time24[i]) < 12)==TRUE){
      newdf$TimeBracket[i] = paste("Morning")
    }
  }
  # Afternoon
  if ((hour(newdf$time24[i]) >= 12)==TRUE){
    if ((hour(newdf$time24[i]) < 16 )==TRUE){
      newdf$TimeBracket[i] = paste("Afternoon")
    }
  }
  # Evening
  if ((hour(newdf$time24[i]) >= 16)==TRUE){
    if ((hour(newdf$time24[i]) < 19 )==TRUE){
      newdf$TimeBracket[i] = paste("Evening")
    }
  }
  # Night
  if ((hour(newdf$time24[i]) >= 19)==TRUE){
    if ((hour(newdf$time24[i]) < 6 )==TRUE){
      newdf$TimeBracket[i] = paste("Night")
    }
  }
  }# master false
}
# Display Time Bracket
newdf$TimeBracket


"Task 10"
"How many patients are repeated visitors?"
" "
# ID is patient
cnt = count(newdf,id,sort=TRUE)
count(cnt)

"Task 11"
"Give us the id of repeated visitors."
" "
cnt = count(newdf,id,sort=TRUE)
filter (cnt, cnt$n > 1)

"Task 12"
"Which patients visited again for the same problem?"
" "
cnt = count(newdf,id,Specialty,sort=TRUE)
cnt = filter (cnt, n > 1)
cnt

"Task 13"
"What is the median age for Females and Males?"
" "
newdfMale <- filter(newdf,Sex == "M")
"Average Male Age"
mean(newdfMale$age,na.rm=TRUE)

newdfFemale <- filter(newdf,Sex == "F")
"Average Female Age"
mean(newdfFemale$age,na.rm=TRUE)


"Task 14"
"What is the total amount in balance"
" "
# Remove comma before converting to numeric
#newdf$`Amount Balance` <- mydf$`Amount Balance`
newdf$`Amount Balance` <- gsub(",","",newdf$`Amount Balance`,fixed=TRUE)
newdf$`Amount Balance` <- as.numeric(newdf$`Amount Balance`, na.rm=TRUE)
"Total Amount Balance :"
sum(newdf$`Amount Balance`,na.rm=TRUE)

"Task 15"
"How much money was made by Procedure Type 'Consultation'"
" "
newdf$`Amount Received ` <- as.numeric(newdf$`Amount Received `, na.rm=TRUE)
"Revenue Generated By Procedure : Consultation "
sum(newdf$`Amount Balance`,newdf$Procedure=='Consultation', na.rm=TRUE)

"Task 16"
"Is there a relation between Age and Total Charges paid?"
" "
newdf$`Total Charges` <- gsub(",","",newdf$`Total Charges`,fixed=TRUE)
newdf$`Total Charges` <- as.numeric(newdf$`Total Charges`, na.rm=TRUE)
plot(newdf$Age, newdf$`Total Charges`)
#ggplot(data=newdf, aes(x=Age, y=`Total Charges`)) + theme_bw() + geom_line() #+ facet_wrap(~ variable)


"Task 17"
"Which Age group had highest number of visits?"
" "
"Age Group and Number of Visits"
count(newdf,adult_child,sort=TRUE)


"Task 18"
"What is the total cost earned by Procedure Type X Ray and Scalling together?"
" "
newdf$`Total Charges` <- gsub(",","",newdf$`Total Charges`,fixed=TRUE)
newdf$`Total Charges` <- as.numeric(newdf$`Total Charges`, na.rm=TRUE)
"Total Cost Earned By Procedures 'X Ray' and 'Scalling' :"
sum(newdf$`Total Charges`,newdf$Procedure=='X Ray', na.rm=TRUE) + sum(newdf$`Total Charges`,newdf$Procedure=='Scalling', na.rm=TRUE)

"Finaly Write the clean data set"
# load dataset
#path2csv = "C:\\Users\\biuser\\Desktop\\DIH\\22mar17-R&P\\hospitaldata.csv"
write.csv(newdf, file = "C:\\Users\\biuser\\Desktop\\DIH\\22mar17-R&P\\clean-data.csv")

" *** END ***"