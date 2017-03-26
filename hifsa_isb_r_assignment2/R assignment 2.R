library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
"1. removed the dots in the names"
names(hospitaldata) <- gsub(x = names(hospitaldata),
                        pattern = "\\.",
                        replacement = " ")
names(hospitaldata)

"2. day of the week is expected to have most visits"
df<-hospitaldata$Date
df<-wday(mdy(df), label=TRUE)
df
table(df)

"3. What is the average age of patients"
nage<-as.numeric(hospitaldata$Age) 
mean(nage, na.rm = TRUE)

"4. How many children were entertained"
age<-as.numeric(hospitaldata$Age)
ages<-subset(age, age >= 1 & age < 12)
length(ages)

"5. Which gender type had what kind of procedure in abundance? 
i.e. Female visit mostly because of Gynae Problem"
select(group_by(hospitaldata, Procedure, Sex) ) %>%
table() %>%
  View()

"6. Which Doctor is earning highest?"
a<-hospitaldata$`Amount  Received `
max(a, na.rm = TRUE, filter =c(hospitaldata$`Amount Received By`) )

"alternatively max(a, na.rm = TRUE, select =c(hospitaldata$`Amount Received By`) ) and 
hospitaldata$`Amount Received By`[which.max(a)] giving same results "