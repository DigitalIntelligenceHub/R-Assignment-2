library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
"removed the dots in the names"
names(hospitaldata) <- gsub(x = names(hospitaldata),
                        pattern = "\\.",
                        replacement = " ")
names(hospitaldata)

"day of the week is expected to have most visits"
df<-hospitaldata$Date
df<-wday(mdy(df), label=TRUE)
df
table(df)

"What is the average age of patients"
nage<-as.numeric(hospitaldata$Age) 
mean(nage, na.rm = TRUE)

"How many children were entertained"
age<-as.numeric(hospitaldata$Age)
ages<-subset(age, age >= 1 & age < 12)
length(ages)

"Which gender type had what kind of procedure in abundance? 
i.e. Female visit mostly because of Gynae Problem"
select(group_by(hospitaldata, Procedure, Sex) ) %>%
table() %>%
  View()
