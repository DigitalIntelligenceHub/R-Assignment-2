library(plyr)
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
a<-hospitaldata$`Total  Charges`
max(a, na.rm = TRUE, filter =c(hospitaldata$`Amount Received By`) )

"alternatively max(a, na.rm = TRUE, select =c(hospitaldata$`Amount Received By`) ) and 
hospitaldata$`Amount Received By`[which.max(a)] giving same results "

"7. Which procedure type earns more money?"
max(a, na.rm = T, filter = c(hospitaldata$Procedure))

"8. Which time of the day has highest frequency of visits by hour?"

"9. Create a bracket of time by 
Morning, Afternoon, Evening, Night 
(6am - 12pm - Morning, 12 pm- 4 pm, 
Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night)."

"10. How many patients are repeated visitors?"
b<- ddply(hospitaldata,.(id),nrow )
repeated <-subset(b, b$V1>1)
View(repeated)

"11. Give us the id of repeated visitors."
View(repeated)

"12. Which patients visited again for the same problem"
select(group_by(hospitaldata, id,  Specialty))%>%
     View()
