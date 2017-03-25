library(dplyr)
library(lubridate)
library(tidyr)
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
"Which gender type had what kind of procedure in abundance? 
i.e. Female visit mostly because of Gynae Problem"
table(group_by(hospitaldata, "Procedure", "Sex") )
