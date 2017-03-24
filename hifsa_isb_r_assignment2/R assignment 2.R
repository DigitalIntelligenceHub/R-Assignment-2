library(dplyr)
library(lubridate)
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

  
