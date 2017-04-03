library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")

getwd()
setwd("C:/Users/muhammad.ihtsham/Documents")
mydata <- read.csv("hospitaldata.csv")

# Converting to table format
mydata <- tbl_df(mydata)

# Formatting date & time columns
mydata$Date <- as.Date(strptime(mydata$Date, "%a, %B %d, %Y"))
mydata$Time <- format(strptime(mydata$Time, format='%I:%M %p'), '%I:%M %p')

# Removing characters from age
mydata$Age <- as.numeric(gsub("[^0-9]",'',mydata$Age))

# Changing sex characters to uppercase
mydata$Sex <- toupper(mydata$Sex)

# Remove Cancelled to NA in total charges
mydata$TotalCharges <- as.numeric(gsub("cancelled", NA, ignore.case = TRUE, mydata$TotalCharges))

# Remove Cancelled to NA in procedure
mydata$Procedure <- (gsub("cancelled", NA, ignore.case = TRUE, mydata$Procedure))


# Coverting AmountBalance to numeric
mydata$AmountBalance <- as.numeric(gsub(",",'',mydata$AmountBalance))
class(mydata$AmountBalance)


# 1: Removing dots from column names
names(mydata) <- gsub("\\.", "", names(mydata))


# 2: Most visits in a day
print(paste("Most visited day of the week is",
            weekdays(mydata$Date[which(table(mydata$Date) == max(table(mydata$Date)))])))

# 3: Average age of patients
mean(mydata$Age, na.rm = TRUE)

# 4: How many children were entertained (Age: 1-12)
child <- which(mydata$Age >= 1 & mydata$Age <= 12)
length(child)

# 5: Visit in abundance by gender wise
mydata %>%
  group_by(Sex, Procedure) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Sex)) %>%
  View()

# 6: Which doctor is earning highest
highest_earner <- mydata %>%
  group_by(ConsultingDoctor) %>%
  summarise(total=sum(TotalCharges), na.rm = TRUE) %>%
  arrange(TotalCharges)

View(highest_earner)

# 7: Which procedure type earns highest

highest_procedure <-  mydata %>%
  group_by(Procedure) %>%
  summarise_each(funs(sum(TotalCharges, na.rm = TRUE))) %>%
  select(Procedure, TotalCharges) %>%
  filter(!is.na(Procedure)) %>%
  arrange(desc(TotalCharges))

View(highest_procedure)

# 8: Time with highest frequency
highest_freq_time <-  mydata %>%
  filter(!is.na(Date), !is.na(Time)) %>%
  group_by(Date,Time) %>%
  tally() %>%
  arrange(desc(n))
View(highest_freq_time)

# 10: Repeated visitors
visitor <-  mydata %>%
  group_by(id) %>%
  tally() %>%
  filter(n > 1) %>%
  arrange(desc(n))

View(visitor)

# 11: ID of repeated visitors
print(visitor$id)

# 12: Patients visited again for same problem
patients_repeated <-  mydata %>%
  group_by(id, Procedure) %>%
  tally() %>%
  filter(!is.na(Procedure), n > 1) %>%
  arrange(desc(n))

View(patients_repeated)

# 13: Median age
mydata %>% 
  group_by(Sex) %>%
  summarise(median(Age, na.rm = TRUE))

# 14: Total amount
paste("Total amount is",sum(mydata$AmountBalance, na.rm = TRUE))

# 15: Money made by consultation
mydata %>%
  group_by(Procedure) %>%
  summarise(Total_Amount = sum(TotalCharges, na.rm = TRUE)) %>%
  filter(Procedure %in% c("Consultation"))

# 16: Relation b/w age & total charges
cor.test(mydata$Age, mydata$TotalCharges)

# 17: Highest age group visits
highest_age_visit <- mydata %>%
  group_by(Age) %>%
  tally() %>%
  filter(!is.na(Age)) %>%
  arrange(desc(n))
View(highest_age_visit)

# 18: Total cost by x-ray and scalling
Total_cost <- mydata %>%
  filter(Procedure %in% c("X Ray", "Scalling")) %>%
  summarise(Total = sum(TotalCharges, na.rm = TRUE)) 
View(Total_cost)








