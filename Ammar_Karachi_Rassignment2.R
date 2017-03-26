library(plotly)
library(lubridate)
library(dplyr)
# Reading the dataset
df <- read.csv("C:/Users/Ammar/Documents/hospitaldata.csv",header=TRUE,sep=",")

#Question 1. Remove the dots in the names, so it may become easier for you to work through it
# Assigning column names after substituting dots from it.
colnames(df) <- gsub("\\.", "", colnames(df))

#Question 2. Which day of the week is expected to have most visits?
#Table shows that the Monday is the day expected to have most visits.
strDate <- as.character(df$Date)
strDate <- strsplit(strDate, ",")
strWday <- list()
for(i in 1:length(strDate)){
  strWday[i] <- strDate[[i]][1]
}
strWday <- unlist(strWday, recursive = FALSE)
maxDay <- table(strWday)
print(maxDay)

# Question 3. What is the average age of patients?
# By removing "M" and replacing "","-" with 0, we get 28.4 as the average age of patient
df$Age <- as.character(df$Age)
df$Age[df$Age == ""] <- "0"
df$Age[df$Age == "-"] <- "0"
df$Age <- gsub("M", "", df$Age)
df$Age <- as.double(df$Age)
average_age = mean(df$Age)
print(average_age)

# Question 4. How many children were entertained? (Make a Bracket of Age from 1-12) 
# 24 children were entertained.
children_count <- 0
for(i in 1:length(df$Age)){
  if(df$Age[i] > 0 & df$Age[i] <= 12)
    children_count <- children_count + 1
}
print(children_count)

# Question 5. Which gender type had what kind of procedure in abundance?
df$Sex[df$Sex==""] <- "NA"
df$Sex[df$Sex=="-"] <- "NA"
df$Sex <- gsub("f", "F", df$Sex)
plot_ly(x = df$Procedure[df$Sex=="M"], type = "histogram")
table(df$Procedure[df$Sex=="M"])
plot_ly(x = df$Procedure[df$Sex=="F"], type = "histogram")
table(df$Procedure[df$Sex=="F"])

# Question 6. Which Doctor is earning highest?
# First we clean list of charges then converting list of total charges to double. 
# Finally using aggregate function to group according to doctors and taking
# sum at same time. Taking max from the list gives us the answer i.e. 'Dr Alaf Khan: 513050'
df$TotalCharges <- as.character(df$TotalCharges)
df$TotalCharges[df$TotalCharges==""] <- "0"
df$TotalCharges[df$TotalCharges=="Cancelled"] <- "0"
df$TotalCharges <- as.double(df$TotalCharges)
totalOfEachDoc <- aggregate(df$TotalCharges, by=list(Category=df$ConsultingDoctor), FUN=sum)
print(totalOfEachDoc$Category[totalOfEachDoc$x == max(totalOfEachDoc[[2]])])

# Question 7. Which procedure type earns more money? 
# Following the steps of last question, the procedure type which earns most money is "Orthodontics"
totalOfEachProc <- aggregate(df$TotalCharges, by=list(Category=df$Procedure), FUN=sum)
print(totalOfEachProc$Category[totalOfEachProc$x == max(totalOfEachProc[[2]])])

# Question 8. Which time of the day has highest frequency of visits by hour? 
# This line of code counts the frequency of each unique time the dataset has. It gives 29 as the highest frquency
# which means 13:00 is the most busiest hour of the day. Plot also shows the same.
df$Time <- as.POSIXct(df$Time,format="%I:%M %p")
busyHours <- hour(df$Time)
freqTable <- table(busyHours)
plot_ly(x = busyHours, type = "histogram", autobinx = TRUE)

# Question 9. Create a bracket of time by Morning, Afternoon, Evening, Night (6am - 12pm - Morning, 12 pm- 4 pm, 
# Afternoon, 4 pm- 7pm, Evening, 7pm - 6am, Night).
# Because the time was in 24hour format I converted time according to it and also the last condition was changed due to
# the 24hours format.
dayTime <- list()
dayTime[busyHours >= 6 & busyHours <= 12] <- "Morning"
dayTime[busyHours > 12 & busyHours <= 16] <- "Afternoon"
dayTime[busyHours > 16 & busyHours <= 19] <- "Evening"
dayTime[busyHours > 19] <- "Night"
dayTime[is.na(busyHours)] <- "NA"
dayTime <- unlist(dayTime, recursive = FALSE)
df <- mutate(df, dayTime)

# Question 10. How many patients are repeated visitors?
# 37 patients are repeated visitors
groupOfrepeatedVisitors <- count(df,id)
groupOfrepeatedVisitors <- groupOfrepeatedVisitors$id[groupOfrepeatedVisitors$n>1]
print(length(groupOfrepeatedVisitors))

# Question 11. Give us the id of repeated visitors.
print(groupOfrepeatedVisitors)

# Question 12. Which patients visited again for the same problem? 
# Group by id and procedure and selecting only values which are greater than 1
sameProblem <- count(df,id,Procedure)
sameProblem <- sameProblem$id[sameProblem$n > 1]
print(sameProblem)

# Question 13. What is the median age for Females and Males? 
medM <-median(df$Age[df$Sex == "M"], na.rm = TRUE)
cat("Median for male is",medM)
medF <-median(df$Age[df$Sex == "F"], na.rm = TRUE)
cat("Median for female is",medF)

# Question 14. What is the total amount in balance? 
# Removing all the nondigits. And taking the sum. The answer is Rs.222500
df$AmountBalance <- as.character(df$AmountBalance)
df$AmountBalance <- gsub(" ","", df$AmountBalance)
df$AmountBalance <- gsub(",","", df$AmountBalance)
df$AmountBalance[df$AmountBalance=="-"] <- "0"
df$AmountBalance[df$AmountBalance==""] <- "0"
df$AmountBalance <- as.double(df$AmountBalance)
print(sum(df$AmountBalance))

#Question 15. How much money was made by Procedure Type "Consultation"?
# Summing up the amount after excluding Cancelled from it gives us Rs. 83,950.
print(sum(df$TotalCharges[df$Procedure=="Consultation"]))

# Question 16. Is there a relation between Age and Total Charges paid? 
# Age list is taken form previous question. The plot is not giving satifying answer but the value of correlation
# says that there is no strong relation between Age and Total Charges
print(cor(df$Age, df$TotalCharges))
qplot(df$Age,df$TotalCharges)

# Question 17. Which Age group had highest number of visits?
# Histogram shows that Age group of 20-40years old visits the most.
plot_ly(x = df$Age, type = "histogram", nbinsx = 5)

# Question 18.  What is the total cost earned by Procedure Type X Ray and Scalling together?
# The sum of X Ray and Scalling is Rs. 22300.
print(sum(df$TotalCharges[df$Procedure == 'X Ray' | df$Procedure == 'Scalling']))
