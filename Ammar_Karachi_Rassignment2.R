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