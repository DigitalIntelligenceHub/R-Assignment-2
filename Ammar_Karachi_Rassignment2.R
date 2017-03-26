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

