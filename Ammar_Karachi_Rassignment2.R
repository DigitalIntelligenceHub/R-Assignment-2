# Reading the dataset
df <- read.csv("C:/Users/Ammar/Documents/hospitaldata.csv",header=TRUE,sep=",")

#Question 1. Remove the dots in the names, so it may become easier for you to work through it
# Assigning column names after substituting dots from it.
colnames(df) <- gsub("\\.", "", colnames(df))
