library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")

getwd()
setwd("C:/Users/muhammad.ihtsham/Documents")
mydata <- read.csv("hospitaldata.csv")

# 1: Removing dots from column names
names(mydata) <- gsub("\\.", "", names(mydata))