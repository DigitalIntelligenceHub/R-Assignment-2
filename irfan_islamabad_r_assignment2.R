# Load the file to Variable MyData
library(dplyr)
library(tidyr)
library(lubridate)
datahosp <- read.csv(file="hospitaldata.csv", header=TRUE, sep=",")
datahosp = tbl_df(datahosp)
# Q1. Please remove the dots in the names, so it may become easier for you to work through it

names(datahosp) <- gsub(x = names(datahosp), pattern = "\\.", replacement = " ")
names(datahosp) <- gsub(x = names(datahosp), pattern = "  ", replacement = " ")
names(datahosp)

