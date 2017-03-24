library(readr)
library(dplyr)
library(plyr)
library(tidyr)
library(mosaic)
library(data.table)

hosp_data <- read_csv("~/DIH/Assignment/Assignment 2/hospitaldata.csv")
View(hosp_data)

###Q1, remove the dots in the names?
names(hosp_data) <- gsub(".", "", names(hosp_data), fixed = TRUE)

###Q2, Which day of the week is expected to have most visits? Ans = Monday 51
hosp_data <- separate(hosp_data , Date , c("day" , "date") , sep="," , extra = "merge") ##making aseparate column Day which contains the weekday
day_occurances <- table(unlist(hosp_data$day))
max(day_occurances)

    
###Q3, What is the average age of patients? Ans==32.40

hosp_data$Age <- gsub("-", NA , hosp_data$Age , fixed = T)
hosp_data$Age <- gsub("", NA , hosp_data$Age , fixed = T)
hosp_data$Age <- gsub(" ", NA , hosp_data$Age, fixed = T)

month2year <- function(x){
  print(x)
  if(is.na(x) == FALSE & grepl("M" , x)){
    age_year <- gsub("M", "",  x , fixed = T)
    age_year <- as.integer(age_year)
    age_year<-age_year/12
    print(age_year)
    return(age_year)
    }
  else{
    x <- as.integer(x)
    return(x)
    }
}

hosp_data$Age <- Map({function (a)  month2year(a)}, hosp_data$Age)
hosp_data$Age <- as.integer(hosp_data$Age)mean_age <- mean(hosp_data$Age, na.rm = TRUE)
print(mean_age)


###Q4, 4. How many children were entertained? (Make a Bracket of Age from 1-12)  Ans == 24
child_count <- filter(hosp_data , Age <=12 & Age >= 1)
nrow(child_count)


###Q5Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem 
