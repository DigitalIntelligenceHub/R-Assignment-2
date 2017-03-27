install.packages("dplyr")
library(dplyr)
mydf <- read.csv("hospitaldata.csv",stringsAsFactors = FALSE)
hos <- tbl_df(mydf)
rm(mydf)

#Question 01  
names(hos) <- gsub(".", "", names(hos), fixed = TRUE)
names(hos)
