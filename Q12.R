library("ggplot2")
library("lubridate")
library("dplyr")
library("tidyr")
library("DataCombine")

bufferedDataFrame <- read.csv("hospitaldata.csv", strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F)
dataf <- tbl_df(bufferedDataFrame)
View(dataf)
glimpse(dataf)

#removing character from age M
dataf$Age <- as.numeric(gsub("[^0-9]",'',dataf$Age))
class(dataf$Age)
unique(dataf$Age)


# Question #1 
#Changing Datatypes from factor to their required formats
names(dataf) <- gsub("\\.",'',names(dataf))

#now lets play with date
dataf$Date <- as.Date(strptime(dataf$Date, "%a, %B %d, %Y"))


dataf %>% 
  count(id, Procedure) %>%
  slice(which(n>1))%>%
  filter(!is.na(Procedure))%>%
  select(id,Procedure)