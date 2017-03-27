# import Modules and data
library(dplyr)
library(tidyr)
library(lubridate)

raw_data <- read.csv("C:/Users/shahid.ammer/Documents/Shahid_Ammer_ISB_R_Assesment2/hospitaldata.csv")
mydf = tbl_df(raw_data)
mydf
