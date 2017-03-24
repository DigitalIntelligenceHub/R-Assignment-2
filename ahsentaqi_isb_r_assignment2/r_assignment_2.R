hsptldf <- read_csv("D:/DIH/Assigments/R-Assignment-2/ahsentaqi_isb_r_assignment2/hospitaldata.csv")
View(hsptldf)
hsptldata <- tbl_df(hsptldf)
rm(hstpldf)
hsptldata


## Question 1 

names(hsptldata) <- gsub(".", "", names(hsptldata), fixed = TRUE)
names(hsptldata)
