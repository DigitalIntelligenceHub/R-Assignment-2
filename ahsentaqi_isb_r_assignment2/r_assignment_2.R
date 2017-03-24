hsptldf <- read_csv("D:/DIH/Assigments/R-Assignment-2/ahsentaqi_isb_r_assignment2/hospitaldata.csv")
View(hsptldf)
hsptldata <- tbl_df(hsptldf)
rm(hstpldf)
hsptldata


## Question 1 

names(hsptldata) <- gsub(".", "", names(hsptldata), fixed = TRUE)
names(hsptldata)

## Question 2 

#separating weekdays, day of month and year column to get the weedays in separate column
hsptldata <-  separate( hsptldata, Date , into = c("day_only" , "date_only", "year"), sep = ",")

#counting weekdays to get total visits on a each day
days_count <- count(hsptldata, day_only)

#getting num of max visits
max_visits <- max(days_count$n)
print(days_count)
print(max_visits)
