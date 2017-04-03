library(dplyr)
library(lubridate)
library(tidyr)

# Loading Data 

hosp_tbl <- read.csv("C:/Users/ittrat.wafa/Documents/Tasks/Week 2/IttratWafa_Rwp_RAssignment2/hospitaldata.csv")

# Removing dots from Column Headers

names(hosp_tbl) <- gsub("\\.", " ", names(hosp_tbl)) 

#Converting Simple Data Frame into Tbl

hosp_tbl <- tbl_df(hosp_tbl)

# Which day of the week is expected to have most visits?

hosp_tbl <- hosp_tbl %>%  
  separate(Date, c("Day","Date"), sep ="," , extra = "merge")

most_visit_day <- hosp_tbl %>% 
                    group_by(Day) %>% 
                    summarise(visit = n())%>%
                    filter(visit  == max(visit))
most_visit_day

#What is the average age of patients?

hosp_tbl$Age <- gsub("M", " " ,hosp_tbl$Age)
hosp_tbl$Age <- gsub("-", NA ,hosp_tbl$Age)
hosp_tbl$Age <- gsub("\\s", NA ,hosp_tbl$Age)
hosp_tbl$Age <- as.numeric(hosp_tbl$Age)

avg_age_pat <- mean(hosp_tbl$Age, na.rm = TRUE)
avg_age_pat

#How many children were entertained? (Make a Bracket of Age from 1-12)

no_of_children <- sum(hosp_tbl$Age <= 12 & hosp_tbl$Age > 0, na.rm = TRUE)
no_of_children

#Which gender type had what kind of procedure in abundance? 
#i.e. Female visit mostly because of Gynae Problem

hosp_tbl$Sex <- gsub("f", "F", hosp_tbl$Sex)
hosp_tbl$Sex <- gsub("-", NA, hosp_tbl$Sex)
hosp_tbl$Sex <- gsub("\\s", NA, hosp_tbl$Sex)

highest_no_procedure <- hosp_tbl %>%
                        group_by(Sex, Procedure) %>%
                        summarise(count = n())%>%
                        filter(count == max(count), Sex != "")
highest_no_procedure
                        
#Which Doctor is earning highest?

hosp_tbl$`Total  Charges` <- as.numeric(as.character(hosp_tbl$`Total  Charges`))

highest_earning_doc <- hosp_tbl %>%
                        group_by(`Consulting  Doctor`) %>%
                        summarise(Total = sum(`Total  Charges`, na.rm = TRUE)) %>% 
                        filter(Total == max(Total))
highest_earning_doc

#Which procedure type earns more money?

highest_earning_procedure <- hosp_tbl %>%
                              group_by(Procedure) %>%
                              summarise(Total = sum(`Total  Charges`, na.rm = TRUE)) %>%
                              filter(Total == max(Total))
highest_earning_procedure

#Which time of the day has highest frequency of visits by hour?

hosp_tbl$Time <- format(strptime(hosp_tbl$Time, "%I:%M%p"), "%H:%M")

freq_hour <- hosp_tbl %>%
              group_by(T = hour(hm(Time))) %>%
              summarise(freq = n()) %>%
              filter(freq == max(freq), !is.na(T))
freq_hour

#Create a bracket of time by 
#Morning, Afternoon, Evening, Night (6am - 12pm - Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night).
breaks <- c(6, 12, 16, 19, 23)
labels <- c("Morning","Afternoon","Evening", "Night")
Te = hour(hm(hosp_tbl$Time))

hosp_tbl <- hosp_tbl%>%
                mutate('Time Bracket' = cut(Te, breaks, labels, include.lowest = TRUE))

#How many patients are repeated visitors?

rep_visitor <- hosp_tbl %>%
                group_by(id) %>%
                summarise(freq = n())
repeated_visitors <- sum(rep_visitor$freq >1)
repeated_visitors

#Give us the id of repeated visitors.

rep_id <- rep_visitor %>%
            filter(freq > 1) %>%
            select(id)
rep_id

#Which patients visited again for the same problem?  

pat_visit_id <- hosp_tbl %>%
              group_by(id) %>%
              count(Specialty, sort = TRUE)%>%
              filter(n >1)%>%
              select(id)
pat_visit_id

#What is the median age for Females and Males?

median <- hosp_tbl %>%
            group_by(Sex) %>%
            summarise(Med = median(as.numeric(unlist(Age)), na.rm = TRUE))%>%
            filter(!is.na(Sex))
median

#What is the total amount in balance?

hosp_tbl$`Amount  Balance` <- gsub("-", NA, hosp_tbl$`Amount  Balance`)
hosp_tbl$`Amount  Balance` <- gsub("\\s", NA, hosp_tbl$`Amount  Balance`)
hosp_tbl$`Amount  Balance` <- gsub(",", "", hosp_tbl$`Amount  Balance`)
hosp_tbl$`Amount  Balance` <- gsub("\\.00$", "", hosp_tbl$`Amount  Balance`)
hosp_tbl$`Amount  Balance` <- as.numeric(hosp_tbl$`Amount  Balance`)

Total_Amount_Balance <- hosp_tbl %>%
                          select(`Amount  Balance`)%>%
                          summarise(sum(`Amount  Balance`, na.rm = TRUE))
Total_Amount_Balance

#How much money was made by Procedure Type "Consultation"?

Total_Money_Made <- hosp_tbl %>%
                      filter(Procedure == "Consultation") %>%
                      summarise(Money_Made = sum(`Total  Charges`, na.rm = TRUE))
Total_Money_Made

#Is there a relation between Age and Total Charges paid?

cor.test(hosp_tbl$Age, hosp_tbl$`Total  Charges`)
#Yes, related!
plot(hosp_tbl$Age, hosp_tbl$`Total  Charges`)

#Which Age group had highest number of visits?

highest_visit <- hosp_tbl%>% 
                  group_by(Age = cut(hosp_tbl$Age, breaks = seq(0,80,5))) %>% 
                  summarise(highest_number_visits = n()) %>%
                  filter(highest_number_visits == max(highest_number_visits))
highest_visit

#What is the total cost earned by Procedure Type X Ray and Scalling together?

total_cost_earned <- hosp_tbl %>%
                      filter(Procedure %in% c("Scalling" , "X Ray"))%>%
                      select(`Total  Charges`) %>%
                      summarise(Money_Made = sum(`Total  Charges`, na.rm = TRUE))
total_cost_earned

#Saving Clean Dataset
write.csv(hosp_tbl, "C:/Users/ittrat.wafa/Documents/Tasks/Week 2/IttratWafa_Rwp_RAssignment2/Clean_hospitaldata.csv")