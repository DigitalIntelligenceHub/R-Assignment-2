#Question 13
medianAge <- dataf %>% 
  select(Sex,Age) %>%
  group_by(Sex) %>%
  summarise(median(Age, na.rm = TRUE)) %>%
  print

