#Question 4
child <- filter(dataf, Age > 1 & Age < 13) %>%
  select(-(Date:Time)) %>%
  select(-(Sex:NextApt)) %>%
  count() %>%
  print

