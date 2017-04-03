#Question 10
repeated_visitors <- select(dataf,id) %>%
  group_by(id) %>%
  summarise(visits=length(id)) %>%
  arrange(desc(visits)) %>%
  filter(visits > 1) %>%
  print   # printing repeated visitors along with their ids

