#Question 8
hour_visits <-  dataf %>%
  select(Time) %>%
  mutate(Hour = hour(hm(format(strptime(dataf$Time, "%I:%M %p"), "%H:%M")))) %>%
  group_by(Hour) %>%
  summarize(visits=length(Hour)) %>%
  arrange(desc(visits)) %>%
  print     # printing 13 the highest hour, i.e. actaully 1 AM/PM in 12 hour format


