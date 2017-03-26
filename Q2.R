#Question 2
day_visits <-  dataf %>%
  mutate(Day=weekdays(dataf$Date),label=TRUE) %>%
  group_by(Day) %>%
  summarize(visits=length(Day)) %>%
  print

ggplot(day_visits,aes(x=Day,y=visits))+geom_bar(stat="identity",fill="slateblue")+ggtitle("Visits per Weekday")+labs(x="Day",y="Visits")


