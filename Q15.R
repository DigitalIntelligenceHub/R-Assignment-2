#Question 15
dataf$TotalCharges<-as.numeric(as.character(dataf$TotalCharges))
consult_amount <- dataf%>%
  select(Procedure,TotalCharges) %>%
  group_by(Procedure) %>%
  filter(Procedure == 'Consultation') %>%
  summarise(sum(TotalCharges,na.rm=TRUE)) %>%
  print


