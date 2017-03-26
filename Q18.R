# Question 18
dataf$TotalCharges<-as.numeric(as.character(dataf$TotalCharges))
consult_amount <- dataf%>%
  select(Procedure,TotalCharges) %>%
  group_by(Procedure) %>%
  filter(Procedure == 'X Ray' & Procedure == 'Scalling') %>%
  summarise(sum(TotalCharges,na.rm=TRUE)) %>%
  print

