#Question 18
dataf$Total_Charges<-as.numeric(dataf$Total_Charges)
sum(dataf$Total_Charges[dataf$Procedure=="X Ray" | dataf$Procedure=="Scalling"],na.rm = TRUE)
