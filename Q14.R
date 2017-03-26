#Question 14
dataf$TotalCharges<-as.numeric(as.character(dataf$TotalCharges))
sum_of_charges <- sum(dataf$TotalCharges, na.rm = TRUE)
print(sum_of_charges)

