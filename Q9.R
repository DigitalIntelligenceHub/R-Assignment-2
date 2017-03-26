#Question 9
my_shifts <- my_shifts %>%
  mutate(Shifts = derivedFactor( "Morning" = (Hour>=6 & Hour<=12),"Afternoon" = (Hour>=12 & Hour<=16),"Evening" = (Hour>=14 & Hour<=19),"Night" =((Hour>=19 & Hour<=23))))

