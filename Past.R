
## Adjust data.Major ----
full_data_select<-full_data_select%>%
  mutate(major=replace(major,major=='Business (Exchange Student)','Business')) %>% #Exchange
  mutate(major=replace(major,major=='Accounting (Exchange)','Accounting')) %>%
  mutate(major=replace(major,major=='International Business (Exchange)','International Business')) %>%
  mutate(major=replace(major,major=='Marketing Strategy','Marketing Strategy And Innovation')) 

