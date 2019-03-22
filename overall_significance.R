#T reg model---- with major
#X
X<-colnames(full_data_T)[c(2,6:8,16:24,33:94)]
#y
y<-colnames(full_data_T)[15]
##reg model
fmla <- as.formula(paste(y,'~', paste(X, collapse= "+")))
fit=lm(fmla,data=full_data_T)
#reg result
anova(fit) 
summary(fit)



#C reg model---- with major
#X
X<-colnames(full_data_C)[c(2,6:8,16:24,33:94)]
#y
y<-colnames(full_data_C)[15]
##reg model
fmla <- as.formula(paste(y,'~', paste(X, collapse= "+")))
fit=lm(fmla,data=full_data_C)
#reg result
anova(fit) 


##plot_NA
full_data_C %>% 
  filter(is.na(major))%>%
  ggplot(aes(lab.name))+
  geom_histogram(bins = 30,stat = 'count')+
  labs(title='Variable NA counts-major',y='count of major in NA value ')