library(ggplot2)
library(RColorBrewer)

#reg model---- 74 variables
#X
X<-colnames(full_data_imp)[c(2,6:7,16:24,33:94)]
#y
y<-colnames(full_data_imp)[15]
##reg model
fmla <- as.formula(paste(y,'~', paste(X, collapse= "+")))

##personal reg and pred ----
full_data_imp<-full_data_imp%>% mutate(fit=NA,lwr=NA,upr=NA)
n<-length(full_data_imp$num.boxes)

for (i in 1:n){
  train.df<-full_data_imp[-i,]
  valid.df<-full_data_imp[i,]
  valid.y<-full_data_imp[i,y]
  valid.x<-full_data_imp[i,c(X)]
  fit=lm(fmla,data=train.df)
  valid.pred<-predict(fit,valid.df,interval = "prediction", na.action = na.exclude)
  full_data_imp$fit[i]<-valid.pred[1]
  full_data_imp$lwr[i]<-valid.pred[2]
  full_data_imp$upr[i]<-valid.pred[3]
}

##plot_actual 
full_data_imp %>% 
  ggplot(aes(num.boxes))+
  geom_histogram(bins = 30)+
  #facet_wrap("lab.name")+
  labs(title='Actual')
##plot_prediction with 74 variables
full_data_imp %>% 
  ggplot(aes(fit))+
  geom_histogram(bins = 30)+
  facet_wrap("lab.name")+
  labs(title='Regression Prediction')


##plot
full_data_imp %>% 
  ggplot(aes(kept.sheet))+
  geom_histogram(bins = 30,stat = 'count')+
  facet_wrap("lab.name")+
  labs(title='Variable')
