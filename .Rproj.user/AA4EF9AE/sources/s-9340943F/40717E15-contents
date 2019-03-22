##---model---
#personal reg and pred 
test<-person_pred_both(full_data_all,full_data_T,full_data_C)
full_data_T<-test %>%filter(maz.prime.cond=='commandments')
full_data_C<-test %>%filter(maz.prime.cond=='books')

##---boxplot---
#plot_actual-boxplot
outcome_boxplot(full_data_C)
#plot_prediction-boxplot
outcome_boxplot(full_data_C,df$if_treat,'if_treat')
outcome_boxplot(full_data_C,df$if_control,'if_control')

#plot_actual -hist
outcome_hist(full_data_C)
outcome_hist(full_data_T)
#plot_prediction-hist_T
outcome_hist(full_data_C,full_data_C$if_treat,'Regression Prediction_if_treat')
outcome_hist(full_data_T,full_data_T$if_treat,'Regression Prediction_if_treat')
#plot_prediction-hist_C
outcome_hist(full_data_C,full_data_C$if_control,'Regression Prediction_if_control')
outcome_hist(full_data_T,full_data_T$if_control,'Regression Prediction_if_control')

#plot_outcomes_hist
outcomes_hist<-full_data_T %>%gather(treat_control,n,if_treat,if_control,Actual=num.boxes)%>%
  ggplot()+
  aes(n,fill=treat_control)+
  geom_density(aes(n),col=NA,alpha=0.3)+
  guides(fill=guide_legend(title="Actual & Prediction"))+
  labs(title='Regression outcomes density')
outcomes_hist

#plot-error actual-pred
error_hist(full_data_C,FALSE)
error_hist(full_data_T,TRUE)


error_bar(full_data_C,FALSE)
error_bar(full_data_T,T)

impact_bar_C(full_data_C)
impact_bar_T(full_data_T)


#cor
cor_plot_TC(full_data_C,TRUE)
cor_plot_TC(full_data_C,FALSE)
cor_plot_TC(full_data_T,TRUE)
cor_plot_TC(full_data_T,F)

#jitter plot for num.boxes, if treat----
# Tgroup
ggplot(data=full_data_T, aes(x=if_treat, y=num.boxes,col=abs(if_treat-num.boxes)>=8))+
  geom_jitter(alpha=0.2)+
  scale_color_manual(values=c("black", "red"),name='Outliers >=8')+
  ggtitle('Actual & Treatment Prediction for treatment subject')
# Cgroup
ggplot(data=full_data_C, aes(x=if_control, y=num.boxes,col=abs(if_control-num.boxes)>=8))+
  geom_jitter(alpha=0.2)+
  scale_color_manual(values=c("black", "red"),name='Outliers >=8')+
  ggtitle('Actual & COntrol Prediction for control subject')

#jitter plot for impact: if treat, if control----
# Tgroup
ggplot(data=full_data_T, aes(x=if_treat, y=if_control,col=abs(if_treat-if_control)>=2))+
  geom_jitter(alpha=0.2)+
  scale_color_manual(values=c("black", "red"),name='difference >=2')+
  ggtitle('Prediction difference for treatment subject')+
  xlim(0,7)+
  ylim(0,12)
# Cgroup
ggplot(data=full_data_C, aes(x=if_control, y=if_treat,col=abs(if_control-if_treat)>=2))+
  geom_jitter(alpha=0.2)+
  scale_color_manual(values=c("black", "red"),name='difference >=2')+
  ggtitle('Prediction difference for control subject')+
  xlim(0,7)+
  ylim(0,12)
