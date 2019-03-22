library(reshape2)
library(ggplot2)
##summary features
each_variable<-function(df,variable,xlab){
  df%>%
    ggplot()+
    aes(variable,fill=maz.prime.cond)+
    geom_bar(position='dodge',col=NA,alpha=0.3)+
    xlab(xlab)+
    theme(legend.position = c(0.85,0.85))
}
each_variable<-function(df,variable,xlab){
  df%>%
    ggplot()+
    aes(variable,fill=maz.prime.cond)+
    geom_density(col=NA,alpha=0.3)+
    xlab(xlab)+
    theme(legend.position = c(0.85,0.85))
}
##personal reg and pred ----
person_pred<-function(df){
  df<-df%>% mutate(fit=NA,lwr=NA,upr=NA)
  n<-length(df$num.boxes)
  for (i in 1:n){
    train.df<-df[-i,]
    valid.df<-df[i,]
    fit=lm(fmla,data=train.df)
    valid.pred<-predict(fit,valid.df,interval = "prediction", na.action = na.exclude)
    df$fit[i]<-valid.pred[1]
    df$lwr[i]<-valid.pred[2]
    df$upr[i]<-valid.pred[3]
  }
  return(df)
}
person_pred_both<-function(df,treat_df,control_df){
  df<-df%>% mutate(if_treat=NA,if_control=NA)
  n<-length(df$num.boxes)
  for (i in 1:n){
    subject<-df[i,]
    #treatment model
    fit_treat=lm(fmla,data=treat_df)
    treat.pred<-predict(fit_treat,subject,interval = "prediction", na.action = na.exclude)
    df$if_treat[i]<-treat.pred[1]
    #control model
    fit_control=lm(fmla,data=control_df)
    control.pred<-predict(fit_control,subject,interval = "prediction", na.action = na.exclude)
    df$if_control[i]<-control.pred[1]
  }
  return(df)
}
#plot-boxplot----
outcome_boxplot<-function(df,outcome=df$num.boxes,title='actual'){
  df %>% 
  ggplot(aes(lab.name,outcome))+
  geom_boxplot()+
  labs(title=title)+  
  theme(axis.text.x = element_text(angle=-90))+
  ylim(0,20)}
#plot -hist
outcome_hist<-function(df,outcome=df$num.boxes,title='actual',facet=FALSE){
  if(facet==FALSE){
    df %>% 
      ggplot(aes(outcome))+
      geom_histogram(bins = 50)+
      labs(title=title)
  }
  else{
    df %>% 
      ggplot(aes(outcome))+
      geom_histogram(bins = 50)+
      facet_wrap("lab.name")+
      labs(title=title)
  }
}
#plot_outcomes_hist----not yet as function----
outcomes_hist<-full_data_T %>%gather(treat_control,n,if_treat,if_control,Actual=num.boxes)%>%
  ggplot()+
  aes(n,fill=treat_control)+
  geom_density(aes(n),col=NA,alpha=0.3)+
  guides(fill=guide_legend(title="Actual & Prediction"))+
  labs(title='Regression outcomes density')
outcomes_hist


#plot-error actual-pred----
error_hist<-function(df,Treatment=TRUE){
  if(Treatment==TRUE){
  df %>%
    mutate(treat_error=(num.boxes-if_treat),lower = quantile(treat_error, probs = .025),upper = quantile(treat_error, probs = .975),mean=mean(treat_error) )%>%
    ggplot(aes(treat_error))+
    geom_histogram(fill='grey')+
    geom_vline(aes(xintercept=lower),linetype='dashed')+
    geom_vline(aes(xintercept=upper),linetype='dashed')+
    geom_vline(aes(xintercept=mean),linetype='dashed',col='red')+
    labs(title='Regression Error for Treatment')+
    xlim(-10,20)
  }
  else{
    df %>%
      mutate(control_error=(num.boxes-if_control),lower = quantile(control_error, probs = .025),upper = quantile(control_error, probs = .975) ,mean=mean(control_error))%>%
      ggplot(aes(control_error))+
      geom_histogram(fill='grey')+
      geom_vline(aes(xintercept=lower),linetype='dashed')+
      geom_vline(aes(xintercept=upper),linetype='dashed')+
      geom_vline(aes(xintercept=mean),linetype='dashed',col='red')+
      labs(title='Regression Error for control')+
      xlim(-10,20)
  }
}
error_bar<-function(df,Treatment=TRUE){
  if (Treatment==T){
    df %>%
      mutate(treat_error=(num.boxes-if_treat),N_P=treat_error>0 )%>%
      arrange(treat_error)%>%
      mutate(ID=seq.int(nrow(df)))%>%
      ggplot(aes(x=ID,y=treat_error,fill=N_P))+
      geom_bar(stat="identity", position = "identity")+
      labs(title='Error Plot for Treatment')+
      ylab('Actual-Predict')+
      theme(legend.position='none')
  }
  else{
    df %>%
      mutate(treat_error=(num.boxes-if_control),N_P=treat_error>0 )%>%
      arrange(treat_error)%>%
      mutate(ID=seq.int(nrow(df)))%>%
      ggplot(aes(x=ID,y=treat_error,fill=N_P))+
      geom_bar(stat="identity", position = "identity")+
      labs(title='Error Plot for Control')+
      ylab('Actual-Predict')+
      theme(legend.position='none')
  }
}

#impact-plot
impact_bar_C<-function(df){
  df %>%
    mutate(treat_error=(if_control-if_treat),N_P=treat_error>0 )%>%
    arrange(treat_error)%>%
    mutate(ID=seq.int(nrow(df)))%>%
    ggplot(aes(x=ID,y=treat_error,fill=N_P))+
    geom_bar(stat="identity", position = "identity")+
    labs(title='Impact Plot',subtitle = 'Control subject predict by giving treatment')+
    ylab('Predict-PredictCF')+
    ylim(-5,5)+
    theme(legend.position='none')
}
impact_bar_T<-function(df){
  df %>%
    mutate(treat_error=(if_treat-if_control),N_P=treat_error>0 )%>%
    arrange(treat_error)%>%
    mutate(ID=seq.int(nrow(df)))%>%
    ggplot(aes(x=ID,y=treat_error,fill=N_P))+
    geom_bar(stat="identity", position = "identity")+
    labs(title='Impact Plot',subtitle = 'Control subject predict by giving treatment')+
    ylab('Predict-PredictCF')+
    ylim(-5,5)+
    theme(legend.position='none')
}

#plot-cor actual-pred----
cor_plot_TC<-function(df,Treatment=TRUE){
  if(Treatment==TRUE){
  a<-df %>%
    summarise(cor(num.boxes,if_treat))
  df %>%
    mutate(error=(num.boxes-if_treat))%>%
    ggplot(aes(x=num.boxes,y=if_treat))+
    geom_point(alpha=0.1)+
    annotate("text", label = paste('correlation=',a), x =15 , y = 3.5)+
    geom_smooth(method = lm)+
    labs(title=paste('Regression Error if treatment','correlation'))
  }
  else{
    a<-df %>%
      summarise(cor(num.boxes,if_control))
    df %>%
      mutate(error=(num.boxes-if_control))%>%
      ggplot(aes(num.boxes,if_control))+
      geom_point(alpha=0.1)+
      annotate("text", label = paste('correlation=',a), x =15 , y = 3.5)+
      geom_smooth(method = lm)+
      labs(title=paste('Regression Error if control','correlation'))
  }
}
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


##function already improved----------
#plot_prediction-hist
prediction_hist_T<-function(df,facet=FALSE){
  if(facet==FALSE){
    df %>% 
      ggplot(aes(if_treat))+
      geom_histogram(bins = 30)+
      labs(title='Regression Prediction_if_treatment')
  }
  else{
    df %>% 
      ggplot(aes(if_treat))+
      geom_histogram(bins = 30)+
      facet_wrap("lab.name")+
      labs(title='Regression Prediction')
  }
}
#plot_prediction-hist
prediction_hist_C<-function(df,facet=FALSE){
  if(facet==FALSE){
    df %>% 
      ggplot(aes(if_control))+
      geom_histogram(bins = 30)+
      labs(title='Regression Prediction_if_control')
  }
  else{
    df %>% 
      ggplot(aes(if_control))+
      geom_histogram(bins = 30)+
      facet_wrap("lab.name")+
      labs(title='Regression Prediction')
  }
}
#plot-cor actual-pred
cor_plot<-function(df){
  a<-df %>%
    summarise(cor(num.boxes,fit))
  df %>%
    mutate(error=(num.boxes-fit))%>%
    ggplot(aes(num.boxes,fit))+
    geom_point(alpha=0.1)+
    annotate("text", label = paste('correlation=',a), x =15 , y = 3.5)+
    geom_smooth(method = lm)+
    labs(title=paste('Regression Error','correlation',a))
}

##########test----
full_data_T%>% filter(lab.name=='Verschuere') %>%.[X]
colnames(full_data_T)
unique(full_data_T$tear)
unique(full_data_C$maz.num.books)
unique(full_data_select$sleep.hours)
glimpse(full_data_select$sleep.hours)
full_data$major=='Business (Exchange Student)'
unique(full_data$lab.name)
sapply(full_data_C,unique)[X]
sapply(full_data_T, function(x) sum(is.na(x)))['num.boxes.correct']
sum(is.na(full_data_T$hex43))
length(unique(full_data_T$major%>%select(lab.name=='Verschuere')))
full_data_C %>%
  mutate(treat_error=(num.boxes-if_treat),lower = quantile(treat_error, probs = .025),upper = quantile(treat_error, probs = .975) )%>%
  ggplot(aes(treat_error))+
  geom_histogram(fill='grey')+
  geom_vline(aes(xintercept=lower),linetype='dashed')+
  geom_vline(aes(xintercept=upper),linetype='dashed')+
  labs(title='Regression Error_if_Treatment')
data23%>% filter(maz.prime.cond=='books') %>% select(num.commandments) %>% unique(.)

