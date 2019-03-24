##---lasso reg-----
library(glmnet)

person_pred_both<-function(treat_df,control_df){
  #exclude categorical variables
  lasso_x_T <- treat_df[c(7,16:24,33:92)]   
  lasso_x_C <- control_df[c(7,16:24,33:92)] 
  lasso_y_T <- treat_df[15]
  lasso_y_C <- control_df[15]
  
  #treatment model
  fit_treat = glmnet(data.matrix(lasso_x_T), as.numeric(data.matrix(lasso_y_T)), family="gaussian")
  # use cv.glmnet() to find best.lambda
  cv_treat_lasso = cv.glmnet(data.matrix(lasso_x_T),as.numeric(data.matrix(lasso_y_T)), alpha = 1, family = "gaussian")
  treat.pred.T <- predict(fit_treat, data.matrix(lasso_x_T), s=cv_treat_lasso$lambda.min)
  treat.pred.C <- predict(fit_treat, data.matrix(lasso_x_C), s=cv_treat_lasso$lambda.min)
  treat_df=treat_df %>% mutate(lasso_if_treat=c(treat.pred.T))
  control_df=control_df %>% mutate(lasso_if_treat=c(treat.pred.C))
  #control model
  fit_control =  glmnet(data.matrix(lasso_x_C), as.numeric(data.matrix(lasso_y_C)), family="gaussian")
  cv_control_lasso = cv.glmnet(data.matrix(lasso_x_C),as.numeric(data.matrix(lasso_y_C)), alpha = 1, family = "gaussian")
  control.pred.T <- predict(fit_control, data.matrix(lasso_x_T), s=cv_control_lasso$lambda.min)
  control.pred.C <- predict(fit_control, data.matrix(lasso_x_C), s=cv_control_lasso$lambda.min)
  treat_df=treat_df %>% mutate(lasso_if_control=c(control.pred.T))
  control_df=control_df %>% mutate(lasso_if_control=c(control.pred.C))
 
  df=rbind(treat_df, control_df)

  return(df)
}

df = person_pred_both(full_data_T, full_data_C)
full_data_T = df %>% filter(maz.prime.cond=='commandments')
full_data_C = df %>% filter(maz.prime.cond=='books')

library(ggplot2)

# impact plot
ggplot(data=full_data_T, aes(x=lasso_if_treat, y=lasso_if_control,col=abs(lasso_if_treat-lasso_if_control)>=2))+
  geom_jitter(alpha=0.2, size=2)+
  scale_color_manual(values=c("black", "red"),name='difference >=2')+
  ggtitle('Prediction difference for treatment subject')+
  xlim(0,7)+
  ylim(0,12)
# Cgroup
ggplot(data=full_data_C, aes(x=lasso_if_control, y=lasso_if_treat,col=abs(lasso_if_control-lasso_if_treat)>=2))+
  geom_jitter(alpha=0.2, size=2)+
  scale_color_manual(values=c("black", "red"),name='difference >=2')+
  ggtitle('Prediction difference for control subject')+
  xlim(0,7)+
  ylim(0,12)

# actual v.s. predict_outcome
# Tgroup
ggplot(data=full_data_T, aes(x=lasso_if_treat, y=num.boxes,col=abs(lasso_if_treat-num.boxes)>=8))+
  geom_jitter(alpha=0.4,size=2)+
  scale_color_manual(values=c("black", "red"),name='Outliers >=8')+
  ggtitle('Actual & Treatment Prediction for treatment subject')+
  xlim(0,10)+
  ylim(0,21)
# Cgroup
ggplot(data=full_data_C, aes(x=lasso_if_control, y=num.boxes,col=abs(lasso_if_control-num.boxes)>=8))+
  geom_jitter(alpha=0.4,size=2)+
  scale_color_manual(values=c("black", "red"),name='Outliers >=8')+
  ggtitle('Actual & Control Prediction for control subject')+
  xlim(0,10)+
  ylim(0,21)

###############################################################################
plot(data.frame(DF=fit_treat$df, dev.ratio=fit_treat$dev.ratio),type="b",cex=0.6, main="treatment model")
plot(data.frame(DF=fit_control$df, dev.ratio=fit_control$dev.ratio),type="b",cex=0.6, main="control model")

plot(fit_treat, xvar='lambda')
abline(v=log(cv_treat_lasso$lambda.min), col="blue", lty=5.5 )
plot(fit_control, xvar='lambda')
abline(v=log(cv_control_lasso$lambda.min), col="blue", lty=5.5 )

# which predictors each model selected
coef(fit_treat, s=cv_treat_lasso$lambda.min)
coef(fit_control, s=cv_control_lasso$lambda.min )

