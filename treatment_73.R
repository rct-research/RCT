library(ggplot2)
library(RColorBrewer)
########73 variable 
#reg model---- 73 variables
#X
X<-colnames(full_data_T)[c(2,6:7,16:24,33:93)]
#y
y<-colnames(full_data_T)[15]
##reg model
fmla <- as.formula(paste(y,'~', paste(X, collapse= "+")))

##personal reg and pred ----
full_data_T<-person_pred(full_data_T)

#plot_actual-boxplot
actual_boxplot(full_data_T)
#plot_prediction-boxplot
prediction_boxplot(full_data_T)
#plot_actual -hist
actual_hist(full_data_T,T)
#plot_prediction-hist
prediction_hist(full_data_T,T)
#plot-error actual-pred
error_density(full_data_T)
#cor
cor_plot(full_data_T)
  

unique(full_data_T$num.boxes.correct)

