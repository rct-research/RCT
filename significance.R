source('Mazar_summary.R')
#reg model-lab----
Component.List = c(unique(full_data_imp$lab.name))
X<-c(colnames(full_data_imp)[c(2,6:8,16:24,33:94)])
sapply(subdata,unique)[X]
sapply(subdata,unique)[X_remove]


for(k in 1:length(Component.List)){
  try({subdata = subset(full_data_imp, lab.name == Component.List[k])
      X_remove<-c(colnames(full_data_imp)[c(6:8,16:24,33:94)])
      fmla_remove<- as.formula(paste(y,'~', paste(X_remove, collapse= "+")))
      fit_lab1=lm(fmla_remove,data=subdata)
      print(k)
      anova(fit_lab1)
      },silent = TRUE)
  try({subdata = subset(full_data_imp, lab.name == Component.List[k])
  X_remove<-c(colnames(full_data_imp)[c(6:8,16:24,33:93)])
  fmla_remove<- as.formula(paste(y,'~', paste(X_remove, collapse= "+")))
  fit_lab1=lm(fmla_remove,data=subdata)
  print(k)
  anova(fit_lab1)},silent = TRUE)
  
}



