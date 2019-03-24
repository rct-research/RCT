##---PCA-----
X<-colnames(full_data_T)[c(7,16:24,33:92)] #exclude language, major, gender
fmla <- as.formula(paste('~', paste(X, collapse= "+")))
pca_treat <- prcomp(formula=fmla, data=full_data_T, scale=TRUE)
plot(pca_treat, type="line" ,ylim=c(-1,5))


X<-colnames(full_data_C)[c(7,16:24,33:92)] #exclude language, major, gender
fmla <- as.formula(paste('~', paste(X, collapse= "+")))
pca_control <- prcomp(formula=fmla, data=full_data_C, scale=TRUE)
plot(pca_control, type="line" ,ylim=c(-1,5))


treat_vars <- (pca_treat$sdev)^2
treat_props <- treat_vars/sum(treat_vars)
treat_cuml.props <- cumsum(treat_props)
plot(treat_cuml.props, main="treatment group")


control_vars <- (pca_control$sdev)^2
control_props <- control_vars/sum(control_vars)
control_cuml.props <- cumsum(control_props)
plot(control_cuml.props, main = "control group")
