#Q1
#shape=beta,scale=theta
n=1000
#1-case1
weibull_1<-rweibull(n, shape=0.5, scale = 1)
#1-case2
weibull_2<-rweibull(n, shape=1, scale = 1)
#1-case3
weibull_3<-rweibull(n, shape=2, scale = 1)

hist(weibull_1,probability = T)
hist(weibull_2,probability = T)
hist(weibull_3,probability = T)

f1 = function(x, theta, beta){
  # component 1
  c1 = beta*(1/theta^2)+x^beta*beta*(-beta-1)*theta^(-beta-2)
  # component 2 & 3
  c2_3 = (-1/theta)+x^beta*log(x)*beta*theta^(-beta-1)+x^beta*(theta^(-beta-1)+log(1/theta)*theta^(-beta-1)*beta)
  # component 4 
  c4 = (-1/beta^2)-log(x/theta)*(x/theta)^beta*log(x/theta)
  
  mc_esti_1 = sum(c1)/n
  mc_esti_2 = sum(c2_3)/n
  mc_esti_3 = sum(c2_3)/n
  mc_esti_4=  sum(c4)/n
  
  se1=var(c1)/n
  se2_3=var(c2_3)/n
  se4=var(c4)/n
  
  return (list('mc'=c(-mc_esti_1, -mc_esti_2, -mc_esti_3, -mc_esti_4),'se'=c(se1,se2_3,se2_3,se4)))
}
ans_1 = f1(weibull_1, theta=1, beta=0.5)
ans_2 = f1(weibull_2, theta=1, beta=1)
ans_3 = f1(weibull_3, theta=1, beta=2)
ans_1
ans_2
ans_3

#Q2-a

Y=c(12,8,6,7,9)
SamplingDistribution<-function(theta,beta,k){
  pweibull(k+0.5,scale = theta,shape = beta)-pweibull(max(k-0.5,0),scale = theta,shape = beta)
}
posterior<- function(arg){
  theta=arg[1]
  beta=arg[2]
  temp=1
  for(i in 1:5){
    temp=temp*SamplingDistribution(theta,beta,Y[i])
  }
  theta^(-3)*exp(-10/theta)*temp*(beta>0)*(beta<2)*(theta>0)
}
proposal<- function(arg){
  theta=arg[1]
  beta=arg[2]
  dgamma(theta,9,1)*1/2
}
#MCMC steps:
m = 31000
eta.mcmc = matrix(0,m,2)
eta.mcmc[1,] = c(10,1) 
move = c()

#RW-MH
for (i in 2:m){
  eta.mcmc[i,1] = rgamma(1,9,1)
  eta.mcmc[i,2] = runif(1,0,2)
  r=proposal(eta.mcmc[i-1,])/proposal(eta.mcmc[i,])
  r=r*posterior(eta.mcmc[i,])/(posterior(eta.mcmc[i-1,]))
  move[i] = rbinom(1,1,prob=min(r,1))
  if (move[i]==0) eta.mcmc[i,] = eta.mcmc[i-1,]
}
mean(move[-1])
#
par(mfcol=c(2,1)) 
for (i in 1:2){ts.plot(eta.mcmc[index,i])} #mcmc draws
par(mfcol=c(1,1)) 
acf(eta.mcmc) #dependence between draws
#iid
index=seq(1000,m,20)[1:1000]
acf(eta.mcmc[index,])

mean(eta.mcmc[index,1])
mean(eta.mcmc[index,2])
sd(eta.mcmc[index,1])
sd(eta.mcmc[index,2])
# Q2-b
temp = c()
for (i in 1:m){
  theta = rgamma(1, 9, 1)
  beta = runif(1, 0, 2)
  temp = c(temp, posterior(c(theta, beta))/proposal(c(theta, beta)))
}
mean(temp)

#Q3
theta = 0.5
set.seed(1)
jb= function(n, x){
  S = (sum(((x-mean(x))^3))/n)/(sum(((x-mean(x))^2))/n)^(3/2)
  K = (sum(((x-mean(x))^4))/n)/(sum(((x-mean(x))^2))/n)^(4/2)
  n/6*S^2+(1/4)*(K-3)^2
}

M = 10000
n = 20
pi_ = c()
for (i in 1:M){
  sample = rt(n, df=1/theta)
  temp = jb(n, sample)
  pi_[i] = ifelse(temp > qchisq(0.95, df=2), 1, 0)
}
mean(pi_)
M = 10000
n = 100
pi_ = c()
for (i in 1:M){
  sample = rt(n, df=1/theta)
  temp = jb(n, sample)
  pi_[i] = ifelse(temp > qchisq(0.95, df=2), 1, 0)
}
mean(pi_)
