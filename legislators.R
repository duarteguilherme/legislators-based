setwd('~/legislators-based/')

source('dados.R')
# source('beta.R')

library(rstan)




yy <- matrix(NA, ncol=3, nrow=48600)
colnames(yy) <- c("n","k","y")
control <- 1
for (i in 1:100) {
  for (j in 1:486) {

    yy[control,1] <- i
    yy[control,2] <- j
    yy[control,3] <- y[i,j]
    control <- control + 1
  }
}
yy <- data.frame(yy)
yy <- na.omit(yy)

K <- length(unique(yy$k))
L <- length(unique(yy$n))

data = list(N=nrow(yy), L=L, K=K, jj=yy$n, kk=yy$k, y=yy$y)

stanstr <-
'
data {
  int<lower=0> N;
  int<lower=0> L; // legisladores
  int<lower=0> K; // rollcalls
  int<lower=0> jj[N];
  int<lower=0> kk[N];
  int<lower=0, upper=1> y[N];
}
parameters {
  real beta0[K];
  real beta1[K];
  vector[L-2] z;
}
transformed parameters {
  vector[L] x;
  for (i in 1:40) x[i] <- z[i];
  for (i in 42:65) x[i] <- z[i-1];
  for (i in 67:100) x[i] <- z[i-2];
  x[41] <- 1;
  x[66] <- -1;
}

model {
for (i in 1:N){
    y[i] ~ bernoulli(Phi_approx(x[jj[i]]*beta1[kk[i]] - beta0[kk[i]]));
}

beta1 ~ normal(0, 1);
beta0 ~ normal(0, 1);
z ~ normal(0,1);  

}
'
fit <- stan(model_code = stanstr, data=data, iter=12000, warmup=2000, thin=10, chains=3)

/* for (i in 1:40){ x[i] ~ normal(0,1); }
for (i in 42:65){ x[i] ~ normal(0,1); }
for (i in 67:100){ x[i] ~ normal(0,1); }
x[41] ~ normal(-1, .0001);
x[66] ~ normal(1, .0001);
*/ 
  
  
  
  
macs <- read.csv('~/Downloads/macs.csv', stringsAsFactors=FALSE)




library(dplyr)



macs <- select(macs, -Numero_ADI)

nrow(macs)
ncol(macs)

teste <- table(macs)


macs <- as.vector(macs)