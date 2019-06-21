
# For the BVS part
vsc_home <- Sys.getenv("VSC_HOME")

library(rjags) 
library(R2jags)
library(runjags)
library(purrr)
library(parallel)
library(tidyverse)
library(mvtnorm)

vc <- function(vx, vy, cor){
  
  Sigma <- list()
  Sigma[[1]] <- matrix(c(vx, corr[1]*sqrt(vx*vy), corr[1]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[2]] <- matrix(c(vx, corr[2]*sqrt(vx*vy),  corr[2]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[3]] <- matrix(c(vx, corr[3]*sqrt(vx*vy), corr[3]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  Sigma[[4]] <- matrix(c(vx, corr[4]*sqrt(vx*vy), corr[4]*sqrt(vx*vy), vy), nrow = 2, byrow = TRUE)
  return(Sigma)
}


corr <- c(0.9,0.75,0.5,0.25)

mu<-c(0,0)
varx=1
vary=1

Sigmaa = vc(varx, vary, corr)

# Sigma = list()
# for (j in 1:4) {
#   Sigma[[j]]<-matrix(c(varx,corr[j],corr[j],vary),2,2)
# }


alpha0<-1
alpha1<-5
beta0<-5
beta1<-2

n<-50
zi<-as.numeric(c(rep(0,n),rep(1,n)))
B<-1000
r.square.h<-c(1:B)


x = array(data = NA, dim=c(100,1000,4)) ; y = array(data = NA, dim=c(100,1000,4))

eps = list(); se=list(); ce=list()

set.seed(1234)
for(i in 1:B){
  for (k in 1:4) {
    eps[[k]] <- rmvnorm(n,mu,Sigmaa[[k]])
    x[,i,k]<-alpha0+alpha1*zi+eps[[k]][,1]
    y[,i,k]<-beta0+beta1*zi+eps[[k]][,2]
    
    corr <- c(0.9,0.75,0.5,0.25)
    
    se[[k]] <- data.frame(x[,,k][,])
    ce[[k]] <- data.frame(y[,,k][,])
  }
}

######################################################################################

set.seed(1234)

init1 = list(
  list(betaT = rep(5.62, 3), ind = rep(0.9, 3), tau.e = 0.45,
       tau.ee = 0.25),
  list( betaT = rep(2.22, 3), ind = rep(1, 3), tau.e = 0.45/2,
        tau.ee = 0.25/2)
)

cat("
    ########### MODEL FORMULATION
    model{
    for(i in 1:N) {
    xx[i] ~ dnorm(muu[i],tau.ee)
    muu[i] <-  beta[1] + (betaT[1]*ind[1])*treatment[i]
    yy[i] ~ dnorm(mu[i],tau.e)
    mu[i] <-  beta[2] + (betaT[2] * ind[2])*treatment[i] +
    (betaT[3]*ind[3]) * xx[i]
    }
    
    ########### PRIORS
    tau.e ~ dgamma(0.00001,0.00001)
    sigma.e <- 1 / sqrt(tau.e)
    
    tau.ee ~ dgamma(0.00001,0.00001)
    sigma.ee <- 1 / sqrt(tau.ee)
    
    ## Indicator beta3, beta4,beta5
    for(k in c(1,2,3)) {
    ind[k] ~ dunif(0,1)
    betaT[k] ~ dnorm(0,taub[k])
    taub[k] ~ dgamma(1,0.001)
    }
    
    beta3 <- ind[1] * betaT[1]
    beta4 <- ind[2] * betaT[2]
    beta5 <- ind[3] * betaT[3]
    
    for (j in c(1,2)){
    beta[j] ~ dnorm(0,0.000001)
    }
    
    }
    ",file ="Model1.jag")

Final_results <- vector("list", 4)


for(j in 1:4) {
  print(j)
  ncl <- makeCluster(detectCores())
  clusterEvalQ(ncl, library(runjags))
  clusterEvalQ(ncl, library(R2jags))
  jmodel <- read.jagsfile("Model1.jag")
  
  #clusterEvalQ(ncl, init1)
  clusterExport(ncl, c("zi", "init1", "jmodel", "j", "ce", "se"))
  
  results1 <- clusterApply(cl = ncl, x = 1:2, 
                           fun = function(i) {
                             x <- se[[j]][, i]
                             y <- ce[[j]][, i]
                             data1 = list(
                               N = length(zi),
                               treatment = zi,
                               yy =  y,
                               xx = x
                             )
                             model1 <- run.jags(model=jmodel,monitor=c("beta","beta3","beta4","beta5","ind","betaT"),data=data1,inits=init1, method="rjags",
                                                n.chains=2,burnin=50000,sample=80000,adapt=70000)
                             
                             return(data.frame(Int_x = model1$summaries["beta[1]", "Mean"],
                                               Int_y = model1$summaries["beta[2]", "Mean"],
                                               Trt_x = model1$summaries["beta3", "Mean"],
                                               Trt_y = model1$summaries["beta4", "Mean"],
                                               Eff_xy = model1$summaries["beta5", "Mean"],
                                               Ind3 = model1$summaries["ind[3]", "Mean"],
                                               Ind2 = model1$summaries["ind[2]", "Mean"],
                                               Ind1 = model1$summaries["ind[1]", "Mean"])
                             )
                           }) %>% do.call(rbind.data.frame, .)
  Final_results[[j]] <- results1 
  stopCluster(ncl)
}


save(list=c("ce","se","zi","Final_results"), file = "Result1.RData")
