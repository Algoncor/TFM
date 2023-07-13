################### READING DATA ####################

#Libraries
library(dplyr)
library(Rfast)
library(neuralnet)
library(pso)

#Work directory
setwd("C:/Users/gonza/OneDrive/Documentos/lipschitz")

#Data frame
df <- read.csv("~/lipschitz/datos100.csv", dec=",")
n <- nrow(df)

################### DATA PROCESSIN ####################

#Scaling
maxs <- apply(df, 2, max) 
mins <- apply(df, 2, min)
scaled <- as.data.frame(scale(df, center = mins, scale = maxs - mins))

#Minimum element
a0 <- which.min(scaled$Index)

#Matrix of distances
D <- Dist(scaled[,1:4])

#Index distances
Dind <- Dist(scaled[,5])

#Optim parameters
p0 <- c(1,0,0,0)
low <- c(1e-16,1e-16,1e-16,1e-16)
up <- c(1,1,1,1)

#################### COMPARATIONS ####################

N <- 20

RMSE_stand <- NULL
RMSE_stand_phi <- NULL
RMSE_MW <- NULL
RMSE_MW_phi <- NULL
RMSE_neuralnet <- NULL

stand_time <- 0
MW_time <- 0
stand_phi_time <- 0
MW_phi_time <- 0
neuralnet_time <- 0

for (iter in 1:N) {
  
  #Train and test
  train <- sort(sample(1:n,round(0.7*n)))
  test <- setdiff(1:n,train)
  
  ### Standard Lipschitz
  t0 <- Sys.time() 
  K <- max(Dind[train,]/D[train,], na.rm = TRUE)
  standard <- K*D[a0,test]
  standard <- standard*(maxs[5]-mins[5])+mins[5]
  RMSE_stand[iter] <- sqrt(sum((standard-df$Index[test])^2)/length(test))
  t1<- Sys.time()
  stand_time <- stand_time + t1-t0
  
  ###McShane-Whitney Lipschitz
  t0 <- Sys.time() 
  index <- scaled$Index[train]
  Whitney <- apply(index + K*D[test,train], 1, min)
  McShane <- apply(index - K*D[test,train], 1, max)
  dif <- Whitney - McShane
  alpha <- sum(dif*(Whitney-scaled$Index[test]))/sum(dif^2)
  I <- (1-alpha)*Whitney + alpha*McShane
  I <- I*(maxs[5]-mins[5])+mins[5]
  RMSE_MW[iter] <- sqrt(sum((I-df$Index[test])^2)/length(test))
  t1<- Sys.time()
  MW_time <- MW_time + t1-t0
  
  ### Standard phi-Lipschitz
  t0 <- Sys.time()
  J <- function(p) {
    
    phi <- function(x) {
      u <-p[1]*x + p[2]*log(1+x) + p[3]*atan(x) + p[4]*x/(1+x)
      return(u)
    }
    
    K <- max(Dind[train,]/phi(D[train,]), na.rm = TRUE)
    
    standard <- K*phi(D[a0,test])
    standard <- standard*(maxs[5]-mins[5])+mins[5]
    
    rmse <- sqrt(sum((standard-df$Index[test])^2)/length(test))
    
    return(rmse)
  }
  
  optimum<-psoptim(p0, J, lower = low, upper = up,
                 control = list(maxit = 100, maxf = 200))
  RMSE_stand_phi[iter] <- optimum$value
  t1<- Sys.time()
  stand_phi_time <- stand_phi_time + t1-t0
  
  ###McShane-Whitney phi-Lipschitz
  t0 <- Sys.time()
  J <- function(p) {
    
    phi <- function(x) {
      u <-p[1]*x + p[2]*log(1+x) + p[3]*atan(x) + p[4]*x/(1+x)
      return(u)
    }
    
    K <- max(Dind[train,]/phi(D[train,]), na.rm = TRUE)
    index <- scaled$Index[train]
    Whitney <- apply(index + K*phi(D[test,train]), 1, min)
    McShane <- apply(index - K*phi(D[test,train]), 1, max)
    dif <- Whitney - McShane
    alpha <- sum(dif*(Whitney-scaled$Index[test]))/sum(dif^2)
    I <- (1-alpha)*Whitney + alpha*McShane
    I <- I*(maxs[5]-mins[5])+mins[5]
    rmse <- sqrt(sum((I-df$Index[test])^2)/length(test))
    
    return(rmse)
  }
  
  optimum<-psoptim(p0, J, lower = low, upper = up,
                   control = list(maxit = 100, maxf = 200))
  RMSE_MW_phi[iter] <- optimum$value
  t1<- Sys.time()
  MW_phi_time <- MW_phi_time + t1-t0
  
  ###Neural Net
  t0 <- Sys.time()
  nn <- neuralnet(Index ~ x + y + z + t, data = scaled[train,])
  pr.nn <- compute(nn, scaled[test,1:4])
  pr.nn <- pr.nn$net.result*(maxs[5]-mins[5])+mins[5]
  RMSE_neuralnet[iter] <- sqrt((sum(pr.nn-df$Index[test])^2)/length(test))
  t1<- Sys.time()
  neuralnet_time <- neuralnet_time + t1-t0
}

#Summary
summary(RMSE_stand)
summary(RMSE_MW)
summary(RMSE_stand_phi)
summary(RMSE_MW_phi)
summary(RMSE_neuralnet)

#Times
stand_time/N
MW_time/N
stand_phi_time/N
MW_phi_time/N
neuralnet_time/N