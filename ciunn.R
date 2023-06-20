################### LECTURA DE DATOS ####################

###Cargamos algunas librerías
library(dplyr)
library(neuralnet)

###Establecemos el directorio de trabajo
setwd("C:/Users/gonza/OneDrive/Documentos/lipschitz")

###Leemos los datos
df <- read.csv("~/lipschitz/ciudades.csv", dec=",")
n <- nrow(df)

################### PROCESADO DE DATOS ####################

t0 <- Sys.time()

###Escalado
maxs <- as.vector(apply(df[2:5], 2, max))
mins <- as.vector(apply(df[2:5], 2, min))
scaled <- data.frame(scale(df[,2:5], mins, maxs-mins))
colnames(scaled) <- c("walk","transit","bike","liveability")

t1 <- Sys.time()

################### EXTENSIÓN ####################

N <- 20

SMAPE <- NN <- 0

for (iter in 1:N) {
  
  ###Elección de train y test
  train <- sort(sample(1:n, round(0.7*n)))
  test <- setdiff(1:n,train)
  
  ###Red neuronal
  nn <- neuralnet(liveability ~ walk + transit + bike, 
                  scaled[train,], hidden = 2)
  
  ###Predicción
  pr.nn <- compute(nn, scaled[test,1:3])
  pr.nn <- pr.nn$net.result*(maxs[4]-mins[4])+mins[4]
  
  ###Error
  smape <- abs(pr.nn-df$liveability[test])/(pr.nn+df$liveability[test])
  SMAPE[iter] <- 200*sum(smape)/length(test)
  
  ###Mejor red neuronal
  if (iter > 1){
    ifelse(SMAPE[iter]<SMAPE[iter-1], NN <- nn, 1+1)
  }
}

t2 <- Sys.time()
tiempo <- (t1-t0)+(t2-t1)/N
summary(SMAPE)

################### RANKING CANADÁ ####################

###Leemos los datos de Canadá
df.can <- read.csv("~/lipschitz/ciudadescan.csv", dec=",")
m <- nrow(df.can)

###Escalado
scaled.can <- data.frame(scale(df.can[,2:4], mins[1:3], maxs[1:3]-mins[1:3]))
colnames(scaled.can) <- c("walk","transit","bike")

###Predicción
pr.nn <- compute(NN, scaled.can)
df.can[,5] <- pr.nn$net.result*(maxs[4]-mins[4])+mins[4]