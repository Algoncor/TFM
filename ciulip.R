################### LECTURA DE DATOS ####################

###Cargamos algunas librerías
library(dplyr)
library(Rfast)

###Establecemos el directorio de trabajo
setwd("C:/Users/gonza/OneDrive/Documentos/lipschitz")

###Leemos los datos
df <- read.csv("~/lipschitz/ciudades.csv", dec=",")
n <- nrow(df)

################### ESCALADO ####################

t0 <- Sys.time()

###Escalado
maxs <- as.vector(apply(df[2:5], 2, max))
mins <- as.vector(apply(df[2:5], 2, min))
scaled <- data.frame(scale(df[,2:5], mins, maxs-mins))
colnames(scaled) <- c("walk","transit","bike","liveability")

###Matriz de distancias
D <- Dist(scaled[,1:3])

###Distancias entre índices
Dind <- Dist(scaled[,4]) 

t1 <- Sys.time()

################### EXTENSIÓN ####################

N <- 20
SMAPE <- NULL

for (iter in 1:N) {

  ###Elección de train y test
  train <- sort(sample(1:n,round(0.7*n)))
  test <- setdiff(1:n,train)
  
  ###Constante de Lipschitz
  L <- max(Dind[train,]/D[train,], na.rm=TRUE)
  
  ###Cálculo alpha óptimo
  index <- scaled$liveability[train]
  Whitney <- apply(index + L*D[test,train], 1, min)
  McShane <- apply(index - L*D[test,train], 1, max)
  dif <- Whitney - McShane
  alpha <- sum(dif*(Whitney-scaled$liveability[test]))/sum(dif^2)
  
  ###Cálculo SMAPE
  I <- (1-alpha)*Whitney + alpha*McShane
  I <- I*(maxs[4]-mins[4])+mins[4]
  
  smape <- abs(I-df$liveability[test])/(I+df$liveability[test])
  SMAPE[iter] <- 200*sum(smape)/length(test)
  
  ###Mejor extensión
  if (SMAPE[iter] == min(SMAPE)) {
    Alpha <- alpha
    Lip <- L
    Train <- train
    Test <- test
  }
}

t2 <- Sys.time()
tiempo <- (t1-t0) + (t2-t1)/N

summary(SMAPE)

################### RANKING CANADÁ ####################

###Leemos los datos de Canadá
df.can <- read.csv("~/lipschitz/ciudadescan.csv", dec=",")
m <- nrow(df.can)

###Predicción
index <- scaled$liveability[Train]
I <- function(city) {
  city <- (city-mins[1:3])/(maxs[1:3]-mins[1:3])
  distancia <- sqrt(apply((scaled[Train,1:3]-city)^2,1,sum))
  Whitney <- min(index + Lip*distancia)
  McShane <- max(index - Lip*distancia)
  indice = (1-Alpha)*Whitney + Alpha*McShane
  
  return(indice*(maxs[4]-mins[4])+mins[4])
}

for (i in 1:m){
  df.can[i,5] <- I(as.numeric(df.can[i,2:4]))
}