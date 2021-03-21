library(stringr)
library(dplyr)
library(NMOF)
library(DEoptim)

######################### Funciones requeridas y datos de mercado ###################
setwd("C:/Users/KB/Documents/Adrián/Tesis/Codigos v3/")
source("Funciones Tesis v3.R")

datos <- read.csv("Precios Opciones de Mercado.csv",header = T)

Tasas <- datos[,1:4]
Strikes <- datos[,c(1,which(str_detect(colnames(datos),"Strike")))]
Vol <- datos[,c(1,which(str_detect(colnames(datos),"Vol")))]
PreciosBS <- datos[,c(1,which(str_detect(colnames(datos),"Call")))]

colnames(Strikes)[2:ncol(Strikes)] <- gsub("Strike.","",colnames(Strikes)[2:ncol(Strikes)])
colnames(Vol)[2:ncol(Vol)] <- gsub("Vol.","",colnames(Vol)[2:ncol(Vol)])
colnames(PreciosBS)[2:ncol(PreciosBS)] <- gsub("Call.","",colnames(PreciosBS)[2:ncol(PreciosBS)])

Tipo <- matrix(1,nrow=nrow(PreciosBS),ncol=ncol(PreciosBS)-1)
colnames(Tipo) <- colnames(PreciosBS)[2:ncol(PreciosBS)]
Tipo[,which(str_sub(colnames(Tipo),start = -1)=="P")] <- -1

# Parametros Mercado
s <- 22.0362
dia <- 360
v <- Vol[which(Vol$dias==dia),"ATM"]^2
metodo <- 1
# metodo 1: error medio cuadrado
# metodo 2: error medio relativo
# metodo 3: error medio cuadrado ponderado por vega

################# Calibracion Differential Evolution #################
set.seed(7954597)
A <- DEoptim(Perdida,lower = c(0.000001,-1,0.1,1),upper = c(1,1,3,20),
             control = list(NP=50*4,itermax=200))
A$optim$bestval
P <- A$optim$bestmem
theta <- P[1]
rho <- P[2]
sigma <- P[3]
kappa <- P[4]

# Comparativa con precios de mercado
precios_bs <- PreciosBS[which(PreciosBS$dias==dia),2:length(PreciosBS)]
precios_heston <- numeric(length(precios_bs))
vega <- numeric(length(precios_bs))
tau <- Tasas[which(Tasas$dias==dia),"tau"]
r <- Tasas[which(Tasas$dias==dia),"r"]
q <- Tasas[which(Tasas$dias==dia),"q"]
for(j in 1:length(precios_heston)){
  K <- Strikes[which(Strikes$dias==dia),1+j]
  tipo <- Tipo[which(Strikes$dias==dia),j]
  vol_mercado <- Vol[which(Vol$dias==dia),1+j]
  precios_heston[j] <- Heston(tipo,s,K,tau,r,q,v,theta,rho,kappa,sigma)
  vega[j] <- Vega(s,r,q,K,tau,vol_mercado)
}

if(metodo==1){
  L <- mean(as.numeric((precios_bs-precios_heston)^2))
  Dif <- (precios_bs-precios_heston)^2
  resultados <- rbind(precios_bs,precios_heston,Dif)
  row.names(resultados) <- c("Precios Mercado","Precios Heston","Diferencia Cuadrado")
}
if(metodo==2){
  L <- mean(as.numeric(abs(precios_bs-precios_heston)/precios_bs))
  Dif <- abs(precios_bs-precios_heston)/precios_bs
  resultados <- rbind(precios_bs,precios_heston,Dif)
  row.names(resultados) <- c("Precios Mercado","Precios Heston","Diferencia Relativa")
}
if(metodo==3){
  L <- mean(as.numeric(((precios_bs-precios_heston)/vega)^2))
  Dif <- ((precios_bs-precios_heston)/vega)^2
  resultados <- rbind(precios_bs,precios_heston,Dif)
  row.names(resultados) <- c("Precios Mercado","Precios Heston","Diferencia Vega")
}

write.csv(resultados,"Resultados.csv")


# Comparativa con volatilidades implicitas
library(RQuantLib)

resultados <- read.csv("Resultados Heston.csv")
names(resultados) <- gsub("X","",names(resultados))
resultados$Modelo <- paste0("Heston",resultados$Modelo)
tau <- Tasas[which(Tasas$dias==dia),"tau"]
r <- Tasas[which(Tasas$dias==dia),"r"]
q <- Tasas[which(Tasas$dias==dia),"q"]
Vol_Implicita <- matrix(nrow = nrow(resultados),ncol = ncol(resultados)-1)
for (i in 2:length(Strikes)) {
  K <- Strikes[which(Strikes$dias==dia),i]
  sigma <- Vol[which(Vol$dias==dia),i]
  tipo <- Tipo[which(Vol$dias==dia),i-1]
  tipo <- ifelse(tipo==-1,"put","call")
  for (j in 1:nrow(resultados)) {
    precio <- resultados[j,i]
    aux <- as.double(EuropeanOptionImpliedVolatility(type=tipo, 
                                           value=precio, 
                                           underlying=s,
                                           strike=K, 
                                           dividendYield=q, 
                                           riskFreeRate=r,
                                           maturity=tau, 
                                           volatility=sigma))
    Vol_Implicita[j,i-1] <- aux
  }
}
Vol_Implicita <- rbind(Vol_Implicita,as.numeric(Vol[which(Vol$dias==dia),-1]))
Modelo <- c(resultados[,1],"Mercado")
Vol_Implicita <- cbind.data.frame(Modelo,Vol_Implicita)
names(Vol_Implicita) <- names(resultados)

write.csv(Vol_Implicita,"Vol_Implicita.csv",row.names = F)
