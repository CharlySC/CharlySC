Vega <- function(s,r,q,K,tau,sigma){
  d1 <- (log(s/K)+(r-q+.5*sigma^2)*tau)/(sigma*sqrt(tau))
  V <- s*exp(-q*tau)*pnorm(d1)*sqrt(tau)
  return(V)
}

Heston <- function(tipo,s,K,tau,r,q,v,theta,rho,kappa,sigma){
  p <- callHestoncf(s,K,tau,r,q,v,theta,rho,kappa,sigma)
  if(tipo==1){
    precio <- p 
  }else if(tipo==-1){
    precio <- p - (s*exp(-q*tau)-K*exp(-r*tau)) 
  }
  return(precio)
}

Perdida <- function(parametros){
  
  theta <- parametros[1]
  rho <- parametros[2]
  sigma <- parametros[3]
  kappa <- parametros[4]
  
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
  if(metodo==1){L <- mean(as.numeric((precios_bs-precios_heston)^2))}
  if(metodo==2){L <- mean(as.numeric(abs(precios_bs-precios_heston)/precios_bs))}
  if(metodo==3){L <- mean(as.numeric(((precios_bs-precios_heston)/vega)^2))}
  return(L)
}
