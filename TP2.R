Proceso_Browniano1<-function(t,R,W,n){
  A<-(R/t)/n
  VARPB<-c(NA)
  PB<-c(NA)
  VARPB[1]<-0
  PB[1]<-W
  for (i in 2:A){
    VARPB[i]<-rnorm(1,0,sqrt(n))
  }
  for (j in 2:A){
    PB[j]<-PB[j-1]+VARPB[j]
    
  }
  return(PB)

}


plot(Proceso_Browniano1(0.1,10,1,0.1),type = "l")


Call_Black_Scholes<-function(r,k,sigma,t,R,St){
  a <- (log(St/k)+(r+0.5*sigma**2)*(R-t))/(sigma*sqrt(R-t))
  b <- (log(St/k)+(r-0.5*sigma**2)*(R-t))/(sigma*sqrt(R-t))
  res <- St*pnorm(a) - k*exp(-r*(R-t))*pnorm(b)
return(res)
}

print(Call_Black_Scholes(0.001,12,0.05,0,10,10))


help(pnorm)
