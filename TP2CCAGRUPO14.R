#PROCESO BROWNIANO

Proceso_Browniano_desdet<-function(t,R,delta,Wt){
  A<-((R-t)/delta)+1
  VarPB <- c(NA)
  PB <- c(NA)
  VarPB[1]<-0
  PB[1]<-Wt
  for (i in 2:A){
    VarPB[i] <- rnorm(1,0, sqrt(delta))
  }
  for (j in 2:A){
    PB[j]<-PB[j-1]+VarPB[j]
  }
  return(PB)
}

#GRAFICO PROCESO BROWNIANO

set.seed(132)
Ejemplo_grafico<-Proceso_Browniano_desdet(0,10,0.1,0)
t<-seq(0,10,0.1)


plot(t,Ejemplo_grafico,type = 'l', main = 'Proceso Browniano' , xlab = 'Tiempo', ylab = 'W',xaxt ='n',yaxt='n' )
abline(h=0,lty=2)
points(10,Ejemplo_grafico[length(Ejemplo_grafico)],pch=16,cex=.7,col = 'red')
points(0,0,pch=16,cex=.7,col = 'red')
segments(0,Ejemplo_grafico[length(Ejemplo_grafico)],10,Ejemplo_grafico[length(Ejemplo_grafico)],lty=2,col = 'blue')
segments(10,min(Ejemplo_grafico),10,Ejemplo_grafico[length(Ejemplo_grafico)],lty=2,col = 'blue')
segments(0,0,0,min(Ejemplo_grafico),lty=2,col = 'blue')
axis(2, at=0,labels='W0=0', col.axis="red",las = 2)
axis(2, at=Ejemplo_grafico[length(Ejemplo_grafico)],labels='WT', col.axis="red",las = 2)
axis(1, at=0,labels='0', col.axis="red")
axis(1, at=10,labels='T', col.axis="red")


  
#GRAFICO PRECIO ACTIVO SUBYACENTE  

delta<-0.1
St<-10
sigma<-0.05
r<-0.01
EJPB <- Proceso_Browniano_desdet(0,20,delta,0)
EJSt<-c(NA)
for (j in 1:length(EJPB)){
  EJSt[j] <- St * exp((r - 0.5 * sigma ** 2) * ( delta * (j-1) ) + sigma * EJPB[j])
}

ej2<-cbind(seq(0,20,0.1),EJPB,EJSt) #Tabla donde se presentan las variaciones de W y St
colnames(ej2)<-c('t','Wt','St')

plot(seq(0,20,0.1),EJSt,type = "l",main="Precio del Activo Subyacente",xlab = "t",ylab = 'St')


#GRAFICO PAYOFF CALL Y PUT 

St<-0:20
k<-10
X<-c(rep(-1,10),0:10)

plot(St,X,type = "l")

#VALUACION DERIVADOS


Valuacion<-function(t,R,Wt,delta,r,sigma,St,s){
  der <- readline("Ingrese el derivado que desea valuar: ")
  A <- (R - t) / delta
  res <-c(NA)
  StV = c(NA)
  for (i in 1:s){
    for (j in 1:A){
    PB <- Proceso_Browniano_desdet(t,R,delta,Wt)
  StV[j] <- St * exp((r - 0.5 * sigma ** 2) * (t + delta * (j-1) - t) + sigma * (PB[j] - Wt))
  res[i] <- eval(parse(text=der))
    }}
  ret <- exp(-r*(T-t))*mean(res)
  return(ret)
  }
  
#VALUACION CALL

set.seed(145)
Valuacion(0,10,0,0.1,0.001,0.05,10,500) #max(StV[A]-12,0)


Call_Black_Scholes<-function(r,k,sigma,t,R,St){
  a <- (log(St/k)+(r+0.5*sigma^2)*(R-t))/(sigma*sqrt(R-t))
  b <- (log(St/k)+(r-0.5*sigma^2)*(R-t))/(sigma*sqrt(R-t))
  res <- St*pnorm(a) - k*exp(-r*(R-t))*pnorm(b)
  return(res)
}


Call_Black_Scholes(0.001,12,0.05,0,10,10)

#GRAFICO CALL

VtCall<-0.1204155
St<-0:20
k<-12
Xc<-c(rep(-VtCall,12),-VtCall:8)


plot(St,Xc,type = "l",xlab = "Precio Activo Subyacente",ylab = "Payoff/Beneficio Comprador",xaxt ='n',yaxt='n',main = "CALL" )
abline(h=0,lty=2)
points(k,-VtCall,pch=16,cex=.7,col = 'red')
points(20,max(Xc),pch=16,cex=.7,col = 'green')
axis(1, at=k,labels='K', col.axis="blue")
axis(2, at=0,labels='0', col.axis="blue",las = 2)


#VALUACION PUT

set.seed(146)
Valuacion(0,10,0,0.1,0.001,0.05,10,500) #max(12-StV[A],0)


Put_Black_Scholes<-function(r,k,sigma,t,R,St){
  a <- (log(St/k)+(r-0.5*sigma^2)*(R-t))/(sigma*sqrt(R-t))
  b <- (log(St/k)+(r+0.5*sigma^2)*(R-t))/(sigma*sqrt(R-t))
  res <- k*exp(-r*(R-t))*(1-pnorm(a)) - St*(1-pnorm(b))
  return(res)
}

Put_Black_Scholes(0.001,12,0.05,0,10,10)

#GRAFICO PUT

VtPut<-1.994563
St<-0:20
k<-12
Xp<-c((k-VtPut):-VtPut,rep(-VtPut,8))


plot(St,Xp,type = "l",xlab = "Precio Activo Subyacente",ylab = "Payoff/Beneficio Comprador",xaxt ='n',yaxt='n',main = "PUT")
abline(h=0,lty=2)
abline(h=-VtPut,lty=2)
points(k,-VtPut,pch=16,cex=.7,col = 'red')
points(0,k-VtPut,pch=16,cex=.7,col = 'green')
axis(1, at=k,labels='K', col.axis="blue")
axis(2, at=0,labels='0', col.axis="blue",las = 2)
axis(2, at=-VtPut,labels='-Vt', col.axis="blue",las = 2)

#VALUACION ULTIMO EJEMPLO

set.seed(147)
Valuacion(0,10,0,0.1,0.001,0.05,10,500) # max(max(StV)-2*min(StV),0)




