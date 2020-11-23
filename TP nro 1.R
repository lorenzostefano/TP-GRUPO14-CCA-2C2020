


rm(list = ls())
path = " "
setwd(path)
getwd()
dir()

install.packages("ggplot2")

library(ggplot2)

##################################EJERCICIO1############################################


datos<-read.csv('flavors_of_cacao - Hoja 1.csv',header = T)


Mom_abs_orden_n<-function(columna,n){
  return(mean(columna^n))
}
#Para orden 2 por ejemplo

Mom_abs_orden_n(datos$Rating,2)

Mom_cen_orden_n<-function(columna,n){
  m<-c(NA)
  ex<-mean(columna)
  for (i in 1:nrow(datos)){
    m[i]<-(columna[i]-ex)^n
  }
  return(mean(m))
}

#para orden 2 por ejemplo 
Mom_cen_orden_n(datos$Rating,2)
var(datos$Rating)  #check

#Para calcular asimetria
skewness=function(x) {
  m3=mean((x-mean(x))^3)
  skew=m3/(sd(x)^3)
  skew
}

#Distribución de las variables
#Rating
ggplot(datos, aes(x = datos$Rating)) +
  geom_histogram(bins = 8, fill = "lightblue", colour = "black")+
  labs(title = 'Histograma',
       x = 'Rating alcanzado',
       y = 'Conteos',
       subtitle = 'Cacao')
skewness(datos$Rating)

ggplot(datos, aes(x = Rating)) +
  geom_density(fill="lightblue")+
  labs(title = 'Curva de densidad',
       x = 'Rating alcanzado',
       y = 'Densidad',
       subtitle = 'Cacao')

#% de cacao
ggplot(datos, aes(x = as.numeric(sub("%","",datos$Cocoa.Percent))/100)) +
  geom_histogram(bins = 6, fill = "lightblue", colour = "black")+
  labs(title = 'Histograma',
       x = '% de cacao',
       y = 'Conteos',
       subtitle = '% de Cacao')+
  coord_cartesian(xlim = c(0.3, 1.1)) 

skewness(as.numeric(sub("%","",datos$Cocoa.Percent))/100)
mean(as.numeric(sub("%","",datos$Cocoa.Percent))/100)

#Años
ggplot(datos, aes(x = Review.Date)) +
  geom_histogram(bins = 6, fill = "lightblue", colour = "black")+
  labs(title = 'Histograma',
       x = 'Años',
       y = 'Conteos',
       subtitle = 'Año de reseña')+
  coord_cartesian(xlim = c(2006, 2018))

skewness(datos$Review.Date)
mean(datos$Review.Date)

#Outliers

boxplot(datos$Rating,horizontal = T)
mtext(paste("Boxplot Rating"))
hist(datos$Rating)
boxplot.stats(datos$Rating)$out #muestra los outliers

boxplot(as.numeric(sub("%","",datos$Cocoa.Percent))/100,horizontal = T,cex=.5)
hist(as.numeric(sub("%","",datos$Cocoa.Percent))/100, main = "Histograma % Cocoa",xlab =  "Histograma % Cocoa")
boxplot.stats(as.numeric(sub("%","",datos$Cocoa.Percent))/100)$out #muestra los outliers

ggplot(datos, (aes(x = Rating))) +
  geom_boxplot(fill="lightblue")+
  labs(title = 'Box-Plot',
       subtitle = 'Rating')

ggplot(datos, (aes(x = as.numeric(sub("%","",datos$Cocoa.Percent))/100))) +
  geom_boxplot(fill="lightblue")+
  labs(title = 'Box-Plot',
       subtitle = '% de cacao',
       x="")

#Las otras variables no presentan valores outliers

#RELACION VARIABLES Y RATING

#companymaker #beantype

install.packages("dplyr") #nos sirve para agrupar datos y filtrar
library("dplyr")

CMaker<- datos %>% group_by("CompanyÂ..Maker.if.known." ) %>% summarise (mean(Rating))
names(CMaker)[2]<-"Average Rating"

write.csv2(CMaker,paste(path="CompanyMaker.csv"))

Btype<- datos %>% group_by("Specific.Bean.Origin.or.Bar.Name") %>% summarise (mean(Rating))
names(Btype)[2]<-"Average Rating"

#reviewdate

Rdate<- datos %>% group_by(Review.Date) %>% summarise (mean(Rating))
names(Rdate)[2]<-"Average Rating"

plot(Rdate,type="l",xlab = "Years",ylab='Rating') #Vemos como fue variando la calificacion promedio a lo largo del tiempo

review<-lm(datos$Rating~datos$Review.Date)
plot(datos$Review.Date,datos$Rating,pch = 19,cex = .7,xlab = "Review Date", ylab = "Rating",main = "Review Date ~ Rating")
abline(review,col = 'red')

summary(review)

#

Rdatelm<-lm(Rdate$`Average Rating`~Rdate$Review.Date)
summary(Rdatelm)

plot(Rdate$Review.Date,Rdate$`Average Rating`,pch = 19,cex = .7,xlab = "Review Date", ylab = "Average Rating")
abline(Rdatelm, col = 'red')


#companylocation


Comploc<- datos %>% group_by(Company.Location) %>% summarise (mean(Rating))
names(Comploc)[2]<-"Average Rating"


ggplot(Comploc, aes(x=Company.Location, y=`Average Rating`)) + 
  geom_bar(stat = "identity") + theme_grey(base_size = 4)

write.csv2(Comploc,paste(path="CompanyLocation.csv"))

#a)Donde se hacen los mejores granos (4 o mas de rating)

mejoresgranos<-filter(datos,Rating>=4)
mejoresgranos<-unique(mejoresgranos$Broad.Bean.Origin)
mejoresgranos<-mejoresgranos[-5] #eliminamos el vacio
  cat(as.character(mejoresgranos),sep='\n')

#mejor de todos

mejorgrano<-filter(datos,Rating==max(datos$Rating))
cat(as.character(mejorgrano$Broad.Bean.Origin[1]))


#B)Donde se hacen las mejores barras (4 o mas de rating)

mejoresbarras<-filter(datos,Rating>=4)
mejoresbarras<-unique(mejoresbarras$Company.Location)
cat(as.character(mejoresbarras),sep='\n')

#mejor de todos

mejorbarra<-filter(datos,Rating==max(datos$Rating))
cat(as.character(unique(mejorbarra$Company.Location)))


#c)Relacion % cacao y rating

cocoaperc<-as.numeric(sub("%","",datos$Cocoa.Percent))/100
Cocoa_rating<-lm(datos$Rating~cocoaperc)
summary(Cocoa_rating)


plot(cocoaperc,datos$Rating,pch = 19, cex = .4,xlab = 'Cocoa Percentage', ylab = "Rating",main = "Cocoa% ~ Rating")
abline(Cocoa_rating,col= 'red')

coefficients(Cocoa_rating)
summary(Cocoa_rating)

#NORMALIDAD

qqnorm(residuos)
qqline(residuos,col="red")

#INCORRELACION

install.packages("lmtest")
library("lmtest")      

dwtest(Cocoa_rating)

#HOMOCEDASTICIDAD

bptest(Cocoa_rating)




##################################EJERCICIO2############################################


datos1<-read.csv('flavors_of_cacao - Hoja 1.csv',header = T) #discreto

#RATING - VARIABLE OBJETIVO

#DATASET DISCRETO 

#EJERCICIO 2)1) #Rating binario - discreta

for (i in 1:nrow(datos1)){
  if(datos1$Rating[i]==5){
    datos1$Rating[i]<-1}
  else {datos1$Rating[i]<-0}
}

install.packages("installr", dependencies = TRUE)
library(installr)
updateR()


# Dummies

install.packages("fastDummies")  #Genera columnas dummy
library("fastDummies")

datos1<-dummy_columns(datos1)

datos1<-datos1[,-c(1,2,3,4,5,6,8,9)] #dejamos el dataset unicamente con variables numericas

# PARTICIONES

#Entrenamiento 70%
#Validacion 30%

install.packages('caTools') #nos ayuda a separar el dataframe
library("caTools")

smp_size <- floor(0.7 * nrow(datos1))

train_ind <- sample(seq_len(nrow(datos1)), size = smp_size)

#Discreta

train_discreta <- datos1[train_ind, ]
test_discreta <- datos1[-train_ind, ]

list2<-list(train_discreta,test_discreta)

listamodelo<-list(list1,list2)

###glm


glm(formula = Rating ~ ., family = binomial, data = train_discreta)


#Hay demasiadas variabes. Tomamos las primeras 3 para mostrar como se resuelve el modelo

reg_log_discreta<-glm(formula = Rating ~ `Company..Maker.if.known._A. Morin`+Company..Maker.if.known._Acalli+Company..Maker.if.known._Adi, family = binomial, data = train_discreta)

summary(reg_log_discreta)

#La funcion de regresion logistica nos permite predecir la probabilidad de que por cada nuevo valor de las variables independientes el rating sea igual a 5

probabilities <- reg_log_discreta %>% predict(test_discreta, type = "response")

probabilities


#DATASET CONTINUO

datos<-read.csv('flavors_of_cacao - Hoja 1.csv',header = T) #continua

library('dplyr')

datos<-datos[,-3] #sacamos REF no lo consideramos util para el analisis
datos$Cocoa.Percent<-as.numeric(sub("%","",datos$Cocoa.Percent))/100

#EJERCICIO 2)2) #Asignamos un grupo a cada variable y toma el valor promedio - continua

CM<- datos %>% group_by("CompanyÂ..Maker.if.known.") %>% summarise (mean(Rating))
SB<- datos %>% group_by("Specific.Bean.Origin.or.Bar.Name") %>% summarise (mean(Rating))
RD<- datos %>% group_by("Review.Date") %>% summarise (mean(Rating))
CP<- datos %>% group_by("Cocoa.Percent" ) %>% summarise (mean(Rating))
CL<- datos %>% group_by("Company.Location") %>% summarise (mean(Rating))
BT<- datos %>% group_by("Bean.Type") %>% summarise (mean(Rating))
BO<- datos %>% group_by("Broad.Bean.Origin") %>% summarise (mean(Rating))

names(datos)


nuevo<-matrix(NA,nrow = nrow(datos),ncol = 7)

for (i in 1:nrow(datos)){
  for(j in 1:nrow(CM)){
    if (datos$CompanyÂ..Maker.if.known.[i]==CM$Company..Maker.if.known.[j]){
      nuevo[i,1]<-CM$`mean(Rating)`[j]
    }
  }
  }

for (i in 1:nrow(datos)){
  for(j in 1:nrow(SB)){
    if (datos$Specific.Bean.Origin.or.Bar.Name[i]==SB$Specific.Bean.Origin.or.Bar.Name[j]){
      nuevo[i,2]<-SB$`mean(Rating)`[j]
    }
  }
}

for (i in 1:nrow(datos)){
  for(j in 1:nrow(RD)){
    if (datos$Review.Date[i]==RD$Review.Date[j]){
      nuevo[i,3]<-RD$`mean(Rating)`[j]
    }
  }
}


for (i in 1:nrow(datos)){
  for(j in 1:nrow(CP)){
    if (datos$Cocoa.Percent[i]==CP$Cocoa.Percent[j]){
      nuevo[i,4]<-CP$`mean(Rating)`[j]
    }
  }
}

for (i in 1:nrow(datos)){
  for(j in 1:nrow(CL)){
    if (datos$Company.Location[i]==CL$Company.Location[j]){
      nuevo[i,5]<-CL$`mean(Rating)`[j]
    }
  }
}

for (i in 1:nrow(datos)){
  for(j in 1:nrow(BT)){
    if (datos$Bean.Type[i]==BT$Bean.Type[j]){
      nuevo[i,6]<-BT$`mean(Rating)`[j]
    }
  }
}

for (i in 1:nrow(datos)){
  for(j in 1:nrow(BO)){
    if (datos$Broad.Bean.Origin[i]==BO$Broad.Bean.Origin[j]){
      nuevo[i,7]<-BO$`mean(Rating)`[j]
    }
  }
}

nuevo<-cbind(nuevo,datos$Rating)

colnames(nuevo) <- c('Company Maker', 'Spec Bean Origin','Review Date','Cocoa %','Company Location','Bean Type','Bean Origin','Rating')

nuevo<-as.data.frame(nuevo)


#Separacion 70/30 Continua


train_continua <- nuevo[train_ind, ]
test_continua <- nuevo[-train_ind, ]

list1<-list(train_continua,test_continua)


reg_lin_continua<-lm(Rating~.,data = train_continua)
summary(reg_lin_continua)


predicciones<-predict(reg_lin_continua,test_continua)

actuals_vs_predicteds <- data.frame(cbind(actuals=test_continua, predicteds=predicciones))

#5)

library("dplyr")

Mejores<-filter(datos, Rating == max(datos$Rating))
Peores<-filter(datos, Rating == min(datos$Rating))

