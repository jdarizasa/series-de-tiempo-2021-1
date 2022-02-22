library(readxl)
pitagoras <- read_excel("C:/Users/sanch/Downloads/pitagoras.xlsx", 
                        sheet = "train")

library(xts)
library(tidyverse)
library(TSstudio)

ts <- as.ts(pitagoras$views)
plot(ts)

difts <- diff(ts)
plot(difts)
acf(difts)
acf(difts,ci.type="ma") 
pacf(difts)
hist(difts)
acf(difts^2)
pacf(difts^2)

TSstudio::ts_plot(ts,
                  title="Web trafic",
                  Xtitle ='dia',
                  Ytitle = 'views',
                  Xgrid = TRUE,
                  Ygrid = TRUE,
                  slider=TRUE) 
library(forecast)
forecast::BoxCox.lambda(ts, method = "guerrero", lower = 0, upper = 2) #valor

logts <- (ts^0.6422 - 1)/0.6422
plot(logts)

auto.arima(logts,stationary = FALSE) ###morro basico
auto.arima(logts, max.p=7, max.q = 8, max.order = 15, stationary = TRUE, seasonal = FALSE, stepwise = FALSE) ####chad, mejor sin stepwise

arima(logts,order=c(6,1,1))
summary(arima(logts,order=c(6,1,1)))

ajuste=Arima(logts,order=c(2,1,4),seasonal = list(order = c(3, 1, 1), period = 7),include.mean=T,lambda =0 )


library(tsoutliers)

resi= residuals(ajuste)
coef= coefs2poly(ajuste)
outliers= locate.outliers(resi,coef, cval = 7.8)
outliers
n=length(logts)
xreg = outliers.effects(outliers,n )

analisis=Arima(logts,order=c(2,1,4),seasonal = list(order = c(3, 1, 1), period=7),include.mean=T,lambda =0 ,xreg=xreg)
analisis
resi_analisis= residuals(analisis)
coef_analisis= coefs2poly(analisis)
outliers_analisis= locate.outliers(resi_analisis,coef_analisis, cval = 7.8)
outliers_analisis

## Análisis de residuales
#x11()
residuales <- analisis$residuals
plot(residuales)
acf(residuales)
pacf(residuales)
######Análisis de Outliers
#Test de normalidad
JarqueBera.test(residuales)
#Test de autocorrelaci?n
Box.test(residuales, lag = (length(residuales)/4), type = "Ljung-Box", fitdf = 2)


###Estad?ticas CUSUM
res=residuales
cum=cumsum(res)/sd(res)
N=length(res)
cumq=cumsum(res^2)/sum(res^2)
Af=0.948 ###Cuantil del 95% para la estad?stica cusum
co=0.14422####Valor del cuantil aproximado para cusumsq para n/2
LS=Af*sqrt(N)+2*Af*c(1:length(res))/sqrt(N)
LI=-LS
LQS=co+(1:length(res))/N
LQI=-co+(1:length(res))/N
par(mfrow=c(2,1))
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUM Square
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")

pasos_adel=12
num_outliers=dim(outliers)[2]
regresoras_aditivos=matrix(c(rep(0,pasos_adel*(num_outliers-1))),pasos_adel,num_outliers-1)
regresoras_LS=matrix(c(rep(1,pasos_adel)),pasos_adel,3)
regresoras=cbind(regresoras_aditivos,regresoras_LS)
colnames(regresoras)=colnames(xreg)

pronostico_out=forecast(object=analisis,xreg=regresoras,h=pasos_adel) 
pronostico_out
plot(pronostico_out)


pasos_adel=163
num_outliers=dim(outliers)[2]
regresoras_aditivos=matrix(c(rep(0,pasos_adel*(num_outliers-1))),pasos_adel,num_outliers-1)
regresoras_LS=matrix(c(rep(1,pasos_adel)),pasos_adel,3)
regresoras=cbind(regresoras_aditivos,regresoras_LS)
colnames(regresoras)=colnames(xreg)

pronostico_out=forecast(object=analisis,xreg=regresoras,h=pasos_adel) 
val_test <- read_excel("C:/Users/sanch/Downloads/pitagoras.xlsx", 
                        sheet = "test")
mean((pronostico_out$mean - val_test$views)^2)

