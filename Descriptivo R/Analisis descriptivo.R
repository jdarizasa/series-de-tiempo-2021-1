#ANALISIS DESCRIPTIVO DE UNA SERIE DE TIEMPO

#CARGAR DATOS####
library(readxl)
data <- read_excel("~/Material Cursos/Series de Tiempo/Serie de Ejercicios.xlsx", 
                                  sheet = "Colcap")

#PRE-PROCESAMIENTO DE LA SERIE####
library(xts)
library(tidyverse)
library(TSstudio)
ApColcap <- xts(data$Apertura, order.by = as.Date(data$Fecha))
ts_info(ApColcap)
str(ApColcap)
class(ApColcap)
head(index(ApColcap))
class(index(ApColcap))
is.regular(ApColcap, strict = FALSE)

#esto es para reversar los datos porque estan al revés, pero xts lo hace automatico
Ap <- data %>% map_df(rev)
ApColcap1 <- xts(Ap$Apertura, order.by = as.Date(Ap$Fecha))
head(ApColcap1)
head(ApColcap)

#PRIMEROS GRAFICOS####
plot(ApColcap)
plot.ts(ApColcap)
#son el mismo
acf(ApColcap)
#gráfico interactivo
TSstudio::ts_plot(ApColcap,
                  title="Precio ColCap",
                  Xtitle ='Dia',
                  Ytitle = 'Precio',
                  Xgrid = TRUE,
                  Ygrid = TRUE,
                  slider=TRUE) 

#ANALISIS DE TENDENCIA####
library(astsa)
#Eliminar la tendencia con y - est(y)
summary(fit <- lm(ApColcap ~ time(ApColcap), na.action = NULL))
plot.ts(ApColcap,ylim=c(500,10000))
abline(fit, col='red')
#claramente no está necesitando eso, mire el r^2
sinten <- ApColcap - predict(fit)
plot(sinten, main='Grafico eliminando la tendencia')
#no le hace nada

#Eliminar la tendencia con diferenciación
plot(diff(ApColcap), type="l", main="Primera Diferencia") 
plot.ts(diff(ApColcap), type='l', main='Primera Diferencia')

#Vemos los cambios en el acf
par(mfrow=c(3,1)) 
acf(ApColcap, 100, main="ACF Colcap")
acf(resid(fit), 100, main="ACF Sin tendencia") 
acf(diff(ApColcap)[2:101], 100, main="ACF Primera Diferencia")

#VARIANZA MARGINAL####
library(FitAR)
library(forecast)
forecast::BoxCox.lambda(ApColcap, method = "guerrero", lower = 0, upper = 2) #valor
#el valor es mayor a 1, no toca hacer la transformación de BoxCox

#GRAFICAS DE RETARDOS####
windows()
par(mar = c(3,2,3,2))
astsa::lag1.plot(ApColcap, 12)
ts_lags(ApColcap,lags=1:12)

#AUTOCORRELACION####
acf(ApColcap,100,main='ColCap')
#ccf(serie1,serie2, retardos, ...)

#DETECCION DE CICLOS PROBLEMAS####
TSstudio::ts_heatmap(ApColcap, title = 'heatmap apertura ColCap')
#pues no sale nada, veamos en una parte más pequeña del gráfico
corte <- window(ApColcap, start ="2019-04-01" , end = "2020-02-15") #window para tomar solo una parte 
ts_plot(corte,
        title = "Corte de apertura ColCap",
        Ytitle = "Valor",
        Xtitle = "Dia",
        Xgrid = TRUE,
        Ygrid = TRUE)
ts_heatmap(corte, title = 'heatmap corte ColCap')

#MEDIDAS DESCRIPTIVAS####
#para series en formato ts
USgas_df <- data.frame(year = floor(time(USgas)), month = cycle(USgas),USgas = as.numeric(USgas))

library(dplyr)
USgas_summary <- USgas_df %>%group_by(month) %>%summarise(mean= mean(USgas),sd = sd(USgas))
USgas_summary

library(plotly)
plot_ly (data = ColCap_summary, x = ~ month, y = ~ mean, type = "bar", name   = "Mean") %>%
  layout (title = "ColCap - Monthly Average", yaxis =list(title = "Mean",   range = c(1000, 1700)))

#para series en formato xts
ApColcap_df <- data.frame(year = as.numeric(format(time(ApColcap),'%Y')), month = as.numeric(format(time(ApColcap),'%m')),ApColcap = as.numeric(ApColcap))
head(ApColcap_df)

ApColcap_df$month <- factor(month.abb[ApColcap_df$month], levels = month.abb)
ApColcap_df$year <- factor(ApColcap_df$year, levels = c(2018,2019,2020,2021))

ColCap_summary <- tapply(ApColcap_df$USgas, ApColcap_df$month,mean)
ColCap_summary

ColCapy_summary <- tapply(ApColcap_df$USgas, ApColcap_df$year,mean)
ColCapy_summary

plot(c(2018:2021), ColCapy_summary, type = 'h',col='red',lwd=10, 
     main = 'ColCap- year average', xlab='años', ylab ='mean')

plot(c(1:12), ColCap_summary, type = 'h',col='green',lwd=10, 
     main = 'ColCap- month average', xlab='meses', ylab = 'mean')

#GRAFICOS DE DENSIDADES PARA EXPLORAR ESTACIONALIDAD####
library(ggplot2)

#MESES
ggplot(ApColcap_df, aes(x = ApColcap)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(as.factor(month)))
#sin tendencia
USgas_df$USgas_detrend <- USgas_df$USgas - decompose(USgas)$trend
ggplot(USgas_df, aes(x = USgas_detrend)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas - Estimación de la densidad vía Kernel por mes") +
  facet_grid(rows = vars(as.factor(month)))
#diferenciada
ApColcap_df$ColDiff <- c(0 , diff(ApColcap_df$ApColcap))
ggplot(ApColcap_df, aes(x = ColDiff)) +
  geom_density(aes(fill = month)) +
  ggtitle("ColCap - Estimación de la densidad vía Kernel por mes") +
  facet_grid(rows = vars(as.factor(month)))

#AÑOS
ggplot(ApColcap_df, aes(x = ColDiff)) +
  geom_density(aes(fill = year)) +
  ggtitle("ColCap - Estimación de la densidad vía Kernel por año") +
  facet_grid(rows = vars(as.factor(year)))

#PAQUETE FORECAST PARA EL ANALISIS DE ESTACIONALIDAD####
#la data no es estacional entonces ni pedo con esto
library(forecast)
ggseasonplot(USgas,year.labels=TRUE,continuous=TRUE)

ggseasonplot(USgas,  polar = TRUE)

#pero tsstudio hace algo similar
ts_seasonal(ApColcap,type ="normal")
ts_quantile(ApColcap, period = "yearly")
ts_quantile(ApColcap, period = "monthly")
ts_quantile(ApColcap, period = "quarterly")

#PERIODOGRAMA####
library(astsa)
colcap.per = astsa::mvspec(ApColcap, log="no")
ubicacioncap=which.max(colcap.per$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para SOI es: %s",colcap.per$freq[ubicacioncap])
n_col <- length(colcap.per$spec)
valor_seg_colcap=sort(colcap.per$spec,partial=n_col-1)[n_col-1]
ubica_segundo_cap=which(colcap.per$spec==valor_seg_colcap)

sprintf("El valor de la frecuencia donde se alcanza el segundo máximo para el periodograma para SOI es: %s",colcap.per$freq[ubica_segundo_cap])
abline(v=colcap.per$freq[ubicacioncap], lty=2,col="blue")
abline(v=colcap.per$freq[ubica_segundo_cap], lty=2,col="blue")

#SUAVIZAMIENTO####

#PROMEDIOS MOVILES
wgts = c(.5, rep(1,11), .5)/12   ###Los pesos de los filtros
colf = stats::filter(ApColcap, sides=2, filter=wgts)
plot.ts(ApColcap)
lines(colf, lwd=2, col=4)

#SUAVIZAMIENTO KERNEL
plot.ts(ApColcap)
est <- ksmooth(time(ApColcap), ApColcap_df$ApColcap, "normal", bandwidth=1)
any(is.na(est$y))
est1 <- na.omit(est$y)
lines(est1 , lwd=3, col=4)

#LOWESS
plot.ts(ApColcap)
lines(lowess(ApColcap, f=.05), lwd=2, col=4)  # Ciclo El Niño 
lines(lowess(ApColcap), lty=2, lwd=2, col=2)  # tendencia (con span por defecto)

#SPLINES
plot.ts(ApColcap, xlim=c(0,1200))

spli1 <-smooth.spline(time(ApColcap), ApColcap, spar=.5)
spli1$x <- spli1$x - 17576
lines(smooth.spline(time(ApColcap), ApColcap, spar=.5), lwd=2, col=4)
lines(spli1, lwd=2, col=4)

spli2 <- smooth.spline(time(ApColcap), ApColcap, spar= 1)
spli2$x <- spli2$x - 17576
lines(smooth.spline(time(ApColcap), ApColcap, spar= 1), lty=2, lwd=2, col=2)
lines(spli2, lty=2, lwd=2, col=2)

#MODELO DE DESCOMPOSICION##########
#Por promedios moviles
#no se le hizo transformación de BoXCox entonces entra OK
desColCap=decompose(ApColcap)
plot(deslAirPass)
deslAirPass

#Por suavizamiento exponencial
HWAP=stats::HoltWinters(ApColcap,seasonal="additive", gamma = FALSE)
plot(HWAP)
ajustados=fitted(HWAP)
plot(ajustados)
ajustados
HWAP
predictionHWAP_1=forecast(HWAP,h=50,level =0.95,lambda = NULL)
predictionHWAP_1
plot(predictionHWAP_1)
predictionHWAP_2=predict(HWAP,n.ahead = 12, prediction.interval = T, level = 0.95)
predictionHWAP_2

#ARBOLES DE DECISION####
library(NTS)

#Dividir los datos
icc <- diff(data$Apertura)
m1 = NNsetting(icc,nfore=365,lags=c(1:18))###configuración prueba y entrenamiento al igual que las covariables.Es decir, los rezagos son y_1,....,y_10, y y_365,..,y_370, debido al ciclo anual.
## Ademas nfore es el conjunto de prueba y lags las variables rezagadas
names(m1)
X= m1$X; y = m1$y; predX = m1$predX; predY = m1$predY
#X es conjunto de prueba, predX es la var. de respuesta de X, igual para Y

#Ajustar el modelo
t1 = tree(y~.,data=data.frame(X))
par(mfcol=c(1,1))
plot(t1)
text(t1,pretty=0) 
pt1 = predict(t1,newdata=data.frame(predX)) ###Predicción sobre la muestra de prueba
er3 = pt1-predY ###Errores de predicción
mean(abs(er3))
sqrt(mean(er3^2))

# Residuales del Modelo

pp1 =predict(t1,newdata=data.frame(X))
resi= y-pp1
acf(resi,lag.max = 400)
plot(resi,type='l')
hist(resi)

#Poda del arbol
cv.t1 = cv.tree(t1) 
plot(cv.t1$size,cv.t1$dev,type="b")##Parece que el tamaño del árbol es 4
prune.t1= prune.tree(t1,best=2)
plot(prune.t1)   ####Gráfica de árbol con la poda
text(prune.t1,pretty=0)
prun = predict(prune.t1,newdata=data.frame(predX)) 
per =predY-prun
mean(abs(per))
sqrt(mean(per^2))

#Residuales con el nuevo tamaño
pp1_tune =predict(prune.t1,newdata=data.frame(X))
resi_tune= y-pp1_tune
acf(resi_tune)
plot(resi_tune,type='l')

#REDES NEURONALES####
library(tidyverse) # metapackage with lots of helpful functions
library(reticulate) #Call Python from R
library(tensorflow) #Neural Network Backend for Keras
use_python("/opt/anaconda3/bin/python3")
library(keras) #Neural Network Modeling
library(plyr) #Data manipulation
library(dplyr) # Data Manipulation
library(caret)

icc <- diff(data$Apertura)
m1_training_test = NNsetting(icc,nfore=365,lags=c(1:18))###configuración prueba y entrenamiento al igual que las covariables.Es decir, los rezagos son y_1,....,y_10, y y_365,..,y_370, debido al ciclo anual.
## Ademas nfore es el conjunto de prueba y lags las variables rezagadas
names(m1_training_test)
X_training_val= m1_training_test$X; y_training_val = m1_training_test$y; predX_test = data.frame(m1_training_test$predX); predY_test = m1_training_test$predY
X_training=data.frame(X_training_val[1:(dim(X_training_val)[1]-365),])
X_val=data.frame(X_training_val[(dim(X_training_val)[1]-365+1):dim(X_training_val)[1],])
y_training=y_training_val[1:(length(y_training_val)-365)]
y_val=y_training_val[(length(y_training_val)-365+1):length(y_training_val)]

pp = caret::preProcess(X_training, method = "range")
X_training_norm=predict(pp, X_training)
predX_test_norm=predict(pp, predX_test)
X_val_norm=predict(pp, X_val)

#definimos el modelo
model<-keras_model_sequential()%>%layer_dense(units=32,input_shape=list(dim(X_training_norm)[[-1]]) ,activation = "relu")%>%layer_dense(1) 

summary(model)

#entrenamos
model %>% compile(
  optimizer = "sgd",
  loss="mse"
)
val_data=list(as.matrix(X_val_norm),as.matrix(y_val))
model %>% 
  fit(
    x = as.matrix(X_training_norm), y = as.matrix(y_training),
    epochs = 50,
    validation_data=val_data,
    verbose = 2
  )

#evaluacion del pronostico
model%>%evaluate(as.matrix(predX_test_norm),predY_test)

ypred=model%>%predict(as.matrix(predX_test_norm))
ypred

#comparacion
RealvsPron=data.frame(Pronostico=ypred,Real=predY_test)
head(RealvsPron)
plot(as.numeric(rownames(RealvsPron)),RealvsPron$Real,type='o',col='blue')
lines(as.numeric(rownames(RealvsPron)),RealvsPron$Pronostico,type='o',col='red')


