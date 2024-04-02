#######################################################################
#################  Red Neuronal tipo Feed Forward Neural ##############
##Neural Network Time Serie Regression ##https://pkg.robjhyndman.com/forecast/reference/nnetar.html
##Función:nnetar: "Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series."
library(forecast)
library(ggplot2)
#####
##Obtenemos precios de AMAZON
AMZN<-getSymbols("AMZN", from="2020-08-01",to="2021-03-31", src = "yahoo", auto.assign = FALSE) #
# Eliminando valores faltantes
AMZN <- na.omit(AMZN)
# Mantenemos columnas con Precios de Cierre  columna 4:
AMZN <- AMZN[,4]
##Podemos graficar:
plot(AMZN, ylab="Precios")
length(AMZN)
##Partimos serie, tomemos el 7% para la prueba
h <- round(length(AMZN)*0.07, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]
##################################################
## A partir de los mismos datos, grafiquemos la serie:
plot(train, col="red")
autoplot(train)

## Generamos la función de pronóstico. En datos de precios, se deben transformar 
#los datos lambda para tratar que los residuos sean cercanos a homocedásticos.  
nn1 <- nnetar(train, lambda = TRUE)
nn1

autoplot(forecast(nn1,PI=TRUE, h=12), include=50)
fnn1<-forecast(nn1,h=12)

## AR Nivel, recordemos que en la primera parte, teníamos un modelo ARMA con la parte AR(7) 
#que podemos incluir:

nn2=nnetar(train, p=7, lambda=TRUE)
nn2
autoplot(forecast(nn2,PI=TRUE, h=12))
fnn2<-forecast(nn2,h=12)

##Cálculo de las méricas de error de pronóstico:
library(Metrics)
RMSE_nnetar<-rmse(test, fnn1$mean)
MAPE_nnetar<-mape(test, fnn1$mean)
RMSE_nnetar2<-rmse(test, fnn2$mean)
MAPE_nnetar2<-mape(test, fnn2$mean)

###Imprimamos los resultados en una tabla:

Modelo<-c("ARIMA(7,1,3)", "AR(3)", "ses", "holt", "HW", "ets", "nnetar_z", "nnetar_ar7")

RMSE<-c(RMSE_arima, RMSEar1, RMSEses, RMSEholt, RMSE_HW, RMSEets, RMSE_nnetar, RMSE_nnetar2)

MAPE<-c(MAPE_arima, MAPEar1, MAPEses, MAPEholt, MAPE_HW, MAPEets,MAPE_nnetar, MAPE_nnetar2)

res<-data.frame(Modelo,RMSE, MAPE)

print((res))










install.packages("tsfeatures")
library(tsfeatures)
nonlinearity(train)
entropy(train)
