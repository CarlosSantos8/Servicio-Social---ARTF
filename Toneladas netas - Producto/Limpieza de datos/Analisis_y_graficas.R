#Dirección en donde tenemos la base de datos a ocupar
setwd("C:\\Users\\81799\\Downloads\\Servicio Social - Git hub\\Toneladas - Netas\\Junio_Maiz")
library(readr) #Para poder leer nuestra base de datos
ARTF <- read.csv("ARTF.csv")#Cargamos base de datos con todos los productos 
ARTF <- ARTF[,-1] #Eliminamos la columna de los índices
library(quantmod) #Me sirve para poder transformar fechas en formato Mes/año
ARTF$FECHA <- as.yearmon(ARTF$FECHA)#Originalmente ya esta en formato fecha, entonces solamente lo convertimos en clas yearmon
PRODUCTOS <- read.csv("Productos.csv") #Cargamos la base de datos de los 10 productos con mayor toneladas netas
colnames(PRODUCTOS)[1] <- "FECHA" #Cambio el nombre de la columna 1 del dataFrame PRODUCTOS y le asigno el nombre de Fecha.
PRODUCTOS$FECHA <- as.yearmon(PRODUCTOS$FECHA)#Como ya esta en formato mes/año, entonces solamente le asigno su clase.
rownames(PRODUCTOS)[99] <- "TOTAL"




#Serie temporal de los 5 productos con mayor toneladas netas del 2014 al 2022
library(xts)
#El DataFrame de PRODUCTOS lo volvemos en clase xts para poder hacer nuestras gráficas de Series de tiempo 

Ton_Netas_P <- xts(PRODUCTOS[-99,2:6], order.by = PRODUCTOS[-99,1] )
color_T_N_P <- rainbow(ncol(Ton_Netas_P)) #Le asigno un color diferente por cada columna
Serie_Temporal <- plot.xts(Ton_Netas_P ,  col = color_T_N_P,
         legend.loc = "topleft", lwd = 2,
         cex = 0.5, ylab = "Toneladas Netas", xlab = "Tiempo",
         cex.axis = 0.7,
         main = "Toneladas Netas - Productos"
         )
Serie_Temporal
 

#---------------------Histograma del total de las Toneladas Netas------------------ 
library(dplyr)
H  <- ARTF %>% group_by(PRODUCTOS) %>% summarise(TOTAL = sum(TONELADAS_NETAS)) #Hago un DF del Total de Toneladas de cada Producto
H <- H[order(H$TOTAL , decreasing = T ), ][1:10, ] #Ordeno el DF de forma decreciente y después me quedó con las primeros 10 filas 
#Histograma Total de Frecuencias Absolutas en donde se muestran los 5 productos con mayor Toneladas Netas.
library(ggplot2)
ggplot(H , aes(x=PRODUCTOS , y=TOTAL ,
               label =scales::comma(TOTAL),
               fill = as.factor(PRODUCTOS)))+
  geom_bar(show.legend = T, stat = "identity" )+
  labs(title = "Los 5 productos con mayor Toneladas Netas de Enero/2014 a Febrero/2022",
       x = "Productos", y= "Toneladas Netas")+
  scale_y_continuous(labels = scales::comma )+
  theme(legend.position = "bottom", legend.text = element_text(size=8),
        legend.title =element_text(size = 0, color = "black"),
        axis.text.x = element_text(size = 0) )+
  geom_text(size=4, position = position_stack(vjust = 1.1))

#-------Histograma del deslgose total por año de las Toneladas Netas--------------
colnames(PRODUCTOS)[2:6]
H_2  <- ARTF %>% group_by(PRODUCTOS, Year) %>% summarise(TOTAL = sum(TONELADAS_NETAS)) #Hago un DF del Total de Toneladas de cada Producto
H_2 <- H_2 %>%
  filter(PRODUCTOS %in% c("MAÍZ", "CEMENTO",
                          "CONTENEDORES","MINERAL DE FIERRO",
                          "LÁMINAS Y PLANCHAS DE FIERRO Y ACERO"))

ggplot(H_2 , aes(x=PRODUCTOS , y=TOTAL ,
               label =scales::comma(TOTAL),
               fill = as.factor(Year)))+
  geom_bar(show.legend = T, stat = "identity" )+
  labs(title = "Desglose de los 5 productos con mayor Toneladas Netas de Enero/2014 a Febrero/2022 por año",
       x = "Productos", y= "Toneladas Netas")+
  scale_y_continuous(labels = scales::comma )+
  theme(legend.position = "bottom", legend.text = element_text(size=8),
        legend.title =element_text(size = 0, color = "black"),
        axis.text.x = element_text(size = 9) )+
  geom_text(size=4, position = position_stack(vjust = 0.5))

# ---------MAÍZ ---------------------------------------------------
library(forecast)
Maiz_ts <- ts(Ton_Netas_P[,1], frequency = 12, star = c(2014,1))
#Gráfica de la serie temporal del Maíz respcto a sus Toneladas Netas.
plot.xts(Ton_Netas_P[,1] , col = color_T_N_P ,
                         legend.loc = "topleft" , lwd = 2,
                         cex = 0.5, ylab = "Toneladas netas", xlab ="Tiempo",
                         cex.axis = 0.7,
                         main = "Toneladas Netas - Productos ")
#Gráfica de las parcelas estacionales del Maíz
ggseasonplot(Maiz_ts,year.labels =T, col = rainbow(9)) #Gráfica de las parcelas estacionales del maíz, nota: ggseasonplot solo acepta objetos de claase ts()
#Gráfica de subseries estacionales.
ggsubseriesplot(Maiz_ts) #Hacemos la gráfica de la subserie estacional del maíz.
#Descomposición de la serie temporal del Maíz 
decompose(Maiz_ts) 
#Gráfica de la descomposición del maíz.
autoplot(decompose(Maiz_ts))+
  labs(title = "Toneladas Netas - Productos de Maíz (Por componentes)",
       x="Período (Mensual)", y="")+theme_gray() #Con lo siguiente aquí tenemos la descomposición de la serie temporal del maíz en forma aditiva
#Gráfica de la serie temporal del maíz con su descomposición (tendencia)
Decom_Maiz_aditiva_y_Serie_original_Maiz <- cbind(Maiz_ts,decompose(Maiz_ts)$trend) #Obtengo la serie original y la tendencia por el método decompose del Maiz 
plot(Decom_Maiz_aditiva_y_Serie_original_Maiz, plot.type = "single",
     col = c("red","blue"),lwd=1:1 , lty=1:1, ylab = "Total de Maíz",
     xlab = "Tiempo (meses)", main = "Suavizado por el método decompose ()")
legend(x="topleft", legend =c("Original", "Decompose()"),col =c("red", "blue"),lty=1:1)
#Gráfica de la tendencia del Maíz
autoplot(decompose(Maiz_ts) $trend)+
  labs(title = "Toneladas Netas - Productos de Maíz (Por componentes)",
       x="Período (Mensual)", y="")+theme_grey()
#Gráfica de Autocorrelación simple
(acf(ts(Maiz_ts), lag.max=36, 
     main = "Auto-Correlación Simple del Maiz",
     xlab ="k-períodos"))
#Gráficas de retraso
gglagplot(Maiz_ts, do.lines =FALSE , diag = TRUE , diag.col = "red")
#Gráfica de Auto.correlación parcial
(pacf(ts(Maiz_ts), lag.max=36, 
      main = "Auto-Correlación Simple del Maiz",
      xlab ="k-períodos"))
#Promedio móvil k período hacía atrás
Maiz_SMA <- SMA(Maiz_ts,n=2) #Promedio hacia atras, es decir (y_{t}+t_{t-1})/2
Maiz_Suavizados <- cbind(Maiz_ts,Maiz_SMA) #Para hacer la gráfica
plot(Maiz_Suavizados, plot.type = "single", col = c("red", "blue"),
     lwd=1:1, lty=1:1, ylab = "Total de Maiz", xlab="Tiempo (meses)",
     main="Suavizado por el médoto SMA con k=1 retraso")
legend(x="topleft", legend=c("Originaal", "SMA(k=1)"),
       col = c("red", "blue"), lty=1:1)
#Promedio móvil centrado de orden m
MM_Maiz<-data.frame(Ton_Netas_P) %>% select(0, MAÍZ) %>% mutate("5-MA"=rollmean(MAÍZ,k=5,fill=NA)) #Promedio dado y_t en medio
MM_Maiz
m5_MA <- ts(MM_Maiz[,2], frequency =12 , start=c(2014,1)) #Para poder graficar 5-MA lo convertimos en class ts
Maiz_ts_y_5_MA <- cbind(Maiz_ts, m5_MA)#Obtengo los datos originales y el del promedio móvil centrado
plot(Maiz_ts_y_5_MA, plot.type = "single",
     col = c("red","blue"),lwd=1:1 , lty=1:1, ylab = "Total de Maíz",
     xlab = "Tiempo (meses)", main = "Suavizado por el método 5-MA")
legend(x="topleft", legend =c("Original", "5-MA"),col =c("red", "blue"),lty=1:1) 
#Suavizado por Holt-Winters
HoltWinters(Maiz_ts)
#Gráfica del suavizado por Holt-Winters
plot(HoltWinters(Maiz_ts), col = "red", col.predicted = "blue")
legend(x="topleft", 
       legend=c("Original", "Holt-Winters filtering"),
       col = c("red", "blue"), lty=1:1)
#Suavizado por la función ets() de R
Suavizado_Maiz_R <- ets(Maiz_ts, model = "ZZZ") #Suavizado por la función ets() en forma automatica
Suavizado_Maiz_R #Obtener los valores del suavizado 
plot(Suavizado_Maiz_R)#Gráfica del suavizado ets()
plot(Maiz_ts, main = "Maiz Enero/2014 - Febrero/2022" )
lines(Suavizado_Maiz_R$fitted , col = "red") #Suavizado Exponencial por R
legend(x="topleft", legend = c("Original", "Suavizado Exponencial por R"),
       col = c("black", "red"), lty=1:1 ) #Gráfica del suavizado vs valores originales
#Pruebas de estacionariedad
adf.test(Maiz_ts, alternative = "stationary", k=0)#Prueba de Dickey-Fuller aumentada
pp.test(Maiz_ts , alternative = "stationary" )#Prueba de Phillips-Perron
#Modelo ARIMA(p,d,q)
ModA_ARIMA =arima(Maiz_ts , order = c(1,0,1))
ModB_ARIMA =arima(Maiz_ts , order = c(1,0,0))
ModC_ARIMA =arima(Maiz_ts , order = c(2,0,1))
ModD_ARIMA =arima(Maiz_ts , order = c(1,0,2))
ModE_ARIMA =arima(Maiz_ts , order = c(0,0,1))
#SARIMA 
auto.arima(Maiz_ts) #Modelo automatico 
ndiffs(Maiz_ts) # Nos da el número de diferenciación ordinaria
nsdiffs(Maiz_ts) #Nos da el número de diferenciación estacional
tsdiag(SARIMA_4) #Diagnostico del modelo
autoplot(SARIMA_4)+theme_gray() #Gráfico de raíces unitarias dell modelo SARIMA
Error = residuals(SARIMA_4) #Errores utilizando SARIMA
autoplot(Error)+geom_hline(yintercept = 0, lty = 2, color ="blue")+
  labs(title = "Errores del modelo SARIMA(1,1,1)(1,1,1)[12]", x="Meses", y= " ")+theme_bw()
#Pruebas de normalidad
jarque.bera.test(SARIMA_4$residuals)#Prueba de Jarque Bera
shapiro.test(SARIMA_4$residuals) #Prueba de Shapiro-Wilk 
#Pronósticos
Suavizado_Maiz_R <- ets(Maiz_ts, model = "ZZZ")
Pronostico_Maiz_ETS  <- forecast(Suavizado_Maiz_R, h=60)  # Hace el pronóstico de 1 año
Pronostico_Maiz_ETS
plot(Pronostico_Maiz_ETS)  #Nos hace la gráfica

Maiz_H_W  <- HoltWinters(Maiz_ts)
prediccion_H_W =predict(Maiz_H_W,60)
prediccion_H_W
Pronostico_H_W <- ts(data.frame(predict(Maiz_H_W,60)), frequency = 12,
                     star =c(2022,3))
plot(Pronostico_H_W)


