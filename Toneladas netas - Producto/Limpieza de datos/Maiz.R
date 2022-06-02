#Direcci´pon en donde tenemos la base de datos a ocupar
setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1\\ARTF_2017_2020\\ABRIL_ARTF")
library(readr) #Para poder leer nuestra base de datos
#Serie temporal     Toneladas_Netas-Productos
#ARTF
ARTF <- read.csv("ARTF_2014_2022.csv")#Cargamos base de datos    
ARTF2 <- ARTF  # Hacemos una copia de la base de datos original
ARTF2 <- ARTF2[ARTF2$PRODUCTOS =="MAÍZ",]#Filtramos columna por maíz
colnames(ARTF2) #Observamos las variables que tenemos
ARTF2 <- ARTF2[,-c(1,2)] # Quitamos la columna X y REGISTRO 
colnames(ARTF2) #Observamos las columnas que ahora tenemos
#Lo siguiente es ver cuales variables nos serviran en el modelo
summary(factor(ARTF2$CONCESIONARIO ))#Variable dummies
summary(factor(ARTF2$TRÁFICO_ARTF ))#Variable dummies
summary(factor(ARTF2$CARROS_CARGADOS ))#Variable numerica
summary(factor(ARTF2$TONELADAS_NETAS ))#Variable numerica
summary(factor(ARTF2$TONELADAS_KM)) #Variable numerica
summary(factor(ARTF2$DISTANCIA_MEDIA ))#Variable numerica
summary(factor(ARTF2$INGRESOS )) #Variable numerica
#Las siguiente columnas las podemos eliminar, ya que pertenecen a la
#Categoría de maíz.
summary(factor(ARTF2$PRODUCTOS)) #Nos quedamos esta columna
summary(factor(ARTF2$SUBGRUPO ))#Podemos eliminar esta variable
summary(factor(ARTF2$GRUPO ))#Podemos eliminar esta variable
#Para columna CONCESIONARIO, TRÁFICO_ARTF Variables dummies
ARTF2 <- ARTF2[,-c(10,11)]#Eliminamos variables que no ocuparemos
colnames(ARTF2) #Observamos las variables que ocuparemos para el modelo
#ARTF2
ARTF3 <- ARTF2 #Hacemos una copia de la base de datos obtenida
str(ARTF3) 
class(ARTF3$FECHA ) #Tendremos que cambiar a formato fecha
class(ARTF3$DISTANCIA_MEDIA )#Esta clase se tiene que convertir a numeric
library(quantmod) #Para convertir fecha a formato Mes/Año
ARTF3$FECHA <- as.yearmon(ARTF3$FECHA, format = "%m/%Y")#Conversión de character a fecha
ARTF3$DISTANCIA_MEDIA <- as.numeric(as.character(ARTF3$DISTANCIA_MEDIA))#Conversion a numeric
#ARTF3
ARTF4 <- ARTF3
ARTF4 <- ARTF4[order(ARTF4$FECHA),] #Ordenamos la matriz conforme a sus fechas
ARTF5 <- ARTF4[ARTF4$INGRESOS == 0,]#Veo cuales son los ingresos que me interesan
summary(factor(ARTF5$CONCESIONARIO )) #Veo que concesionarias "no me sirven"
ARTF6 <- ARTF4[ARTF4$CONCESIONARIO == "FTVM",] #Me quedo con la concesionaria que me sirve
summary(factor(ARTF6$TRÁFICO_ARTF))#Como solo es local, entonces lo podemos eliminar
ARTF6 <- ARTF6[,c(1,9,2:8)]#Eliminamos las columnas CONCESIONARIO y TRÁFICO_ARTF 
#ARTF6
library(dplyr)
#Agrupo mis datos FECHA; PRODUCTOS y CONCESIONARIO para poder
#Sumar las filas que tengan repetición y quedarme con la suma
#Esto lo hago para todas las columnas y les pongo el mismo nombre
ARTF7 <- ARTF6 %>% group_by(FECHA, PRODUCTOS , CONCESIONARIO, TRÁFICO_ARTF) %>%
  summarise( CARROS_CARGADOS =sum(CARROS_CARGADOS),
             TONELADAS_NETAS = sum(TONELADAS_NETAS),
             TONELADAS_KM = sum(TONELADAS_KM),
             DISTANCIA_MEDIA = sum(DISTANCIA_MEDIA),
             INGRESOS = sum(INGRESOS)
             )
ARTF7$time <- c(rep(2014,12),rep(2015,12),rep(2016,12),rep(2017,12),rep(2018,12),rep(2019,12),
                          rep(2020,12),rep(2021,12),rep(2022,2)) #Columna en donde estarán los años correspondientes.


r =c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO",
     "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")#Genero lista donde coloco los meses del año
ARTF7$Mes<- c(rep(r,8),c("ENERO","FEBRERO"))#Hago una columna en donde coloco los meses correspondiente a la fecha dada

setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1\\ARTF_2017_2020\\JUNIO_ARTF")
write.csv(ARTF7 , file = "Maiz_FTVM_LOCAL.csv" )

