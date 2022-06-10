#Direcci´pon en donde tenemos la base de datos a ocupar
setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1\\ARTF_2017_2020\\ABRIL_ARTF")
library(readr) #Para poder leer nuestra base de datos
#Serie temporal     Toneladas_Netas-Productos
#ARTF
ARTF <- read.csv("ARTF_2014_2022.csv")#Cargamos base de datos    
ARTF2 <- ARTF  # Hacemos una copia de la base de datos original
library(quantmod) #Para convertir fecha a formato Mes/Año
ARTF2$FECHA <- as.yearmon(ARTF2$FECHA, format = "%m/%Y")#Conversión de character a fecha
ARTF2$DISTANCIA_MEDIA <- as.numeric(as.character(ARTF2$DISTANCIA_MEDIA))#Conversion a numeric
ARTF2 <- ARTF2[,-c(1,2)] #Elimino la primera y segunda columna de mi DataFrame
str(ARTF2) #Saber las clases de cada columna 
ARTF2$Year <- as.numeric(format(ARTF2$FECHA, "%Y"))#Extrae en una columna el Año
ARTF2$Mes <- as.numeric(format(ARTF2$FECHA, "%m")) #Extrae el mes 
library(dplyr)
#Agrupo mis datos FECHA,PRODUCTOS, CONCESIONARIO Y TRÁFICO_ARTF  para poder
#Sumar las filas que tengan repetición y quedarme con la suma
#Esto lo hago para todas las columnas y les pongo el mismo nombre
ARTF3 <- ARTF2 %>% group_by(FECHA,Mes,Year,PRODUCTOS,CONCESIONARIO, TRÁFICO_ARTF) %>%
  summarise( CARROS_CARGADOS =sum(CARROS_CARGADOS),
             TONELADAS_NETAS = sum(TONELADAS_NETAS),
             TONELADAS_KM = sum(TONELADAS_KM),
             DISTANCIA_MEDIA = sum(DISTANCIA_MEDIA),
             INGRESOS = sum(INGRESOS)
  )
Mess <- seq(1,12,1) #Creo un asecuencia del 1 al 12
MES <- c("ENERO","FEBRERO", "MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO",
         "SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE") #Escribo los mes en forma "ascendente"
Meses <- data.frame(Mess,MES) #Hago un DataFrame llamado Meses, esto con la finalidad de compararlo más adelante y quedarme con la columna MES cuando coincidan
#La función left_join() me sirve para comparar dos DataFrame y al momento de compararlos
#se queda con la siguiente columna del 2do DataFrame que se coloca en la función
#En nuestro caso cuando la columna Mes del DataFrame ARTF3 es igual a la columna 
#MESS del DataFrame, entonces hace una nueva columna llamada MES y le coloca el valor.
ARTF3 <- left_join(ARTF3,Meses, by = c("Mes"="Mess"))
ARTF3 <- ARTF3[,c(1,12,3:11)]
ARTF3[,c(2,3,5,6)] <- lapply(ARTF3[,c(2,3,5,6)], as.factor) #Convierte la columna 2,3,5 y 6 en factor
levels(ARTF3$CONCESIONARIO)#Para ver el orden que lo pondra en el eje x
levels(factor(ARTF3$Year))#Es el orden que hay en la columna Year

setwd("C:\\Users\\81799\\Downloads\\Servicio Social - Git hub\\Toneladas - Netas\\Junio_Maiz")
write.csv(ARTF3, "ARTF.csv")



