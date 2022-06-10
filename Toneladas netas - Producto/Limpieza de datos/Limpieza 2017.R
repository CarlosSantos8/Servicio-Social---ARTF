library(readr)
library(dplyr)
setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1")
 #LIMPIEZA DE BASE DE DATOS EN CUESTIÓN DE LAS FECHAS 
MESES <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO"
           ,"JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE"
           ,"DICIEMBRE")
MES <- c("01","02","03","04","05","06","07","08","09","10","11","12")
MESESS <- data.frame(MESES, MES) #Creación de un nvo DataFrame 

MESES17 <- c("ENERO/2017", "FEBRERO/2017", "MARZO/2017", "ABRIL/2017",
             "MAYO/2017", "JUNIO/2017","JULIO/2017", "AGOSTO/2017",
             "SEPTIEMBRE/2017", "OCTUBRE/2017", "NOVIEMBRE/2017","DICIEMBRE/2017")
MES17 <- c("01/2017","02/2017","03/2017","04/2017","05/2017","06/2017","07/2017",
           "08/2017","09/2017","10/2017","11/2017","12/2017")
MESESS17 <- data.frame(MESES17, MES17) #Creación de un nvo DataFrame 

MESES17_1 <- c(" 01/2017", " 02/2017", " 03/2017", " 04/2017",
               " 05/2017", " 06/2017"," 07/2017", " 08/2017",
               " 09/2017", " 10/2017", " 11/2017"," 12/2017")
MES17_1 <- c("01/2017","02/2017","03/2017","04/2017","05/2017","06/2017","07/2017",
             "08/2017","09/2017","10/2017","11/2017","12/2017")
MESESS17_1 <- data.frame(MESES17_1, MES17_1) #Creación de un nvo DataFrame 

MESES2 <- c("ENE/2017", "FEB/2017","MAR/2017", "NOV/2017", "OCT/2017", "SEPT/2017", "DIC/2017")
MES2 <- c("01/2017","02/2017","03/2017","11/2017","10/2017","09/2017","12/2017")
MESESS2 <- data.frame(MESES2, MES2 )
#Los antteriores Data Frame me serviran para poder hacer comparaciones de los tipos de datos 
# en formato fecha que están en el cvs y cambiarlos de la forma dd/AAAA
#setwd es para indicarme enel directorio donde buscará R la informacion requerida
setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1")
Matriz_Total_ARTF_2017  <- read.csv("Matriz O-D_2017_Total.csv") #Lectura del csv
ARTF_2017 <- data.frame(Matriz_Total_ARTF_2017[-c(1:5),-1]) #subconjunto
ARTF_2017$X.1 <- toupper(ARTF_2017$X.1) # Paso la columna X.1 a mayusculas
#Comparación del data frame leído con los creados y cuando hay considencia me quedo con la
#2da columna de los data frame creados
ARTF_2017 <- left_join(ARTF_2017 , MESESS17, by = c("X.1" = "MESES17")) 
#Esto me sirve para cuando hay NA ponga los de la columna X.1 y sino los de MES17
ARTF_2017$MES17 <- ifelse(is.na(ARTF_2017$MES17), ARTF_2017$X.1, ARTF_2017$MES17 )
ARTF_2017 <- left_join(ARTF_2017 , MESESS17_1, by = c("MES17" = "MESES17_1"))
ARTF_2017 <-  left_join(ARTF_2017 , MESESS2 , by = c("MES17"="MESES2") )
#Estas lineas me sirven para ir quitando los NA y hacer que nos quede en formato fecha todos iguales
ARTF_2017$MES17_1 <- ifelse(is.na(ARTF_2017$MES17_1), ARTF_2017$MES17, ARTF_2017$MES17_1 )
ARTF_2017$MES2<- ifelse(is.na(ARTF_2017$MES2), ARTF_2017$MES17_1 , ARTF_2017$MES2 )
#Hago una nueva columna llamada EO, en la cual pego las columnas X.6 y X.7 con un separador "-"
ARTF_2017$EO <- paste(ARTF_2017$X.6, ARTF_2017$X.7, sep = "-")

ARTF_2017$X.4 <- ifelse(ARTF_2017$X.4 == "CEMENTO Y MORTERO PARA CONSTRUCCION",
                               "CEMENTO", ARTF_2017$X.4 )
ARTF_2017$X.4 <- ifelse(ARTF_2017$X.4 == "VEHICULOS AUTOMOTORES ARMADOS",
                               "VEHÍCULOS.AUTOMOTORES.ARMADOS", ARTF_2017$X.4)
ARTF_2017$X.4 <- ifelse(ARTF_2017$X.4 == "CONTENEDOR CARGADO (FAK)" | 
                          ARTF_2017$X.4 == "CONTENEDOR VACIO (CON O SIN CHASIS)",
                               "CONTENEDORES", ARTF_2017$X.4 )

ARTF_2017$X.4 <-  ifelse(ARTF_2017$X.4 == "FIERRO P/CONSTR",
                                "FIERRO.PARA.CONSTRUCCIÓN", ARTF_2017$X.4 )

 
Matriz_2017_limpia <- ARTF_2017[,c(36,2:32)]
names(Matriz_2017_limpia) <- Matriz_Total_ARTF_2017[4,c(-1,-34)]
Matriz_2017_limpia$Estacion_y_Entidad_Origen <- ARTF_2017$EO
Matriz_2017_limpia <- Matriz_2017_limpia[,c(1:7,33,8:32)]
names(Matriz_2017_limpia)
 
summary(as.factor(as.character(Matriz_2017_limpia$`Producto `)))

setwd("C:\\Users\\81799\\OneDrive\\Documentos\\ESFM_CLASES\\Servicio Social ARTF\\Machine Learning\\section1\\ARTF_2017_2020\\Limpieza de base de datos script")
write.csv(Matriz_2017_limpia , file = "Matriz_2017_limpia.csv")
