#Dirección en donde tenemos la base de datos a ocupar
setwd("C:\\Users\\81799\\Downloads\\Servicio Social - Git hub\\Toneladas - Netas\\Junio_Maiz")
library(readr) #Para poder leer nuestra base de datos
#Serie temporal     Toneladas_Netas-Productos
ARTF <- read.csv("ARTF.csv")#Cargamos base de datos  
ARTF$FECHA <- as.factor(ARTF$FECHA)
#Con la siguiente función sumo todas las toneladas netas de mi dataFrame ARTF
#tomando en cuenta su fecha y el tipo de producto, es decir, obtengo las toenladas netas transportadas de 
#cada producto, independientenmente de la concesionaria y del tipo de carga en cada fecha determinada.
#pongo na.rm = T especifiicando que puedo obtener valores faltantes.
Producto <- tapply(ARTF$TONELADAS_NETAS,
                   list(ARTF$FECHA , ARTF$PRODUCTOS),
                   function(x){sum(x, na.rm = T)})
#Crea un DataFrame en donde las columnas son el nombre de los productos Y en la última columna del DataFrame coloco la suma total de las toneladas netas de todos los productos.
Producto <- data.frame(rownames(Producto), Producto ,
                       TOTAL =rowSums(Producto, na.rm=T) )
library(quantmod)#Me sirve para poder manipular mi fecha tipo Mes/año
Producto$Fecha <- as.yearmon(Producto$rownames.Producto.)#Hago una nueva columna con los datos de la columna rownames:Prodycto. con la condición de que 
#sea una columna de clase yearmon (sirve para un formato de fecha mes año)
Producto <- Producto[,c(549,2:547)] #Me quedó con la columna creada (en primer lugar) y con todas las demás con excepción de la columna rownames.Producto.
Producto <- Producto[order(Producto$Fecha ),]#Hago una nueva columna llamada Fecha en la cual conserva los valores de la columna rownames.Producto.(Esta última columna ya en formato Fecha)
P_1 <- Producto[, c(2:547)]
#Hago una copia del Data}Frame Producto con la excepción de que quitó la columna que contiene las fechas (1) 
P_1 <- data.frame(Producto)[,-c(1,548)]
#Creó una fila en el DataFrame P_1 la cual la llamó TOTAL y en ella sumo todas las Toneladas Netas que se transportaron del año 2014 al 2022
P_1["TOTAL", ] <- colSums(P_1, na.rm = T) 
#Primero ordenamos en orden decreciente, y de esa lista agarramos los 10 productos con mayor Toneladas Netas transportadas y la guardamos en el DataFrame PRODUCTOS
PRODUCTOS <- data.frame(P_1[,order(P_1["TOTAL",], decreasing = T)[1:10]])
#Pongo la dirección en donde guardaré mi csv
setwd("C:\\Users\\81799\\Downloads\\Servicio Social - Git hub\\Toneladas - Netas\\Junio_Maiz")
#Escribo los productos y los guardo como "Productos.csv"
write.csv(PRODUCTOS, "Productos.csv")


