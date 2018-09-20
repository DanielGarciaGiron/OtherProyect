
#Ciclo que instala librerias requeridas para el funcionamiento correcto del programa. Revisa si ya estan instaladas
#si no lo estan, las instala, si ya estan solamente las carga.
for (libreria in c("stringr","dplyr","tidyr", "plotrix")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#----------------- Primera parte: Limpieza de datos ----------------------------

#Se lee la base de datos, esta se convierte a formato .CSV, esto se hace desde excel, se presiona 
#guardar como y se selecciona .csv
Datos <- read.csv("./Database.csv", stringsAsFactors = F)

#Se elimina la palabra cliente de la primera columna, asi se tiene solamente un numero para identificar
#al usuario. esto facilita los calculos y demas procedimientos
Datos$Cuenta <- as.numeric(gsub("Cliente"," ",Datos$Cuenta))

#Como todos los datos son del 2018, se elimina este de la fecha, asi se queda solamente
#dia y mes en la base de datos.
Datos$Fecha <- gsub("/2018"," ",Datos$Fecha)

#se separa la fecha en dos columnas distintas, una por dia y otra por mes.
DatosLimpios <- Datos %>% separate(Fecha, c("Dia", "Mes"), sep = "/")

#Estos datos al separarse se guardan como texto, entonces se convierten a numeros.
DatosLimpios$Dia <- as.numeric(DatosLimpios$Dia)
DatosLimpios$Mes <- as.numeric(DatosLimpios$Mes)

#----------------- Segunda parte: Seleccion del cliente a investigar ----------------------------
#OJO: ESTE NUMERO SE DEBE DE CAMBIAR SEGUN EL CLIENTE QUE SE DESEE Y GENERA NUEVA INFORMACION AL CORRER EL PROGRAMA NUEVAMENTE
#se selecciona el cliente al que se le desea obtener la informacion
IDcliente <- 1



#----------------- tercera parte: Exploracion de datos ----------------------------
#se crea una nueva base de datos que contiene solamente la informacion del cliente a investigar
DatosIndividuales <- subset(DatosLimpios, DatosLimpios$Cuenta == IDcliente)

#Se obtiene el tipo de gasto mas comun del usuario
GastoMasComun <- tail(names(sort(table(DatosIndividuales$Segmento))), 1)
#se usa esto para que se imprima en consola
print(GastoMasComun)
#Se obtiene el lugar mas frecuente de gastos
LugarGastoMasComun <- tail(names(sort(table(DatosIndividuales$Lugar))), 1)
#se usa esto para que se imprima en consola
print(LugarGastoMasComun)

#se encuentra la mediana
mediana <- median(DatosIndividuales$Monto)
print(mediana)

#se encuentra la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda<-getmode(DatosIndividuales$Monto)
print(moda)

#se encuentra la media
media <- mean(DatosIndividuales$Monto)
print(media)

#Grafico de pie para lugares donde se gasta en total, todos los meses incluidos.
pie(table(DatosIndividuales$Segmento), col = topo.colors(15), main = "Lugares de consumo")

#grafico por mes de los lugares donde se gasta.
for(i in min(DatosIndividuales$Mes):max(DatosIndividuales$Mes))
{
  TempDF <- subset(DatosIndividuales, DatosIndividuales$Mes == i)
  pie(table(TempDF$Segmento),col = topo.colors(15), main = paste ("Gastos del mes:", i))
}




