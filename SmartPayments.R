
#Ciclo que instala librerias requeridas para el funcionamiento correcto del programa. Revisa si ya estan instaladas
#si no lo estan, las instala, si ya estan solamente las carga.
for (libreria in c("stringr","dplyr","tidyr")) {
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
Datos$CUENTA <- as.numeric(gsub("Cliente"," ",Datos$CUENTA))

#Como todos los datos son del 2018, se elimina este de la fecha, asi se queda solamente
#dia y mes en la base de datos.
Datos$TRA_FECHA <- gsub("/2018"," ",Datos$TRA_FECHA)

#se separa la fecha en dos columnas distintas, una por dia y otra por mes.
DatosLimpios <- Datos %>% separate(TRA_FECHA, c("Dia", "Mes"), sep = "/")

#Estos datos al separarse se guardan como texto, entonces se convierten a numeros.
DatosLimpios$Dia <- as.numeric(DatosLimpios$Dia)
DatosLimpios$Mes <- as.numeric(DatosLimpios$Mes)

#----------------- Segunda parte: Calculo por cliente ----------------------------

#
DatosIndividuales <- DatosLimpios[1]
