# Ciclo que carga y/o instala librerias necesarias para los calculos
for (libreria in c("class","caret","stringr","dplyr","tidyr","ggplot2","fpc","scales", "zoo")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}


## Limpieza de datos
Datos <- read.csv("./Database.csv", stringsAsFactors = F)
Datos$Cuenta <- as.numeric(gsub("Cliente"," ",Datos$Cuenta))
DatosLimpios <- Datos %>% separate(Fecha, c("Dia", "Mes","Año"), sep = "/")
DatosLimpios$Dia <- as.numeric(DatosLimpios$Dia)
DatosLimpios$Mes <- as.numeric(DatosLimpios$Mes)
DatosLimpios$Año <- as.numeric(DatosLimpios$Año)
DatosLimpios$TipoCompra <- as.numeric(factor(DatosLimpios$Segmento))
DatosLimpios <- subset(DatosLimpios, Segmento != "Afiliado Visanet")



## Analisis exploratorio de los datos (graficas para el banco)
summary(DatosLimpios)
GastoMasComun <- tail(names(sort(table(DatosLimpios$Segmento))), 1)
GastoMasComun
LugarGastoMasComun <- tail(names(sort(table(DatosLimpios$Lugar))), 1)
LugarGastoMasComun

wss <- (nrow(DatosLimpios[,c(1,4,5,6,8)])-1)*sum(apply(DatosLimpios[,c(1,4,5,6,8)],2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(DatosLimpios[,c(1,4:8)], centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Numero de clusters a utilizar",  ylab="Grupos segun la suma de sus cuadrados")
km<-kmeans(DatosLimpios[,c(1,4,5,6,8)],3)
DatosLimpios$Grupo<-km$cluster
plotcluster(DatosLimpios[,c(1,4,5,6,8)],km$cluster) 


ggplot(DatosLimpios, aes(TipoCompra, Dia, fill = Monto)) + 
  geom_tile(colour = "white") + 
  facet_grid(Año~Mes) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="ID del cliente",
       y="Dia del mes",
       title = "Mapa de calor, transacciones por cliente.", 
       subtitle="Transacciones realizadas.", 
       fill="Close")

g <- ggplot(DatosLimpios, aes(Dia, Monto))
g + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Distribucion de pagos por dia", 
       subtitle="Monto vs Dia: Cada punto representa una transaccion realizada ese dia.",
       caption="Source",
       x="Dia del mes",
       y="Monto realizado por transaccion")

theme_set(theme_bw()) 
g <- ggplot(DatosLimpios, aes(Cuenta, Monto))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="Cliente vs Monto realizado en transaccion", 
       y="Monto por transaccion", 
       x="ID del cliente", 
       title="Monto por transaccion realizada de todos los clientes.")




## removiendo los no recurrentes 
NoRecurrentes <- read.csv("./NoRecurrentes.csv", stringsAsFactors = F)
nombres<-rownames(NoRecurrentes)
DatosLimpios2 <- subset(DatosLimpios, Segmento != nombres)





## 
IDcliente <- 6



## Prediccion de dias, usando metodo creado
DatosIndividuales <- subset(DatosLimpios2[,c(1:7)], DatosLimpios2$Cuenta == IDcliente)
ListaValores <- names(sort(table(DatosIndividuales$Segmento), decreasing=TRUE)[1:4])
Valor1 <- DatosIndividuales$Segmento == ListaValores[1] 
Valor2 <- DatosIndividuales$Segmento == ListaValores[2] 
Valor3 <- DatosIndividuales$Segmento == ListaValores[3] 
Valor4 <- DatosIndividuales$Segmento == ListaValores[4] 
ValoresMasComunes <- Valor1 | Valor2 | Valor3 | Valor4
TablaPrediccion <- DatosIndividuales[ValoresMasComunes,]
DiaPago = c()
DiaPagoMax = c()
DiaPagoMin = c()
for(i in ListaValores[1:4])
{
  ListaTemp <- TablaPrediccion$Segmento == i
  DiaPago[i] <- round(mean(TablaPrediccion[ListaTemp,]$Dia))
  Desviacion <- sd(TablaPrediccion[ListaTemp,]$Dia)
  DiaPagoMax[i] <- round(DiaPago[i] + Desviacion)
  DiaPagoMin[i] <- round(DiaPago[i] - Desviacion)
}
DiaPago[is.na(DiaPago)]<-15
DiaPagoMax[is.na(DiaPagoMax)]<-17
DiaPagoMin[is.na(DiaPagoMin)]<-13
InfoDiaPago <- data.frame(as.numeric(DiaPago),as.numeric(DiaPagoMax),as.numeric(DiaPagoMin))
colnames(InfoDiaPago) <- c("Dia promedio", "Dia maximo", "Dia minimo")
InfoDiaPago[InfoDiaPago<=1]<-1
InfoDiaPago[InfoDiaPago>=31]<-31

## aprendizaje de maquinas utilizando Knn.
set.seed(123)
porciento <- 65/100 
muestra<-sample(1:nrow(DatosLimpios2),porciento*nrow(DatosLimpios2))
TrainSet<-DatosLimpios2[muestra,] 
TestSet<-DatosLimpios2[-muestra,] 

predKnn<-knn(TrainSet[,c(1,5,8)],TestSet[,c(1,5,8)],as.factor(TrainSet$Dia),k=3)
cfm<-confusionMatrix(as.factor(TestSet$Dia),predKnn)
cfm

## aprendizaje de maquinas utilizando un modelo lineal multilineal.
modeloLinealMulti<-lm(Dia~Cuenta+Mes+TipoCompra , data = TrainSet)
summary(modeloLinealMulti)
prediccion<-predict(modeloLinealMulti,newdata = TestSet[,c(1,5,8)])
prediccion
dif<-abs(prediccion-TestSet$Dia)
write.csv(dif, file = "ResultadosMulti.csv")




## Datos para el usuario.
pie(table(DatosIndividuales$Segmento), col = topo.colors(15), main = "Lugares de consumo")

for(i in min(DatosIndividuales$Mes):max(DatosIndividuales$Mes))
{
  TempDF <- subset(DatosIndividuales, DatosIndividuales$Mes == i)
  pie(table(TempDF$Segmento),col = topo.colors(15), main = paste ("Gastos del mes:", i))
}

Presupuesto <- 5000
for(i in min(DatosIndividuales$Mes):max(DatosIndividuales$Mes))
{
  TempDF <- subset(DatosIndividuales, DatosIndividuales$Mes == i)
  Suma <- sum(TempDF$Mes)
  if((Presupuesto - Suma)<0)
  {
    print(paste("Presupuesto no es suficiente para el mes falta:", Presupuesto - Suma, "Quetzales"))
  }
  else
  {
    print(paste("El presupuesto si alcanza para el mes tiene un ahorro de:", Presupuesto - Suma))
  }
}





