#################################################################
# Programa de analisis de la calas con parametros ambientales
# - INPUT: archivos regiones + shorelinePeru + shelfbreak
# - OUTPUT: graficos/mapas sobre :
#           - posiciones de las calas
# 
#################################################################

rm(list=ls())

library(ggplot2)
library(sp)

#---- lectura de archivos

#lectura del archivo de calas
carpeta_datos_calas <- 'C:/Users/jhabasqu/Desktop/SNP/2013_june/datos/'
nombre_archivo_calas <- paste(carpeta_datos_calas, "calas_con_po.csv",sep="")
calas <- read.csv2(nombre_archivo_calas, sep=";", dec=",",header=T)

#shoreline
directory_data = 'G:/_sauvegarde_Jeremie/_data/'
filename_shoreline <- read.table(paste(directory_data,'/Shorelines/shoreline_Peru.csv',sep=""),header=F,sep=',');
colnames(filename_shoreline) <- c("LONGI","LATIT")

#shelf break
filename_shelfbreak = paste(directory_data, 'Bathymetry/ShelfBreakPosition.csv', sep="")
ShelfBreakPosition <- read.csv(filename_shelfbreak, header=TRUE, sep=";", dec=".")


#---- checkear las posiciones de las calas_con_po
x11(width=10,height=10,pointsize=12)
plot(calas$LONGI,calas$LATIT, xlab="Longitude",ylab="Latitude",main="Posiciones de las calas 2012-2013 \n con parametros oceanograficos")
lines(filename_shoreline)
lines(ShelfBreakPosition,col="red")
legend("topright","shelf break",lwd=2,col="red")
savePlot(filename="../figures/calas_con_po_posiciones_total.png",type =c("png"))

calas$ANO <- substr(calas$FECHA.INI.RECEP, 7,10)
calas$MES <- substr(calas$FECHA.INI.RECEP, 4,5)

#numero de calas por ano/mes
tapply(calas$ID, list(calas$ANO, calas$MES), function(x) length(unique(x)))
  

#mapa de las posiciones por ano/mes
x11(width=15,height=8,pointsize=12)
p <-qplot(LONGI, LATIT, data=calas, facets=ANO~MES,xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',main="Posiciones de las calas 2012-2013", col="red")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
savePlot(filename="../figures/calas_con_po_posiciones_ano_mes.png",type =c("png"))

#---- distribucion de las capturas con parametros ambientales
#tsm
tapply(calas$TSM, list(calas$ANO), min)
tapply(calas$TSM, list(calas$ANO), max)
x11(width=15,height=8,pointsize=12)
p <-qplot(TSM, data=calas, geom="histogram",facets=ANO~MES,binwidth=0.5,main="Calas 2012-2013 - TSM por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_con_po_tsm_ano_mes.png",type =c("png"))

#salinidad
x11(width=15,height=8,pointsize=12)
p <-qplot(Salinidad, data=calas, geom="histogram",facets=ANO~MES,binwidth=0.01,main="Calas 2012-2013 - Salinidad por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_con_po_salinidad_ano_mes.png",type =c("png"))

#CHLA
min(calas$Clorofila[!is.na(calas$Clorofila)]>0)
x11(width=15,height=8,pointsize=12)
p <-qplot(log(Clorofila+1), data=calas, geom="histogram",facets=ANO~MES,binwidth=0.1,main="Calas 2012-2013 - Clorofila por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_con_po_clorofila_ano_mes.png",type =c("png"))

#interaccion TSM / CHLA
x11(width=15,height=8,pointsize=12)
plot(calas$TSM, log(calas$Clorofila+1), xlim=c(17,25), ylim=c(0,5),xlab="TSM (°C)", ylab="CHLA (mg/m3)", main="Distribucion de las calas 2012-2013 en relacion con TSM y CHLA")
savePlot(filename="../figures/calas_con_po_clorofila_tsm.png",type =c("png"))

#calculo de la distancia a la costa
calas$distancia_costa <- NaN * dim(calas)[1]
xy.coast <- cbind(filename_shoreline$LONGI, filename_shoreline$LATIT)
pts <- matrix(c(calas$LON,calas$LAT), ncol = 2)
for (i in 1:nrow(pts)) {
  calas[i,]$distancia_costa <- min(spDistsN1(xy.coast, pts[i,], longlat = TRUE))
}
#calculo de la distancia a la plataforma
calas$distancia_plataforma <- NaN * dim(calas)[1]
xy.plataforma <- cbind(ShelfBreakPosition$Longitude, ShelfBreakPosition$Latitude)
pts <- matrix(c(calas$LON,calas$LAT), ncol = 2)
for (i in 1:nrow(pts)) {
  calas[i,]$distancia_plataforma <- min(spDistsN1(xy.plataforma, pts[i,], longlat = TRUE))
}

# calas en la plataforma ? 
calas$diff_distancia_costa_plataforma = calas$distancia_costa-calas$distancia_plataforma
calas$presencia_plataforma[calas$diff_distancia_costa_plataforma>0] <- 1 #outside
calas$presencia_plataforma[calas$diff_distancia_costa_plataforma<0] <- 0 # en la plataforma

x11(width=15,height=8,pointsize=12)
hist(calas$diff_distancia_costa_plataforma, n=100,xlab="Distancia costa-distancia plataforma (kms)", main="Calas 2012-2013 - Distancia costa - distancia plataforma (kms)")
savePlot(filename="../figures/calas_con_po_distancia_costa_plataforma.png",type =c("png"))

tapply(calas$presencia_plataforma, list(calas$presencia_plataforma), function(x) length(x))
