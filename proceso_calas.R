#################################################################
# Programa de analisis de la calas 
# - INPUT: archivos regiones + shorelinePeru + shelfbreak
# - OUTPUT: graficos/mapas sobre :
#           - posiciones de las calas
# 
#################################################################

rm(list=ls())

library(ggplot2)

#---- lectura de archivos

#lectura del archivo de calas
carpeta_datos_calas <- 'C:/Users/jhabasqu/Desktop/SNP/2013_june/datos/'
nombre_archivo_calas <- paste(carpeta_datos_calas, "Calas 2011 a 2013.csv",sep="")
calas <- read.csv2(nombre_archivo_calas, sep=";", dec=".",header=T)

#seleccion de calas con posiciones correctas
calas_incorrectas = subset(calas, LON <= -83 | LON >= -70 | LAT <= -20 | LAT >= 0)
calas = subset(calas, LON > -83 & LON < -70 & LAT > -20 & LAT < 0)
summary(calas)

#shoreline
directory_data = 'G:/_sauvegarde_Jeremie/_data/'
filename_shoreline <- read.table(paste(directory_data,'Shorelines/shoreline_Peru.csv',sep=""),header=F,sep=',');
colnames(filename_shoreline) <- c("LON","LAT")

#---- checkear las posiciones de las calas
x11(width=8,height=10,pointsize=12)
plot(calas$LON,calas$LAT,xlab="Latitude",ylab="Longitude",main="Distribucion de las calas 2011-2013")
lines(filename_shoreline)
savePlot(filename="../figures/calas_posiciones_total.png",type =c("png"))

#numero de calas por ano/mes
tapply(calas$ID, list(calas$ANHO, calas$MES), function(x) length(unique(x)))

#mapa de las posiciones por ano/mes
x11(width=15,height=8,pointsize=12)
p <- qplot(LON, LAT, data=calas, facets=ANHO~MES,xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',main="Posiciones de las calas 2011-2013", col="red")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
savePlot(filename="../figures/calas_posiciones_ano_mes.png",type =c("png"))

#---- distribucion de las capturas con parametros ambientales
#tsm
tapply(calas$TEMP, list(calas$ANHO), min, na.rm=TRUE)
tapply(calas$TEMP, list(calas$ANHO), max, na.rm=TRUE)
x11(width=15,height=8,pointsize=12)
p <- qplot(TEMP, data=calas, geom="histogram",facets=ANHO~MES,binwidth=0.5,main="Calas 2011-2013 - TSM por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_tsm_ano_mes.png",type =c("png"))

