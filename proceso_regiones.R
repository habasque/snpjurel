#################################################################
# Programa de analisis de la geometria y distribucion vertical
# de los cardumenes de jurel
# - INPUT: archivos regiones + shorelinePeru + shelfbreak
# - OUTPUT: graficos/mapas sobre :
#            - profundidad media
#            - altura
#            - longitud
#            - volumen
#            - distancia la costa
#            - ratio longitud/altura
#################################################################

rm(list=ls())
library(sp)
library(ggplot2)
source('C:/Workspace_R/utilities/distance.R')
directory_data = 'G:/_sauvegarde_Jeremie/_data/'
#shoreline
filename_shoreline <- read.table(paste(directory_data,'/Shorelines/shoreline_Peru.csv',sep=""),header=F,sep=',');
#shelf break
filename_shelfbreak = paste(directory_data, 'Bathymetry/ShelfBreakPosition.csv', sep="")
ShelfBreakPosition <- read.csv(filename_shelfbreak, header=TRUE, sep=";", dec=".")

#lectura del archivo 
carpeta_datos_acusticos <- 'C:/Users/jhabasqu/Desktop/SNP/2013_june/datos/DATOS ACUSTICOS-2012-2013 TASA/'
nombre_archivo_regiones <- paste(carpeta_datos_acusticos, "SNP regiones 120 kHz.csv",sep="")
archivo_regiones <- read.csv2(nombre_archivo_regiones, sep=",", dec=".",header=T,na.strings=c("NA","NaN", "nan", "-", "#VALEUR!"))
summary(archivo_regiones)
archivo_regiones$Region_class <- as.character(archivo_regiones$Region_class)
archivo_regiones$Time_S <- strptime(archivo_regiones$Time_S, format = " %H:%M%OS")
archivo_regiones$Hora <- as.numeric(substr(as.POSIXct(archivo_regiones$Time_S),12,13))
archivo_regiones$Mes <- substr(archivo_regiones$Date_S,5,6)
archivo_regiones$Ano <- substr(archivo_regiones$Date_S,1,4)
archivo_regiones$dia_noche[archivo_regiones$Hora <= 5 | archivo_regiones$Hora >= 19] <- 1
archivo_regiones$dia_noche[archivo_regiones$Hora >= 7 & archivo_regiones$Hora <= 17] <- 0
  
#seleccion del jurel (JU, PG, JC, CA)
regiones_jurel <- subset(archivo_regiones, Region_class %in% c(" Ju"," PG"," JC"," Ca") & !is.na(dia_noche))
regiones_jurel_transition <- subset(archivo_regiones, Region_class %in% c(" Ju"," PG"," JC"," Ca") & is.na(dia_noche))
  
#numero de cardumenes por meses/anos
tapply(regiones_jurel$Region_ID, list(regiones_jurel$Ano, regiones_jurel$Mes), function(x) length(x))

#calculo de la longitud de los cardumenes (entre posicion S y posicion E)
regiones_jurel$longitud <- NA * dim(regiones_jurel)[1]
for (i in 1:nrow(regiones_jurel)) {
  regiones_jurel[i,]$longitud <-distance(regiones_jurel[i,]$Lon_S,regiones_jurel[i,]$Lat_S,regiones_jurel[i,]$Lon_E,regiones_jurel[i,]$Lat_E)
}
regiones_jurel$longitud <- regiones_jurel$longitud*1000 #en metros
regiones_jurel$ratio_longitud_altura <- regiones_jurel$longitud / regiones_jurel$Height_mean

#calculo de la distancia a la costa
regiones_jurel$distancia_costa <- NaN * dim(regiones_jurel)[1]
xy.coast <- cbind(filename_shoreline$V1, filename_shoreline$V2)
pts <- matrix(c(regiones_jurel$Lon_S,regiones_jurel$Lat_S), ncol = 2)
for (i in 1:nrow(pts)) {
  regiones_jurel[i,]$distancia_costa <- min(spDistsN1(xy.coast, pts[i,], longlat = TRUE))
}
#calculo de la distancia a la plataforma
regiones_jurel$distancia_plataforma <- NaN * dim(regiones_jurel)[1]
xy.plataforma <- cbind(ShelfBreakPosition$Longitude, ShelfBreakPosition$Latitude)
pts <- matrix(c(regiones_jurel$Lon_S,regiones_jurel$Lat_S), ncol = 2)
for (i in 1:nrow(pts)) {
  regiones_jurel[i,]$distancia_plataforma <- min(spDistsN1(xy.plataforma, pts[i,], longlat = TRUE))
}

#----- analisis de la geometria de los cardumenes

# global
savePlot(filename="../figures/profundidad_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))
hist(regiones_jurel$Depth_mean,100,main="Histogramma de frecuencia de la profundidad \n de los cardumenes de jurel 2012-2013",xlab="Profundidad media")

regiones_jurel$Depth_group[regiones_jurel$Depth_mean > 0 & regiones_jurel$Depth_mean<=10] <- "0-10"
regiones_jurel$Depth_group[regiones_jurel$Depth_mean > 10 & regiones_jurel$Depth_mean<=20] <- "10-20"
regiones_jurel$Depth_group[regiones_jurel$Depth_mean > 20 & regiones_jurel$Depth_mean<=30] <- "20-30"
regiones_jurel$Depth_group[regiones_jurel$Depth_mean > 30 & regiones_jurel$Depth_mean<=40] <- "30-40"
regiones_jurel$Depth_group[regiones_jurel$Depth_mean > 40] <- "40+"
regiones_jurel$Ano <- as.factor(regiones_jurel$Ano)
regiones_jurel$Mes <- as.factor(regiones_jurel$Mes)
regiones_jurel$dia_noche <- as.factor(regiones_jurel$dia_noche)

#distinccion dia y noche
regiones_jurel_dia <- subset(regiones_jurel, dia_noche ==0)
regiones_jurel_noche <- subset(regiones_jurel, dia_noche ==1)

#------ profundidad media de los cardumenes
tapply(regiones_jurel$Depth_mean, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
# profundidad media por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Depth_mean,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 0.5, xlab= 'Profundidad media (metros)', main ='Distribucion de la profundidad media de los cardumenes de jurel por ano y dia/noche')
savePlot(filename="../figures/profundidad_jurel_ano_dia_noche_histogram.png",type =c("png"))
# profundidad media por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Depth_mean,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 0.5, xlab= 'Profundidad media (metros)', main ='Distribucion de la profundidad media de los cardumenes de jurel por mes y dia/noche')
savePlot(filename="../figures/profundidad_jurel_mes_dia_noche_histogram.png",type =c("png"))
# profundidad media por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(Depth_mean,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 0.5, xlab= 'Profundidad media (metros)', main ='Distribucion de la profundidad media de los cardumenes de jurel por ano/mes')
savePlot(filename="../figures/profundidad_jurel_ano_mes_histogram.png",type =c("png"))
# profundidad media por mes/ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Mes,-Depth_mean,Depth_mean,data=regiones_jurel, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Profundidad del los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="Profundidad media")
savePlot(filename="../figures/profundidad_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#------ altura de los cardumenes
tapply(regiones_jurel$Height_mean, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
# altura por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Height_mean,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 0.5, xlab= 'Altura', main ='Distribucion de la altura de los cardumenes de jurel por ano y dia/noche')
savePlot(filename="../figures/altura_jurel_ano_dia_noche_histogram.png",type =c("png"))
# altura por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Height_mean,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 0.5, xlab= 'Altura', main ='Distribucion de la altura de los cardumenes de jurel por mes y dia/noche')
savePlot(filename="../figures/altura_jurel_mes_dia_noche_histogram.png",type =c("png"))
# altura por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(Height_mean,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 0.5, xlab= 'Altura', main ='Distribucion de la altura de los cardumenes de jurel por ano*mes')
savePlot(filename="../figures/altura_jurel_ano_mes_histogram.png",type =c("png"))
# altura de cardumen por mes/ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Mes,Height_mean,Depth_mean,data=regiones_jurel, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Altura del los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="Altura (metros)")
savePlot(filename="../figures/altura_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#------ longitud de los cardumenes
tapply(regiones_jurel$longitud, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
# longitud por ano
tapply(regiones_jurel$longitud, list(regiones_jurel$Ano), mean, na.rm=TRUE)
x11(width=15,height=8,pointsize=12)
boxplot(regiones_jurel$Corrected_length~regiones_jurel$Ano,data=regiones_jurel, xlab="Ano",ylab="Longitud (metros)", main="Distribucion de la longitud de los cardumenes de jurel por ano")
savePlot(filename="../figures/longitud_jurel_ano_boxplot.png",type =c("png"))
# longitud por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(longitud,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 0.5, xlab= 'longitud (metros)', main ='Distribucion de la longitud de los cardumenes de jurel por ano y dia/noche')
savePlot(filename="../figures/longitud_jurel_ano_dia_noche_histogram.png",type =c("png"))
# longitud por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(longitud,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 0.5, xlab= 'longitud (metros)', main ='Distribucion de la longitud de los cardumenes de jurel por mes y dia/noche')
savePlot(filename="../figures/longitud_jurel_mes_dia_noche_histogram.png",type =c("png"))
# longitud por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(longitud,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 0.5, xlab= 'longitud (metros)', main ='Distribucion de la longitud de los cardumenes de jurel por ano y mes')
savePlot(filename="../figures/longitud_jurel_ano_mes_histogram.png",type =c("png"))
# longitud de cardumen por mes/ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Mes,longitud,longitud,data=regiones_jurel, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Longitud de los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="longitud (metros)")
savePlot(filename="../figures/longitud_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#------ distancia a la costa de los cardumenes
tapply(regiones_jurel$distancia_costa, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
# distancia a la costa por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(distancia_costa,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 0.5, xlab= 'Distancia a la costa (kilometros)', main ='Distribucion de los cardumenes de jurel vs distancia a la costa por ano y dia/noche')
savePlot(filename="../figures/distancia_costa_jurel_ano_dia_noche_histogram.png",type =c("png"))
# distancia a la costa por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(distancia_costa,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 0.5, xlab= 'Distancia a la costa (kilometros)', main ='Distribucion de los cardumenes de jurel vs distancia a la costa por mes y dia/noche')
savePlot(filename="../figures/distancia_costa_jurel_mes_dia_noche_histogram.png",type =c("png"))
# distancia a la costa por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(distancia_costa,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 0.5, xlab= 'Distancia a la costa (kilometros)', main ='Distribucion de los cardumenes de jurel vs distancia a la costa por ano y mes')
savePlot(filename="../figures/distancia_costa_jurel_ano_mes_histogram.png",type =c("png"))
# distancia a la costa de los cardumenes por mes/ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Mes,distancia_costa,distancia_costa,data=regiones_jurel, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Distancia a la costa de los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="Distancia a la costa (metros)")
savePlot(filename="../figures/distancia_costa_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#------ volumen de los cardumenes
tapply(regiones_jurel$X3D_school_volume, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
regiones_jurel_volumen_ok <- subset(regiones_jurel, X3D_school_volume < 5000)
# volumen por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(X3D_school_volume,data=regiones_jurel_volumen_ok, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 10, xlab= 'Volumen', main ='Volumen de los cardumenes de jurel por ano y dia/noche')
savePlot(filename="../figures/X3D_school_volume_jurel_ano_dia_noche_histogram.png",type =c("png"))
# volumen por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(X3D_school_volume,data=regiones_jurel_volumen_ok, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 10, xlab= 'Volumen', main ='Volumen de los cardumenes de jurel por mes y dia/noche')
savePlot(filename="../figures/X3D_school_volume_jurel_mes_dia_noche_histogram.png",type =c("png"))
# volumen por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(X3D_school_volume,data=regiones_jurel_volumen_ok, geom=c("histogram"),facets= Ano~Mes,binwidth = 10, xlab= 'Volumen', main ='Volumen de los cardumenes de jurel por ano y mes')
savePlot(filename="../figures/X3D_school_volume_jurel_ano_mes_histogram.png",type =c("png"))
# volumen de los cardumenes por mes/ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(Mes,X3D_school_volume,X3D_school_volume,data=regiones_jurel_volumen_ok, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Volumen de los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="Volumen")
savePlot(filename="../figures/X3D_school_volume_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#------ ratio longitud/altura de los cardumenes (para comprobar la compresion de los cardumenes en funcion de la oxiclina)
tapply(regiones_jurel$ratio_longitud_altura, list(regiones_jurel$Ano, regiones_jurel$Mes,regiones_jurel$dia_noche), mean, na.rm=TRUE)
# ratio_longitud_altura por ano, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(ratio_longitud_altura,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Ano,binwidth = 0.5, xlab= 'Ratio longitud/altura', main ='Ratio longitud/altura de los cardumenes de jurel por ano y dia/noche')
savePlot(filename="../figures/ratio_longitud_altura_jurel_ano_dia_noche_histogram.png",type =c("png"))
# ratio_longitud_altura por mes, dia/noche
x11(width=15,height=8,pointsize=12)
qplot(ratio_longitud_altura,data=regiones_jurel, geom=c("histogram"),facets= dia_noche~Mes,binwidth = 0.5, xlab= 'Ratio longitud/altura', main ='Ratio longitud/altura de los cardumenes de jurel por mes y dia/noche')
savePlot(filename="../figures/ratio_longitud_altura_jurel_mes_dia_noche_histogram.png",type =c("png"))
# ratio_longitud_altura por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(ratio_longitud_altura,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 0.5, xlab= 'Ratio longitud/altura', main ='Ratio longitud/altura de los cardumenes de jurel por ano*mes')
savePlot(filename="../figures/ratio_longitud_altura_jurel_ano_mes_histogram.png",type =c("png"))
# ratio_longitud_altura de los cardumenes por mes/ano, dia/noche
#regiones_jurel_ratio_inf_100 <- subset(regiones_jurel, ratio_longitud_altura<50)
x11(width=15,height=8,pointsize=12)
qplot(Mes,ratio_longitud_altura,ratio_longitud_altura,data=regiones_jurel, geom=c("boxplot"), facets= dia_noche~Ano,
      main="Ratio longitud/altura de los cardumenes de jurel por ano/mes - dia (0) vs noche (1)",
      xlab="", ylab="Volumen")
savePlot(filename="../figures/ratio_longitud_altura_jurel_ano_mes_dia_noche_boxplot.png",type =c("png"))

#----- distribucion de la energia de los cardumenes 
x11(width=15,height=8,pointsize=12) 
hist(log(regiones_jurel$NASC),100, main = "Distribucion del NASC de los cardumenes de jurel", xlab = "log(NASC)")
savePlot(filename="../figures/distribucion_NASC_jurel.png",type =c("png"))

x11(width=15,height=8,pointsize=12) 
plot(log(regiones_jurel_dia$NASC+1),regiones_jurel_dia$Depth_mean, main = "Distribucion del NASC vs profundidad de los cardumenes de jurel", xlab = "log(NASC)",ylab=("Profundidad(metros)"))
points(log(regiones_jurel_noche$NASC+1),regiones_jurel_noche$Depth_mean, col="red")
legend("topright",legend=c("Dia","Noche"),lwd=2,col= c("red","blue"))
savePlot(filename="../figures/distribucion_NASC_profundidad_dia_noche_jurel.png",type =c("png"))

x11(width=15,height=8,pointsize=12) 
plot(regiones_jurel_dia$Sv_mean,regiones_jurel_dia$Depth_mean, main = "Distribucion del Sv_mean vs profundidad de los cardumenes de jurel", xlab = "log(NASC)",ylab=("Profundidad(metros)"))
points(regiones_jurel_noche$Sv_mean,regiones_jurel_noche$Depth_mean, col="red")
legend("topright",legend=c("Dia","Noche"),lwd=2,col= c("red","blue"))
savePlot(filename="../figures/distribucion_Sv_mean_profundidad_dia_noche_jurel.png",type =c("png"))

#----- Distribución de las detecciones en relación a la distancia a la plataforma
x11(width=15,height=8,pointsize=12) 
plot(regiones_jurel$Lon_S, regiones_jurel$Lat_S)
lines(ShelfBreakPosition)
lines(filename_shoreline)

x11(width=15,height=8,pointsize=12) 
hist(regiones_jurel$distancia_plataforma-regiones_jurel$distancia_costa,100)
# ratio_longitud_altura por ano*mes
x11(width=15,height=8,pointsize=12)
qplot(distancia_plataforma,data=regiones_jurel, geom=c("histogram"),facets= Ano~Mes,binwidth = 5, xlab= 'Distancia a la plataforma', main ='Distancia a la plataforma de los cardumenes de jurel por ano*mes')
savePlot(filename="../figures/distancia_plataforma_jurel_ano_mes_histogram.png",type =c("png"))


#---------- mapas 

x11(width=10,height=12,pointsize=12) 
plot(regiones_jurel$Lon_S, regiones_jurel$Lat_S,main="Posiciones de las detecciones de jurel - 2012-2013",xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude', col="blue")
lines(ShelfBreakPosition, col="red")
lines(filename_shoreline)
savePlot(filename="../figures/posiciones_jurel_mapa.png",type =c("png"))


colnames(filename_shoreline) <- c("Lon_S","Lat_S")

x11(width=15,height=8,pointsize=12) 
p <-qplot(Lon_S, Lat_S, data=regiones_jurel,facets=Ano~Mes,
          xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',
          main="Posiciones de las detecciones de jurel por ano y mes")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 0.5)
savePlot(filename="../figures/detecciones_jurel_ano_mes_mapa.png",type =c("png"))

#---- mapa de la profundidad media de dia
x11(width=15,height=8,pointsize=12) 
p <-qplot(Lon_S, Lat_S, data=regiones_jurel_dia, col=Depth_mean, facets=Ano~Mes,
          xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',
          main="Profundidad media de jurel por ano y mes - dia")
breaks <- c(10, 30, 50)
p + scale_colour_gradientn(colours = rainbow(3))
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
savePlot(filename="../figures/profundidad_jurel_ano_mes_dia_mapa.png",type =c("png"))

#---- mapa de noche
x11(width=15,height=8,pointsize=12) 
p <-qplot(Lon_S, Lat_S, data=regiones_jurel_noche,colour=factor(Depth_group), facets=Ano~Mes,
          xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',
          main="Profundidad media de jurel por ano y mes - noche")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
p + scale_colour_manual(values = c("red","blue", "green", "yellow", "orange"))
savePlot(filename="../figures/profundidad_jurel_ano_mes_noche_mapa.png",type =c("png"))

#------- relacion a las descargas


#------- relacion a la profundidad del oxigeno







