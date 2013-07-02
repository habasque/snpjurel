rm(list=ls())

#lectura del archivo de calas
carpeta_datos_calas <- 'C:/Users/jhabasqu/Desktop/SNP/2013_june/datos/'
nombre_archivo_calas <- paste(carpeta_datos_calas, "calas_con_po.csv",sep="")
calas <- read.csv2(nombre_archivo_calas, sep=";", dec=",",header=T)
calas$ANO <- substr(calas$FECHA.INI.RECEP, 7,10)
calas$MES <- substr(calas$FECHA.INI.RECEP, 4,5)

#numero de calas por ano/mes
tapply(calas$ID, list(calas$ANO, calas$MES), function(x) length(unique(x)))
  
#shoreline
directory_data = 'G:/_sauvegarde_Jeremie/_data/'
filename_shoreline <- read.table(paste(directory_data,'/Shorelines/shoreline_Peru.csv',sep=""),header=F,sep=',');
colnames(filename_shoreline) <- c("LONGI","LATIT")

#mapa de las posiciones por ano/mes
x11(width=15,height=8,pointsize=12)
p <-qplot(LONGI, LATIT, data=calas, facets=ANO~MES,xlim=c(-82,-70),ylim =c(-20,-5),xlab = 'Longitude', ylab = 'Latitude',main="Posiciones de las calas 2012-2013", col="red")
p + theme_bw()
p + geom_line(data = filename_shoreline, colour="black", size = 1)
savePlot(filename="../figures/calas_posiciones_ano_mes.png",type =c("png"))

#---- distribucion de las capturas con parametros ambientales
#tsm
tapply(calas$TSM, list(calas$ANO), min)
tapply(calas$TSM, list(calas$ANO), max)
x11(width=15,height=8,pointsize=12)
p <-qplot(TSM, data=calas, geom="histogram",facets=ANO~MES,binwidth=0.5,main="Calas 2012-2013 - TSM por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_tsm_ano_mes.png",type =c("png"))

#salinidad
x11(width=15,height=8,pointsize=12)
p <-qplot(Salinidad, data=calas, geom="histogram",facets=ANO~MES,binwidth=0.01,main="Calas 2012-2013 - Salinidad por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_salinidad_ano_mes.png",type =c("png"))

#CHLA
x11(width=15,height=8,pointsize=12)
p <-qplot(log(Clorofila+1), data=calas, geom="histogram",facets=ANO~MES,binwidth=0.1,main="Calas 2012-2013 - Clorofila por ano/mes")
p + theme_bw()
savePlot(filename="../figures/calas_clorofila_ano_mes.png",type =c("png"))

#interaccion TSM / CHLA
x11(width=15,height=8,pointsize=12)
plot(calas$TSM, log(calas$Clorofila+1), xlim=c(17,25), ylim=c(0,5),xlab="TSM (°C)", ylab="CHLA (mg/m3)", main="Distribucion de las calas en relacion con TSM y CHLA")
savePlot(filename="../figures/calas_clorofila_tsm.png",type =c("png"))


