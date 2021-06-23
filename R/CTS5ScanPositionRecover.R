require("sp")
require("rgdal")


#*************
# Conversion de deg min(decimal) vers degr (decimal)
ConvDeg<-function(str){
  L<-substr(str,nchar(str),nchar(str))
  num<-as.numeric(substr(str,1,nchar(str)-1))/100 #/100 pour CTS5
  deg<-trunc(num)
  min<-100*(num-deg)/60
  pos<-deg+min
  pos[which((L=="S") | (L=="W"))]<--pos[which((L=="S") | (L=="W"))]
  return(pos)
}

#************* conversion en minutes decimales
ConvMin<-function(pos){
  deg<-trunc(pos)
  min<-60*(pos-deg)
  
  #Position convertie Txt
  paste(formatC(deg,width=3,flag="0"),"d",formatC(min,digits=5,format = "f"),"'",sep="") #
  
}

#************* conversion deg decimaux en minutes secondes
ConvMinSec<-function(pos){
  deg<-trunc(pos)
  min<-trunc(60*(pos-deg))
  sec<-(60*(pos-deg)-min)*60
  
  #Position convertie
  paste(formatC(deg,width=3,flag="0"),"d",formatC(min,width=2,flag="0"),"'",formatC(sec,width=4,flag="0"),"''",sep="")
  
}

#*************
# dist.2pts.nm.lonlat.in.deg
# dist en nm

dist.2pts <- function(lon1, lat1, lon2, lat2, r.earth = 3441.037) { 
#d2r <- acos(-1) / 180
#r.earth * acos(sin(lat1 * d2r) * sin(lat2 * d2r) + cos(lat1 * d2r) * cos(lat2 * d2r) * cos(lon1 * d2r - lon2 * d2r))  

zrad = 2*asin(1)/180
zrae = 6371229/1852  

zrae * zrad * sqrt(((lon2-lon1) * cos(zrad*(lat2+lat1)/2))^2 + (lat2-lat1)^2 )
  
#selon 

}

#*************
cap.2pts<-function(lon1, lat1, lon2, lat2){

result<-NA

if ((lon2>=lon1) & (lat2>=lat1)){
  result<-atan((lon2-lon1)*cos(pi*(lat1+lat2)/360)/(lat2-lat1))*180/pi}

if ((lon2>=lon1) & (lat2<lat1)){
  result<-180+atan((lon2-lon1)*cos(pi*(lat1+lat2)/360)/(lat2-lat1))*180/pi}

if ((lon2<lon1) & (lat2<lat1)){
  result<-180+atan((lon2-lon1)*cos(pi*(lat1+lat2)/360)/(lat2-lat1))*180/pi}

if ((lon2<lon1) & (lat2>=lat1)){
  result<-360+atan((lon2-lon1)*cos(pi*(lat1+lat2)/360)/(lat2-lat1))*180/pi}

return(result)
  
}

#***************************************************************************
#' read Position file to recover the float.
#'
#' @description
#' This function read the position of the float during a recovering. Positions are entered by hand in a text file or 
#' read from default files by \code{\link{Recover_ScanDefault}}. 
#'
#' @param filename name of the text file which contains the positions.
#' @param KMLfile name of the KML file generated
#' 
#' @return a data frame which contains the positions in various format and the speed and course of the float.
#' 
#' @examples The positions must be entered in a text file by copying the [GPS] line of the technical/default file as follow :
#' UTC=19-11-25 08:02:00 Lat=4315.57717N Long=00658.52514E Clock drift=+0.032 s
#' UTC=19-11-25 08:16:35 Lat=4315.27231N Long=00657.97657E Clock drift=+0.000 s
#' 
#' 
#' 
#' @export
#'
#'
Recover_ScanPosition<-function(filename="Positions.txt",KMLfile="Positions.kml"){
  datatemp<-read.table(filename,header=FALSE,sep=" ",stringsAsFactors=FALSE)
  
  #lecture date
  temp<-unlist(strsplit(datatemp[,1],split="="))
  data<-temp[temp!="UTC"]
  
  #lecture heure
  data<-paste(data,datatemp[,2])
  
  #lecture Lat  
  temp<-unlist(strsplit(datatemp[,3],split="="))
  temp<-temp[temp!="Lat"]
  data<-cbind(data,ConvDeg(temp))

  
  #lecture Lon
  temp<-unlist(strsplit(datatemp[,4],split="="))
  temp<-temp[temp!="Long"]
  data<-cbind(data,ConvDeg(temp))

  
  data<-as.data.frame(data,stringsAsFactors=FALSE)
  data[,2]<-as.numeric(data[,2])
  data[,3]<-as.numeric(data[,3])
  colnames(data)<-c("time","Lat.deg.","Lon.deg.")
  
  data<-cbind(data,ConvMin(data[,2]),ConvMin(data[,3]))
  colnames(data)[4:5]<-c("Lat.Mindec","Lon.Mindec")
  
  data<-cbind(data,ConvMinSec(data[,2]),ConvMinSec(data[,3]))
  colnames(data)[6:7]<-c("Lat.Min.Sec","Lon.Min.Sec")
  
  date<-strptime(data[,1],format="%y-%m-%d %H:%M:%S")
  
  #distance / temps
  dist_nm<-NA
  tempsvect<-NA
  course<-NA
  
  if (length(data[,1])>1){
    for (i in 2:length(data[,1])){
      dist_nm<-c(dist_nm,dist.2pts(data[i-1,3],data[i-1,2],data[i,3],data[i,2]))
      tempsvect<-c(tempsvect,difftime(date[i],date[i-1],units="hours"))
      course<-c(course,cap.2pts(data[i-1,3],data[i-1,2],data[i,3],data[i,2]))
    }
    speed_kts<-dist_nm/tempsvect #en noeuds
    
    data<-cbind(data,dist_nm,speed_kts,course)
  }

  #Creation du fichier KML
  if (file.exists(KMLfile)){file.remove(KMLfile)}
  
  datapoint<-data
  colnames(datapoint)[1]<-"name"
  coordinates(datapoint)<-c("Lon.deg.","Lat.deg.")
  proj4string(datapoint) <- CRS("+proj=longlat +datum=WGS84")
  
  cat("write:",KMLfile,"\n")
  writeOGR(datapoint,KMLfile, layer="CTS5",driver="KML")
  
  return(data)
  
}

#***************************************************************************
#' read technical/default files to recover the float.
#'
#' @description
#' This function read technical/default files and write the Potions file to be called by \code{\link{ScanPosition2Recover}}
#'
#' @param pattern pattern of the technical/default files
#' @param Outputfilename filename of the Positions file
#' @param KMLfile name of the KML file generated
#' 
#' @return identical as \code{\link{ScanPosition2Recover}}
#' 
#' @examples ScanDefault2Recover()
#' 
#' 
#' 
#' @export
#'
#'
Recover_ScanDefault<-function(pattern=".*_default_.*.txt",Outputfilename="Positions.txt",KMLfile="Positions.kml"){
 
filenames<-list.files(pattern = pattern)   

Positions<-NULL

for (filename in filenames){
  cat('Open:',filename,"\n")
  t<-scan(filename,what=character(0),sep="\n")
  ind<-grep("Lat=",t)
  if (length(ind)>0){
    Positions<-c(Positions,t[ind])
  }
  
}

cat("write:",Outputfilename,"\n")
write(Positions,file = Outputfilename)

Recover_ScanPosition(filename=Outputfilename,KMLfile=KMLfile)

}


#***************************************************************************
#' Predict position based on previous Positions
#'
#' @description
#' extrapolates the futur position of the float in nextMin minutes based on previous positions read 
#' by \code{\link{Recover_ScanPosition}} or \code{\link{Recover_ScanDefault}}
#'
#' @param nextMin Number of minutes from now to project the position
#' @param data Positions read from \code{\link{Recover_ScanPosition}} or \code{\link{Recover_ScanDefault}}
#' @param ind The drift of the float is estimated between the last position and the position [Last-ind]
#' 
#' @return projected position
#' 
#' @examples data<-Recover_ScanDefault()
#' Recover_PredictPosition(nextMin=1,data=data) estimate the position in 1 min from now
#' 
#' 
#' 
#' @export
#'
#'

Recover_PredictPosition<-function(nextMin=0,data=data,ind=1){

date<-strptime(data[,1],format="%y-%m-%d %H:%M:%S",tz="UTC")

last<-length(data[,1])

now<-Sys.time()
t1<-as.numeric(difftime(date[last],date[last-ind],units="mins"))
t2<-as.numeric(difftime(now,date[last-ind],units="mins"))

t2<-t2+nextMin

Lon.Pred<-data[last-ind,"Lon.deg."]+(data[last,"Lon.deg."]-data[last-ind,"Lon.deg."])*t2/t1
Lat.Pred<-data[last-ind,"Lat.deg."]+(data[last,"Lat.deg."]-data[last-ind,"Lat.deg."])*t2/t1

dist<-dist.2pts(data[last,"Lon.deg."],data[last,"Lat.deg."],Lon.Pred,Lat.Pred)

result<-list(Lon.deg=Lon.Pred,Lat.deg=Lat.Pred,
             Lon.degmin=ConvMin(Lon.Pred),Lat.degmin=ConvMin(Lat.Pred),
             dist.fromlast.nm=dist,time.fromlast.min=difftime(date[last-ind]+t2*60,date[last],units="mins"),
             date.pred=date[last-ind]+t2*60)

return(result)

}


#***************************************************************************

## Utilisation

## ajouter les lignes UTC=18-04-17 12:20:15 Lat=4304.90557N Long=00728.80634E dans un fichier Positions.txt

## Choisir le bon repertoire

#setwd("D:/Data/Provor_APMT/lovapm006_ProVal/lovapm006f")
#setwd("D:/Data/Provor_APMT/lovapm005a")

## Utilisation simple
#ScanPosition(filename="Positions.txt")

# ou dans le repertoire avec les fichier default
#ScanDefault2Recover()


## Prediction de position
# A utiliser en cas de positions tres vieille ou derive tres rapide. Verifier l'horloge de l'ordi et verifier la coherence de la prediction
# data<-ScanPosition(filename="Positions.txt")
# Predict(nextMin=1,data=data)

# data<-ScanDefault2Recover()
# Predict(nextMin=1,data=data)


