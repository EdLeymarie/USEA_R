

#define aAPMT_TELNET_LOGGER_SBE41      0
#define aAPMT_TELNET_LOGGER_DO         1
#define aAPMT_TELNET_LOGGER_OCR4       2
#define aAPMT_TELNET_LOGGER_OCR7       3
#define aAPMT_TELNET_LOGGER_OCR14      4
#define aAPMT_TELNET_LOGGER_ECO1       5
#define aAPMT_TELNET_LOGGER_ECO2       6
#define aAPMT_TELNET_LOGGER_ECO3       7
#define aAPMT_TELNET_LOGGER_ECO1v2     8
#define aAPMT_TELNET_LOGGER_ECO2v2     9
#define aAPMT_TELNET_LOGGER_ECO3v2    10   
#define aAPMT_TELNET_LOGGER_ECO4v2    11
#define aAPMT_TELNET_LOGGER_SBEPH     12
#define aAPMT_TELNET_LOGGER_CROVER    13
#define aAPMT_TELNET_LOGGER_SUNA45    14
#define aAPMT_TELNET_LOGGER_SUNA90    15
#define aAPMT_TELNET_LOGGER_RAMSES    16    
#define aAPMT_TELNET_LOGGER_OPUS      17
#define aAPMT_TELNET_LOGGER_MPE       18

filename<-"3aa9_datalogger.csv"

#**************************************************

# read datalogger file

#**************************************************

#' cts5_readDatalogger : Concat .csv files to one data.frame
#'
#' @description
#' read datalogger .csv ASCII files 
#'
#' @param filename 
#' 
#' @return a list containing the data with the same format than \code{\link{cts5_readProfile}}
#' 
#' 
#' @examples 
#' 
#' logger<-cts5_readDatalogger("3aa9_datalogger.csv")
#' 
#' Meta<-cts5_readMetaSensor()
#' 
#' logger<-cts5_ProcessData(Meta$SENSORS,logger)
#' 
#' @export
#'

cts5_readDatalogger<-function(filename){

datalogger<-scan(file = filename,skip = 1,what = character())

VectId<-as.character(c(0:2,7,12,13,16,18,20))
VectName<-c("sbe41","do","ocr","eco","sbeph","crover","ramses","mpe","ramses2")


## Decodage
logger<-list(filename=filename,data=NULL)

for (l in 1:length(datalogger)){
  temp<-datalogger[l]
  temp<-strsplit(temp,split=",")[[1]]
  
  
  s<-match(temp[2],VectId)
  
  if (sum(s)>0){
  
    #creation
    if (is.null(logger$data[[VectName[s]]])){
      logger$data[[VectName[s]]]<-NULL
    }
    
    # add data
    logger$data[[VectName[s]]]<-rbind(logger$data[[VectName[s]]],as.numeric(temp[-2]))
  }
  
}

## Format Standard
for (i in 1:length(logger$data)){
  logger$data[[i]]<-cbind(0,logger$data[[i]][,1],"ASC","(RW)",logger$data[[i]][,-1])
  colnames(logger$data[[i]])<-paste("V",1:ncol(logger$data[[i]]),sep="")
  colnames(logger$data[[i]])[1:4]<-c("Pressure_dbar","Date","PhaseName","processing")
  
  logger$data[[i]]<-data.frame(logger$data[[i]],stringsAsFactors = F,check.names = F)
  
  logger$data[[i]]$Date<-as.POSIXlt(as.numeric(logger$data[[i]]$Date),origin = "1970-01-01",tz="UTC")
  
  for (j in c(1,5:ncol(logger$data[[i]]))){
    logger$data[[i]][,j]<-as.numeric(logger$data[[i]][,j])
  }
}

## sbe41
if (!is.null(logger$data$sbe41)){
  logger$data$sbe41$Pressure_dbar<-logger$data$sbe41$V5
  logger$data$sbe41<-logger$data$sbe41[,-5]
  colnames(logger$data$sbe41)[-(1:4)]<-c("Temperature_degC","Salinity_PSU")
}

## do
if (!is.null(logger$data$do)){
  colnames(logger$data$do)[-(1:4)]<-c("c1phase_deg","c2phase_deg","tempdoxy_degC")
}

## eco
if (!is.null(logger$data$eco)){
  colnames(logger$data$eco)[-(1:4)]<-c("chlorophyll-a_CN","beta-theta_CN","colored-dissolved-organic-matter_CN")
}

## ocr
if (!is.null(logger$data$ocr)){
  colnames(logger$data$ocr)[-(1:4)]<-c("Downwelling-irradiance-380nm_CN","Downwelling-irradiance-412nm_CN","Downwelling-irradiance-490nm_CN","Photosynthetic-Active-Radiation_CN")
}


## RAMSES
if (!is.null(logger$data$ramses)){
  lspectre<-length(colnames(logger$data$ramses))-6
  colnames(logger$data$ramses)[-(1:4)]<-c("ramses_int_time","ramses_dark_count",paste("ramses_raw_count",1:lspectre,sep = ""))
}

## RAMSES2
if (!is.null(logger$data$ramses2)){
  lspectre<-length(colnames(logger$data$ramses2))-6
  colnames(logger$data$ramses2)[-(1:4)]<-c("ramses_int_time","ramses_dark_count",paste("ramses_raw_count",1:lspectre,sep = ""))
}

## MPE
if (!is.null(logger$data$mpe)){
  colnames(logger$data$mpe)[-(1:4)]<-c("Voltage","Temperature")
}

return(logger)

}


##### donnees raw

# filename="081_02_imu_raw.csv"
# 
# CycleNumber=81
# 
# PatternNumber=2
# 
# data.colnames<-c("Date","Temperature","RawAx","RawAy","RawAz",
#                  "RawGx","RawGy","RawGz","RawMx","RawMy","RawMz")
# 
# MaxCol=length(data.colnames)
# 
# Data<-read.table(filename,header=FALSE,sep=";",dec=".",stringsAsFactors = FALSE,fill = TRUE,col.names = 1:MaxCol)
# 
# Dataclean<-NULL
# 
# colnames(Data)<-data.colnames
# 
# #Recherche des phases
# ind<-grep("\\[",Data[,1])
# ind<-c(ind,nrow(Data))
# for (i in 1:(length(ind)-1)){
#   datatemp<-Data[(ind[i]+1):(ind[i+1]),]
#   # suppression des non numeriques
#   datatemp<-datatemp[!is.na(datatemp[,2]),]
#   
#   if (nrow(datatemp)>0){
#     
#     # identification des phases
#     if (Data[ind[i],1]=="[DESCENT]"){NumberPhase<-"DES"}
#     if (Data[ind[i],1]=="[PARK]"){NumberPhase<-"PAR"}
#     if (Data[ind[i],1]=="[DEEP_PROFILE]"){NumberPhase<-"DEE"}
#     if (Data[ind[i],1]=="[SHORT_PARK]"){NumberPhase<-"SHP"}
#     if (Data[ind[i],1]=="[ASCENT]"){NumberPhase<-"ASC"}
#     if (Data[ind[i],1]=="[SURFACE]"){NumberPhase<-"SUR"}
#     
#     datatemp<-cbind(datatemp[,1],CycleNumber,PatternNumber,NumberPhase,as.character(filename),SensorType,datatemp[,-1])
#     
#     Dataclean<-rbind(Dataclean,datatemp)
#   }
#   
# }
# 
# colnames(Dataclean)[c(1,5)]<-c("Date","filename")
# 
# dataprofile<-list()
# 
# dataprofile$data$imu<-Dataclean
# 
# dataprofile$data$imu<-Process_RawIMU(dataprofile$data$imu,Meta$SENSORS$SENSOR_IMU)
# 
# write.table(dataprofile$data$imu,file = "081_02_imu_raw_phy.csv",col.names = T,row.names = F,sep=";")
# 
