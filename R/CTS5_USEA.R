options(stringsAsFactors = FALSE)
require(chron)

#**************************************************

# FindFloatName

#**************************************************
#' find the floatname within a float directory
#'
#' @description
#' take the last file with the pattern "ffff_ccc_pp.*" a return the hexafloatname
#' 
#' @examples 
#' 
#' findfloatname(pattern="[[:alnum:]]{4}_[[:digit:]]{3}_[[:digit:]]{2}.*")
#' 
#' 
#' 
findfloatname<-function(pattern="^[[:alnum:]]{4}_[[:digit:]]{3}_[[:digit:]]{2}.*",
                        CycleNumber=NULL,PatternNumber=1){
  
if (!is.null(CycleNumber)){
  pattern=paste("^[[:alnum:]]{4}_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),".*",sep="")
}
  
flist<-list.files(pattern=pattern)
if (length(flist)>0){
  i<-which.max(file.mtime(flist))
  return(strsplit(flist[i],split="_")[[1]][1])
}
else {
  return("")
}
}

#**************************************************

# Decod USEA

#**************************************************
#' Use NKE routine to decode .hex data files
#'
#' @description
#' call apmtdecoder.exe routine to decode .hex data files and create NKE .csv ASCII files.
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param sensors list of sensor to decode. From the list CTS5_supported_sensors
#' @param subdir subdir where to put .csv ASCII files
#' @param Nke_ProgPath path to the nke decoder (APMTDecrypt.exe or Decoder). This path is stored to Sys.getenv("USEAR_Nke_ProgPath").
#' For Linux, if the Decoder path is in the .bashrc, you can leave this parameters empty. 
#' 
#' @return No return. This function create ASCII files in the subdir directory
#' 
#' @details this function must be call in the directory where are .hex files
#' 
#' 
#' @export
#'
cts5_decode<-function(floatname="",CycleNumber,PatternNumber=1,sensors=CTS5_supported_sensors,subdir=".",
                      Nke_ProgPath=""){
  
  
  # Automatic hexa floatname
  if (floatname==""){
    floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
  }
  
  
  #Positionnement dans le repertoire Decoder
  if (Nke_ProgPath == ""){
    if (Sys.getenv("USEAR_Nke_ProgPath") != ""){
      ProgDir=Sys.getenv("USEAR_Nke_ProgPath")
    }
    else {
      if (Sys.info()["sysname"] == "Windows"){
        warning("Nke_ProgPath where APMTDecrypt.exe are must be defined for windows. Provide Nke_ProgPath 
                or set Sys.getenv('USEAR_Nke_ProgPath')",immediate.=T)
      }
      ProgDir=""
    }
  }
  else {
    ProgDir=Nke_ProgPath
    Sys.setenv(USEAR_Nke_ProgPath=Nke_ProgPath)
  }
  
  
  #windows
  if (Sys.info()["sysname"] == "Windows"){
    ProgName="APMTDecoder.exe"
    OSlabel=""
  }
  else {
    ProgName="Decoder"
    OSlabel=".linux"
  }

  
  for (cycle in CycleNumber){
    for (Pattern in PatternNumber){
      for (sensor in sensors){ #sensor<-sensors[1]
        
      SensorFilename<-paste(floatname,"_",formatC(cycle,width=3,flag="0"),"_",formatC(Pattern,width=2,flag="0"),"_",sensor,".hex",sep="")
      
      #Concat  
      pattern=paste(floatname,"_",formatC(cycle,width=3,flag="0"),"_",formatC(Pattern,width=2,flag="0"),"_",sensor,".*.hex",sep="")
      filenames<-list.files(pattern=pattern)
      if ((length(filenames)>1) & !file.exists(SensorFilename)){
        #cat(pattern,", multiple files. Need cat \n")
        filenames<-filenames[grep("#",filenames)]
        
        datacat<-NULL
        for (filename in filenames){
          datacat<-c(datacat,readBin(filename,what="raw",n=25*1024))
        }
        
        cat("cat ",pattern," > ",SensorFilename,"\n")
        
        writeBin(datacat,SensorFilename)
        
      }
      
      #decodage
      if (file.exists(SensorFilename)){
        
        # Decodage windows
        if (Sys.info()["sysname"] == "Windows"){
        
          dataDir<-getwd()
          
          filename<-paste(dataDir,"/",SensorFilename, " 0",sep="")
          
          ## decompression
          setwd(ProgDir)
          cat(paste(ProgName,filename,sep=" "),"\n")
          system(paste(ProgName,filename,sep=" "))
          
          setwd(dataDir)
        }
        
        # Decodage Linux / MacOS
        else {
          
          cmd<-paste(ProgDir,"Decoder ",SensorFilename,sep="")
          cat(cmd,"\n")
          system(cmd)
          
          #setwd(dataDir)
          
        }
        
        
        #Deplacement des fichiers
        if (subdir != "."){
          filename<-paste(floatname,"_",formatC(cycle,width=3,flag="0"),"_",formatC(Pattern,width=2,flag="0"),"_",sensor,OSlabel,".csv",sep="")
          file.rename(from=filename,to=paste(subdir,filename,sep="/"))
        }
      }
      else {
        cat(SensorFilename,", does not exists \n")
      }
      }
    }
  }
  
}


#**************************************************

# Sensors

#**************************************************

#' list of available sensors
#' @description vector of names of available sensors
#' @rawNamespace export(CTS5_supported_sensors)
CTS5_supported_sensors<-c("sbe41","do","eco","ocr","crover","suna","sbeph",
                          "uvp6_lpm","uvp6_blk","ramses","opus_lgt","opus_blk","ext_trig")
#' 

#**************************************************
#' provide sensor Id used un csv file
#'
#' @description
#' provide sensor Id used un csv file \code{\link{cts5_readcsv}} .
#'
#' @param pattern name of the sensor to look for. if "", provide the full list.
#' 
#' @return a vector containing the id of the sensors
#' 
#' @details 
#' 
#'  SensorType
#'   0 : CTD
#'   3 : DO Aanderaa
#'   9 : ECOpuck
#'  12 : OCR504
#'  18 : cROVER
#'  21 : SUNA
#'  22 : PHSEABIRD
#' 109 : UVP6 lpm
#' 110 : UVP6 blk
#' 111 : UVP6 TAXO1 (future use)
#' 112 : UVP6 TAXO2 (future use)
#' 113 : Ramses
#' 114 : opus_lgt
#' 115 : opus_blk
#' 116 : ext_trig
#' 
#' @examples 
#' cts5_SensorTypeId("")
#' 
#' cts5_SensorTypeId("uvp6")
#' 
#' cts5_SensorTypeId("uvp6_blk")
#' 
#' @export
#'
cts5_SensorTypeId<-function(pattern=""){
  
# !!!! MUST be in the same order than CTS5_supported_sensors !!!!!
SensorTypeId<-c(0,3,9,12,18,21,22,109,110,113,114,115,116)

names(SensorTypeId)<-CTS5_supported_sensors

if (pattern == ""){
  return(SensorTypeId)
}
else {
  ind<-grep(pattern,CTS5_supported_sensors,ignore.case = T)
  return(SensorTypeId[ind])
}
  
}


#**************************************************

# read USEA data file (generic)

#**************************************************
#' read USEA .csv data file
#'
#' @description
#' read NKE .csv ASCII files obtained from \code{\link{cts5_decode}} .
#'
#' @param floatname hexa name of the float
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param sensor name of the sensor to read from the list CTS5_supported_sensors
#' @param dec decimal character in ASCII
#' 
#' @return data.frame containing the data
#' 
#' @details this function add an identification for the sensorType from \code{\link{cts5_SensorTypeId}} 
#' and acquisition phase.
#' 
#' 
#' Phase
#' [DESCENT]->"DES"
#' [PARK]->"PAR"
#' [DEEP_PROFILE]->"DEE"
#' [SHORT_PARK]->"SHP"
#' [ASCENT]->"ASC"
#' [SURFACE]->"SUR"
#' 
#' @export
#'
cts5_readcsv<-function(floatname="ffff",CycleNumber,PatternNumber=1,sensor="sbe41",dec="."){
  
  # nom du fichier
  pattern<-paste("^",floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_",sensor,".*.csv",sep="")
  filename<-list.files(pattern=pattern)[1]
  
  DepthName<-"Pressure_dbar"
  
  #****************************
  #* BEGIN Sensors description
  #* **************************
  
  ##-1 sbe41
  if (sensor == "sbe41"){
    data.colnames<-c("Temperature_degC","Salinity_PSU")
    # SensorType=0
  }
  
  ##-2 do
  if (sensor == "do"){
    data.colnames<-c("c1phase_deg","c2phase_deg","tempdoxy_degC")
    # SensorType=3
  }
  
  ##-3 eco
  if (sensor == "eco"){
    data.colnames<-c("chlorophyll-a_CN","beta-theta_CN","colored-dissolved-organic-matter_CN")
    # SensorType=9
  }
  
  ##-4 ocr
  if (sensor == "ocr"){
    data.colnames<-c("Downwelling-irradiance-380nm_CN","Downwelling-irradiance-412nm_CN","Downwelling-irradiance-490nm_CN","Photosynthetic-Active-Radiation_CN")
    # SensorType=12
  }
  
  ##-5 crover
  if (sensor == "crover"){
    data.colnames<-c("Corr-Sig-Raw_CN")
    # SensorType=18
  }
  
  ##-6 suna
  if (sensor == "suna"){
    data.colnames<-c("CTDtempsuna","CTDsalsuna","SunaInterTemp","SunaSpectroTemp","SunaInterHumidty","SunaDarkSpectrumMean",
                     "SunaDarkSpectrumSD","nitrate-concentration_uMol/l","SunaAbsorbanceFitResuduals",paste("OutSpectrum",1:90,sep=""))
    # SensorType=21
  }
  
  ##-7 sbeph
  if (sensor == "sbeph"){
    data.colnames<-"pH_mV"
    # SensorType=22
  }
  
  ##-8 uvp6_lpm
  if (sensor == "uvp6_lpm"){
    # OctopusClass<-c(64, 80.6, 102, 128, 161, 203, 256, 323, 406, 512, 645, 813, 1020, 1290, 1630, 2050, 2580, 3250, 4100)
    # OctopusClass<-paste("[",OctopusClass[1:18],",",OctopusClass[2:19],"[(um)",sep="")
    
    data.colnames<-c(paste("NP_Class",1:18,sep=""),paste("MG_Class",1:18,sep=""))
    
    
    data.colnames<-c("Nimages","UVP6_Temp",
                      data.colnames,paste("SD(",data.colnames,")",sep=""),paste("MEAN(",c(DepthName,data.colnames),")",sep=""))
    
    
    # SensorType=109
    
  }
  
  ##-9 uvp6_blk
  if (sensor == "uvp6_blk"){

    data.colnames<-c("uvp-blk_Internal_temp","uvp-blk_Count1","uvp-blk_Count2","uvp-blk_Count3","uvp-blk_Count4","uvp-blk_Count5")
    
    
    # SensorType=110
    
  }
  
  ##-10 ramses
  if (sensor == "ramses"){
    data.colnames<-c("ramses_int_time","ramses_depth1","ramses_depth2","ramses_tilt1","ramses_tilt2","ramses_dark_count",
                     "ramses_N_channels",paste("ramses_raw_count",1:250,sep=""))
    # SensorType=113
  }
  
  ##-11 Opus_lgt
  if (sensor == "opus_lgt"){
    data.colnames<-c("opus_spectrum_type","opus_lgt_averaging","opus_lgt_flash_count","opus_lgt_int_temp",
                     "opus_N_channels",paste("opus_raw_count",1:250,sep=""))
    # SensorType=114
  }
  
  ##-12 Opus_blk
  if (sensor == "opus_blk"){
    data.colnames<-c("opus_blk_averaging","opus_blk_flash_count","opus_blk_int_temp",
                     "opus_blk_dark_mean","opus_blk_dark_std")
    # SensorType=115
  }
  
  ##-13 ext_trig
  if (sensor == "ext_trig"){
    data.colnames<-NULL
    # SensorType=116
  }
  
  #****************************
  #* END Sensors description
  #* **************************

  SensorType = cts5_SensorTypeId(sensor)
  
  Sensor_NChannel<-length(data.colnames)
  
  
  datatemp.colnames<-c("Date",DepthName,"processing",data.colnames,paste("SD(",data.colnames,")",sep=""),paste("MEAN(",c(DepthName,data.colnames),")",sep=""))

  
  MaxCol=length(datatemp.colnames)-1
  
  #traitement
  
  if (file.exists(filename)){
    cat("open:",filename,"\n")
    Data<-read.table(filename,header=FALSE,sep=",",dec=dec,stringsAsFactors = FALSE,fill = TRUE,col.names = 1:MaxCol)
    #Data<-Data[-length(Data[,1]),]
    
    Dataclean<-NULL
    
    
    #Recherche des mode de processing {(RW),(DW), ...}
    indPro<-grep("^\\(",Data[,1])
    processing<-rep("",length=nrow(Data))
    
    if (length(indPro)>0){
      indPro<-c(indPro,nrow(Data))
      for (i in 1:(length(indPro)-1)){
        processing[indPro[i]:indPro[i+1]]<-Data[indPro[i],1]
      }
    }
    
    Data<-cbind(Data[,1:2],processing,Data[,-(1:2)])
    
    colnames(Data)<-datatemp.colnames
    
    #Recherche des phases
    ind<-grep("\\[",Data[,1])
    ind<-c(ind,nrow(Data))
    for (i in 1:(length(ind)-1)){
      datatemp<-Data[(ind[i]+1):(ind[i+1]),]
      # suppression des non numeriques
      datatemp<-datatemp[!is.na(datatemp[,2]),]
      
      # identification des phases
      if (Data[ind[i],1]=="[DESCENT]"){NumberPhase<-"DES"}
      if (Data[ind[i],1]=="[PARK]"){NumberPhase<-"PAR"}
      if (Data[ind[i],1]=="[DEEP_PROFILE]"){NumberPhase<-"DEE"}
      if (Data[ind[i],1]=="[SHORT_PARK]"){NumberPhase<-"SHP"}
      if (Data[ind[i],1]=="[ASCENT]"){NumberPhase<-"ASC"}
      if (Data[ind[i],1]=="[SURFACE]"){NumberPhase<-"SUR"}
      
      # En mode Park et Surface le processing est RW obligatoirement
      if (NumberPhase %in% c("PAR","SHP","SUR")){
        datatemp$processing<-"(RW)"
      }
      
      # Reorganisation des donnees en fonction du processing
      for (pro in unique(datatemp$processing)){
        if (pro == "(AM)(MD)"){
          datatemp[datatemp$processing == pro,(6+Sensor_NChannel):(6+2*Sensor_NChannel)]<-datatemp[datatemp$processing == pro,(4+Sensor_NChannel):(4+2*Sensor_NChannel)]
          datatemp[datatemp$processing == pro,(4+Sensor_NChannel):(3+2*Sensor_NChannel)]<-NA
        }
      }
      
      
      datatemp<-cbind(datatemp[,c(2,1)],CycleNumber,PatternNumber,NumberPhase,as.character(filename),SensorType,datatemp[,-(1:2)])
      
      Dataclean<-rbind(Dataclean,datatemp)
      
      
    }
    
    colnames(Dataclean)[3:7]<-c("NumberCycle","NumberPattern","PhaseName","Files","SensorType")
    
    if (sensor %in% c("uvp6_lpm")){
      Dataclean<-cbind(Dataclean[,2],Dataclean[,9],Dataclean[,3:8],Dataclean[,1],Dataclean[,-(1:9)])
      colnames(Dataclean)[c(1:2,9)]<-c("Date",DepthName,"NSamples")
    }
    
    ## Elimination des colonnes inutiles
    if (length(grep("SD",Dataclean$processing))==0){
      Dataclean<-Dataclean[,-grep("SD\\(",colnames(Dataclean))]
    }
    
    if (length(grep("MD",Dataclean$processing))==0){
      Dataclean<-Dataclean[,-grep("MEAN\\(",colnames(Dataclean))]
    }
    
    # Elimination des colonnes NA pour suna et ramses
    if (sensor %in% c("suna","ramses","opus_lgt")){
      indNA<-apply(Dataclean,2,function(c){all(is.na(c))})
      Dataclean<-Dataclean[,!indNA]
    }
    
    # Elimination processing
    if (sensor %in% c("ext_trig")){
      Dataclean<-Dataclean[,-grep("processing",colnames(Dataclean))]
    }
    
    # Conversion temps
    if (length(grep("linux",filename))>0){
      #Format Linux
      Dataclean$Date<-as.POSIXct(as.numeric(Dataclean$Date),origin = "1970-01-01",tz="UTC")
    }
    else {
      #Format Windows
      Dataclean$Date<-strptime(Dataclean$Date,format = "%Y-%m-%d %H:%M:%S",tz="UTC")
    }
    Dataclean$Date<-strptime(Dataclean$Date,format = "%Y-%m-%d %H:%M:%S",tz="UTC")
    
    #Forcage Numerique
    if (ncol(Dataclean) >= 9 ){
      for (i in (9:ncol(Dataclean))){
        Dataclean[,i]<-as.numeric(Dataclean[,i])
      }
    }
    
    
    #write.table(Dataclean,file = "Dataclean.txt",col.names = T,row.names = F,sep=";")
    
  }
  else {
    cat("No file for ",pattern,"\n")
    Dataclean<-NULL
  }
  
  
  return(Dataclean)
}



#**************************************************

# convert to ASCII

#**************************************************

#' convert dataprofile list object to one data.frame
#'
#' @description
#' convert dataprofile list object to one data.frame
#' 
#' @param dataprofile data list from \code{\link{cts5_readprofile}} or \code{\link{cts5_ProcessData}}
#' @param AddGPS if TRUE, add GPS from technical
#' 
#' @return data.frame containing the data
#' 
#' @details this function add an identification for the sensorType and acquisition phase 
#' as explained in \code{\link{cts5_readcsv}}
#' 
#' @examples 
#' 
#' login="lovuse001a"
#' 
#' Meta<-cts5_readMetaSensor()
#'
#' cts5_decode(CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' dataprofile<-cts5_readProfile(CycleNumber=c,PatternNumber = p,include.inifile=T)
#' 
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#' 
#' dataMerged<-cts5_convert2Ascii(dataprofile)
#' 
#' cts5_save2Ascii(login = login,dataMerged = dataMerged,subdir="./csv/")
#' 
#' @export
#'

cts5_convert2Ascii<-function(dataprofile,AddGPS=T){
  
EnTeteCom<-c("Pressure_dbar","Date","CycleNumber","PatternNumber","PhaseName","SensorType","processing")

dataMerged<-NULL

if (!is.null(dataprofile$data)){

  dataMerged<-dataprofile$data[[1]]
  dataMerged$SensorType<-rep(cts5_SensorTypeId(names(dataprofile$data)[1]),nrow(dataMerged))
  dataMerged$CycleNumber<-rep(dataprofile$CycleNumber,nrow(dataMerged))
  dataMerged$PatternNumber<-rep(dataprofile$PatternNumber,nrow(dataMerged))
  dataMerged<-cbind(dataMerged[,EnTeteCom],dataMerged[,!(colnames(dataMerged) %in% EnTeteCom)])

  if (length(dataprofile$data)>1){
    for (i in 2:length(dataprofile$data)){
    
    data<-dataprofile$data[[i]]
    data$SensorType<-rep(cts5_SensorTypeId(names(dataprofile$data)[i]),nrow(data))
    data$CycleNumber<-rep(dataprofile$CycleNumber,nrow(data))
    data$PatternNumber<-rep(dataprofile$PatternNumber,nrow(data))
    
    if (is.data.frame(data)){
      addCol<-colnames(data)[!(colnames(data) %in% EnTeteCom)]
      
      if (!is.null(dataMerged)){
      
        data<-cbind(data[,EnTeteCom],matrix(NA,nrow = dim(data[,EnTeteCom])[1],ncol = dim(dataMerged)[2]+length(addCol)-dim(data)[2]),data[,addCol])
        
        dataMerged<-cbind(dataMerged,matrix(NA,nrow = dim(dataMerged)[1],ncol=length(addCol)))
        colnames(dataMerged)[(dim(dataMerged)[2]-length(addCol)+1):dim(dataMerged)[2]]<-addCol
        
        colnames(data)<-colnames(dataMerged)
        dataMerged<-rbind(dataMerged,data)}
      else {
        dataMerged<-data
      }
    }
      
    }
  }

  ## Tri temporel
  if (!is.null(dataMerged)){
    dataMerged<-dataMerged[order(dataMerged$Date),]
  }
  
  ## AddGPS
  if (AddGPS){
    if (!is.null(dataprofile$technical$GPS)){
      GPS<-dataprofile$technical$GPS
      dataMerged<-cbind(dataMerged[,1:2],NA,NA,dataMerged[,-(1:2)])
      dataMerged<-rbind(dataMerged,NA)
      dataMerged[dim(dataMerged)[1],c(1,3:6,9)]<-c(0,GPS$`lat (deg)`,GPS$`lon (deg)`,as.numeric(dataMerged[1,5:6]),-1)
      
      ##temp
      dataMerged[,2]<-as.numeric(dataMerged[,2])
      dataMerged[dim(dataMerged)[1],2]<-as.numeric(GPS$time)
      dataMerged[,2]<-as.POSIXct(dataMerged[,2],origin = "1970-01-01",tz="UTC")
      
      colnames(dataMerged)[3:4]<-c("lat_deg","lon_deg")
      
    }
  }
}

return(dataMerged)

}


#**************************************************
#' Process data to physical values
#'
#' @description
#' apply processing to raw values in order to compute physical values
#'
#' @param metadata is a list containing technical data including calibration coefficients 
#' obtained by using \code{\link{cts5_readMetaSensor}}
#' @param dataprofile data and technical files read from \code{\link{cts5_readProfile}}
#' @param ProcessUncalibrated if True, process physical data with default calibration if needed
#' 
#' @return data.frame containing the data
#' 
#' @details calibration coefficients are usually in 00_metadata.xml file.
#' 
#' @examples 
#' 
#' login="lovuse001a"
#' 
#' Meta<-cts5_readMetaSensor()
#'
#' cts5_decode(CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' dataprofile<-cts5_readProfile(CycleNumber=c,PatternNumber = p,include.inifile=T)
#'
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#'
#' PlotCTS5(login=login,dataprofile,PhaseToPlot=c("PRE","DES","PAR","ASC","SUR"),add=FALSE,technical=TRUE,paper = "A4",mfrow=c(3,2))
#'
#' 
#' @export
#'
cts5_ProcessData<-function(metadata,dataprofile,ProcessUncalibrated=F){
  
  ### ECO
  if ("eco" %in% names(dataprofile$data)) {
    
    SENSOR_ECO<-NULL
    
    if (!is.null(metadata$SENSOR_ECO)){
      
      SENSOR_ECO<-metadata$SENSOR_ECO
    }
    else {
      cat("!! Warning : No ECO calibration found \n")
      
      if (ProcessUncalibrated){
        cat("!! Default calibration is used \n")
        SENSOR_ECO<-list(CHANNEL_01=c(0.0073,47.0000),CHANNEL_02=c(1.753e-06,4.700e+01),CHANNEL_03=c(0.0907,50.0000))
      }
      
      
    }
      
    if (!is.null(SENSOR_ECO)){
      ## Raw or mean
      if ("chlorophyll-a_CN" %in% colnames(dataprofile$data$eco)){
        dataprofile$data$eco[,"chlorophyll-a_ug/l"]<-SENSOR_ECO$CHANNEL_01[1]*(dataprofile$data$eco[,"chlorophyll-a_CN"]-SENSOR_ECO$CHANNEL_01[2])
        dataprofile$data$eco[,"beta-theta_1/msr"]<-SENSOR_ECO$CHANNEL_02[1]*(dataprofile$data$eco[,"beta-theta_CN"]-SENSOR_ECO$CHANNEL_02[2])
        dataprofile$data$eco[,"colored-dissolved-organic-matter_ppb"]<-SENSOR_ECO$CHANNEL_03[1]*(dataprofile$data$eco[,"colored-dissolved-organic-matter_CN"]-SENSOR_ECO$CHANNEL_03[2])
      }
        
      ## SD
      if ("SD(chlorophyll-a_CN)" %in% colnames(dataprofile$data$eco)){
        dataprofile$data$eco[,"SD(chlorophyll-a_ug/l)"]<-SENSOR_ECO$CHANNEL_01[1]*dataprofile$data$eco[,"SD(chlorophyll-a_CN)"]
        dataprofile$data$eco[,"SD(beta-theta_1/msr)"]<-SENSOR_ECO$CHANNEL_02[1]*dataprofile$data$eco[,"SD(beta-theta_CN)"]
        dataprofile$data$eco[,"SD(colored-dissolved-organic-matter_ppb)"]<-SENSOR_ECO$CHANNEL_03[1]*dataprofile$data$eco[,"SD(colored-dissolved-organic-matter_CN)"]
      }
    }
  }
  
  ### OCR
  if ("ocr" %in% names(dataprofile$data)) {
    if (!is.null(metadata$SENSOR_OCR) & ("Downwelling-irradiance-380nm_CN" %in% colnames(dataprofile$data$ocr))){
      dataprofile$data$ocr[,"Downwelling-irradiance-380nm"]<-metadata$SENSOR_OCR$CHANNEL_01[2]*metadata$SENSOR_OCR$CHANNEL_01[3]*
        (dataprofile$data$ocr[,"Downwelling-irradiance-380nm_CN"]-metadata$SENSOR_OCR$CHANNEL_01[1])
      
      dataprofile$data$ocr[,"Downwelling-irradiance-412nm"]<-metadata$SENSOR_OCR$CHANNEL_02[2]*metadata$SENSOR_OCR$CHANNEL_02[3]*
        (dataprofile$data$ocr[,"Downwelling-irradiance-412nm_CN"]-metadata$SENSOR_OCR$CHANNEL_02[1])
      
      dataprofile$data$ocr[,"Downwelling-irradiance-490nm"]<-metadata$SENSOR_OCR$CHANNEL_03[2]*metadata$SENSOR_OCR$CHANNEL_03[3]*
        (dataprofile$data$ocr[,"Downwelling-irradiance-490nm_CN"]-metadata$SENSOR_OCR$CHANNEL_03[1])
      
      dataprofile$data$ocr[,"Photosynthetic-Active-Radiation"]<-metadata$SENSOR_OCR$CHANNEL_04[2]*metadata$SENSOR_OCR$CHANNEL_04[3]*
        (dataprofile$data$ocr[,"Photosynthetic-Active-Radiation_CN"]-metadata$SENSOR_OCR$CHANNEL_04[1])
      
      }
  }
  
  ### uvp6_lpm
  if ("uvp6_lpm" %in% names(dataprofile$data)) {
    if (!is.null(metadata$SENSOR_UVP6)){
      uvp6_Size_class<-strsplit(metadata$SENSOR_UVP6$HW_CONF,split=",")[[1]][26:43]
      data.colnames<-c(paste("NP_Size_",uvp6_Size_class,sep=""),paste("MG_Size_",uvp6_Size_class,sep=""))
      
      colnames(dataprofile$data$uvp6_lpm)[7:42]<-data.colnames
    }
  }
  
  ### crover
  if (("crover" %in% names(dataprofile$data)) & ProcessUncalibrated) {
    if ("Corr-Sig-Raw_CN" %in% colnames(dataprofile$data$crover)){
      CSCdark=0
      CSCcal=12766
      x=0.25
      dataprofile$data$crover[,"c-uncalibrated_1/m"] <- -log((dataprofile$data$crover[,"Corr-Sig-Raw_CN"]-CSCdark)/(CSCcal-CSCdark))/x
    }
  }
  
  ### DO
  if (("do" %in% names(dataprofile$data)) & ("sbe41" %in% names(dataprofile$data))) {
    if ("c1phase_deg" %in% colnames(dataprofile$data$do)){
      
    if (is.list(metadata$SENSOR_DO)){
      coefs<-metadata$SENSOR_DO$SVU_FOIL_COEFF
    }
      else {
      cat("!! Warning : No DO calibration found \n")
      coefs<-NULL
      
      if (ProcessUncalibrated){
        cat("!! Default calibration is used \n")
        coefs <- c(5.6725661e-03,8.2915275e-05,1.0033795e-06,6.2236942e-02,-9.3470722e-05,-1.4554620e-02,1.2110645e-03) # From Henry
      }
      
      }
        
      if (!is.null(coefs)){
      dataprofile$data$do[,"doxy_uncalibrated"]<-Process_DO_Bittig(C1phase=dataprofile$data$do[,"c1phase_deg"],C2phase=dataprofile$data$do[,"c2phase_deg"],temp=dataprofile$data$do[,"tempdoxy_degC"],Pres=dataprofile$data$do[,"Pressure_dbar"],
                                             tempCTD=dataprofile$data$sbe41[,"Temperature_degC"],salCTD=dataprofile$data$sbe41[,"Salinity_PSU"],PRESCTD=dataprofile$data$sbe41[,"Pressure_dbar"],
                                             COEF = coefs)
      }
    }
  }
  
  ### sbepH
  if (("sbeph" %in% names(dataprofile$data)) & ("sbe41" %in% names(dataprofile$data))) {
    if (("pH_mV" %in% colnames(dataprofile$data$sbeph)) & ProcessUncalibrated){
      
      pH_Uncal<-rep(NA,nrow(dataprofile$data$sbeph))
      
      ##DESCENT
      ind<-dataprofile$data$sbeph$PhaseName=="DES"
      if (dim(dataprofile$data$sbeph[ind,])[1] > 2){
        ## Il y a des donnees en descent
        pH_Uncal[ind]<-Process_pH_SBE(dataprofile$data,NumberPhase="DES")
      }
      ##ASCENT
      ind<-dataprofile$data$sbeph$PhaseName=="ASC"
      if (dim(dataprofile$data$sbeph[ind,])[1] > 2){
        ## Il y a des donnees en Asc
        pH_Uncal[ind]<-Process_pH_SBE(dataprofile$data,NumberPhase="ASC")
      }
      
      dataprofile$data$sbeph[,"pH_Uncal"]<-pH_Uncal
      
    }
  }
  
  ### Ramses
  if ("ramses" %in% names(dataprofile$data)) {
    
    if ("inifile" %in% names(dataprofile)){
      PixelStart=dataprofile$inifile$SENSOR_14$P54
      PixelStop=dataprofile$inifile$SENSOR_14$P55
      PixelBinning=dataprofile$inifile$SENSOR_14$P56
    }
    else {
      # default configuration valid for datalogger mode
      PixelStart=1
      PixelStop=length(grep("ramses_raw_count",colnames(dataprofile$data$ramses)))
      PixelBinning=1
      
      warning("RAMSES calibration: No inifile found. Apply default setting")
    }
    
    dataCal<-Process_Ramses(dataprofile$data$ramses,PixelStart=PixelStart,PixelStop=PixelStop,
                            PixelBinning=PixelBinning,calib_file="SAM.*AllCal.txt")
    
    dataprofile$data$ramses<-cbind(dataprofile$data$ramses,dataCal)
    
  }
  
  
  
  return(dataprofile)
  
}

#**************************************************
#' save data to CTS5 csv format
#'
#' @description
#' save data to CTS5 csv format
#'
#'
#' @param login login of the float used as prefix
#' @param dataMerged Merged data obtained by \code{\link{cts5_convert2Ascii}}
#' @param subdir sub directory where to save the data
#' 
#' @examples 
#' 
#' login="lovuse001a"
#' 
#' Meta<-cts5_readMetaSensor()
#'
#' cts5_decode(CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' dataprofile<-cts5_readProfile(CycleNumber=c,PatternNumber = p,include.inifile=T)
#' 
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#' 
#' dataMerged<-cts5_convert2Ascii(dataprofile)
#' 
#' cts5_save2Ascii(login = login,dataMerged = dataMerged,subdir="./csv/")
#' 
#' @export
#'

cts5_save2Ascii<-function(login,dataMerged,subdir="./csv/"){
  
  if (!is.null(dim(dataMerged))){  
    filename<-paste(subdir,login,"_",formatC(unique(dataMerged[,"CycleNumber"]),width=3,flag="0"),"_",
                    formatC(unique(dataMerged[,"PatternNumber"]),width=2,flag="0"),".csv",sep="")
    cat("save data : ",filename,"\n")
    
    write.table(dataMerged,filename,col.names=TRUE,row.names=FALSE,sep="\t",quote = FALSE)  
    
  }
}

#**************************************************

# read USEA data and Meta file for a profile

#**************************************************

#' cts5_readProfile : Concat .csv files to one data.frame
#'
#' @description
#' read NKE .csv ASCII files obtained from \code{\link{cts5_decode}} and 
#' concat them into a list
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param sensors list of sensor to decode
#' @param dec decimal character in ASCII
#' @param include.inifile If True, read the corresponding iniFile and add it to the list.
#' 
#' @return a list containing the data
#' 
#' 
#' @examples 
#' 
#' login="lovuse001a"
#' 
#' Meta<-cts5_readMetaSensor()
#'
#' cts5_decode(CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' dataprofile<-cts5_readProfile(CycleNumber=c,PatternNumber = p,include.inifile=T)
#' 
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#' 
#' @export
#'

cts5_readProfile<-function(floatname="",CycleNumber,PatternNumber=1,sensors=CTS5_supported_sensors,dec=".",
                           include.inifile=F,csv.subdir="./CSV"){
  

# Automatic hexa floatname
if (floatname==""){
  floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
}  
  
## read technical
dataprofile<-list(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber)

## read technical
dataprofile$technical<-cts5_readtechnical(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber)

## read inifile
if (include.inifile){
  dataprofile$inifile<-cts5_readIni(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber)
}


if (csv.subdir != ""){
  setwd(csv.subdir)
}


## Open data
dataprofile$data<-list()

for (sensor in sensors){ #sensor<-sensors[1]
  data<-try(cts5_readcsv(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber,sensor=sensor,dec=dec))
  
  if (is.data.frame(data)){
    dataprofile$data[[sensor]]<-data[,!(colnames(data) %in% c("NumberCycle","NumberPattern","Files","SensorType"))]
  }
}

if (csv.subdir != ""){
  setwd("../")
}

## dataprofile NULL if no data
if (is.null(dataprofile$technical) & (length(dataprofile$data) == 0)){
  dataprofile<-NULL
}

return(dataprofile)

}

#**************************************************

# save profile to RData

#**************************************************

#' cts5_save2RData
#'
#' @description
#' save dataprofile to RData
#'
#' @param dataprofile dataprofile to be saved
#' @param login identifiant used at the beginning of the pdf filename
#' @param subdir sub directory used to save the file. Must include / at the end.
#' 
#' @examples 
#' cts5_save2RData(dataprofile,login = login)
#' 
#' cts5_save2RData(dataprofile,login = login,subdir="./csv/")
#' 
#' @export
#'


cts5_save2RData<-function(dataprofile,login="",subdir=""){

if (!is.null(dataprofile)){   
    
  CycleNumber<-dataprofile$CycleNumber
  PatternNumber<-dataprofile$PatternNumber
  
  filename<-paste(subdir,login,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),".RData",sep="")
  cat("save to:",filename,"\n",sep="")
  
  save(dataprofile,file = filename)
}
}

#**************************************************

# Fichier Ini

#**************************************************
#' read CTS5 _apmt.ini file
#'
#' @description
#' read CTS5 _apmt.ini file and store the information into a list
#' 
#'
#'
#' @param inifilename filename of the ini file
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param CycleNumber numeric : number of the cycle to look for
#' @param PatternNumber numeric : number of the Pattern to look for
#' 
#' @return list containing the information
#' 
#' @details #' if the inifilename is not provided, the inifile which describe the profile CycleNumber,PatternNumber
#' will be open
#' 
#' @examples 
#' 
#' IniParam<-cts5_readIni("3e82_046_02_apmt.ini")
#' 
#' IniParam<-cts5_readIni(floatname="3e82",CycleNumber=46,PatternNumber=2)
#' 
#' @export
#'
cts5_readIni<-function(inifilename="",floatname="",CycleNumber,PatternNumber=1){
  
  
  ## Selection of the file
  if (inifilename == ""){
    ## look for the ini file which describe the cycle and pattern
    
    # Automatic hexa floatname
    if (floatname==""){
      floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
    }
    
    listini<-list.files(pattern=paste("^",floatname,".*_apmt.ini",sep=""))
    
    ## More than one inifile
    if (length(listini)>1){
      listini<-cbind(listini,matrix(unlist(strsplit(listini,split="_")),ncol = 4,byrow = T)[,2:3])
      listini<-data.frame(listini,stringsAsFactors = F)
      colnames(listini)<-c("filename","c","p")
      listini$c<-as.numeric(listini$c)
      listini$p<-as.numeric(listini$p)
      
      ind<-((CycleNumber >= listini$c) & (PatternNumber > listini$p)) | (CycleNumber > listini$c)
  
      ind<-max(which(ind))
      
      inifilename<-listini$filename[ind]
    }
    
    ## One inifile
    if (length(listini)==1){
      inifilename<-listini
    }
    
    if (length(listini)==0){
      warning("No Inifile for pattern:",paste("^",floatname,".*_apmt.ini",sep=""),"\n")
    }
    
  }
  
  
  ## read and Parse
  if (file.exists(inifilename)){
    cat("open:",inifilename,"\n")
    data<-scan(inifilename,sep="\n",what=character(0))
    
    ## 1: split [balise]
    ind<-grep("^\\[",data)
    ind<-c(ind,length(data)+1)
    
    inifile<-list()
    
    for (i in 1:(length(ind)-1)){
      balisename<-substr(data[ind[i]],2,nchar(data[ind[i]])-1)
      inifile[[balisename]]<-data[(ind[i]+1):(ind[i+1]-1)]
    }
    
    ## 2: Analyse
    for (i in 1:length(inifile)){
      
      inifile[[i]]<-as.list(inifile[[i]])
      for (j in 1:length(inifile[[i]])){
        s<-inifile[[i]][[j]][1]
        
        s1<-strsplit(s,split = "=")[[1]][1]
        s2<-strsplit(s,split = "=")[[1]][2]
        
        #detection type numeric
        val<-suppressWarnings(as.numeric(s2))
        
        names(inifile[[i]])[j]<-s1
        if (is.na(val)){
          inifile[[i]][[j]]<-s2
        } else {
          inifile[[i]][[j]]<-as.numeric(s2)
        }
        
      }
    }
    
    return(inifile)
  }
  else {
    warning("No Inifile:",inifilename,"\n")
    return(NULL)
  }
  
}

#**************************************************

#' create a _command.txt files by comparing inifiles
#'
#' @description
#' create a _command.txt files by comparing newini and oldini files
#'
#'
#' @param newini filename of the new ini file
#' @param oldini filename of the old ini file. Used the latest ini file if equal to ""
#' @param sections section scanned to create command
#' @param output name of the command file
#' 
#' @return vector of command and _command.txt file
#' 
#' @details CTS5_create_command compare the two ini files only for sections given in the section parameter. Difference in other sections are ignored.
#' 
#' @examples 
#' 
#' cts5_create_command("apmt_update.ini",output = "_command.txt")
#' 
#' cts5_create_command(newini="apmt_uvp6_standard.ini",sections=c("SENSOR_08","SENSOR_09","SENSOR_10"))
#' 
#' @export
#'

cts5_create_command<-function(newini="apmt_update.ini",oldini="",sections=c("PATTERN","SENSOR"),output="_command.txt"){

if (file.exists(oldini)){
  cat("open old: ",oldini,"\n")
  oldinifile<-cts5_readIni(oldini)
}
else {
  oldini<-rev(list.files(pattern = ".*_apmt.ini"))[1]
  cat("old is latest ini files: ",oldini,"\n")  
  oldinifile<-cts5_readIni(oldini)
  }

newinifile<-cts5_readIni(newini)

command<-NULL

cat("WARNING: scanned sections are:",sections,"\n")

for (s in sections){ #s<-sections[1]

  ind<-grep(s,names(oldinifile))
  
  for (i in ind){
    #recherche de la même section dans newinifile
    inew<-which(names(newinifile)==names(oldinifile)[i])
    
    for (j in 1:length(oldinifile[[i]])){
      if (oldinifile[[i]][[j]] != newinifile[[inew]][[j]]){
        temp<-paste("!param-",tolower(names(oldinifile)[i]),"-",substr(names(oldinifile[[i]])[j],2,5)
                    ,":",newinifile[[inew]][[j]],sep="")
        command<-c(command,temp)
        
      }
    }
    
  }
  
  
}


if (output != ""){
  write.table(command,file = output,col.names = F,row.names = F,quote = F)
  cat("command save to: ",output,"\n")
}

return(command)
  
}


