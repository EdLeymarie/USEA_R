options(stringsAsFactors = FALSE)
require(chron)


#**************************************************

# Decod USEA

#**************************************************
#' Use NKE routine to decode .hex data files
#'
#' @description
#' call apmtdecoder.exe routine to decode .hex data files and create NKE .csv ASCII files.
#'
#' @param floatname hexa name of the float
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param sensors list of sensor to decode. From the list "sbe41","sbeph,"do","eco","ocr","suna","sbeph","uvp6_lpm","uvp6_blk"
#' @param subdir subdir where to put .csv ASCII files
#' @param Nke_ProgPath path to the nke decoder (APMTDecrypt.exe or Decoder). This path is stored to Sys.getenv("USEAR_Nke_ProgPath") 
#' 
#' @return No return. This function create ASCII files in the subdir directory
#' 
#' @details this function must be call in the directory where are .hex files
#' 
#' 
#' @export
#'
cts5_decode<-function(floatname="ffff",CycleNumber,PatternNumber=1,sensors=c("sbe41","do","eco","ocr","suna","sbeph","uvp6_lpm","uvp6_blk"),subdir=".",
                      Nke_ProgPath=""){
  
  #Positionnement dans le repertoire Decoder
  if (Nke_ProgPath == ""){
    if (Sys.getenv("USEAR_Nke_ProgPath") != ""){
      ProgDir=Sys.getenv("USEAR_Nke_ProgPath")
    }
    else {
      warning("Nke_ProgPath where APMTDecrypt.exe or Decoder are must be defined. Provide Nke_ProgPath 
              or set Sys.getenv('USEAR_Nke_ProgPath')",immediate.=T)
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
  #Linux
  if (Sys.info()["sysname"] == "Linux"){
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
        
        # Decodage Linux
        if (Sys.info()["sysname"] == "Linux"){
          
          dataDir<-getwd()
          
          #Positionnement dans le repertoire Decoder
          #Lubunto14-32
          if (Sys.info()[4] == "oao2016"){
            setwd("/home/data-usea/TOOLS/UTILS/decoder")
          }
          else {
            setwd(OS_Linux_progpath)
          }
          
          filename<-paste(dataDir,"/",SensorFilename,sep="")
          cat(paste("./Decoder",filename,sep=" "),"\n")
          system(paste("./Decoder",filename,sep=" "))
          
          setwd(dataDir)
          
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
#' @param sensor name of the sensor to read from the list c("sbe41","do","eco","ocr")
#' @param dec decimal character in ASCII
#' 
#' @return data.frame containing the data
#' 
#' @details this function add an identification for the sensorType and acquisition phase.
#' 
#'  SensorType
#'   0 : CTD
#'   3 : DO Aanderaa
#'   9 : ECOpuck
#'  12 : OCR504
#'  21 : SUNA
#'  22 : PHSEABIRD
#' 101 : OCR507 1
#' 102 : OCR507 2
#' 103 : Tilt
#' 104 : PSA916
#' 105 : RTSys
#' 106 : Contros
#' 107 : OptTak
#' 108 : Trios_Nitrate
#' 109 : Octopus lpm
#' 110 : Octopus blk
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
  
  DepthName<-"Pressure [dbar]"
  
  ## sbe41
  if (sensor == "sbe41"){
    data.colnames<-c("Temperature [deg. C.]","Salinity [PSU]")
    SensorType=0
  }
  
  ## do
  if (sensor == "do"){
    data.colnames<-c("c1phase_doxy [deg]","c2phase_doxy [deg]","temp_doxy [deg. C.]")
    SensorType=3
  }
  
  ## eco
  if (sensor == "eco"){
    data.colnames<-c("chlorophyll_a, [CN]","beta_theta, [CN]","colored_dissolved_organic_matter, [CN]")
    SensorType=9
  }
  
  ## ocr
  if (sensor == "ocr"){
    data.colnames<-c("Downwelling_irradiance_380nm, [CN]","Downwelling_irradiance_412nm, [CN]","Downwelling_irradiance_490nm, [CN]","Photosynthetic_Active_Radiation, [CN]")
    SensorType=12
  }
  
  ## suna
  if (sensor == "suna"){
    data.colnames<-c("CTDtempsuna","CTDsalsuna","SunaInterTemp","SunaSpectroTemp","SunaInterHumidty","SunaDarkSpectrumMean",
                     "SunaDarkSpectrumSD","nitrate_concentration, [micoMol/l]","SunaAbsorbanceFitResuduals",paste("OutSpectrum",1:45,sep=""))
    SensorType=21
  }
  
  ## sbeph
  if (sensor == "sbeph"){
    data.colnames<-"pH [mV]"
    SensorType=22
  }
  
  ## uvp6_lpm
  if (sensor == "uvp6_lpm"){
    OctopusClass<-c(64, 80.6, 102, 128, 161, 203, 256, 323, 406, 512, 645, 813, 1020, 1290, 1630, 2050, 2580, 3250, 4100)
    OctopusClass<-paste("[",OctopusClass[1:18],",",OctopusClass[2:19],"[ (um)",sep="")
    
    data.colnames<-c(paste("NP_",OctopusClass,sep=""),paste("MG_",OctopusClass,sep=""))
    
    
    data.colnames<-c("Nimages","UVP6_Temp",
                      data.colnames,paste("SD(",data.colnames,")",sep=""),paste("MEAN(",c(DepthName,data.colnames),")",sep=""))
    
    
    SensorType=109
    
  }
  
  ## uvp6_blk
  if (sensor == "uvp6_blk"){

    data.colnames<-c("uvp-blk_Internal_temp","uvp-blk_Count1","uvp-blk_Count2","uvp-blk_Count3","uvp-blk_Count4","uvp-blk_Count5")
    
    
    SensorType=110
    
  }
  
  
  
  ## End Of Sensors descriptino

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
    
    colnames(Dataclean)[3:7]<-c("Number Cycle","Number Pattern","Number Phase","Files","SensorType")
    
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
    for (i in (9:ncol(Dataclean))){
      Dataclean[,i]<-as.numeric(Dataclean[,i])
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

# Concat USEA data for a profile

#**************************************************

#' cts5_concatProfile : Concat .csv files to one data.frame
#'
#' @description
#' read NKE .csv ASCII files obtained from \code{\link{cts5_decode}} and 
#' concat them into one data.frame
#'
#' @param floatname hexa name of the float
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param sensors list of sensor to decode
#' @param dec decimal character in ASCII
#' 
#' @return data.frame containing the data
#' 
#' @details this function add an identification for the sensorType and acquisition phase 
#' as explained in \code{\link{cts5_readcsv}}
#' 
#' @examples 
#' dataMerged<-usea_concatProfile(floatname="ffff",CycleNumber=275)
#' 
#' @export
#'

cts5_concatProfile<-function(floatname="ffff",CycleNumber,PatternNumber=1,sensors=c("sbe41","do","eco","ocr","suna","sbeph","uvp6_lpm","uvp6_blk"),dec="."){
  
EnTeteCom<-c("Pressure [dbar]","Date","Number Cycle","Number Pattern","Number Phase","Files","SensorType","processing")

dataMerged<-cts5_readcsv(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber,sensor=sensors[1],dec=dec)  

if (length(sensors)>1){
  for (sensor in sensors[-1]){
    
    data<-try(cts5_readcsv(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber,sensor=sensor,dec=dec))
    
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
#' @param dataMerged is the merged data.frame obtained by \code{\link{cts5_concatProfile}}
#' @param sensors list of sensor to process
#' 
#' @return data.frame containing the data
#' 
#' @details calibration coefficients are usually in 00_technical.txt file.
#' 
#' @examples 
#' 
#' Meta<-cts5_readMetaSensor()
#' 
#' tech<-cts5_readtechnical(floatname=floatname,CycleNumber=c,PatternNumber = p)
#'
#' cts5_decode(floatname=floatname,CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' setwd("./CSV")
#'
#' dataMerged<-cts5_concatProfile(floatname=floatname,CycleNumber=c,PatternNumber = p)
#'
#' dataMerged<-cts5_ProcessData(Meta$SENSORS,dataMerged)
#'
#' PlotCTS5(login=login,dataMerged,PhaseToPlot=c("PRE","DES","PAR","ASC","SUR"),add=FALSE,technical=TRUE,paper = "A4",mfrow=c(3,2))
#'
#' SaveToCTS5(login = login,dataMerged = dataMerged,GPS = tech$GPS)
#' 
#' @export
#'
cts5_ProcessData<-function(metadata,dataMerged,sensor=c("eco","ocr","do","suna","sbeph")){
  
  ### ECO
  if ("eco" %in% sensor) {
    
    if (!is.null(metadata$SENSOR_ECO)){
    
      ## Raw or mean
      if ("chlorophyll_a, [CN]" %in% colnames(dataMerged)){
        dataMerged[,"chlorophyll_a, [ug/l]"]<-metadata$SENSOR_ECO$CHANNEL_01[1]*(dataMerged[,"chlorophyll_a, [CN]"]-metadata$SENSOR_ECO$CHANNEL_01[2])
        dataMerged[,"beta_theta, [1/m.sr]"]<-metadata$SENSOR_ECO$CHANNEL_02[1]*(dataMerged[,"beta_theta, [CN]"]-metadata$SENSOR_ECO$CHANNEL_02[2])
        dataMerged[,"colored_dissolved_organic_matter, [ppb]"]<-metadata$SENSOR_ECO$CHANNEL_03[1]*(dataMerged[,"colored_dissolved_organic_matter, [CN]"]-metadata$SENSOR_ECO$CHANNEL_03[2])
      }
      
      ## SD
      if ("SD(chlorophyll_a, [CN])" %in% colnames(dataMerged)){
        dataMerged[,"SD(chlorophyll_a, [ug/l])"]<-metadata$SENSOR_ECO$CHANNEL_01[1]*dataMerged[,"SD(chlorophyll_a, [CN])"]
        dataMerged[,"SD(beta_theta, [1/m.sr])"]<-metadata$SENSOR_ECO$CHANNEL_02[1]*dataMerged[,"SD(beta_theta, [CN])"]
        dataMerged[,"SD(colored_dissolved_organic_matter, [ppb])"]<-metadata$SENSOR_ECO$CHANNEL_03[1]*dataMerged[,"SD(colored_dissolved_organic_matter, [CN])"]
        
      }
    }
    else {
      cat("!! Warning : No ECO calibration found \n")
    }
    
  }
  
  ### OCR
  if ("ocr" %in% sensor) {
    if (!is.null(metadata$SENSOR_OCR) & ("Downwelling_irradiance_380nm, [CN]" %in% colnames(dataMerged))){
      dataMerged[,"Downwelling_irradiance_380nm"]<-metadata$SENSOR_OCR$CHANNEL_01[2]*metadata$SENSOR_OCR$CHANNEL_01[3]*
        (dataMerged[,"Downwelling_irradiance_380nm, [CN]"]-metadata$SENSOR_OCR$CHANNEL_01[1])
      
      dataMerged[,"Downwelling_irradiance_412nm"]<-metadata$SENSOR_OCR$CHANNEL_02[2]*metadata$SENSOR_OCR$CHANNEL_02[3]*
        (dataMerged[,"Downwelling_irradiance_412nm, [CN]"]-metadata$SENSOR_OCR$CHANNEL_02[1])
      
      dataMerged[,"Downwelling_irradiance_490nm"]<-metadata$SENSOR_OCR$CHANNEL_03[2]*metadata$SENSOR_OCR$CHANNEL_03[3]*
        (dataMerged[,"Downwelling_irradiance_490nm, [CN]"]-metadata$SENSOR_OCR$CHANNEL_03[1])
      
      dataMerged[,"Photosynthetic_Active_Radiation"]<-metadata$SENSOR_OCR$CHANNEL_04[2]*metadata$SENSOR_OCR$CHANNEL_04[3]*
        (dataMerged[,"Photosynthetic_Active_Radiation, [CN]"]-metadata$SENSOR_OCR$CHANNEL_04[1])
      
      }
  }
  
  ### DO
  if ("do" %in% sensor) {
    if (("c1phase_doxy [deg]" %in% colnames(dataMerged)) & ("Temperature [deg. C.]" %in% colnames(dataMerged))){
      
    if (is.list(metadata$SENSOR_DO)){
      coefs<-metadata$SENSOR_DO$SVU_FOIL_COEFF
    }
      else {
      coefs<-NULL
      }
        
    dataMerged[,"doxy_uncalibrated"]<-Process_DO_Bittig(C1phase=dataMerged[,"c1phase_doxy [deg]"],C2phase=dataMerged[,"c2phase_doxy [deg]"],temp=dataMerged[,"temp_doxy [deg. C.]"],Pres=dataMerged[,"Pressure [dbar]"],
                                             tempCTD=dataMerged[,"Temperature [deg. C.]"],salCTD=dataMerged[,"Salinity [PSU]"],PRESCTD=dataMerged[,"Pressure [dbar]"],
                                             COEF = coefs)
    }
  }
  
  ### sbepH
  if ("sbeph" %in% sensor) {
    if (("pH [mV]" %in% colnames(dataMerged)) & ("Temperature [deg. C.]" %in% colnames(dataMerged))){
      
      pH_Uncal<-rep(NA,dim(dataMerged)[1])
      
      ##DESCENT
      ind<-dataMerged$`Number Phase`=="DES"
      if (dim(dataMerged[ind,])[1] > 2){
        ## Il y a des donnees en descent
        pH_Uncal[ind & (dataMerged$SensorType==22)]<-Process_pH_SBE(dataMerged[ind,])
      }
      ##ASCENT
      ind<-dataMerged$`Number Phase`=="ASC"
      if (dim(dataMerged[ind,])[1] > 2){
        ## Il y a des donnees en Asc
        pH_Uncal[ind & (dataMerged$SensorType==22)]<-Process_pH_SBE(dataMerged[ind,])
      }
      
      dataMerged<-cbind(dataMerged,pH_Uncal)
      
    }
  }
  
  return(dataMerged)
  
}

#**************************************************
#' save data to CTS5 csv format
#'
#' @description
#' save data to CTS5 csv format
#'
#'
#' @param login login of the float used as prefix
#' @param dataMerged Merged data obtained by \code{\link{cts5_concatProfile}}
#' @param subdir sub directory where to save the data
#' @param GPS GPS data to be included in the csv. obtained by \code{\link{cts5_readtechnical}}
#' 
#' 
#' @export
#'
#SaveToCTS5 : Entete normalisee CTS5, sep="\t"
SaveToCTS5<-function(login,dataMerged,subdir=".",GPS=NULL){
  
  if (!is.null(dim(dataMerged))){  
    filename<-paste(subdir,"/",login,"_",formatC(unique(dataMerged[,"Number Cycle"]),width=3,flag="0"),"_",
                    formatC(unique(dataMerged[,"Number Pattern"]),width=2,flag="0"),".csv",sep="")
    cat("save data : ",filename,"\n")
    
    if (!is.null(GPS)){
      dataMerged<-cbind(dataMerged[,1:2],NA,NA,dataMerged[,-(1:2)])
      dataMerged<-rbind(dataMerged,NA)
      dataMerged[dim(dataMerged)[1],c(1,3:6,9)]<-c(0,GPS$`lat (deg)`,GPS$`lon (deg)`,as.numeric(dataMerged[1,5:6]),-1)
      
      ##temp
      dataMerged[,2]<-as.numeric(dataMerged[,2])
      dataMerged[dim(dataMerged)[1],2]<-as.numeric(GPS$time)
      dataMerged[,2]<-as.POSIXct(dataMerged[,2],origin = "1970-01-01",tz="UTC")
      
      colnames(dataMerged)[3:4]<-c("lat (deg)","lon (deg)")
      
    }
    
    write.table(dataMerged,filename,col.names=TRUE,row.names=FALSE,sep="\t",quote = FALSE)  
    
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
#' @param inifilename filename of the ini file
#' 
#' @return list containing the information
#' 
#' @export
#'
cts5_readIni<-function(inifilename){
  
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


