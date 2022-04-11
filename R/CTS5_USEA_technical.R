
require(XML)
require("sp")
require("rgdal")

# Conversion de degree min(decimal) vers degree (decimal)
ConvDeg<-function(str){
  if (length(str)==1){
    if (is.na(str)){
      pos<-NA}
    else {
      L<-substr(str,nchar(str),nchar(str))
      num<-as.numeric(substr(str,1,nchar(str)-1))/100
      deg<-trunc(num)
      min<-100*(num-deg)/60
      pos<-deg+min
      if ((L=="S") | (L=="W")) {pos<--pos}
    }
  }
  else {
    pos<-unlist(lapply(str,ConvDeg))
  }
  
  return(pos)
}


#**************************************************

## fonctions de split

#**************************************************

split_One_Key<-function(string){
  s<-strsplit(string,split="=")
  key<-s[[1]][1]
  s<-strsplit(s[[1]][2],split=" ")
  if (length(s[[1]])>1){
    key<-paste(key, " (",s[[1]][2],")",sep="")
  }
  
  return(key)
}

split_One_value<-function(string,value.as.numeric=T){
  s<-strsplit(string,split="=")
  
  s<-strsplit(s[[1]][2],split=" ")

  if (value.as.numeric){
    Value<-as.numeric(s[[1]][1])
  }
  else {
    Value<-s[[1]][1]
  }
  
  return(Value)
}



#**************************************************
#' read and parse CTS5 technical file.
#'
#' @description
#' read and parse CTS5 technical file (_tecnical.txt)
#'
#' @param filename file to open. If exist, will replace the automatic name provided by Cycle
#' and Pattern number.
#' @param floatname hexa name of the float
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' 
#' @return list containing the technical data
#' 
#' 
#' @export
#'


cts5_readtechnical<-function(filename="",floatname="",CycleNumber,PatternNumber=1){
  
  # nom du fichier
  if (filename==""){
    
    if (floatname==""){
      floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
    }
    
    pattern<-paste("^",floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical.txt",sep="")
    filename<-list.files(pattern=pattern)[1]
  }
  
  if (file.exists(filename)){
  
    cat("open:",filename,"\n")
    data<-scan(filename,sep="\n",what=character(0))
    
    #0: remove padding
    if (length(grep("\32",data))>0){
      data<-data[-grep("\32",data)]
    }
    
    ## 1: split [balise]
    ind<-grep("^\\[",data)
    ind<-c(ind,length(data)+1)
    
    technical<-list()
    technical$filename=filename
    
    technical$floatname=strsplit(filename,split="_")[[1]][1]
    
    for (i in 1:(length(ind)-1)){
      balisename<-substr(data[ind[i]],2,nchar(data[ind[i]])-1)
      technical[[balisename]]<-data[(ind[i]+1):(ind[i+1]-1)]
    }
    
    ## 2: Analyse
    
    ## USER
    if ("USER" %in% names(technical)){
      technical$USER<-as.list(technical$USER)
      for (i in 1:length(technical$USER)){
        s<-technical$USER[[i]][1]
        names(technical$USER)[i]<-split_One_Key(s)
        technical$USER[[i]]<-split_One_value(s,value.as.numeric = F)
      }
    }
    
    ## SYSTEM
    if ("SYSTEM" %in% names(technical)){
      technical$SYSTEM<-as.list(technical$SYSTEM)
      for (i in 1:length(technical$SYSTEM)){
        s<-technical$SYSTEM[[i]][1]
        names(technical$SYSTEM)[i]<-split_One_Key(s)
        technical$SYSTEM[[i]]<-split_One_value(s)
      }
    }
    
    ## GPS
    if ("GPS" %in% names(technical)){
      s<-technical$GPS
      time<-paste(strsplit(strsplit(s,split="=")[[1]][2],split=" ")[[1]][1:2],collapse = " ")
      time<-strptime(time,format = "%y-%m-%d %H:%M:%S",tz="UTC")
      technical$GPS<-list(time=time)
      technical$GPS[["lat (deg)"]]<-ConvDeg(strsplit(strsplit(s,split="=")[[1]][3],split=" ")[[1]][1])
      technical$GPS[["lon (deg)"]]<-ConvDeg(strsplit(strsplit(s,split="=")[[1]][4],split=" ")[[1]][1])
      technical$GPS[["Clock_drift"]]<-as.numeric(strsplit(strsplit(s,split="=")[[1]][5],split=" ")[[1]][1])
    }
    
    ## PROFILE
    if ("PROFILE" %in% names(technical)){
      technical$PROFILE<-as.list(technical$PROFILE)
      ## traitement
      for (i in 1:length(technical$PROFILE)){
        s<-technical$PROFILE[[i]][1]
       
        s1<-strsplit(s,split = "=")[[1]][2]
        s2<-paste(strsplit(s,split = "=")[[1]][-(1:2)],collapse = " ")
        
        #key
        time<-paste(strsplit(s1,split = " ")[[1]][1:2],collapse = " ")
        time<-strptime(time,format = "%y-%m-%d %H:%M:%S",tz="UTC")
        
        key<-paste(strsplit(s1,split = " ")[[1]][-(1:2)],collapse = " ")
        
        names(technical$PROFILE)[i]<-key
        
        technical$PROFILE[[i]]<-list(time=time)
        
        if (!is.na(s2)){
          technical$PROFILE[[i]]$value<-s2
        }
        
        # traitement specifique
        if (key %in% c("Flotation","Descent","Deep profile")){
          technical$PROFILE[[i]]$volume<-as.numeric(strsplit(s2,split=" ")[[1]][1])
          s3<-strsplit(s2,split=" ")[[1]][3]
          technical$PROFILE[[i]]$Nvalve<-as.numeric(substr(s3,2,nchar(s3)-1))
        }
        
        if (key == "First stabilization"){
          technical$PROFILE[[i]]$value<-as.numeric(strsplit(s2,split=" ")[[1]][1])
        }
        
        if (key %in% c("Park","Short Park")){
          s3<-strsplit(s2,split=" ")[[1]][1]
          technical$PROFILE[[i]]$MinDepth<-as.numeric(strsplit(s3,split="/")[[1]][1])
          technical$PROFILE[[i]]$MaxDepth<-as.numeric(strsplit(s3,split="/")[[1]][2])
          
          s3<-strsplit(s2,split=" ")[[1]][3]
          s3<-substr(s3,2,nchar(s3)-1)
          s3<-strsplit(s3,split="/")[[1]]
          technical$PROFILE[[i]]$Nvalve<-as.numeric(s3[1])
          technical$PROFILE[[i]]$Npump<-as.numeric(s3[2])
          
          
        }
        
        if (key %in% c("Ascent")){
          technical$PROFILE[[i]]$volume<-as.numeric(strsplit(s2,split=" ")[[1]][1])
          s3<-strsplit(s2,split=" ")[[1]][3]
          s3<-substr(s3,2,nchar(s3)-1)
          technical$PROFILE[[i]]$Npump<-as.numeric(strsplit(s3,split="/")[[1]][1])
          technical$PROFILE[[i]]$Npump_takeoff<-as.numeric(strsplit(s3,split="/")[[1]][2])
          technical$PROFILE[[i]]$from<-as.numeric(strsplit(s2,split=" ")[[1]][5])
        }
      }
    }
    
    ## DATA
    if ("DATA" %in% names(technical)){
      technical$DATA<-as.list(technical$DATA)
      ## traitement
      for (i in 1:length(technical$DATA)){
        s<-technical$DATA[[i]][1]
        
        key<-strsplit(s,split = "=")[[1]][1]
        s2<-strsplit(s,split = "=")[[1]][2]
        
        # pour la key Download, il peut y avoir plusieurs entres !
        if (!key %in% names(technical$DATA)){
          names(technical$DATA)[i]<-key
        }
        
        if (key == "Upload"){
          s3<-strsplit(s2,split=" ")[[1]]
          technical$DATA[[i]]<-list(volume=as.numeric(s3[1]))
          technical$DATA[[i]]$Nfiles<-as.numeric(s3[4])
          technical$DATA[[i]]$speed<-as.numeric(s3[7])
          technical$DATA[[i]]$Nsessions<-as.numeric(s3[10])
        }
        
        if (key == "Download"){
          
          if (!is.list(technical$DATA$Download)){
            technical$DATA$Download<-list()
          }
          
          if (sum(grep("command file",s2))>0){
            s3<-strsplit(s2,split=" ")[[1]]
            technical$DATA$Download$cmd_accepted=as.numeric(substr(s3[3],2,10))
            technical$DATA$Download$cmd_refused<-as.numeric(s3[5])
            technical$DATA$Download$cmd_unknown<-as.numeric(s3[7])
          }
          
          if (sum(grep("script file",s2))>0){
            s3<-strsplit(s2,split=" ")[[1]]
            technical$DATA$Download$script=1
          }
          
          
        }
        
        if (key == "Pattern"){
          technical$DATA[[i]]<-as.numeric(strsplit(s2,split=" ")[[1]][1])
        }
        
        if (!(key %in% c("Upload","Pattern","Download"))){
          technical$DATA[[i]]<-list(pts=as.numeric(strsplit(strsplit(s2,split=" ")[[1]][1],split="\\/")[[1]]))
          technical$DATA[[i]]$TotalPts=sum(technical$DATA[[i]]$pts)
        }
      }
      
      
      ## Elimination des elements sans nom
      technical$DATA[is.na(names(technical$DATA))]<-NULL
      
    }
    
    ## POWER
    if ("POWER" %in% names(technical)){
      technical$POWER<-as.list(technical$POWER)
      ## traitement
      for (i in 1:length(technical$POWER)){
        s<-technical$POWER[[i]][1]
        
        key<-split_One_Key(s)
        
        names(technical$POWER)[i]<-split_One_Key(s)
        
        if (!(key %in% c("EV/Pump (cs)"))){
          technical$POWER[[i]]<-split_One_value(s)
        }
        
        if (key %in% c("EV/Pump (cs)")){
          technical$POWER[[i]]<-as.numeric(strsplit(strsplit(strsplit(s,split="=")[[1]][2],split=" ")[[1]][1],split="\\/")[[1]])
        }
        
        
      }
    }
    
    
    ## Sensor
    SensorTag<-c("SENSOR_DO","SENSOR_OCR","SENSOR_ECO","SENSOR_SBEPH","SENSOR_SUNA","SENSOR_UVP6")
    for (j in 1:length(SensorTag)){
      itag<-grep(SensorTag[j],names(technical))[1]
      if (!is.na(itag)){
        technical[[itag]]<-as.list(technical[[itag]])
        
        ## traitement
        for (i in 1:length(technical[[itag]])){
          s<-technical[[itag]][[i]][1]
          
          key<-strsplit(s,split = "=")[[1]][1]
          s2<-strsplit(s,split = "=")[[1]][2]
          
          names(technical[[itag]])[i]<-key
          technical[[itag]][[i]]<-s2
          
          
          #traitement particuliers
          if (length(grep("Channel",key))==1){
            technical[[itag]][[i]]<-as.numeric(strsplit(s2,split="\\/")[[1]])
          }
          
          if (length(grep("Counters",key))==1){
            technical[[itag]][[i]]<-as.numeric(strsplit(s2,split="\\/")[[1]])
          }
          
          if (length(grep("Power supply",key))==1){
            technical[[itag]][[i]]<-as.numeric(strsplit(substr(s2,1,nchar(s2)-2),split="V\\/")[[1]])
          }
          
        }
      }
    }
  }
  else {
    cat("No file for:",pattern,"\n")
    technical<-NULL
  }
  
  return(technical)
}
      

#**************************************************
#' Compare the number of available data point
#'
#' @description
#' cts5_CheckDataCount compare the number of data point per sensor and per phase
#' between the technical file and the dataMerged file
#'
#' @param dataprofile data and technical files read from \code{\link{cts5_readProfile}}
#' 
#' @return list containing check (= True if the number of data point are the same);
#' and DataCheck a data.frame with the full comparison.
#' 
#' 
#' @examples 
#' cts5_decode(floatname=floatname,CycleNumber=c,PatternNumber = p,subdir="./CSV",sensors=c("sbe41","do","eco","ocr"))
#'
#' dataprofile<-cts5_readProfile(floatname=floatname,CycleNumber=c,PatternNumber = p)
#'     
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#'
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#' 
#' if (!cts5_CheckDataCount(dataprofile)$check){
#'  cat("!! Warning, data count error \n")
#' }
#' 
#' 
#' @export
#'


cts5_CheckDataCount<-function(dataprofile){

if (!is.null(dataprofile$technical) & (length(dataprofile$data)>0)){  
    
  DataCheck<-NULL
  DataInfile<-NULL
  
  sensorList<-names(dataprofile$technical$DATA)
  
  sensorList<-sensorList[!(sensorList %in% c("Upload","Pattern","Download"))]
    
  for (sensor in sensorList){ #sensor<-"SBE41"
    DataCount<-dataprofile$technical$DATA[[sensor]]$pts
    
    #On elimine le cas subsurface
    if (sensor == "SBE41"){
      DataCount[5]<-sum(DataCount[c(5,7)])
      DataCount<-DataCount[1:6]
    }
    
    #correspondance SensorType
    SensorType <- tolower(sensor)
    SensorType <- gsub("-","_",SensorType) #"UVP6-LPM" -> "uvp6_lpm"
    
    if (SensorType %in% names(dataprofile$data)){
    
      temp<-dataprofile$data[[SensorType]]
      
      temp<-c(nrow(temp[temp$PhaseName=="DES",]),
              nrow(temp[temp$PhaseName=="PAR",]),
              nrow(temp[temp$PhaseName=="DEE",]),
              nrow(temp[temp$PhaseName=="SHP",]),
              nrow(temp[temp$PhaseName=="ASC",]),
              nrow(temp[temp$PhaseName=="SUR",]))
    }
    else {
      temp<-rep(0,6)
    }
      
    
    DataLine<-DataCount-temp
    
    DataCheck<-rbind(DataCheck,DataLine)
    
    names(temp)<-paste(sensor,c("DES","PAR","DEE","SHP","ASC","SUR"),sep = "_")
    
    DataInfile<-c(DataInfile,temp)
      
    }
  
  DataCheck<-data.frame(DataCheck,stringsAsFactors = F)
  
  colnames(DataCheck)<-c("DES","PAR","DEE","SHP","ASC","SUR")
  rownames(DataCheck)<-sensorList
  
  DataInfile<-c(dataprofile$CycleNumber,dataprofile$PatternNumber,DataInfile)
  
  names(DataInfile)[1:2]<-c("Cycle_Number","Pattern_Number")
  
  return(list(check=sum(abs(DataCheck)) == 0,DataCheck=DataCheck,DataInfile=DataInfile))
}
else {
  
  return(list(check=FALSE,DataCheck=NULL,DataInfile=NULL))
  
  }
}
#**************************************************
#' read CTS5 Metadata file.
#'
#' @description
#' read CTS5 xml files with float and sensors meta data
#'
#' @param floatname hexa name of the float or .* to select all float name. If "" the floatname of the last file transmitted will be used
#' @param CycleNumber numeric : number of the cycle to decode. If NA, 
#' the last file is decoded
#' @param PatternNumber numeric : number of the Pattern to decode. If NA, 
#' the last file is decoded
#' @param filename if not null, used as meta file filename
#' 
#' @return list containing the Meta data
#' 
#' @examples 
#' 
#' Meta<-cts5_readMetaSensor(floatname="ffff",CycleNumber=118)
#' 
#' Meta<-cts5_readMetaSensor()
#' 
#' @export
#'


cts5_readMetaSensor<-function(floatname="",CycleNumber=NULL,PatternNumber=NULL,filename=NULL){
  
  if (is.null(filename)){
    
    # Automatic hexa floatname
    if (floatname==""){
      floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
    }  
    
    
    # CycleNumber
    if (is.null(CycleNumber)) {
      CycleNumber_tmp<-"[[:digit:]]{3}"
    }
    else {
      CycleNumber_tmp<-formatC(CycleNumber,width=3,flag="0")
    }
    
    # PatternNumber
    if (is.null(PatternNumber)) {
      PatternNumber_tmp<-"[[:digit:]]{2}"
    }
    else {
      PatternNumber_tmp<-formatC(PatternNumber,width=2,flag="0")
    }
    
    pattern<-paste("^",floatname,"_",CycleNumber_tmp,"_",PatternNumber_tmp,"_metadata.xml",sep="")  
    
    filename<-rev(list.files(pattern=pattern))[1]
  }
  
  if (file.exists(filename)){
    cat("Open:",filename,"\n")
    
    xml<-scan(filename,what = character(0))
    
    ##elimination padding
    ind<-grep("</FLOAT>",xml)
    xml<-xml[1:ind[1]]
    
    xml<-xmlParse(xml)
    L<-xmlToList(xml)
    
    #conversion en numeric
    if ("SENSOR_DO" %in% names(L$SENSORS)){
      if (length(L$SENSORS$SENSOR_DO)>1){      
        L$SENSORS$SENSOR_DO$PHASE_COEFF<-as.numeric(L$SENSORS$SENSOR_DO$PHASE_COEFF)
        L$SENSORS$SENSOR_DO$SVU_FOIL_COEFF<-as.numeric(L$SENSORS$SENSOR_DO$SVU_FOIL_COEFF)}
    }
    
    if ("SENSOR_ECO" %in% names(L$SENSORS)){
      if (length(names(L$SENSORS$SENSOR_ECO)) > 0){
        for (i in grep("CHANNEL",names(L$SENSORS$SENSOR_ECO))){
          L$SENSORS$SENSOR_ECO[[i]]<-as.numeric(L$SENSORS$SENSOR_ECO[[i]])
        }
      }
      else {
        # For Flbb with no calib in the data frame
        L$SENSORS$SENSOR_ECO<-NULL
      }
    }
    
    if ("SENSOR_OCR" %in% names(L$SENSORS)){
      if (length(names(L$SENSORS$SENSOR_OCR)) > 0){
        for (i in 2:length(names(L$SENSORS$SENSOR_OCR))){
          L$SENSORS$SENSOR_OCR[[i]]<-as.numeric(L$SENSORS$SENSOR_OCR[[i]])
        }
      }
      else {
        # For ocr with no calib in the data frame
        L$SENSORS$SENSOR_OCR<-NULL
      }
    }
    

  }
  else {
      cat("No xml file for ",pattern,"\n")
      L<-NULL
  }
      
  
return(L)
  
}

#**************************************************

read_list_splitfromstr<-function(list,key){
key_split<-strsplit(key,split="\\$")[[1]]

result<-list
for (i in 1:length(key_split)){
  result<-result[[key_split[i]]]
  
}

return(result)

}

#**************************************************
list_time_as_character<-function(list){
  for (i in 1:length(list)){
    
    if (inherits(list[[i]],what="POSIXlt")){
      list[[i]]<-as.character(list[[i]])
    }
    
    
    if (is.list(list[[i]])){
      list[[i]]<-list_time_as_character(list[[i]])
    }
    
    
  }
  
  return(list)
}


#**************************************************
#' All technical file in a data Frame
#'
#' @description
#' read all _technical.txt files and return results in a data.frame
#'
#' @param pattern pattern used to select files
#' @param filenames vector of files to read. exclude all other options 
#' @param CycleNumber vector of cycle number to read. If Null, all technical files are read.
#' @param include_tech0 If True, include 00_technical files
#' @param FromLastReset IF True, start from the last 00_technical files
#' 
#' @return a data.frame 
#' 
#' @examples 
#' 
#' ## Scan Alarms
#' tech<-cts5_readalltech()
#' cbind(tech$Cycle_Number,tech$Pattern_Number,tech[,grep("ALARM",colnames(tech))])
#' 
#' 
#' @export
#'

cts5_readalltech<-function(pattern=".*_technical.*.txt",filenames=NULL,CycleNumber=NULL,include_tech0=FALSE,FromLastReset=T){
  
  if (is.null(filenames)){
    filenames<-list.files(pattern=pattern)
    
    if (length(filenames)>1){
      filetab<-matrix(unlist(strsplit(filenames,split="_")),ncol=4,byrow = T)
      
      #We start from the last 00_technical
      if (FromLastReset){
        ind<-filetab[,3]=="00"
        if (sum(ind)>0){
          filetab<-filetab[max(which(ind)):length(ind),]
          filenames<-filenames[max(which(ind)):length(ind)]
        }
      }
      
      #remove 00_technical
      if ((!include_tech0) & (length(filenames)>1)){
        ind<-filetab[,3]=="00"
        if (sum(ind)>0){
          filetab<-filetab[!ind]
          filenames<-filenames[!ind]
        }
      }
      
      
      
    }
    
    if (!is.null(CycleNumber)){
      CycleV<-as.numeric(matrix(unlist(strsplit(filenames,split="_")),ncol=4,byrow = T)[,2])
      
      ind<-CycleV %in% CycleNumber
      
      filenames<-filenames[ind]
    }
  }
  
  result<-NULL
  
  if (length(filenames)>=1){
  
    for (filename in filenames){ #filename<-filenames[9]
      
      #cat("open:",filename,"\n")
      dataTech<-cts5_readtechnical(filename)
      
      dataTech<-list_time_as_character(dataTech)
      
      dataTech<-c(as.numeric(strsplit(filename,split="_")[[1]][2:3]),
                  unlist(dataTech))
      
      dataTech<-as.data.frame(t(dataTech),stringsAsFactors = F)
      
      names(dataTech)[1:2]<-c("Cycle_Number","Pattern_Number")
      
      #Forcage du nom Alarm a Alarm1
      ind<-names(dataTech) == "ALARM"
      if (sum(ind)==1){
        names(dataTech)[ind]<-"ALARM1"
      }
      
      if (is.null(result)){
        result<-dataTech
      }
      else {
        
        ### New list of names
        if (! all(names(dataTech) %in% names(result))){
          NewNames<-unique(c(names(result),names(dataTech)))
          
          #agrandissement de result
          temp<-matrix("",nrow=dim(result)[1],ncol=length(NewNames))
          colnames(temp)<-NewNames
          temp<-data.frame(temp,stringsAsFactors = F,check.names =F)
          
          temp[,names(temp) %in% names(result)]<-result[match(names(temp)[names(temp) %in% names(result)],names(result))]
          
          result<-temp
          
        }
      
        # Merge avec dataTech
        dataTechtoMerge<-rep("",times=length(result[1,]))
        names(dataTechtoMerge)<-names(result)
        dataTechtoMerge[names(dataTechtoMerge) %in% names(dataTech)] <- dataTech[match(names(dataTechtoMerge)[names(dataTechtoMerge) %in% names(dataTech)],names(dataTech))]
        result<-rbind(result,dataTechtoMerge)
          
  
          
      }
      
    }
    
    
    ######################### Format
    #ordre Alpha
    result<-result[order(names(result))]
    
    ## alarm at the end
    indAlarm<-grep("ALARM",names(result))
    if (length(indAlarm)>0){
      result<-cbind(result[-indAlarm],result[indAlarm])
    }
    
    ## Conversion in numeric
    toConvert<-c("GPS.lat (deg)","GPS.lon (deg)")
    
    for (label in toConvert){
      if (label %in% names(result)){
        result[[label]]<-as.numeric(result[[label]])
      }
    }
  }
  else {
    warning("no technical file")
  }
  

  
  
  return(result)
  
}

#**************************************************

##### KML

#**************************************************

#**************************************************
#' create a KML file from technical files
#'
#' @description
#' read  _technical.txt files and create a KML file to be read with Google Earth
#'
#' @param pattern pattern used to select files
#' @param filenamelist list of files to open
#' @param output name of the KML file
#' @param CycleToProcess vector of float cycle to include in the KML file. If Null, all cycle will be included
#' @param start First Cycle number to process. 
#'
#' @return a KML file
#' 
#' @examples  
#' 
#' # Automatic
#' cts5_create_kml()
#' 
#' # with filenamelist
#' tech<-cts5_readalltech()
#' cts5_create_kml(filenamelist=tech$filename)
#'  
#' 
#' @export
#'


cts5_create_kml<-function(pattern=".*technical.*.txt",filenamelist=NULL,output="PositionAPMT.kml",start=1,CycleToProcess=NULL,path=".",id="cycle"){
  setwd(path)
  
  if (is.null(filenamelist)) {
    filenamelist<-list.files(pattern=pattern)
    
    if (pattern==".*technical.*.txt"){
      vectnum<-as.numeric(matrix(unlist(strsplit(filenamelist,split="_")),ncol=4,byrow=TRUE)[,2]) 
      filenamelist<-filenamelist[vectnum>=start]
      
      if (!is.null(CycleToProcess)){
        vectnum<-as.numeric(matrix(unlist(strsplit(filenamelist,split="_")),ncol=4,byrow=TRUE)[,2]) 
        filenamelist<-filenamelist[vectnum %in% CycleToProcess]
      }
    }
  }
  
  
  datapoint<-data.frame()
  NCycle<-0
  
  #creation du data frame
  for (filename in filenamelist){
    cat("Open:",filename,"\n")
    NCycle<-NCycle+1
    data<-scan(filename,what=character(0),sep="\n")
    
    #0: remove padding
    if (length(grep("\32",data))>0){
      data<-data[-grep("\32",data)]
    }
    
    
    indGPS<-grep("\\[GPS\\]",data)
    if (length(indGPS)>0){
      indGPS<-indGPS+1
      str<-strsplit(data[indGPS],split="=")
      Lat<-ConvDeg(strsplit(str[[1]][3],split=" ")[[1]][1])
      Lon<-ConvDeg(strsplit(str[[1]][4],split=" ")[[1]][1])
      if (id=="date"){
        name<-paste(strsplit(str[[1]][2],split=" ")[[1]][1:2],collapse=" ")}
      if (id=="cycle") {
        name<-strsplit(filename,split="_")[[1]][2]}
      if (id=="relativecycle") {
        name<-NCycle}
      
      if (dim(datapoint)[1]==0){
        datapoint<-as.data.frame(t(c(Lon,Lat)))
        datapoint[,3]<-name
        datapoint[,4]<-filename
        datapoint[,5]<-paste(data,sep = "", collapse = "\n")
      }
      else {
        datapoint<-rbind(datapoint,c(Lon,Lat,name,filename,paste(data,sep = "", collapse = "\n")))
      }       
    }   
    else {
      datapoint<-rbind(datapoint,c(0,0,"NoGPS",filename,paste(data,sep = "", collapse = "\n")))
      
    }
  }
  
  #Creation du fichier KML
  if (output != ""){
    if (file.exists(output)){file.remove(output)}
    datapoint[,1]<-as.numeric(datapoint[,1])
    datapoint[,2]<-as.numeric(datapoint[,2])
    dimnames(datapoint)[[2]]<-c("Lon","Lat","name","filename","infos")
    
    # Utilisation de rgdal et sp
    # coordinates(datapoint)<-c("Lon","Lat")
    # proj4string(datapoint) <- CRS("+proj=longlat +datum=WGS84")
    # writeOGR(datapoint,output, layer="APMT",driver="KML")
    
    # kml in manual
    
    #header
    kmlf<-c('<kml xmlns="http://www.opengis.net/kml/2.2">',
            '<Document id="root_doc">',
            '<Schema name="APMT" id="APMT">',
	          '<SimpleField name="filename" type="string"></SimpleField>',
	          '<SimpleField name="infos" type="string"></SimpleField>',
            '</Schema>',
            '<Folder><name>APMT</name>')
    
    #Point
    for (i in 1:nrow(datapoint)){
      kmlf<-c(kmlf,
              '<Placemark>',
              paste('<name>',datapoint$name[i],'</name>',sep=""),
              '<ExtendedData><SchemaData schemaUrl="#APMT">',
              paste('<SimpleData name="filename">',datapoint$filename[i],'</SimpleData>',sep=""),
              paste('<SimpleData name="infos">',
                    datapoint$infos[i],'</SimpleData>',sep=""),
              '</SchemaData></ExtendedData>',
              paste('<Point><coordinates>',
                    datapoint$Lon[i],',',datapoint$Lat[i],
                    '</coordinates></Point>',sep=""),
              '</Placemark>')
    }
    
    #kml End
    kmlf<-c(kmlf,'</Folder>','</Document></kml>')
    
    
    #write
    write(kmlf,file=output)
    
  }
  
  #  return(datapoint)
  
}

#**************************************************
#' Plot Technical informations in one plot
#'
#' @description
#' Plot Technical informations in one plot
#'
#' @param tech technical data read from \code{\link{cts5_readalltech}}
#' @param output name of the pdf file
#' @param floatname name of the float to add on the plot
#' @param toplot list of information to plot
#' 
#' @return a data.frame 
#' 
#' @examples 
#' tech<-cts5_readalltech()
#' cts5_PlotTechnical(tech,output="Plot_technical.pdf",floatname=tech$floatname[1])
#' 
#' @export
#'

cts5_PlotTechnical<-function(tech,output="Plot_technical.pdf",floatname="",mfrow=c(4,3),
                   toplot=c("Date","Pi","Vbatt(V)","Volumes","HydroActions","EV/Pump","Depths","Data","Iridium","Commands","Power","ALARM")){
  
  cat("create:",output,"\n",sep="")
  pdf(file=output, paper="A4",width = 0, height = 0)
  par(mfrow=mfrow,xpd = TRUE)
  
  
  # Cycle / Date
  if ("Date" %in% toplot ){
    ind<-grep("PROFILE.Ascent end.time",colnames(tech))
    
    timeTemp<-tech[,ind]
    timeTemp[timeTemp==""]<-NA
    
    if (sum(ind)>0){
      plot(as.POSIXlt(timeTemp,tz="UTC"),1:dim(tech)[1],type="b",
           xlab="date",ylab="Profile",main = "Profile vs date")
      
      legend("topleft",legend=paste(c("First :","Last :"),range(as.POSIXlt(timeTemp,tz="UTC"))),
             lty=NULL,bty="n",cex=0.6)
      
    }
  }
  
  if ("Pi" %in% toplot ){
      ind<-grep("Pi \\(mbar",colnames(tech))
      if (sum(ind)>0){
        plot(1:dim(tech)[1],tech[,ind],xlab="profile",ylab=colnames(tech)[ind],main = colnames(tech)[ind],type="b")
      }
  }
  
  #"Vbatt(V)"
  if ("Vbatt(V)" %in% toplot ){
    ind<-grep("Vbatt",colnames(tech))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1,lty=1,xlab="profile",ylab="(V)",type="b")  
      title(main="Vbatt(V)")
      legend("topleft",inset = c(0, -0.1),legend=c("Vbatt(V)","Vbatt-peak-min(V)"),lty=1,col=1:2,cex=0.5,ncol=2,bty="n")
    }
  }
  
  mtext(paste("float:",floatname,", Process:",as.character(Sys.time())),side=3,line=-1,outer=T,cex=0.6,adj=0.95)
  
  #Volumes
  if ("Volumes" %in% toplot ){
    ind<-c(grep("Descent.volume",colnames(tech)),grep("Ascent.volume",colnames(tech)),
           grep("Flotation.volume",colnames(tech)))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1,xlab="profile",ylab="(cc)",type="b")  
      title(main="Volumes")
      legend("topleft",inset = c(0, -0.1),legend=c("Descent(cc)","Ascent(cc)","Flotation(cc)"),lty=1,col=1:3,cex=0.5,ncol=3,bty="n")
    }
  }
  
  #HydroActions
  if ("HydroActions" %in% toplot ){
    ind<-c(grep("Descent.Nvalve",colnames(tech)),grep("Ascent.Npump",colnames(tech)),
           grep("Park.Nvalve",colnames(tech)),grep("Park.Npump",colnames(tech)))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1,xlab="profile",ylab="(N)",type="b")  
      title(main=c("EV/Pump actions (N)"))
      legend("topleft",inset = c(0, -0.15),legend=c("Descent","Ascent","Ascent_takeoff","Park.Nvalve","Park.Npump"),lty=1,col=1:5,cex=0.5,ncol=3,bty="n")
    }
  }
  
  ## "EV/Pump"
  if ("EV/Pump" %in% toplot ){
    ind<-grep("POWER.EV",colnames(tech))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1,xlab="profile",ylab="time (cs)",type="b")  
      title(main=c("EV(cs), Pump(cs)"))
      legend("topleft",inset = c(0, -0.1),legend=c("EV(cs)","Pump(cs)"),lty=1,col=1:2,cex=0.5,ncol=2,bty="n")
    }
  }
  
  ## Depths
  if ("Depths" %in% toplot ){
    ind<-c(grep("PROFILE.Park.MinDepth",colnames(tech)),grep("PROFILE.Park.MaxDepth",colnames(tech)),
           grep("Ascent.from",colnames(tech)))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1,xlab="profile",ylab="Depth (dbar)",type="b")  
      title(main="Depths")
      legend("topleft",inset = c(0, -0.1),legend=c("Park Min depth","Park Maxdepth","Ascent.from"),lty=1,col=1:3,cex=0.5,ncol=3,bty="n")
    }
  }
  
  ## Data
  if ("Data" %in% toplot ){
    ind<-grep("DATA.*.TotalPts",colnames(tech))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],pch=1:4,xlab="profile",ylab="Points per Sensor",type="b")  
      sensornames<-colnames(tech)[ind]
      sensornames<-unlist(strsplit(sensornames,split="\\."))
      sensornames<-sensornames[!(sensornames %in% c("DATA","TotalPts"))]
      legend("topleft",inset = c(0, -0.15),legend=sensornames,pch=1:4,lty=1:5,col=1:6,ncol=4,cex=0.5,bty="n") 
    }
  }
  
  ## "Iridium"
  if ("Iridium" %in% toplot ){
    if (sum(grep("DATA.Upload",colnames(tech)))>0){
      plot(1:dim(tech)[1],tech[,"DATA.Upload.volume"],xlab="profile",ylab="Upload(ko)",type="b")
      par(new=TRUE)
      plot(1:dim(tech)[1],tech[,"DATA.Upload.speed"],type="b",axes=FALSE,col=4,xlab="",ylab="")
      axis(4,col=4,col.axis=4)
      title(main="Upload Ko and Ko/min")
      
      # plot(1:dim(tech)[1],tech[,"DATA.Upload.Nfiles"],xlab="profile",ylab="Upload(files)",type="b")
      # par(new=TRUE)
      # plot(1:dim(tech)[1],tech[,"DATA.Upload.Nsessions"],type="b",axes=FALSE,col=4,xlab="",ylab="")
      # axis(4,col=4,col.axis=4)
      # title(main="Upload Nbr Files and Iridium session")
    }
  }
  
  ## "Commands"
  if ("Commands" %in% toplot ){
    ind<-grep("DATA.Download",colnames(tech))
    if (sum(ind)>0){
      matplot(1:dim(tech)[1],tech[,ind],type="p",pch=16,xlab="profile",ylab="Count")
      legend("topleft",inset = c(0, -0.1),legend=c("Command_Accepted","Command_Refused","Command_Unknown","Script"),pch=16,col=1:4,ncol=2,cex=0.5,bty="n") 
      title(main="telecommands")
    }
  }
  
  ## "Power"
  if ("Power" %in% toplot ){
    ind<-c(grep("POWER.*min",colnames(tech)),grep("POWER.GPS",colnames(tech)))
    matplot(1:dim(tech)[1],tech[,ind],type="l",pch=1,xlab="profile",ylab="time (min/s)",log="y")
    legend("topleft",inset = c(0, -0.3),legend=colnames(tech)[ind],lty=1:5,col=1:6,ncol=2,cex=0.5,bty="n") 
    #title(main="Power")
  }
  
  ## "ALARM"
  if ("ALARM" %in% toplot ){
    alarm<-cbind(tech$Cycle_Number,tech$Pattern_Number,tech[,grep("ALARM",colnames(tech))])
    #delete (N)
    if (ncol(alarm)>2){
      alarmU<-NULL
      for (c in 3:ncol(alarm)){
        alarm[,c]<-unlist(lapply(alarm[,c],FUN=function(x){strsplit(x,split="\\(")[[1]][1]}))
        alarmU<-c(alarmU,alarm[,c])
      }
      
      alarmU<-unique(alarmU)
      alarmU<-alarmU[!is.na(alarmU)]
      plot(NULL,NULL,xlab="Profile",ylab="ALARM",xlim=c(1,dim(tech)[1]),ylim=c(0,ncol(alarm)-2),main="Alarms")
      for (c in 3:ncol(alarm)){
        points(1:dim(tech)[1],rep(c-2,dim(tech)[1]),pch=16,col=match(alarm[,c],alarmU))
      }
      legend("bottomleft",alarmU,col=1:length(alarmU),pch=16,cex=0.5,bty="n")
    }
  } 
  
  dev.off()
  
}

