
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
    }
    
    ## PROFILE
    if ("PROFILE" %in% names(technical)){
      technical$PROFILE<-as.list(technical$PROFILE)
      ## traitement
      for (i in 1:length(technical$PROFILE)){
        s<-technical$PROFILE[[i]][1]
       
        s1<-strsplit(s,split = "=")[[1]][2]
        s2<-strsplit(s,split = "=")[[1]][3]
        
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
          technical$PROFILE[[i]]$value<-as.numeric(strsplit(s2,split=" ")[[1]][1])
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
        }
        
        if (key %in% c("Ascent")){
          technical$PROFILE[[i]]$vol<-as.numeric(strsplit(s2,split=" ")[[1]][1])
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
        
        names(technical$DATA)[i]<-key
        
        if (key == "Upload"){
          s3<-strsplit(s2,split=" ")[[1]]
          technical$DATA[[i]]<-list(volume=as.numeric(s3[1]))
          technical$DATA[[i]]$Nfiles<-as.numeric(s3[4])
          technical$DATA[[i]]$speed<-as.numeric(s3[7])
          technical$DATA[[i]]$Nsessions<-as.numeric(s3[10])
        }
        
        if (key == "Download"){
          s3<-strsplit(s2,split=" ")[[1]]
          technical$DATA[[i]]<-list(cmd_accepted=as.numeric(substr(s3[3],2,10)))
          technical$DATA[[i]]$cmd_refused<-as.numeric(s3[5])
          technical$DATA[[i]]$cmd_unknown<-as.numeric(s3[7])
        }
        
        if (key == "Pattern"){
          technical$DATA[[i]]<-as.numeric(strsplit(s2,split=" ")[[1]][1])
        }
        
        if (!(key %in% c("Upload","Pattern","Download"))){
          technical$DATA[[i]]<-as.numeric(strsplit(strsplit(s2,split=" ")[[1]][1],split="\\/")[[1]])
        }
      }
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
    DataCount<-dataprofile$technical$DATA[[sensor]]
    
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
      
      temp<-c(nrow(temp[temp$`Number Phase`=="DES",]),
              nrow(temp[temp$`Number Phase`=="PAR",]),
              nrow(temp[temp$`Number Phase`=="DEE",]),
              nrow(temp[temp$`Number Phase`=="SHP",]),
              nrow(temp[temp$`Number Phase`=="ASC",]),
              nrow(temp[temp$`Number Phase`=="SUR",]))
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
#' @param floatname hexa name of the float or .* to select all float name
#' @param CycleNumber numeric : number of the cycle to decode. If NA, 
#' the last file is decoded
#' @param PatternNumber numeric : number of the Pattern to decode
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


cts5_readMetaSensor<-function(floatname=".*",CycleNumber=NA,PatternNumber=0,filename=NULL){
  
  if (is.null(filename)){
    if (is.na(CycleNumber)) {
      CycleNumber_tmp<-".*"
    }
    else {
      CycleNumber_tmp<-formatC(CycleNumber,width=3,flag="0")
    }
    
    PatternNumber_tmp<-formatC(PatternNumber,width=2,flag="0")
    
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
        for (i in 2:length(names(L$SENSORS$SENSOR_ECO))){
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
#' @param CycleNumber vector of cycle number to read. If Null, all technical files are read.
#' @param include_tech0 not used
#' 
#' @return a data.frame 
#' 
#' 
#' @export
#'

cts5_AllTech_inTab<-function(pattern=".*_technical.*.txt",CycleNumber=NULL,include_tech0=FALSE){
  
  filenames<-list.files(pattern=pattern)
  
  if (!is.null(CycleNumber)){
    CycleV<-as.numeric(matrix(unlist(strsplit(filenames,split="_")),ncol=4,byrow = T)[,2])
    
    ind<-CycleV %in% CycleNumber
    
    filenames<-filenames[ind]
  }
  
  result<-NULL
  
  if (length(filenames)>1){
  
    for (filename in filenames){ #filename<-filenames[9]
      
      #cat("open:",filename,"\n")
      dataTech<-cts5_readtechnical(filename)
      
      dataTech<-list_time_as_character(dataTech)
      
      dataTech<-c(filename,as.numeric(strsplit(filename,split="_")[[1]][2:3]),
                  unlist(dataTech))
      
      dataTech<-as.data.frame(t(dataTech),stringsAsFactors = F)
      
      names(dataTech)[1:3]<-c("filename","Cycle_Number","Pattern_Number")
      
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
#' @param output name of the KML file
#' @param CycleToProcess vector of float cycle to include in the KML file. If Null, all cycle will be included
#' @param start First Cycle number to process. 
#'
#' @return a KML file
#' 
#' @details 
#' 
#' CTS5_create_kml()
#'  
#' 
#' @export
#'


cts5_create_kml<-function(pattern=".*technical.*.txt",output="PositionAPMT.kml",start=1,CycleToProcess=NULL,path=".",id="cycle"){
  setwd(path)
  filenamelist<-list.files(pattern=pattern)
  
  if (pattern==".*technical.*.txt"){
    vectnum<-as.numeric(matrix(unlist(strsplit(filenamelist,split="_")),ncol=4,byrow=TRUE)[,2]) 
    filenamelist<-filenamelist[vectnum>=start]
    
    if (!is.null(CycleToProcess)){
      vectnum<-as.numeric(matrix(unlist(strsplit(filenamelist,split="_")),ncol=4,byrow=TRUE)[,2]) 
      filenamelist<-filenamelist[vectnum %in% CycleToProcess]
    }
  }
  
  
  datapoint<-data.frame()
  NCycle<-0
  
  #creation du data frame
  for (filename in filenamelist){
    cat("Open:",filename,"\n")
    NCycle<-NCycle+1
    data<-scan(filename,what=character(0),sep="\n")
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
    
    coordinates(datapoint)<-c("Lon","Lat")
    proj4string(datapoint) <- CRS("+proj=longlat +datum=WGS84")
    writeOGR(datapoint,output, layer="APMT",driver="KML")
  }
  
  #  return(datapoint)
  
}


