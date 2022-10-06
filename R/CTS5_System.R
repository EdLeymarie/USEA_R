# require("sp")
# require("rgdal")
# require("oce")
# options(stringsAsFactors = FALSE)

date.format="%d/%m/%y %H:%M:%S" ## Anciennement "%y-%m-%d %H:%M:%S"


##**************************************************
##*
##*   Fonctions Internes
##*
##**************************************************

##**************************************************
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
##**************************************************
#*** Input "UTC=21-04-13 09:39:05 Lat=3305.86498S Long=01352.03987E"
#** Ouput : Lat: -33.09775 , Lon: 13.86733 (Position dd.ddddd) 

ConvUTC<-function(str){

cat(str,"(Position ddmm.mmmm)","\n")
str<-strsplit(str,split="=")
Lat<-ConvDeg(strsplit(str[[1]][3],split=" ")[[1]][1])
Lon<-ConvDeg(strsplit(str[[1]][4],split=" ")[[1]][1])  

cat("Lat:",Lat,", Lon:",Lon,"(Position dd.ddddd)","\n")
  
}



##***************************************************
## Trouve le numero de cycle et pattern d'une date en fonction du log

FindCyclePattern<-function(date,CycleRecord){
  
if (!is.null(CycleRecord)){
  Cycle<-NULL
  Pattern<-NULL
  for (i in 1:length(date))  {
    
    #Cycle
    tempCycle<-NA
    tempPattern<-NA
    if (!is.na(date[i])){
      if (date[i]>=min(CycleRecord$date)){
        if (date[i]>max(CycleRecord$date)){
          tempCycle<-rev(CycleRecord$CycleNumber)[1]
          tempPattern<-rev(CycleRecord$PatternNumber)[1]
        }
        else {
          #ind<-which((date[i]>c(DateCycle[1:(length(DateCycle)-1)])) & (date[i]<=DateCycle[2:(length(DateCycle))]))
          ind<-which(date[i]>CycleRecord$date)
          tempCycle<-CycleRecord$CycleNumber[max(ind)]
          tempPattern<-CycleRecord$PatternNumber[max(ind)]
        }
      }
    }

    Cycle<-c(Cycle,tempCycle)
    Pattern<-c(Pattern,tempPattern)
    
  }
}
else {
  Cycle<-rep(NA,length(date))
  Pattern<-rep(NA,length(date))
}
return(list(Cycle=Cycle,Pattern=Pattern))
  
}

##**************************************************
InterpDepth<-function(date,NavData){
  depth<-approx(NavData$AllDepth$date,NavData$AllDepth$depth,xout=date,rule = 2,ties = "mean")$y
  return(depth)
}

##**************************************************
FindCycle<-function(date,NavData){
  cycle<-trunc(approx(NavData$AllDepth$date,NavData$AllDepth$CycleNumber,xout=date,rule = 2)$y)
  return(cycle)
}

##**************************************************
AddDateToNav<-function(date,NavData,offsettime=0){
  date<-as.POSIXct(date,origin = "1970-01-01",tz="UTC")
  
  date<-date-offsettime
  depth<-InterpDepth(date=date,NavData=NavData)
  points(date,-depth,col="green",pch=46)
}


##**************************************************
##*
##*   Fonctions Externes
##*
##**************************************************

#**************************************************
#' Use NKE routine to decode .hex system files
#'
#' @description
#' call APMTDecrypt.exe routine to decode .hex system files and create NKE .csv ASCII files.
#'
#' @param filename filename of the system file
#' @param floatname hexa name of the float
#' @param sysfile_number numeric : system file number
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param subdir not used
#' @param Nke_ProgPath path to the nke decoder (APMTDecrypt.exe). This path is stored to Sys.getenv("USEAR_Nke_ProgPath") !! Attention, on linux : required
#' the installation of wine
#' 
#' @return filename of the decoded file
#' 
#' @details this function must be call in the directory where are .hex files
#' 
#' This function will decode the system file in the following order of priority :
#' 1- filename if provided, 
#' 2- the system file with the sysfile_number if provided, 
#' 3- the system file recorded at the same time than the technical file specified by CycleNumber and PatternNumber
#' 
#' required the Nke_ProgPath for the first time or the parametrization of Sys.getenv("USEAR_Nke_ProgPath")
#' 
#' @examples 
#' decrypt_filename<-cts5_system_decode(floatname = "3ab3",sysfile_number=131,
#' Nke_ProgPath="my path to APMTDecrypt.exe")
#' 
#' Navdata<-cts5_system_parse(decrypt_filename)
#' 
#' cts5_system_plot(Navdata)
#' 
#' @export
#'
cts5_system_decode<-function(filename="",
                             floatname="ffff",sysfile_number=NULL,
                             CycleNumber=NULL,PatternNumber=1,
                             subdir=".",
                             Nke_ProgPath=""){
  
  #Positionnement dans le repertoire Decoder
  if (Nke_ProgPath == ""){
    if (Sys.getenv("USEAR_Nke_ProgPath") != ""){
      ProgDir=Sys.getenv("USEAR_Nke_ProgPath")
    }
    else {
      warning("Nke_ProgPath where APMTDecrypt.exe is must be defined. Provide Nke_ProgPath 
              or set Sys.getenv('USEAR_Nke_ProgPath')",immediate.=T)
    }
  }
  else {
    ProgDir=Nke_ProgPath
    Sys.setenv(USEAR_Nke_ProgPath=Nke_ProgPath)
  }
  
  
  
  #windows
  if (Sys.info()["sysname"] == "Windows"){
    ProgName="APMTDecrypt.exe"
    OSlabel=""
  }
  #Linux / MacOS
  else {
    ProgName="wine APMTDecrypt.exe"
    OSlabel=""
  }
  
  SysFilename<-""
  
  # If filename is provided, then it has priority.
  if (filename != ""){
    SysFilename<-filename
  }
  
  # If sysfile_number is provided then ..
  if ((SysFilename == "") & (!is.null(sysfile_number)) ){
    SysFilename<-paste(floatname,"_system_",formatC(sysfile_number,width=5,flag="0"),".hex",sep="")
  }
  
  # If CycleNumber is provided then ..
  if ((SysFilename == "") & (!is.null(CycleNumber)) ){
    #recherche du fichier technical le plus proche en temps
    pattern<-paste("^",floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical.txt",sep="")
    tech_filename<-list.files(pattern=pattern)[1]

    sysfilelist<-list.files(pattern = paste("^",floatname,"_system_.*.hex",sep=""))
    
    tech_minus_sys<-difftime(file.mtime(tech_filename),file.mtime(sysfilelist),units = "min")
    
    ind<-which.min(abs(tech_minus_sys))
    
    SysFilename<-sysfilelist[ind]
    
    cat("technical - system files time difference is (min) :",tech_minus_sys[ind],"\n")
    
  }
  
  # cat if several packet
  baseName<-strsplit(SysFilename,split = "\\.")[[1]][1]
  listCat<-list.files(pattern = paste(baseName,"#.*",sep=""))
  if (length(listCat)>0){
    datacat<-NULL
    for (tempfilename in listCat){
      datacat<-c(datacat,readBin(tempfilename,what="raw",n=25*1024))
    }
    
    cat("cat ",baseName," > ",SysFilename,"\n")
    
    writeBin(datacat,SysFilename)
    
  }
  
  #decodage
  if (file.exists(SysFilename)){
    
    # Decodage windows
    if (Sys.info()["sysname"] == "Windows"){
      
      dataDir<-getwd()
      
      cmd<-paste(dataDir,"/",SysFilename, " 0",sep="")
      
      ## decompression
      setwd(ProgDir)
      cat(paste(ProgName,cmd,sep=" "),"\n")
      system(paste(ProgName,cmd,sep=" "))
      
      setwd(dataDir)
    }
    # Decodage Linux
    else {
      
      dataDir<-getwd()
      
      #deplacement du fichier dans ProgDir
      file.copy(SysFilename, paste(ProgDir,"/",SysFilename, sep=""), overwrite = T,
                copy.mode = TRUE, copy.date = FALSE)
      
      cmd<-paste(SysFilename, " 0",sep="")
      
      ## decompression
      setwd(ProgDir)
      cat(paste(ProgName,cmd,sep=" "),"\n")
      system(paste(ProgName,cmd,sep=" "))
      
      ## copy
      SysFilename_decrypt<-paste(strsplit(SysFilename,split = "\\.")[[1]][1],"_decrypt.log",sep="")
      file.copy(SysFilename_decrypt, paste(dataDir,"/",SysFilename_decrypt, sep=""), overwrite = T,
                copy.mode = TRUE, copy.date = FALSE)
      file.remove(SysFilename)
      file.remove(SysFilename_decrypt)
      
      setwd(dataDir)
    }
    
    if (SysFilename != ""){
      SysFilename<-paste(strsplit(SysFilename,split = "\\.")[[1]][1],"_decrypt.log",sep="")
    }
    
    
  }
  else {
    cat("No file to decode \n")
    SysFilename<-""
  }
  

  
  return(SysFilename)
  
}

##**************************************************
#' Parse CTS5 system file.
#'
#' @description
#' parse the cts5 system file after decrypting
#'
#' @param filename filename of the decrypted system file (*.log)
#' @param userdefine Key word to look for in the system file
#' 
#' @return list containing the parsed system information
#' 
#' @examples
#' 
#' NavData<-cts5_system_parse("ffff_system_01755_decrypt.log",OS_windows_progpath="my path to APMTDecrypt.exe")
#' NavData<-cts5_system_parse("_system_decrypt.log",userdefine="Grounding detected")
#' 
#' sysfile<-cts5_system_decode(floatname="ffff",CycleNumber=15,PatternNumber = 1,OS_windows_progpath="my path to APMTDecrypt.exe")
#  NavData<-cts5_system_parse(sysfile)
#' 
#' decrypt_filename<-cts5_system_decode(floatname = "3ab3",sysfile_number=131,OS_windows_progpath="my path to APMTDecrypt.exe")
#' 
#' Navdata<-cts5_system_parse(decrypt_filename)
#' 
#' cts5_system_plot(Navdata)
#' 
#' @export
#'
cts5_system_parse<-function(filename="_system_decrypt.log",userdefine=""){

cat("open:",filename,"\n")
logdata<-scan(filename,what=character(0),sep="\n",encoding="latin1")    

sysFileNumber<-as.numeric(substr(strsplit(filename,split="_")[[1]][3],1,5))

#Cycle Pattern table
#detection new Pattern
indCycle<-grep(pattern = "NAV.*> Cycle N?",logdata)

#detection new Pattern
indPattern<-grep(pattern = "NAV.*> Pattern N?",logdata)

#identification with technical file FILE     > Creation of
indTech<-grep(pattern = "FILE.*> Creation of .*technical.txt",logdata)

#Association
CycleRecord<-NULL
DatePattern<-NULL

if (length(indPattern)>0){
  for (p in indPattern){
    DataPattern<-strsplit(logdata[p],split=" ")[[1]][c(1:2,13)]
    DatePattern<-c(DatePattern,paste(DataPattern[1:2],collapse=" "))
    
    DataCycle<-NA
    #do we have a cycle loaded before the pattern 
    if (length(indCycle[indCycle<p])>0){
      c<-max(indCycle[indCycle<p])
      DataCycle<-strsplit(logdata[c],split=" ")[[1]][13]
    }
    else {
      #if not, do we have a technical file 
      if (length(indTech[indTech>p])>0){
        c<-min(indTech[indTech>p])
        DataCycle<-strsplit(logdata[c],split="_")[[1]][2]
      }
    }
    
    CycleRecord<-rbind(CycleRecord,c(DataCycle,DataPattern[3]))
    
  }
  
  CycleRecord<-data.frame(strptime(DatePattern,format=date.format, tz="UTC"),as.numeric(CycleRecord[,1]),as.numeric(CycleRecord[,2]))
  
  names(CycleRecord)<-c("date","CycleNumber","PatternNumber")
  rownames(CycleRecord)<-NULL 
  
}



### Analyse Pe
#"CTRL.*> Pe2="
indPe<-grep(pattern = "CTRL.*> Pe2=",logdata)
cat("grep CTRL Pe:",length(indPe),"\n")

if (length(indPe)>0){
  # Elimine les lignes avec une vitesse (rares)
  # "11/05/20 22:30:46 : CTRL     > Pe2=9.7 dbar, V=-3.3 cm/s"
  temp<-logdata[indPe]
  for (i in grep(pattern=".*,",temp)){
    temp[i]<-strsplit(temp[i],split=",")[[1]][1]
  }
  DepthRecord<-matrix(unlist(strsplit(temp,split=" ")),ncol=11,byrow = TRUE)
  DepthRecord<-DepthRecord[,c(1,2,10)]
}
else {
  DepthRecord<-NULL
}

# "CHECK.*> Pe="

indPe<-grep(pattern = "CHECK.*> Pe=",logdata)
cat("grep CHECK Pe:",length(indPe),"\n")

if (length(indPe)>1){
  DepthRecord<-rbind(DepthRecord,matrix(unlist(strsplit(logdata[indPe],split=" ")),ncol=10,byrow = TRUE)[,c(1,2,9)])}

if (length(indPe)==1){
  DepthRecord<-rbind(DepthRecord,unlist(strsplit(logdata[indPe],split=" "))[c(1,2,9)])}

# "ICE.*> Perigee="

indPe<-grep(pattern = "ICE.*> Perigee=",logdata)
cat("grep ICE Perigee:",length(indPe),"\n")

if (length(indPe)>1){
  DepthRecord<-rbind(DepthRecord,matrix(unlist(strsplit(logdata[indPe],split=" ")),ncol=12,byrow = TRUE)[,c(1,2,11)])}

if (length(indPe)==1){
  DepthRecord<-rbind(DepthRecord,unlist(strsplit(logdata[indPe],split=" "))[c(1,2,11)])}

# Analyse

if (!is.null(DepthRecord)){
  if (dim(DepthRecord)[1]>1){
    date<-apply(DepthRecord[,1:2],1,paste,collapse=" ")}
  else {
    date<-paste(DepthRecord[,1:2], collapse=" ")
  }
  date<-strptime(date,format=date.format, tz="UTC")
  
  DepthRecord[,3]<-matrix(unlist(strsplit(DepthRecord[,3],split="=")),ncol=2,byrow = TRUE)[,2]
  #DepthRecord[,4]<-matrix(unlist(strsplit(DepthRecord[,4],split="=")),ncol=2,byrow = TRUE)[,2] #ancienne vitesse
  
  DepthRecord<-data.frame(date,as.numeric(DepthRecord[,3]))#,as.numeric(DepthRecord[,4]))
  
  CyclePattern<-FindCyclePattern(date,CycleRecord)
  
  DepthRecord<-cbind(CyclePattern$Cycle,CyclePattern$Pattern,DepthRecord)
  DepthRecord<-DepthRecord[order(date),]
  
  colnames(DepthRecord)<-c("CycleNumber","PatternNumber","date","depth")#,"Speed")
  rownames(DepthRecord)<-NULL
}

## EV
EVRecord<-NULL
indEV<-grep(pattern = "HYDRAU.*> EV=",logdata)
cat("grep EV:",length(indEV),"\n")

if (length(indEV)>0){
  
  EVRecord<-matrix(unlist(strsplit(logdata[indEV],split=" ")),ncol=12,byrow = TRUE)
  
  EVRecord<-EVRecord[,c(1,2,8,11)]
  
  if (length(indEV)>1){
    
    if (!is.vector(EVRecord)){
      date<-apply(EVRecord[,1:2],1,paste,collapse=" ")}
    else {
      date<-paste(EVRecord[1:2], collapse=" ")
    }
    
    date<-strptime(date,format=date.format, tz="UTC")
    
    EVRecord[,3]<-matrix(unlist(strsplit(EVRecord[,3],split="=")),ncol=2,byrow = TRUE)[,2]
    EVRecord<-data.frame(date,as.numeric(EVRecord[,4]),as.numeric(EVRecord[,3]))
  }
  else {
    date<-paste(EVRecord[1:2],collapse=" ")
    date<-strptime(date,format=date.format, tz="UTC")
    EVRecord[3]<-strsplit(EVRecord[3],split="=")[[1]][2]
    EVRecord<-data.frame(date,as.numeric(EVRecord[4]),as.numeric(EVRecord[3]))
  }
  
  
  
  CyclePattern<-FindCyclePattern(date,CycleRecord)
  
  EVRecord<-cbind(CyclePattern$Cycle,CyclePattern$Pattern,EVRecord)
  
  colnames(EVRecord)<-c("CycleNumber","PatternNumber","date","depth","EV")
  rownames(EVRecord)<-NULL
}
else {
  EVRecord<-NULL
}
## Pump
indPump<-grep(pattern = "HYDRAU.*> Pump=",logdata)

#supression des Pump=30000 cs"
if (length(grep("Pump=30000.* cs",logdata[indPump]))>0){
  indPump<-indPump[-grep("Pump=30000.* cs",logdata[indPump])]}
cat("grep Pump:",length(indPump),"\n")

if (length(indPump)>0){
  PumpRecord<-matrix(unlist(strsplit(logdata[indPump],split=" ")),ncol=12,byrow = TRUE)
  
  PumpRecord<-PumpRecord[,c(1,2,8,11)]
  
  if (!is.vector(PumpRecord)){
    date<-apply(PumpRecord[,1:2],1,paste,collapse=" ")
    date<-strptime(date,format=date.format, tz="UTC")
    PumpRecord[,3]<-matrix(unlist(strsplit(PumpRecord[,3],split="=")),ncol=2,byrow = TRUE)[,2]
    PumpRecord<-data.frame(date,as.numeric(PumpRecord[,4]),as.numeric(PumpRecord[,3]))}
  else {
    date<-paste(PumpRecord[1:2], collapse=" ")
    date<-strptime(date,format=date.format, tz="UTC")
    PumpRecord[3]<-matrix(unlist(strsplit(PumpRecord[3],split="=")),ncol=2,byrow = TRUE)[,2]
    PumpRecord<-data.frame(date,as.numeric(PumpRecord[4]),as.numeric(PumpRecord[3]))
  }
  
  
  
  
  
  
  CyclePattern<-FindCyclePattern(date,CycleRecord)
  
  PumpRecord<-cbind(CyclePattern$Cycle,CyclePattern$Pattern,PumpRecord)
  
  colnames(PumpRecord)<-c("CycleNumber","PatternNumber","date","depth","Pump")
  rownames(PumpRecord)<-NULL
  }
else {
  PumpRecord<-NULL
}
   
## Approx Depth
AllDepth<-rbind(DepthRecord[,c("CycleNumber","PatternNumber","date","depth")],
                EVRecord[,c("CycleNumber","PatternNumber","date","depth")],
                PumpRecord[,c("CycleNumber","PatternNumber","date","depth")])

if (!is.null(AllDepth)){
AllDepth<-AllDepth[order(AllDepth$date),]}

## Ice Brake
indAbort<-grep(pattern = ".*ICE.*Brake*",logdata)

BrakeRecord<-NULL
cat("grep Brake:",length(indAbort),"\n")

if (length(indAbort)>0){

  date<-strptime(substr(logdata[indAbort],0,17),format=date.format, tz="UTC")
  
  depth<-approx(as.numeric(AllDepth$date),AllDepth$depth,xout=as.numeric(date),ties = "mean")$y
  
  BrakeRecord<-data.frame(date,depth)
  
  CyclePattern<-FindCyclePattern(date,CycleRecord)
  
  BrakeRecord<-cbind(CyclePattern$Cycle,CyclePattern$Pattern,BrakeRecord)
  
  colnames(BrakeRecord)<-c("CycleNumber","PatternNumber","date","depth")
  rownames(BrakeRecord)<-NULL
}

## GPS
indGPS<-grep(pattern = "UTC=",logdata)

GPSRecord<-NULL
cat("grep UTC=:",length(indGPS),"\n")

if (length(indGPS)>0){
  dateStr<-NULL
  LatStr<-NULL
  LonStr<-NULL
  
  for (i in 1:length(indGPS)){
    dateStr<-c(dateStr,strsplit(logdata[indGPS[i]],split="=")[[1]][2])
    LatStr<-c(LatStr,strsplit(strsplit(logdata[indGPS[i]+1],split=" ")[[1]][11],split="=")[[1]][2])
    LonStr<-c(LonStr,strsplit(strsplit(logdata[indGPS[i]+1],split=" ")[[1]][12],split="=")[[1]][2])
  }
    
    GPSRecord<-cbind(dateStr,ConvDeg(LonStr),ConvDeg(LatStr))
    
    date<-strptime(GPSRecord[,1],format="%y/%m/%d %H:%M:%S", tz="UTC") 
    
    if (length(indGPS)>1){
      GPSRecord<-data.frame(date,GPSRecord[,2:3])}
    else {
      GPSRecord<-data.frame(date,GPSRecord)
      GPSRecord<-GPSRecord[-2]
    }
    
    CyclePattern<-FindCyclePattern(date,CycleRecord)
    
    GPSRecord<-cbind(CyclePattern$Cycle,CyclePattern$Pattern,GPSRecord)
    
    colnames(GPSRecord)<-c("CycleNumber","PatternNumber","date","Lon(deg)","Lat(deg)")
    rownames(GPSRecord)<-NULL
    
}

  
#User
UserRecord<-NULL
if (userdefine !=""){
indUser<-grep(pattern = userdefine,logdata)

cat("grep ",userdefine," : ",length(indUser),"\n")
  
  if (length(indUser)>0){
    temp<-strsplit(logdata[indUser],split=" ")
    date<-NULL
    msg<-NULL
    for (i in 1:length(temp)){
      date<-c(date,paste(temp[[i]][1:2],collapse=" "))
      msg<-c(msg,paste(temp[[i]][-(1:3)],collapse=" "))
    }
    
    date<-strptime(date,format=date.format, tz="UTC") 
    

    UserRecord<-data.frame(date,msg)

  }

}

return(list(filename=filename,sysFileNumber=sysFileNumber,DepthRecord=DepthRecord,EVRecord=EVRecord,PumpRecord=PumpRecord,AllDepth=AllDepth,BrakeRecord=BrakeRecord,GPSRecord=GPSRecord,CycleRecord=CycleRecord,UserRecord=UserRecord))
  
}

##**************************************************
#' Merge Parsed system file
#' 
#' @description
#' Merge Parsed system file from \code{\link{cts5_system_parse}}
#'
#' @param NavData1 parsed data from \code{\link{cts5_system_parse}}
#' @param NavData2 parsed data from \code{\link{cts5_system_parse}}
#' 
#' @return parsed data
#' 
#' @examples 
#' 
#' NavData<-cts5_system_parse("ffff_system_01755_decrypt.log")
#' NavData2<-cts5_system_parse("ffff_system_01756_decrypt.log")
#' 
#' Navdata<-cts5_system_merge(Navdata,Navdata2)
#' 
#' @export

cts5_system_merge<-function(Navdata1,Navdata2){

if (is.null(Navdata1)){
  Navdata1<-Navdata2
}  
else {
  if (length(Navdata1) == length(Navdata2)){
    for (i in 1:length(Navdata1)){
      if (is.null(Navdata1[[i]])){
        #Merge NULL
        if (!is.null(Navdata2[[i]])) {Navdata1[[i]]<-Navdata2[[i]]}
      }
      else {
        # Merge data Frame
        if (is.data.frame(Navdata1[[i]])){
          Navdata1[[i]]<-rbind(Navdata1[[i]],Navdata2[[i]])
        }
        # Merge vector
        if (is.vector(Navdata1[[i]])){
          Navdata1[[i]]<-c(Navdata1[[i]],Navdata2[[i]])
        }
      }
    }  
    
    
    ## update CyclePattern
    for (n in names(Navdata1)){
      if (all(c("CycleNumber","PatternNumber") %in% names(Navdata1[[n]]))){
        ind<-is.na(Navdata1[[n]]$CycleNumber)
        if (sum(ind)>0){
          temp<-FindCyclePattern(Navdata1[[n]]$date[ind],Navdata1$CycleRecord)
          
          Navdata1[[n]]$CycleNumber[ind]<-temp$Cycle
          Navdata1[[n]]$PatternNumber[ind]<-temp$Pattern
        }
      }
    }
    
    
  } 
  else {
    warning("lenght(Navdata1) != length(Navdata2)")
  }
}
  
return(Navdata1)

}

##**************************************************
#' decode, parse and Merge system file
#' 
#' @description
#' call \code{\link{cts5_system_decode}}, \code{\link{cts5_system_parse}} and \code{\link{cts5_system_merge}}.
#'
#' @param Navdata already processed system files
#' @param floatname hexa name of the float. If "" then name will be found automatically.
#' @param OnlyNew if true and if Navdata is not null, only system files older than files already
#' in Navdata will be processed
#' @param sysfile_number numeric : vector of system file numbers. If null, all files available
#' in the directory
#' @param subdir not used
#' @param userdefine Key word to look for in the system file
#' @param AutoSaveLoad If True, Navdata are saved and load automatically
#' @param Nke_ProgPath path to the nke decoder (APMTDecrypt.exe). This path is stored to Sys.getenv("USEAR_Nke_ProgPath") !! Attention, on linux : required
#' the installation of wine
#' 
#' @return list containing the parsed system information as from \code{\link{cts5_system_parse}}
#' 
#' @details this function must be call in the directory where are .hex files
#' 
#' @examples 
#' Navdata<-cts5_system_process(floatname = "3ab3")
#' Navdata<-cts5_system_process(floatname = "3ab3",userdefine="ICE")
#' Navdata<-cts5_system_process(userdefine="ICE")
#' Navdata<-cts5_system_process(userdefine="ICE",sysfile_number=125:130)
#' 
#' 
#' @export

cts5_system_process<-function(Navdata=NULL,floatname="",OnlyNew=T,
                              sysfile_number=NULL,
                              subdir=".",
                              userdefine="",
                              AutoSaveLoad=F,
                              Nke_ProgPath=""){
  
if (floatname==""){
  floatname<-findfloatname()
}

# Recherche des fichiers system
if (is.null(sysfile_number)){
  syslist<-list.files(pattern=paste(floatname,".*_system.*.hex",sep=""))
  sysfile_number<- matrix(unlist(strsplit(syslist,split="_")),ncol=3,byrow = T)[,3]
  sysfile_number<- unique(as.numeric(substr(sysfile_number,1,5)))
}
  
if (is.null(Navdata) & AutoSaveLoad){
  NavdataFilename<-paste(floatname,"_Navdata.RData",sep="")
  if (file.exists(NavdataFilename)){
    cat("load Navdata from ",NavdataFilename,"\n")
    load(NavdataFilename)
  }
}
  
# reduction de la liste
if (!is.null(Navdata)){
  #Minus already processed
  sysfile_number<-sysfile_number[!(sysfile_number %in% Navdata$sysFileNumber)]
  
  #Only files older than in the Navdata
  if (OnlyNew){
  sysfile_number<-sysfile_number[sysfile_number > max(Navdata$sysFileNumber)]
  }
}
  

# Process
if (length(sysfile_number)>0){
  for (i in 1:length(sysfile_number)){
    sysfile<-cts5_system_decode(floatname=floatname,sysfile_number = sysfile_number[i],Nke_ProgPath=Nke_ProgPath)
    Navdata2<-cts5_system_parse(sysfile,userdefine=userdefine)
    Navdata<-cts5_system_merge(Navdata1=Navdata,Navdata2=Navdata2)
  }
  cat(length(sysfile_number),"system files added","\n")
  
  #Auto Save
  if (!(is.null(Navdata)) & AutoSaveLoad){
    NavdataFilename<-paste(floatname,"_Navdata.RData",sep="")
    cat("Navdata saved to ",NavdataFilename,"\n")
    save(Navdata,file = NavdataFilename)
  }
}
else {
  cat("No system file to add","\n")
}
  
return(Navdata)
  
}

##**************************************************
#' Plot parsed system file
#'
#' @description
#' plot trajectory of the float from \code{\link{cts5_system_parse}}
#'
#' @param NavData pased data from \code{\link{cts5_system_parse}}
#' @param cycle list of cycle number to plot
#' @param pattern pattern to plot
#' @param xlim for plot function
#' @param ylim plot ylim
#' @param timeWin relative time window (if xlim=NULL) expressed as a vector of 2 floats. timeWin=c(0,1) is the full window.
#' @param relatif If True, time in min is relative to the first date 
#' @param timeRange vector of two dates that delimit the plotted data.
#' 
#' @return list containing the parsed system information
#' 
#' NavData<-cts5_system_parse("ffff_system_01755_decrypt.log")
#' NavData<-cts5_system_parse("_system_decrypt.log",userdefine="Grounding detected")
#' 
#' cts5_system_Plot(NavData)
#' cts5_system_Plot(NavData,cycle=5:10)
#' cts5_system_Plot(NavData,cycle=5,pattern=1)
#' 
#' ## To compare profiles
#' cts5_system_Plot(Navdata,cycle = 46,relatif = T,xlim=c(0,1500),show.cycle = F)
#' cts5_system_Plot(Navdata,cycle = 49,relatif = T,show.cycle = F,add=T)
#' 
#' ## To plot an Ice braking
#' Brakedate<-NavData$BrakeRecord$date[1]
#' cts5_system_Plot(NavData,timeRange=c(Brakedate-1800,Brakedate+1800))
#' 
#' @export
#'
cts5_system_Plot<-function(NavData,cycle=NULL,pattern=1:10,xlim=NULL,ylim=NULL,timeWin=c(0,1),relatif=FALSE,timeRange=NULL,
                           DepthPch=1,PumpPch=2,EVPch=3,Depthtype="b",show.EV=TRUE,show.pump=TRUE,show.title=F,show.cycle=T,
                           show.user=T,legendPos="",add=F){
  
if (is.null(timeRange)){
  if (is.null(cycle)){
    cycle=unique(NavData$DepthRecord[,"CycleNumber"])
  }
  if (is.null(pattern)){
    pattern<-unique(NavData$DepthRecord[,"PatternNumber"])
  }  
   
  
  #Depth
  ind<-(NavData$AllDepth[,"CycleNumber"] %in% cycle) & (NavData$AllDepth[,"PatternNumber"] %in% pattern)
  
  timeToPlot<-NavData$AllDepth$date[ind]
  
  # Echelle relative de temps
  RefTime<-0
  if (relatif){
    RefTime<-min(timeToPlot)
    timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
  }
}
else {
  ind<-(NavData$AllDepth$date > timeRange[1]) & (NavData$AllDepth$date < timeRange[2])
  timeToPlot<-NavData$AllDepth$date[ind]
}

if (is.null(xlim)){
  timerange<-range(timeToPlot)
  xlim<-timerange[1]+timeWin[1]*(timerange[2]-timerange[1])
  xlim[2]<-timerange[1]+timeWin[2]*(timerange[2]-timerange[1])
}

if (!add){
  #New graph
  plot(timeToPlot,-NavData$AllDepth$depth[ind],lty=1,type=Depthtype,pch=DepthPch,
       xlim=xlim,ylim=ylim,xlab="time",ylab="Depth")
}
else {
  #added
  points(timeToPlot,-NavData$AllDepth$depth[ind],lty=1,type=Depthtype,pch=DepthPch)
}

if (show.title){
    if (is.null(cycle)){
    title(main="All profils")
  }
  else {
    title(main=paste("Profils:",paste(patternTOPlot,collapse=", "),sep=" "))
  }
}

#EV
if (show.EV){
  # ind<-(NavData$EVRecord[,"CycleNumber"] %in% cycle) & (NavData$EVRecord[,"PatternNumber"] %in% pattern)
  # timeToPlot<-NavData$EVRecord$date[ind]
  # if (relatif){
  #   timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
  # }
  # 
  # points(timeToPlot,-NavData$EVRecord$depth[ind],pch=20,col="cyan")
  
  points(NavData$EVRecord$date,-NavData$EVRecord$depth,pch=EVPch,col="cyan")
}

#pump
if (show.pump){
  # ind<-(NavData$PumpRecord[,"CycleNumber"] %in% cycle) & (NavData$PumpRecord[,"PatternNumber"] %in% pattern)
  # 
  # timeToPlot<-NavData$PumpRecord$date[ind]
  # if (relatif){
  #   timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
  # }
  # points(timeToPlot,-NavData$PumpRecord$depth[ind],pch=18,col="red")
  points(NavData$PumpRecord$date,-NavData$PumpRecord$depth,pch=PumpPch,col="red")
}

#Abort
if (!is.null(NavData$BrakeRecord)){
  # ind<-(NavData$BrakeRecord[,"CycleNumber"] %in% cycle) & (NavData$BrakeRecord[,"PatternNumber"] %in% pattern)
  # 
  # timeToPlot<-NavData$BrakeRecord$date[ind]
  # if (relatif){
  #   timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
  # }
  # points(timeToPlot,-NavData$BrakeRecord$depth[ind],pch=19,col="orange")
  points(NavData$BrakeRecord$date,-NavData$BrakeRecord$depth,pch=5,col="purple")
}

# cycle
if (show.cycle){
  if (!is.null(NavData$CycleRecord)){
    for (i in which(NavData$CycleRecord[,2] %in% cycle)){
      timeToPlot<-NavData$CycleRecord[i,1]
      if (relatif){
        timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
      }
      lines(rep(timeToPlot,2),c(-2000,-15),pch=0,col="green")
      text(timeToPlot,-10,labels = NavData$CycleRecord[i,2],col="green")
    }   
  }
}

#show.user
if (show.user){
  if (!is.null(NavData$UserRecord)){
    y0<--10
    for (i in 1:dim(NavData$UserRecord)[1]){
      timeToPlot<-NavData$UserRecord[i,1]
      if (relatif){
        timeToPlot<-difftime(timeToPlot,RefTime,units = "min")
      }
      lines(rep(timeToPlot,2),c(-2000,-15),pch=0,col="red")
      text(timeToPlot,y0,labels = NavData$UserRecord[i,2],col="red",cex=0.8)
      y0<-y0-30
    }   
  }
}

if (legendPos != ""){
  legend(legendPos,pch=c(DepthPch,20,18,19),col=c("black","red","blue","orange"),legend=c("point","pump","EV","Retro"))
}
  
}


##**************************************************

# Sauvegarde des donnees navigation au format csv dans un fichier login_ccc_pp_trajectory.csv

cts5_SaveToTrajFile<-function(login,NavData,split.file=TRUE,show.PumpEV=TRUE){
Result<-cbind(NavData$DepthRecord[,1:4],0,0,NA,NA)
colnames(Result)[5:8]<-c("EV","Pump","Lon(deg)","Lat(deg)")

temp<-cbind(NavData$EVRecord[,1:4],1,0,NA,NA)
colnames(temp)[5:8]<-c("EV","Pump","Lon(deg)","Lat(deg)")
Result<-rbind(temp,Result)

temp<-cbind(NavData$PumpRecord[,1:4],0,1,NA,NA)
colnames(temp)[5:8]<-c("EV","Pump","Lon(deg)","Lat(deg)")
Result<-rbind(temp,Result)

temp<-cbind(NavData$GPSRecord[,1:3],0,0,0,NavData$GPSRecord[,4:5])
colnames(temp)[4:8]<-c("depth","EV","Pump","Lon(deg)","Lat(deg)")
Result<-rbind(temp,Result)

colnames(Result)[3]<-"date(UTC)"

# elimination des dates NA
Result<-Result[!is.na(Result$date),]

#tri
Result<-Result[order(Result$date),]

if (!show.PumpEV){
  Result<-Result[(Result$EV==0) & (Result$EV==0),c("CycleNumber","PatternNumber","date(UTC)","depth","Lon(deg)","Lat(deg)")]
}

#date en numeric
Date<-as.numeric(Result[,"date(UTC)"])
Result<-cbind(Result[,1:3],Date,Result[,-(1:3)])

#Correction des Cycle NA
# Result[Result$CycleNumber==0,"CycleNumber"]<-min(Result$CycleNumber,na.rm = T)


## save
if (split.file){
  profile.Id<-unique(Result[,1:2])
  for (i in 1:dim(profile.Id)[1]){
    temp<-Result[(Result[,1]==profile.Id[i,1]) & (Result[,2]==profile.Id[i,2]),]
    filename<-paste(login,formatC(profile.Id[i,1],width=3,flag="0"),formatC(profile.Id[i,2],width=2,flag="0"),"trajectory.csv",sep="_")
    cat("save:",filename,"\n")
    write.table(temp,file=filename,row.names = F,sep="\t",quote = F)
  }
}
else {
  filename<-paste(login,"trajectory.csv",sep="_")
  cat("save:",filename,"\n")
  write.table(Result,file=filename,row.names = F,sep="\t",quote = F)
}

}


#NavData<-analyseNavFromLog("ffff_system_01755_decrypt.log")
#NavData<-analyseNavFromLog("_system_decrypt.log",userdefine="Grounding detected")
#NavData<-analyseNavFromLog("24a0_system_00097_decrypt.log")
#save(NavData,file="NavData.Rdata")
#load("NavData.Rdata")
#analyseTimeatSurf(NavData$DepthRecord,targetdepth=50)
#PlotNav(NavData,cycle=115:127)
#PlotNav(NavData)
#ScanPositionsFromLog("_system_decrypt.log")
#write.table(NavData$DepthRecord,"DepthRecord.csv",col.names = TRUE,row.names = FALSE,sep="\t")
#SaveToTrajFile("lovuse001a-2",NavData=NavData,split.file = F)


#utilisation
#ScanPositions(path="C:/Users/Edouard/Documents/data-APMT-temp/lovapm006d",start=5)
#ScanPositions(path="C:/Users/Edouard/Documents/data-APMT-temp/lovapm009d",start=50,pattern=".*default.*")
#ScanPositions(path="D:/Data/Provor APMT/lovapm005a",pattern=".*default.*")
#ScanPositions(path="C:/Users/Edouard/Documents/data-APMT-temp/Amundsen2015/takapm003b",pattern=".*default.*.txt")
#ScanPositions(path=".",start=45,id="relativecycle")
#ScanPositions(path=".",pattern=".*default.*.txt",id="relativecycle")


# tcut<-hist(NavData$PumpRecord$CycleNumber,breaks=unique(NavData$PumpRecord$CycleNumber),plot = FALSE)
# plot(tcut$breaks[-1],tcut$counts,type="b",xlab="Cycle",ylab="Pump count")
# 
# tcut<-hist(NavData$EVRecord$CycleNumber,breaks=unique(NavData$EVRecord$CycleNumber),plot = FALSE)
# plot(tcut$breaks[-1],tcut$counts,type="b",xlab="Cycle",ylab="EV count")

