

#**************************************************

# Decod Energy

#**************************************************
#' Use NKE routine to estimate energy from technical file
#'
#' @description
#' call NKE routine to estimate energy from technical file
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param techfilename use to specified directly the name of the technical file
#' @param CycleNumber numeric : number of the cycle to decode, if techfilename=""
#' @param PatternNumber numeric : number of the Pattern to decode, if techfilename=""
#' @param metadata from \code{\link{cts5_readMetaSensor}} to get SDCard status
#' @param subdir subdir where to put .csv ASCII files
#' @param Nke_ProgPath path to the nke decoder (APMTEnergy.exe or ...). This path is stored to Sys.getenv("USEAR_Nke_ProgPath").
#' For Linux, if the Decoder path is in the .bashrc, you can leave this parameters empty. 
#' @param default_vol default emergence volume in cc. To be used if the inifile file is not found
#' @param default_TS default temperature at surface. To be used if sbe41 data are not found
#' @param default_SDCard default SDCard status (0 = not present). To be used if information is not found in metadata.
#' @param default_bat_self_discharge batterie self discharge in A / year
#' 
#' @return This function create ASCII files in the subdir directory and return a list : TechFilename, IniFilename, filename,
#' CycleNumber,PatternNumber, VolEmergence, TSurf, SDCard)
#' 
#' @details this function must be call in the directory where are technicals and ini files. RData files from \code{\link{cts5_readProfile}} 
#' with embeded ini file must be saved
#' 
#' 
#' @examples 
#' 
#' Meta<-cts5_readMetaSensor()
#' cts5_energy_decode(CycleNumber=c,PatternNumber = p,subdir="./ENY",metadata = Meta)
#' 
#' cts5_energy_decode(floatname = floatname,CycleNumber=c,PatternNumber=p,subdir="./ENY",metadata = Meta,
#' Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/")
#' 
#'
cts5_energy_decode<-function(floatname="",
                             techfilename="",
                             CycleNumber,
                             PatternNumber=1,
                             metadata=NULL,
                             subdir="./ENY",
                             Nke_ProgPath="",
                             default_vol=800,
                             default_TS=5,
                             default_SDCard=0,
                             default_bat_self_discharge=1.5){
  
  if (!techfilename ==""){
    s<-strsplit(techfilename,split = "_")[[1]]
    floatname<-s[1]
    CycleNumber<-as.numeric(s[2])
    PatternNumber<-as.numeric(s[3])
  }
  
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
    ProgName="APMTEnergy.exe"
    OSlabel=""
  }
  else {
    ProgName="Energy_x32"
    OSlabel=""
  }
  
  ## techfile
  TechFilename<-paste(floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical.txt",sep="")
  
  ## Inifile
  IniFilename<-cts5_readIni(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber,OnlyFilename = T)
        
  #decodage
  if (file.exists(TechFilename)){
    
    # Decodage windows
    if (Sys.info()["sysname"] == "Windows"){
      
      VolEmergence<-500 #default
      TSurf<-5 #default
      SDCard<-0 #default
      
      dataDir<-getwd()
      
      TechFilename_l<-paste(dataDir,"/",TechFilename,sep="")
      IniFilename_l<-paste(dataDir,"/",IniFilename,sep="")
      
      ## decompression
      setwd(ProgDir)
      cmd<-paste(ProgName,TechFilename_l,IniFilename_l,sep=" ")
      cat(cmd,"\n")
      system(cmd)
      
      setwd(dataDir)
    }
    
    # Decodage Linux / MacOS
    else {
      
      ## recherche RData
      Datafilename<-list.files(pattern = paste(".*","_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),".RData",sep=""),
                               recursive = TRUE)
      
      dataprofile<-NULL
      
      VolEmergence<-NA
      TSurf<-NA
      SDCard<-NA
      
      if (length(Datafilename)>0){
        cat("open:",Datafilename[1],"\n")
        load(Datafilename[1])
        
        #Emergence
        if (is.numeric(dataprofile$inifile$TECHNICAL$P19)){
          VolEmergence<-dataprofile$inifile$TECHNICAL$P19
        }
        
        #SDCard
        if ("SDCard" %in% names(metadata$HARDWARE$CONTROL_BOARD)){
          SDCard<-as.numeric(metadata$HARDWARE$CONTROL_BOARD["SDCard"]=="Installed")
        }
        
        #TSurf
        if ("sbe41" %in% names(dataprofile$data)){
          temp<-mean(dataprofile$data$sbe41$Temperature_degC[order(dataprofile$data$sbe41$Pressure_dbar)][1:5],na.rm = T)
          if (is.finite(temp)){
            TSurf<-temp
          }
        }
        
      }
      
      ## Default
      if (!is.finite(VolEmergence)){
        cat("Warning use default volume for emergence \n")
        VolEmergence<-default_vol
      }

      if (!is.finite(SDCard)){
        cat("Warning use default value for SDCard \n")
        SDCard<-default_SDCard
      }
      
      if (!is.finite(TSurf)){
        cat("Warning use default value for TSurface \n")
        TSurf<-default_TS
      }
      
      
      cmd<-paste(ProgDir,ProgName," ",TechFilename," ",VolEmergence," ",TSurf," ",SDCard," ",default_bat_self_discharge,sep="")
      cat(cmd,"\n")
      system(cmd)
      # 
      # #setwd(dataDir)
      
    }
    
    #Deplacement des fichiers
    if (subdir != "."){
      filename<-paste(floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical_energy",OSlabel,".csv",sep="")
      file.rename(from=filename,to=paste(subdir,filename,sep="/"))
    }
    
    return(list(TechFilename=TechFilename,IniFilename=IniFilename,filename=filename,
                CycleNumber=CycleNumber,PatternNumber=PatternNumber,
                VolEmergence=VolEmergence,TSurf=TSurf,SDCard=SDCard))
  }
  else {
    cat(TechFilename,", does not exists \n")
  }
}

#**************************************************

# read energy file

#**************************************************
cts5_energy_read<-function(filename){
  data<-read.table(filename,sep=",",header = T,stringsAsFactors = F)
  
  #On elimine la colonne usage
  data<-data[,-4]
  colnames(data)<-c("Task","Duration","Consumption")
  
  #Passage en vecteur
  result<-c(data[,2],data[,3])
  names(result)<-c(paste(data[,1],"Duration",sep="-"),paste(data[,1],"Consumption",sep="-"))

  return(result)
  
}

#**************************************************

# cts5_energy_process

#**************************************************
#' Use NKE routine to estimate energy from all technical files
#'
#' @description
#' call NKE routine to estimate energy from all technical files included in a directory
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param AutoLoad if true, the last estimation will be loaded and only new technical files will be processed
#' @param CycleNumber if not null, restricts the decoding to the list of cycles
#' @param subdir subdir where to put .csv ASCII files
#' @param FromLastReset if true, restricts the decoding from the last reset
#' @param Nke_ProgPath path to the nke decoder (APMTEnergy.exe). This path is stored to Sys.getenv("USEAR_Nke_ProgPath").
#' For Linux, if the Decoder path is in the .bashrc, you can leave this parameters empty. 
#' @param default_vol default emergence volume in cc. To be used if the inifile file is not found
#' @param default_TS default temperature at surface. To be used if sbe41 data are not found
#' @param default_SDCard default SDCard status (0 = not present). To be used if information is not found in metadata.
#' @param default_bat_self_discharge batterie self discharge in A / year
#' 
#' @return This function create an ASCII files in the subdir directory
#' 
#' @details this function must be call in the directory where are technical files
#' 
#' @examples 
#' 
#' Meta<-cts5_readMetaSensor()
#' 
#' cts5_energy_process(floatname="3ab0",Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/",metadata = Meta)
#' 
#' cts5_energy_plot(login,floatname,metadata = Meta)
#' 
#' 
#' @export
#'
cts5_energy_process<-function(floatname="",
                              subdir="./ENY",
                              AutoLoad=T,
                              FromLastReset=F,
                              CycleNumber=NULL,
                              metadata=NULL,
                              default_vol=800,
                              default_TS=5,
                              default_SDCard=0,
                              default_bat_self_discharge=1.5,
                              Nke_ProgPath=""){
  

# Automatic hexa floatname
if (floatname==""){
  floatname<-findfloatname()
}
  
result<-NULL
  
if (AutoLoad)  {
  #load old results
  enyfilename <- paste(subdir,"/",floatname,"energy.csv",sep = "")
  if (file.exists(enyfilename)){
    cat("open:",enyfilename,"\n")
    result<-read.table(file=enyfilename,sep=";",header = T,check.names = F,
                       stringsAsFactors = F,colClasses="character")
  }
}
  
## liste des technicals
  
filenames<-list.files(pattern=".*_technical.*.txt")

if (length(filenames)>1){
  filetab<-matrix(unlist(strsplit(filenames,split="_")),ncol=4,byrow = T)
  
  #We start from the last 00_technical
  if (FromLastReset){
    ind<-filetab[,3]=="00"
    if (sum(ind)>0){
      filetab<-filetab[max(which(ind)):length(ind),]
      filenames<-filenames[max(which(ind)):length(ind)]
    }
    cat("Warning : Starting From Last reset may underestimate the energy budget")
  }
  
  #remove 00_technical
  ind<-filetab[,3]=="00"
  if (sum(ind)>0){
    filetab<-filetab[!ind]
    filenames<-filenames[!ind]
    }
}

if (!is.null(CycleNumber)){
  CycleV<-as.numeric(matrix(unlist(strsplit(filenames,split="_")),ncol=4,byrow = T)[,2])
  ind<-CycleV %in% CycleNumber
  filenames<-filenames[ind]
}

## reduction filenames si result 
if (!is.null(result)){
  filenames<-filenames[!(filenames %in% result$techfilename)]
}

if (length(filenames)>=1){
  
  #result<-NULL #deplace au debut
  
  for (f in filenames){ #f<-filenames[1]
    
    ## decodage
    enyinfo<-cts5_energy_decode(techfilename=f,subdir=subdir,metadata=metadata,
                                default_vol=default_vol,default_TS=default_TS,
                                default_SDCard=default_SDCard,default_bat_self_discharge=default_bat_self_discharge,
                                Nke_ProgPath=Nke_ProgPath)
    
    ## read techfile
    techfile<-cts5_readtechnical(filename=f)
    
    ## read enyfile
    enyfile<-cts5_energy_read(paste(subdir,enyinfo$filename,sep = "/"))
    
    temp<-c(unlist(enyinfo),as.character(techfile$PROFILE$`Ascent end`$time),enyfile)
    names(temp)[1:9]<-c("techfilename","inifilename","enyfilename",
                           "CycleNumber","PatternNumber",
                        "VolEmergence","TSurf","SDCard","Ascent_end_Time")
    
    temp<-as.data.frame(t(temp),stringsAsFactors = F)
    
    if (is.null(result)){
      #Premier tour
      result<-temp
    }
    else {
      #tour suivant
      result<-merge(result,temp,all=T)
    }
  }
 
  cat("save:",paste(subdir,"/",floatname,"energy.csv",sep = ""),"\n") 
  write.table(result,file = paste(subdir,"/",floatname,"_energy.csv",sep = ""),row.names = F,sep=";",quote = F)
  
}
else {
  cat("no technical to add \n")
}

#return(result)
  
}


#**************************************************

# Plot Energy

#**************************************************
#' Plot energy estimation as pdf file
#'
#' @description
#' open csv file created by \code{\link{cts5_energy_process}} and plot data
#'
#' @param login identifiant used at the beginning of the pdf filename
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param subdir subdir where energy .csv ASCII files are stored from \code{\link{cts5_energy_process}}
#' @param metadata from \code{\link{cts5_readMetaSensor}} to get battery capacity
#' @param batteryInitialCapacity initial battery capacity [0-1]
#' @param fitLength nombre of profile taken into account to fit the remaining number of profile
#' 
#' @return a pdf file in the current directory and a list with various outputs.
#' 
#' @details this function must be call in the directory where are technical files
#' 
#' @examples 
#' Meta<-cts5_readMetaSensor()
#' 
#' cts5_energy_process(floatname="3ab0",Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/",metadata=Meta)
#' 
#' cts5_energy_plot(login,floatname,metadata=Meta)
#'
#' @export
#'

cts5_energy_plot<-function(login,floatname,subdir="./ENY",metadata=NULL,batteryInitialCapacity=0.90,fitLength=10){

# Automatic hexa floatname
if (floatname==""){
  floatname<-findfloatname(CycleNumber=CycleNumber,PatternNumber=PatternNumber)
}
  
enyData<-read.table(file = paste(subdir,"/",floatname,"_energy.csv",sep = ""),stringsAsFactors = F,sep=";",header = T)

file=paste(login,"energy.pdf",sep="_")

cat("open:",file,"\n")
pdf(file=paste(login,"energy.pdf",sep="_"),paper = "a4", width = 0, height = 0)
par(mfrow=c(3,2))

plot(enyData$PROFILE.Consumption,type="l",xlab = "profiles",ylab = "Energy used per profile (A.h)",main=login)

indConsumption<-grep("Consumption",names(enyData))[-1] #sauf Profile

matplot(enyData[,indConsumption],type="l",lty=1:5,col=1:6,xlab = "profiles",ylab = "energy (A.h)")
legend("topleft",legend=names(enyData)[indConsumption],lty=1:5,col=1:6,cex=0.5,bty="n")


#plotcumul
enyData$PROFILE.cumul.Consumption<-rep(NA,length(enyData$PROFILE.Consumption))
for (i in 1:length(enyData$PROFILE.Consumption)){
  enyData$PROFILE.cumul.Consumption[i]<-sum(enyData$PROFILE.Consumption[1:i])
}
plot(enyData$PROFILE.cumul.Consumption,type="l",xlab = "profiles",ylab = "energy cumul (A.h)")
title(main = list("!! CAUTION BETA VERSION !!",col=2))

IniBat<-NULL

## Battery
if (!is.null(metadata)){
  if (!is.null(metadata$HARDWARE$BATTERY$PACK_1)){
    Battemp<-metadata$HARDWARE$BATTERY$PACK_1[3]
    Battemp<-strsplit(Battemp,split = " ")[[1]][1]
    IniBat<-2*as.numeric(Battemp)
  }
}

if (!is.numeric(IniBat)){
  cat("Warning : Default battery capacity \n")
  IniBat<-260
}

IniBat<-batteryInitialCapacity*IniBat



par(new=TRUE)
plot(100*(IniBat-enyData$PROFILE.cumul.Consumption)/IniBat,type="l",axes=FALSE,col="blue",xlab="",ylab="")
axis(4,col="blue",col.axis="blue")

EnyAv<-mean(enyData$PROFILE.Consumption)

BatStatus=min(100*(IniBat-enyData$PROFILE.cumul.Consumption)/IniBat)

y<-100*(IniBat-enyData$PROFILE.cumul.Consumption)/IniBat
x<-1:length(y)

#Fit sur les fitLength derniers profils
if (length(y)>fitLength){

  y_last<-y[(length(y)-fitLength):length(y)]
  x_last<-x[(length(y)-fitLength):length(y)]
  
  l<-lm(y_last~x_last)
  
  l$coefficients
  
  xMax<-round(-l$coefficients[1]/l$coefficients[2])
  
  xfit<-min(x_last):xMax
  lines(xfit,l$coefficients[1]+l$coefficients[2]*xfit,lty=2,col="blue")



  
}
else {
  xMax<-round(IniBat/EnyAv)
}  

legend("topleft",legend=c(paste("Energy average (A.h / profile) =",formatC(EnyAv,digits = 2)),
                          paste("Battery status (%) = ",round(BatStatus)," from ",round(IniBat),"A.h"),
                          paste("Current profile = ",length(y)),
                          paste("Max Profile = ",xMax),
                          paste("remaining = ",xMax-length(y))),lty=NULL,bty="n",cex=0.8)


##4 Pie

SumByDevice<-apply(enyData[,indConsumption],2,sum,na.rm=T)
labels<-names(SumByDevice)
labels<-matrix(unlist(strsplit(labels,split="\\.")),ncol = 2,byrow = T)[,1]

SumByDevice<-100*SumByDevice/sum(SumByDevice)

labels<-paste(labels, " ",round(SumByDevice),"%",sep="")

pie(apply(enyData[,indConsumption],2,sum,na.rm=T),col=rainbow(length(indConsumption)),radius=0.75,
    labels = labels,cex=0.6)

#plot(NULL,NULL)
legend("topleft",legend=labels,ncol=5,bty="n",cex=0.6,text.col="blue")

dev.off()

return(list(IniBat=IniBat,EnyAv=EnyAv,BatStatuspercent=BatStatus,MaxProfile=xMax,remaining=xMax-length(y)))

}