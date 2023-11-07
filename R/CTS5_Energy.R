

#**************************************************

# Decod Energy

#**************************************************
#' Use NKE routine to estimate energy from technical file
#'
#' @description
#' call NKE routine to estimate energy from technical file
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param subdir subdir where to put .csv ASCII files
#' @param Nke_ProgPath path to the nke decoder (APMTEnergy.exe or ...). This path is stored to Sys.getenv("USEAR_Nke_ProgPath").
#' For Linux, if the Decoder path is in the .bashrc, you can leave this parameters empty. 
#' 
#' @return No return. This function create ASCII files in the subdir directory
#' 
#' @details this function must be call in the directory where are technicals and ini files
#' 
#' 
#' @examples 
#' cts5_energy_decode(CycleNumber=c,PatternNumber = p,subdir="./eny")
#' 
#' cts5_energy_decode(floatname = floatname,CycleNumber=c,PatternNumber=p,subdir="./eny",
#' Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/")
#' 
#'
cts5_energy_decode<-function(floatname="",techfilename="",CycleNumber,PatternNumber=1,subdir="./eny",Nke_ProgPath=""){
  
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
    ProgName="Decoder"
    OSlabel=".linux"
  }
  

        
  ## techfile
  TechFilename<-paste(floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical.txt",sep="")
        
  ## Inifile
  IniFilename<-cts5_readIni(floatname=floatname,CycleNumber=CycleNumber,PatternNumber=PatternNumber,OnlyFilename = T)
        
  #decodage
  if (file.exists(TechFilename)){
    
    # Decodage windows
    if (Sys.info()["sysname"] == "Windows"){
      
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
      
      # cmd<-paste(ProgDir,"Decoder ",SensorFilename,sep="")
      # cat(cmd,"\n")
      # system(cmd)
      # 
      # #setwd(dataDir)
      
    }
    
    #Deplacement des fichiers
    if (subdir != "."){
      filename<-paste(floatname,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),"_technical_energy",OSlabel,".csv",sep="")
      file.rename(from=filename,to=paste(subdir,filename,sep="/"))
    }
    
    return(c(TechFilename,IniFilename,filename,CycleNumber,PatternNumber))
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

# Decod Energy

#**************************************************
#' Use NKE routine to estimate energy from technical file
#'
#' @description
#' call NKE routine to estimate energy from technical file
#'
#' @param floatname hexa name of the float. If "", the floatname will automatically found.
#' @param CycleNumber numeric : number of the cycle to decode
#' @param PatternNumber numeric : number of the Pattern to decode
#' @param subdir subdir where to put .csv ASCII files
#' @param Nke_ProgPath path to the nke decoder (APMTDecrypt.exe or Decoder). This path is stored to Sys.getenv("USEAR_Nke_ProgPath").
#' For Linux, if the Decoder path is in the .bashrc, you can leave this parameters empty. 
#' 
#' @return No return. This function create ASCII files in the subdir directory
#' 
#' @details this function must be call in the directory where are .hex files
#' 
#' @examples 
#' cts5_energy_process(floatname="3ab0",Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/")
#' 
#' cts5_system_decode(floatname = floatname,CycleNumber=c,PatternNumber=p,
#' Nke_ProgPath="D:/Data/Provor_USEA/USEA_R/")
#' 
#'
cts5_energy_process<-function(Energydata=NULL,floatname="",subdir="./eny",
                              AutoSaveLoad=F,
                              FromLastReset=F,
                              CycleNumber=NULL,
                              Nke_ProgPath=""){
  
if (floatname==""){
  floatname<-findfloatname()
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

if (length(filenames)>=1){
  
  result<-NULL
  
  for (f in filenames){ #f<-filenames[1]
    
    ## decodage
    enyinfo<-cts5_energy_decode(techfilename=f,subdir=subdir,
                                Nke_ProgPath=Nke_ProgPath)
    
    ## read techfile
    techfile<-cts5_readtechnical(filename=f)
    
    ## read enyfile
    enyfile<-cts5_energy_read(paste(subdir,enyinfo[3],sep = "/"))
    
    temp<-c(enyinfo,as.character(techfile$PROFILE$`Ascent end`$time),enyfile)
    names(temp)[1:6]<-c("techfilename","inifilename","enyfilename",
                           "CycleNumber","PatternNumber","Ascent_end_Time")
    
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
  
  write.table(result,file = paste(subdir,"/",floatname,"energy.csv",sep = ""),row.names = F,sep=";",quote = F)
  
}

  
}


#**************************************************

# Plot Energy

#**************************************************
#*

cts5_energy_plot<-function(login,floatname){

enyData<-read.table(file = paste(subdir,"/",floatname,"energy.csv",sep = ""),stringsAsFactors = F)
enyData<-read.table(file = "3ab2energyold.csv",sep=";",stringsAsFactors = F,header = T)
login="tasbio001b"

enyData<-read.table(file = "3ab0energyold.csv",sep=" ",stringsAsFactors = F,header = T)
login="lovuse003b"

enyData<-read.table(file = "3aa9energy.csv",sep=";",stringsAsFactors = F,header = T)
login="lovuse005b"

pdf(file=paste(login,"energyold.pdf",sep="_"),paper = "a4", width = 0, height = 0)
par(mfrow=c(3,2))

plot(enyData$PROFILE.Consumption,type="l",xlab = "profiles",ylab = "Energy total",main=login)

indConsumption<-grep("Consumption",names(enyData))[-1] #sauf Profile

matplot(enyData[,indConsumption],type="l",lty=1:5,col=1:6,xlab = "profiles",ylab = "energy")
legend("topleft",legend=names(enyData)[indConsumption],lty=1:5,col=1:6,cex=0.5,bty="n")


#plotcumul
enyData$PROFILE.cumul.Consumption<-rep(NA,length(enyData$PROFILE.Consumption))
for (i in 1:length(enyData$PROFILE.Consumption)){
  enyData$PROFILE.cumul.Consumption[i]<-sum(enyData$PROFILE.Consumption[1:i])
}
plot(enyData$PROFILE.cumul.Consumption,type="l",xlab = "profiles",ylab = "energy cumul")

par(new=TRUE)
plot((250-enyData$PROFILE.cumul.Consumption)/2.5,type="l",axes=FALSE,col="blue",xlab="",ylab="")
axis(4,col="blue",col.axis="blue")

EnyAv<-mean(enyData$PROFILE.Consumption)

y<-(250-enyData$PROFILE.cumul.Consumption)/2.5
x<-1:length(y)

y_last<-y[(length(y)-15):length(y)]
x_last<-x[(length(y)-15):length(y)]

l<-lm(y_last~x_last)

l$coefficients

xMax<-round(-l$coefficients[1]/l$coefficients[2])

xfit<-min(x_last):xMax
lines(xfit,l$coefficients[1]+l$coefficients[2]*xfit,lty=2,col="blue")

legend("topleft",legend=c(paste("Energy average =",formatC(EnyAv,digits = 2)),
                          paste("Current profile = ",length(y)),
                          paste("Max Profile = ",xMax),
                          paste("remaining = ",xMax-length(y))),lty=NULL,bty="n",cex=0.8)


SumByDevice<-apply(enyData[,indConsumption],2,sum,na.rm=T)
labels<-names(SumByDevice)
labels<-matrix(unlist(strsplit(labels,split="\\.")),ncol = 2,byrow = T)[,1]

SumByDevice<-100*SumByDevice/sum(SumByDevice)

labels<-paste(labels, "(",round(SumByDevice),"%)",sep="")

pie(apply(enyData[,indConsumption],2,sum,na.rm=T),col=rainbow(length(indConsumption)),
    labels = labels)

dev.off()

}