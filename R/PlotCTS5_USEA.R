require(oce)
require(fields)

Sys.setenv(TZ = "utc") #pour imposer timezone = UTC
cat("WARNING: environment variable TZ (timezone is set to utc)\n")

## RunMedian Pour ISA
RunMedian<-function(x){
  result<-NULL
  for (i in 1:length(x)){
    result<-c(result,median(x[1:i]))}
  return(result)
}

## Find Depth Zone from initfile
FindZoneDepth<-function(inifile,sensorName){
  result<-NULL
  if (sensorName %in% names(inifile)){
    result<-unlist(inifile[[sensorName]][47:50])
  }
  
  return(result)
}

## plotDepthZones
plotDepthZones<-function(ZoneDepth,x=c(0,1E5),col="lightgrey",lty=2){
if (!is.null(ZoneDepth)){
  for (i in 1:length(ZoneDepth)){
    lines(x,c(-ZoneDepth[i],-ZoneDepth[i]),col=col,lty=lty)
  }
}
}

#**************************************************
plotCTD<-function(data,ylim=NULL,technical=TRUE,ZoneDepth=NULL){  
if (!is.null(data)){
  
  #Plot technical
  if (technical){
    
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("CTD",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
  }

#density
data<-cbind(data,swRho(data[,"Salinity_PSU"],data[,"Temperature_degC"],data[,"Pressure_dbar"]))
dimnames(data)[[2]][length(dimnames(data)[[2]])]<-"Density"
#swSigmaT
data<-cbind(data,swSigmaT(data[,"Salinity_PSU"], temperature=data[,"Temperature_degC"], pressure=data[,"Pressure_dbar"]))
dimnames(data)[[2]][length(dimnames(data)[[2]])]<-"swSigmaT"

  phaseToPlot<-c("DES","ASC")
  phaseToPlot<-phaseToPlot[phaseToPlot %in% unique(data[,"PhaseName"])]

  for (ph in phaseToPlot){        
    ind<-data[,"PhaseName"]==ph
    plot(data$swSigmaT[ind],-data[ind,"Pressure_dbar"],type="l",col=1,xlab="potential density anomaly",ylab="Depth",ylim=ylim)
    plotDepthZones(ZoneDepth)
    par(new=TRUE)
    plot(data[ind,"Salinity_PSU"],-data[ind,"Pressure_dbar"],type="l",axes=FALSE,col=4,xlab="",ylab="",ylim=ylim)
    axis(3,col=4,col.axis=4)
    par(new=TRUE)
    plot(data[ind,"Temperature_degC"],-data[ind,"Pressure_dbar"],type="l",axes=FALSE,col=2,xlab="",ylab="",ylim=ylim)
    axis(3,col=2,col.axis=2,line=2)
    par(new=FALSE)
    legend("bottomleft",legend=paste("CTD:",ph))
    
    ## ISA
    if ((ph=="ASC") & (dim(data)[1]>1)){
      #ISA Antarctique
      InterpT<-approx(data[,"Pressure_dbar",],data[,"Temperature_degC"],50:1,ties = "mean")
      RunM<-RunMedian(InterpT$y)
      ISA_Antarctique<-min(RunM[InterpT$x<=20],na.rm=TRUE)
      
      #ISA Baffin
      InterpT<-approx(data[,"Pressure_dbar",],data[,"Temperature_degC"],30:1,ties = "mean")
      RunM<-RunMedian(InterpT$y)
      ISA_Baffin<-min(RunM[InterpT$x<=10],na.rm=TRUE)
      
      #Legend
      if ((!is.na(ISA_Antarctique)) & (!is.na(ISA_Baffin))){
        if ((ISA_Antarctique<0.5)|(ISA_Baffin<0.5)) {
          l<-c(paste("ISA 50-20:",formatC(ISA_Antarctique,digit=3)),paste("ISA 30-10:",formatC(ISA_Baffin,digit=3)))
          legend("topright",legend=l,cex=0.75)
        }
      }
    }
    
    
    }
  }
}

#**************************************************
#Plot Eco Standard 

PlotEcoStd<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("EcoPuck",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta ECO",xlab="delta [db]",ylab="depth [db]")
    
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
      }
    
  }
  
  #Plot Chla
  Chla<-data[,"chlorophyll-a_ug/l"]
  if (sum(!is.na(Chla))>2){ 
    plot(NULL,NULL,xlim=range(Chla,na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="Chla [ug/l]",ylab="depth")
    for (i in unique(data[,"PhaseName"])){
      lines(Chla[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
    plotDepthZones(ZoneDepth)
  }
  
  #Plot BB
  bb<-data[,"beta-theta_1/msr"]
  if (sum(!is.na(bb))>2){
    plot(NULL,NULL,xlim=range(bb,na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="bb [1/m.sr]",ylab="depth")
    for (i in unique (data[,"PhaseName"])){
      lines(bb[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
    plotDepthZones(ZoneDepth)
  }
  
  #Plot CDOM
  if ("colored-dissolved-organic-matter_ppb" %in% colnames(data)){
    cdom<-data[,"colored-dissolved-organic-matter_ppb"]
    if (sum(!is.na(cdom))>2){
      plot(NULL,NULL,xlim=range(cdom,na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="CDOM [ppb]",ylab="depth")
      for (i in unique (data[,"PhaseName"])){
        lines(cdom[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
      }
      plotDepthZones(ZoneDepth)
    }
  }
}

#**************************************************
#Plot OCR4 
PlotOCR4<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("OCR",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){   
      depth<-data$Pressure_dbar[ind]
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta OCR",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
      }
  }
  
  #Plot Radio
  for (rad in c("Downwelling-irradiance-380nm","Downwelling-irradiance-412nm","Downwelling-irradiance-490nm","Photosynthetic-Active-Radiation")){
  
    temp<-data[,rad]
    #temp<-temp-min(temp,na.rm=TRUE) #normalisation au minimum
    #temp[temp<0]<-NA
    
    xlim=range(temp,na.rm = TRUE, finite = TRUE)
    if (xlim[2]<=0){xlim[2]<-1}  
    if (xlim[1]<=0){xlim[1]<-xlim[2]/50000}   
    plot(NULL,NULL,xlim=xlim,ylim=range(-data[!(data$PhaseName=="PAR"),"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab=rad,ylab="depth",log="x")
    for (i in unique(data[,"PhaseName"])){
      if (i != "PAR"){
        lines(temp[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
      }
    }
    plotDepthZones(ZoneDepth,x = xlim)
    
  }
  
}

#**************************************************
#Plot OCR507IR 
PlotOCR507IR<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("OCR",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){   
      depth<-data$Pressure_dbar[ind]
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta OCR",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }
  
  #Plot Radio
  colpal<-rev(rainbow(7))
  
  data<-data[data$PhaseName=="ASC",]
  data$PAR<-data$PAR/10
  
  ## Irradiance
  xlim=c(1E-3,max(data[,19:25],na.rm=TRUE))
  matplot(data[,19:25],-data$Pressure_dbar,log="x",type="l",col=c(colpal[2:7],1),lty=1,xlab="Irradiance",ylab="Depth"
          ,xlim=xlim)
  
  title(main="Irradiance Ascent")
  legend("bottomright",legend=c("IRR380","IRR412","IRR443","IRR490","IRR510","IRR560","IRR665","PAR/10"),lty=1,col=c(colpal,1),cex=0.75,bty="n")
  plotDepthZones(ZoneDepth,x = xlim)
  
  ## Radiance
  xlim=c(1E-5,max(data[,26:32],na.rm=TRUE))
  matplot(data[,26:32],-data$Pressure_dbar,log="x",type="l",col=colpal,lty=1,xlab="Irradiance",ylab="Depth"
          ,xlim=xlim)
  
  title(main="Radiance Ascent")
  legend("bottomright",legend=c("Rad380","Rad412","Rad443","Rad490","Rad510","Rad560","Rad665"),lty=1,col=colpal,cex=0.75,bty="n")
  plotDepthZones(ZoneDepth,x = xlim)
    
  
}

#**************************************************
#Plot Optode


###

PlotOptode<-function(data,technical=TRUE,ZoneDepth=NULL){

  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("DO",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta DO",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }    

  
  #PlotDO
  if (dim(data)[1]>5){
    plot(NULL,NULL,xlim=range(data[,"doxy_uncalibrated"],na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="doxy_uncalibrated",ylab="depth")
    for (i in unique (data[,"PhaseName"])){
      lines(data[data[,"PhaseName"]==i,"doxy_uncalibrated"],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
    plotDepthZones(ZoneDepth)
  }
  
}

#**************************************************
#Plot CRover
# data<-dataprofile$data$crover

###

PlotCROVER<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("cRover",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta CROVER",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }    
  
  
  #Plot c
  if (dim(data)[1]>5){
    
    # All time
    depth_breaks <- pretty(data$Pressure_dbar, n = 50)
    cs <- list(cols = tim.colors(length(depth_breaks)-1),breaks = depth_breaks,name = "depth",unit = "(db)",labels = seq(1,length(depth_breaks), 5))
    cols = tim.colors(length(depth_breaks)-1)[cut(data$Pressure_dbar,breaks = depth_breaks)]
    
    plot(x=data$Date,y=data$`c-uncalibrated_1/m`,type="b",pch=20,col=cols,xlab="Date",ylab="c-uncalibrated_1/m")
    cs.draw(cs,horiz=T,side = 1)
    
    
    # Profile
    ind<-data$PhaseName=="ASC"
    if (sum(ind)>1){
      plot(data$`c-uncalibrated_1/m`[ind],-data$Pressure_dbar[ind],type="l",
           xlab="c-uncalibrated_1/m",ylab="depth",col=2)
      plotDepthZones(ZoneDepth)
    }
    
  }
  
}

#**************************************************
#Plot SUNA


###

PlotSUNA<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("SUNA",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta SUNA",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }    
  
  
  #PlotSUNA
  if (dim(data)[1]>=5){
    
    plot(NULL,NULL,xlim=range(data[,"nitrate-concentration_uMol/l"],na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="nitrate-concentration_uMol/l",ylab="depth")
    for (i in unique (data[,"PhaseName"])){
      lines(data[data[,"PhaseName"]==i,"nitrate-concentration_uMol/l"],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
    plotDepthZones(ZoneDepth)
    
    
    ##Spectre
    if (technical){
      indSpec<-grep("OutSpectrum",colnames(data))
      temp<-data[,indSpec]
      temp<-temp[,temp[1,]>0]
      
      
      depth_breaks <- pretty(data$Pressure_dbar, n = 50)
      #cs <- list(cols = tim.colors(length(depth_breaks) - 1),breaks = depth_breaks,name = "",unit = "",labels = seq(1,length(depth_breaks), 5))
      #cols <- cs.use(depth_breaks, cs)
      cols = tim.colors(length(depth_breaks)-1)[cut(data$Pressure_dbar,breaks = depth_breaks)]
      
      matplot(t(temp),lty=1,type="l",xlab="pixel",ylab="Suna counts",col=cols) #
      
      #cs.draw(cs,horiz=T,width = 0.25,pos=1,side = 1)
    }
    
  }
  
  
  
  
}

#**************************************************
#Plot PlotSbepH

PlotSbepH<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("pH",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta pH",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }    
  
  
  #PlotpH
  if (dim(data)[1]>5){
    
    plot(NULL,NULL,xlim=range(data[,"pH_Uncal"],na.rm = TRUE, finite = TRUE),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="pH_uncalibrated",ylab="depth")
    for (i in unique (data[,"PhaseName"])){
      lines(data[data[,"PhaseName"]==i,"pH_Uncal"],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
    plotDepthZones(ZoneDepth)
    
  }
  
}

#**************************************************
#Plot lpm
# data<-dataprofile$data$uvp6_lpm
PlotUVP_lpm<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot Chronologie
  if (technical){
    plot(as.POSIXct(data[,"Date"],origin = "1970-01-01"),-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("UVP6",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta UVP",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }
  
  #### Class Size
  title_list<-c("UVP6 NPart_Class1-6","UVP6 NPart_Class7-12","UVP6 NPart_Class13-18")
  IndSize<-grep("NP_Size",colnames(data))
  class_list<-rbind(IndSize[1:6],IndSize[7:12],IndSize[13:18])
  
  for (i in 1:length(title_list)){
    temp<-data[,class_list[i,]]
    
    # ## correction Nimages or NSamples
    # Now in Processdata
    # if ("Nimages" %in% colnames(data)){
    #   # new taxo format
    #   temp<-temp/data$Nimages}
    # if ("NSamples" %in% colnames(data)){
    #   #old format without taxo
    #   NSamples<-data$NSamples
    #   NSamples[NSamples==0]<-1 #correction for NSamples=0
    #   temp<-temp/NSamples}
    
    if (sum(temp>0,na.rm=T) > 2){
      temp.min<-min(temp[temp>0],na.rm=T)
      temp.max<-max(temp,na.rm=T)
      
      plot(NULL,NULL,xlim=c(temp.min,temp.max),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="count",ylab="depth",log="x")
      title(main=title_list[i])
      for (j in 1:6){
        if (sum(temp[,j]>0,na.rm = T)>4){
          for (pn in unique(data[,"PhaseName"])){
            lines(temp[data[,"PhaseName"]==pn,j],-data[data[,"PhaseName"]==pn,"Pressure_dbar"],col=match(pn,unique(data[,"PhaseName"])),lty=j)
            }
        }
      }
      plotDepthZones(ZoneDepth,x = c(temp.min,temp.max))
      legend("bottomright",col=1,lty=1:6,legend=colnames(temp))
    }
  }
  
  #### Mean Grey
  title_list<-c("UVP6 MGrey_Class1-6","UVP6 MGrey_Class7-12","UVP6 MGrey_Class13-18")
  IndSize<-grep("MG_Size",colnames(data))
  class_list<-rbind(IndSize[1:6],IndSize[7:12],IndSize[13:18])
  
  for (i in 1:length(title_list)){
    temp<-data[,class_list[i,]]
    
    
    if (sum(temp>0,na.rm=T) > 2){
      temp.min<-min(temp[temp>0],na.rm=T)
      temp.max<-max(temp,na.rm=T)
      
      plot(NULL,NULL,xlim=c(temp.min,temp.max),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="count",ylab="depth",log="x")
      title(main=title_list[i])
      for (j in 1:6){
        if (sum(temp[,j]>0,na.rm = T)>4){
          for (pn in unique(data[,"PhaseName"])){
            lines(temp[data[,"PhaseName"]==pn,j],-data[data[,"PhaseName"]==pn,"Pressure_dbar"],col=match(pn,unique(data[,"PhaseName"])),lty=j)
          }
        }
      }
      plotDepthZones(ZoneDepth,x = c(temp.min,temp.max))
      legend("bottomright",col=1,lty=1:6,legend=colnames(temp))
    }
  }
  
  
}

#**************************************************
#Plot UVP_blk
PlotUVP_blk<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot Chronologie
  if (technical){
    plot(as.POSIXct(data[,"Date"],origin = "1970-01-01"),-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("UVP6 Black",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
  
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta UVP black",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }
  
  #### Class
    temp<-data[,c("uvp-blk_Count1","uvp-blk_Count2","uvp-blk_Count3","uvp-blk_Count4","uvp-blk_Count5")]
    if (sum(temp>0,na.rm=T) > 2){
      temp.min<-min(temp[temp>0],na.rm=T)
      temp.max<-max(temp,na.rm=T)
      
      plot(NULL,NULL,xlim=c(temp.min,temp.max),ylim=range(-data[,"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="count",ylab="depth",log="x")
      title(main="UVP black count")
      for (j in 1:5){
        if (sum(temp[,j]>0,na.rm=T)>4){
          for (i in unique (data[,"PhaseName"])){
            lines(temp[data[,"PhaseName"]==i,j],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])),lty=j)}
        }
      }
      plotDepthZones(ZoneDepth,x = c(temp.min,temp.max))
      legend("bottomright",col=1,lty=1:6,legend=colnames(temp))
    }
  
}

#**************************************************
#Plot UVP_txo

## Taxo Name
# Model_reference = Mglob_20220421
taxo_name<-c("Acantharia","Actinopterygii","Appendicularia","Aulacanthidae",
             "Calanoida","Chaetognatha","Collodaria","Creseis","Foraminifera",
             "Rhizaria","Salpida","artefact","crystal","detritus","fiber<detritus",
             "other<living","puff","small-bell<Hydrozoa","solitaryglobule","tuff")

PlotUVP_txo<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot Chronologie
  if (technical){
    plot(as.POSIXct(data[,"Date"],origin = "1970-01-01"),-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("UVP6 Taxo",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta UVP Taxo",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
  }
  
  ## ASCENT
  ## limitation Ascent
  dataASC<-data[data$PhaseName=="ASC",]
  
  #### Object Nbr
  indNbr<-grep("ObjectNbr",colnames(dataASC))
  ExistClass<-apply(dataASC[,indNbr],2,sum,na.rm=T) > 0
  
  Nclass<-sum(ExistClass)
  if (Nclass>0){
    cols<-rainbow(Nclass)
    matplot(dataASC[,indNbr][,ExistClass],-dataASC$Pressure_dbar,type="l",lty=1,xlab="ObjectNbr",ylab = "Depth",
            main="Ascent Object Number",log="x",col=cols)
    xlim<-dataASC[,indNbr][,ExistClass]
    xlim<-range(xlim[xlim>0])
    plotDepthZones(ZoneDepth,x=xlim)
    legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
  }
  else {
    cat("No ObjectNbr Class in UVP6 Taxo\n")
  }
  
  #### Class Size
  indSize<-grep("ObjectSize",colnames(dataASC))
  ExistClass<-apply(dataASC[,indSize],2,sum,na.rm=T) > 0
  
  Nclass<-sum(ExistClass)
  if (Nclass>0){
    cols<-rainbow(Nclass)
    matplot(dataASC[,indSize][,ExistClass],-dataASC$Pressure_dbar,type="l",lty=1,xlab="ObjectSize",ylab = "Depth",
            main="Ascent Object Size",col=cols)
    plotDepthZones(ZoneDepth)
    legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
  }
  else {
    cat("No Size Class in UVP6 Taxo\n")
  }
  
  #### ObjectGL
  indGL<-grep("ObjectGL",colnames(dataASC))
  ExistClass<-apply(dataASC[,indGL],2,sum,na.rm=T) > 0
  
  Nclass<-sum(ExistClass)
  if (Nclass>0){
    cols<-rainbow(Nclass)
    matplot(dataASC[,indGL][,ExistClass],-dataASC$Pressure_dbar,type="l",lty=1,xlab="ObjectGL",ylab = "Depth",
            main="Ascent Object Grey Level",col=cols)
    plotDepthZones(ZoneDepth)
    legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
  }
  else {
    cat("No GL Class in UVP6 Taxo\n")
  }
  
  ## Sum Ascent
  SumNbr<-apply(dataASC[,indNbr],2,sum,na.rm=T)
  SizeAv<-apply(dataASC[,indSize],2,mean,na.rm=T)
  GLAv<-apply(dataASC[,indGL],2,mean,na.rm=T)
  
  if (sum(SumNbr)>0){
    
    indPie<-SumNbr>0
    
    barplot(SumNbr[indPie],log="y",names.arg = taxo_name[indPie],col = rainbow(length(taxo_name)),
            ylab = "ASC integrated abundance (AU)",
            las=2,cex.names = 0.65)
  
    # xrange<-range(which(SumNbr>0))
    # 
    # yrange<-c(SumNbr,SizeAv,GLAv)
    # yrange<-range(yrange[yrange>0],finite = T)
    # 
    # plot(1:40,SumNbr,type="l",log="y",ylim=yrange,xlim=xrange,xlab="class",ylab="",main="Ascent")
    # lines(1:40,SizeAv,col=2)
    # lines(1:40,GLAv,col=3)
    # legend("bottomright",legend=c("Object Number sum","Object Size Av","Object GL Av"),lty=1,col=1:3,bty="n",cex=0.75)
    # 
  }
  
  ## PARKING
  ## limitation Parking
  dataPAR<-data[data$PhaseName=="PAR",]
  
  if (nrow(dataPAR) > 1){
    
    #### ObjectNbr
    indNbr<-grep("ObjectNbr",colnames(dataPAR))
    ExistClass<-apply(dataPAR[,indNbr],2,sum,na.rm=T) > 0
    
    Nclass<-sum(ExistClass)
    if (Nclass>0){
      cols<-rainbow(Nclass)
      matplot(as.numeric(dataPAR$Date),dataPAR[,indNbr][,ExistClass],type="l",lty=1,xlab="Time",ylab = "ObjectNbr",
              main="Parking Object Number", col=cols)
      legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
    }
    else {
      cat("No ObjectNbr Class in UVP6 Taxo at Parking \n")
    }
    
    
    #### Class Size
    indSize<-grep("ObjectSize",colnames(dataPAR))
    ExistClass<-apply(dataPAR[,indSize],2,sum,na.rm=T) > 0
    
    Nclass<-sum(ExistClass)
    if (Nclass>0){
      cols<-rainbow(Nclass)
      matplot(as.numeric(dataPAR$Date),dataPAR[,indSize][,ExistClass],type="l",lty=1,xlab="Time",ylab = "ObjectSize",
              main="Parking Object Size",col=cols)
      legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
    }
    else {
      cat("No Size Class in UVP6 Taxo at Parking \n")
    }
    
    #### Class Size
    indGL<-grep("ObjectGL",colnames(dataPAR))
    ExistClass<-apply(dataPAR[,indGL],2,sum,na.rm=T) > 0
    
    Nclass<-sum(ExistClass)
    if (Nclass>0){
      cols<-rainbow(Nclass)
      matplot(as.numeric(dataPAR$Date),dataPAR[,indGL][ExistClass],type="l",lty=1,xlab="Time",ylab = "ObjectGL",
              main="Parking Object Grey Level",col=cols)
      legend("bottomright",legend=taxo_name[ExistClass],lty=1,bty="y",col=cols)
    }
    else {
      cat("No GL Class in UVP6 Taxo at Parking\n")
    }
  }
  
}

#**************************************************
#Plot Ramses
# data<-dataprofile$data$ramses[dataprofile$data$ramses$PhaseName=="ASC",]
PlotRamses<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("Ramses",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){  
      depth<-data$Pressure_dbar[ind]
      
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta Ramses",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
    
    ## IntTime et Tilt
    ind<-which(data[,"PhaseName"] == "ASC")
    plot(data$ramses_int_time[ind],-data[ind,"Pressure_dbar"],type="l",col=1,xlab="Integration Time (ms)",ylab="Depth",main="Tilt and Integration Time")
    par(new=TRUE)
    plot(apply(cbind(data$ramses_tilt1[ind],data$ramses_tilt2[ind]),1,mean),-data[ind,"Pressure_dbar"],type="l",axes=FALSE,col=4,xlab="",ylab="")
    axis(3,col=4,col.axis=4)
    plotDepthZones(ZoneDepth)
  }    
  
  ## Data
  data<-data[data$PhaseName=="ASC",]
  
  depth_breaks <- pretty(data$Pressure_dbar, n = 50)
  cs <- list(cols = tim.colors(length(depth_breaks)-1),breaks = depth_breaks,name = "depth",unit = "(db)",labels = seq(1,length(depth_breaks), 5))
  #cols <- cs.use(depth_breaks, cs)
  cols = tim.colors(length(depth_breaks)-1)[cut(data$Pressure_dbar,breaks = depth_breaks)]
  
  #breaks <- pretty(data$Pressure_dbar, n = 50)
  #cols <- cm.colors(length(breaks)-1)[cut(data$Pressure_dbar,breaks = breaks)]
  #cols <- rainbow(length(breaks)-1)[cut(data$Pressure_dbar,breaks = breaks)]
  
  # indraw<-grep("ramses_raw_count",colnames(data))
  # if (length(indraw)>0){
  #   matplot(1:length(indraw),t(data[,indraw]),lty=1,pch=0,type="l",xlab="pixel",ylab = "Ramses Irradiance Count",col=cols)
  # }
  
  indsig<-grep("ramses_sig",colnames(data))
  if (length(indsig)>0){
    waves<-colnames(data)[indsig]
    waves<-as.numeric(matrix(unlist(strsplit(waves,split="_")),ncol=3,byrow = T)[,3])
    
    matplot(waves,t(data[,indsig]),lty=1,pch=0,type="l",xlab="wavelegnth (nm)",ylab = "Ramses Irradiance physical",col=cols,log="y")
    
    cs.draw(cs,horiz=T,width =  max(t(data[,indsig]),na.rm = T)/2,pos= max(t(data[,indsig]),na.rm = T),side = 1)

  
    ## profil
    wavelist<-seq(320,800,by=10)
    wavelist<-wavelist[(wavelist>min(waves)) & (wavelist<max(waves))]
    colpal<-rev(rainbow(length(wavelist),end=0.8))
    
    
    datatemp<-data[,indsig]
    temp<-NULL
    for (i in 1:length(wavelist)){
      temp<-cbind(temp,apply(datatemp[,abs(waves-wavelist[i])<10],1,mean))
    }
    matplot(temp,-data$Pressure_dbar,lty=1,type="l",log="x",col=colpal,xlab="Ramses Irradiance physical",ylab="Depth")
    temp<-as.vector(temp)
    plotDepthZones(ZoneDepth,x = c(min(temp[temp>0]),max(temp)))
    
    ind<-seq(from=1,to=length(wavelist),by=5)
    if (max(ind) < length(wavelist)){ind<-c(ind,length(wavelist))}
    legend("bottomright",legend=wavelist[ind],col=colpal[ind],lty=1)

    
  }
}

#**************************************************
#Plot mpe 
PlotMPE<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main=paste("MPE",rev(data$date)[1],sep=" "))
    ind<-which(data[,"PhaseName"] %in% c("PRE","DES"))
    rangedescent<-range(data[ind,"Pressure_dbar"])
    ind<-which(data[,"PhaseName"]=="ASC")
    rangeascent<-range(data[ind,"Pressure_dbar"])
    legend("bottomleft",legend=c(paste("Descent [",paste(format(rangedescent,digit=2),collapse=" - "),"]",sep=""),paste("Ascent [",paste(format(rangeascent,digit=2),collapse=" - "),"]",sep="")))
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    #Plot Ecart
    ind<-which(data[,"PhaseName"] == "ASC")
    if (length(ind)>2){   
      depth<-data$Pressure_dbar[ind]
      delta<-depth[-length(depth)]-depth[-1]
      plot(delta,-depth[-1],log="x",main="delta MPE",xlab="delta [db]",ylab="depth [db]")
      plotDepthZones(ZoneDepth,x = c(min(delta[delta>0]),max(delta)))
      
    }
    
    #Plot Temp
    Sig<-data$Temperature
    
    xlim=range(Sig,na.rm = TRUE, finite = TRUE)
    if (xlim[2]<=0){xlim[2]<-1}  
    if (xlim[1]<=0){xlim[1]<-xlim[2]/50000}   
    plot(NULL,NULL,xlim=xlim,ylim=range(-data[!(data$PhaseName=="PAR"),"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="MPE Temperature",ylab="depth")
    for (i in unique(data[,"PhaseName"])){
      if (i != "PAR"){
        lines(Sig[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
      }
    }
    plotDepthZones(ZoneDepth,x = xlim)
    
    #Plot Voltage
    Sig<-data$Voltage
    
    xlim=range(Sig,na.rm = TRUE, finite = TRUE)
    if (xlim[2]<=0){xlim[2]<-1}  
    if (xlim[1]<=0){xlim[1]<-xlim[2]/50000}   
    plot(NULL,NULL,xlim=xlim,ylim=range(-data[!(data$PhaseName=="PAR"),"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="MPE Voltage",ylab="depth")
    for (i in unique(data[,"PhaseName"])){
      if (i != "PAR"){
        lines(Sig[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
      }
    }
    plotDepthZones(ZoneDepth,x = xlim)
    
  }
  

  
  #Plot Radio
  Sig<-data$Physical
  Sig[Sig<0]<-NA
  
  xlim=range(Sig,na.rm = TRUE, finite = TRUE)
  if (xlim[2]<=0){xlim[2]<-1}  
  if (xlim[1]<=0){xlim[1]<-0.1}   
  plot(NULL,NULL,xlim=xlim,ylim=range(-data[!(data$PhaseName=="PAR"),"Pressure_dbar"],na.rm = TRUE, finite = TRUE),xlab="MPE Physical",ylab="depth",log="x")
  for (i in unique(data[,"PhaseName"])){
    if (i != "PAR"){
        lines(Sig[data[,"PhaseName"]==i],-data[data[,"PhaseName"]==i,"Pressure_dbar"],col=match(i,unique(data[,"PhaseName"])))
    }
  }
  plotDepthZones(ZoneDepth,x = xlim)
  
}


#**************************************************
#Plot External - Trig (Pump)
# data<-dataprofile$data$ext_trig
Plotext_trig<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main="External Trig")
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    legend("topright",legend=unique(data[,"PhaseName"]),pch = 16,col=1:length(unique(data[,"PhaseName"])),bty="n")
    
  }

  
}

#**************************************************
#Plot IMU
# data<-dataprofile$data$imu
PlotIMU<-function(data,technical=TRUE,ZoneDepth=NULL){
  
  #Plot technical
  if (technical){
    #Plot Chronologie
    plot(data[,"Date"],-data[,"Pressure_dbar"],col=match(data[,"PhaseName"],unique(data[,"PhaseName"])),xlab="time",ylab="depth",type="b")
    title(main="IMU")
    plotDepthZones(ZoneDepth,x = range(data[,"Date"]))
    
    legend("topright",legend=unique(data[,"PhaseName"]),pch = 16,col=1:length(unique(data[,"PhaseName"])),bty="n")
    
  }
  
  #Plot data
  ## IntTime et Tilt
  ind<-which(data[,"PhaseName"] == "ASC")
  if (("tilt" %in% colnames(data)) & ("heading" %in% colnames(data))){
    plot(data$tilt[ind],-data[ind,"Pressure_dbar"],type="l",col=1,xlab="Tilt",ylab="Depth",main="Tilt")
    plotDepthZones(ZoneDepth)
    
    # #convertion en coordonnees ndc
    # InsertPos<-c(grconvertX(c(0.5,0.95), from = "npc", to = "ndc"),
    #              grconvertY(c(0.15,0.8), from = "npc", to = "ndc"))
    # 
    # #creation de l'insert
    # oldpar <- par()
    # par(fig = InsertPos, new = TRUE)
    # par(mar = c(0.1, 0.1, 0.1, 0.1),mgp=c(3,0.1,0))
    
    plot(data$tilt[ind],-data[ind,"Pressure_dbar"],type="l",col=1,ylim=c(-20,0),
         main="Tilt-Zoom")#cex=0.5,cex.axis=0.5,tcl=-0.1)

    # par(fig = oldpar$fig)
    # par(mar = oldpar$mar)
    # par(mgp = oldpar$mgp)
    # par(mfrow = oldpar$mfrow,new=F)
    
    plot(data$heading[ind],-data[ind,"Pressure_dbar"],type="l",col="blue",xlab="Tilt",ylab="Depth",main="Tilt")
    plotDepthZones(ZoneDepth)
    
    
  }
  
}

#**************************************************
#Plot wave
# data<-dataprofile$data$wave
PlotWave<-function(data){
  plot(data$Date,data$heading,type="l",xlab="date",ylab="Heading",
       main="Heading at surface")
  
  plot(data$Date,data$tilt,type="l",xlab="date",ylab="Tilt",
       main="Tilt at surface")
  legend("topright",legend=paste("mean=",formatC(mean(data$tilt),digit=3)),
         bty="n")
  
  plot(data$Date,data$acceleration,type="l",xlab="date",ylab="Acceleration",
       main="Acceleration at surface")
  legend("topright",legend=paste("mean=",formatC(mean(data$acceleration),digit=3)),
         bty="n")
  
}

#**************************************************
#Plot RAD sensors together
# data<-dataprofile$data$ext_trig
PlotCompRad<-function(dataprofile){
  
  if (("ocr" %in% names(dataprofile$data)) & ("mpe" %in% names(dataprofile$data))){
    
  #Plot MPE
  data<-dataprofile$data$mpe
  data<-data[data[,"PhaseName"] == "ASC",]
    
  if (nrow(data) > 2){
    SigMPE<-data$Physical
    DepthMPE<-data$Pressure_dbar
    SigMPE<-SigMPE-min(SigMPE)
    
    MeanMPE<-median(SigMPE[DepthMPE<5])
  
    #Plot OCR
    data<-dataprofile$data$ocr
    data<-data[data[,"PhaseName"] == "ASC",]
    
    if (nrow(data) > 2){
      Sig<-data$`Photosynthetic-Active-Radiation`
      Sig<-Sig-min(Sig)
      
      MeanOCR<-median(Sig[data$Pressure_dbar<5])
      
      if (!is.na(MeanOCR)){     
        CoefNorm<-MeanOCR/MeanMPE
        SigMPE<-CoefNorm*SigMPE}
      
      ## Plot
      xlim=range(SigMPE[SigMPE>0],na.rm = TRUE, finite = TRUE)
      # if (xlim[2]<=0){xlim[2]<-1}  
      # if (xlim[1]<=0){xlim[1]<-0.1}   
      plot(SigMPE,-DepthMPE,xlim=xlim,ylim=range(-DepthMPE,na.rm = TRUE, finite = TRUE),
           xlab="PAR Physical",ylab="depth",log="x",type="l")
      
      lines(Sig,-data$Pressure_dbar,col=2,type="l")
      
      #Dark sur 20 dernier m
      DarkMPE<-median(SigMPE[DepthMPE>(max(DepthMPE-20))])
      lines(rep(DarkMPE,2),-c(max(DepthMPE),150),lty=2)
      
      DarkOCR<-median(Sig[data$Pressure_dbar>(max(data$Pressure_dbar-20))])
      lines(rep(DarkOCR,2),-c(max(DepthMPE),150),lty=2,col=2)
      
      legend("topleft",legend=c("MPE adjusted to OCR","OCR",
                                paste("MeanOCR/MeanMPE (5m)=",formatC(CoefNorm,digit=3)),
                                paste("DarkMPE=",formatC(DarkMPE,digit=3)),
                                paste("DarkOCR=",formatC(DarkOCR,digit=3))),
             lty=c(1,1,0,2,2),col=c(1,2,1,1,2),bty="n",cex=0.75)
    }
  }
    
  }
  
  
}


#**************************************************

## Concatenation plots

#**************************************************
#' PlotCTS5 : Plot a profile
#'
#' @description
#' PlotCTS5 : create a pdf file with profile plots
#'
#'
#' @param login identifiant used at the beginning of the pdf filename
#' @param dataprofile data and technical files read from \code{\link{cts5_readProfile}}
#' processing by \code{\link{usea_ProcessData}}
#' @param PhaseToPlot list of phase to plot
#' @param add if false a new pdf will be created. if true all plots will be generated in the 
#' current open device
#' @param technical if true, technical information will be plotted
#' @param paper paper size
#' @param mfrow mfrow
#' @param SensorsToPlot vector of sensors to include in the plot. If NULL plot all sensors.
#' 
#' @examples 
#' 
#' login="lovuse001a"
#' 
#' floatname="ffff"
#' 
#' Meta<-cts5_readMetaSensor(floatname=floatname)
#'
#' cts5_decode(floatname=floatname,CycleNumber=c,PatternNumber = p,subdir="./CSV")
#'
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#'
#' dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
#'
#' PlotCTS5(login=login,dataprofile,PhaseToPlot=c("PRE","DES","PAR","ASC","SUR"),add=FALSE,technical=TRUE,paper = "A4",mfrow=c(3,2))
#'
#' # small list of sensors in the terminal
#' PlotCTS5(dataprofile,add=TRUE,SensorsToPlot=c("ramses","ramses2"))
#' 
#' @export
#'
PlotCTS5<-function(login="lov",dataprofile,PhaseToPlot=c("PRE","DES","PAR","ASC","SUR"),add=FALSE,
                   technical=TRUE,paper = "A4",mfrow=c(3,2),
                   SensorsToPlot=NULL){


if (!is.null(dataprofile)){   

  CycleNumber<-dataprofile$CycleNumber
  PatternNumber<-dataprofile$PatternNumber
  
  #Ouverture du pdf
  if (!add){
      filename<-paste(login,"_",formatC(CycleNumber,width=3,flag="0"),"_",formatC(PatternNumber,width=2,flag="0"),".pdf",sep="")
      cat("create:",filename,"\n",sep="")
      pdf(file=filename,paper = paper, width = 0, height = 0)
      par(mfrow=mfrow)

  }
  
  
  ## sensor selection
  if (is.null(SensorsToPlot)){
    SensorsToPlot<-names(dataprofile$data)
  } else {
    SensorsToPlot<-SensorsToPlot[SensorsToPlot %in% names(dataprofile$data)]
  }
  
  
  #CTD
  #ind<-(dataMerged[,"SensorType"]==0) & (dataMerged[,PhaseName] %in% c("DES","ASC"))
  if ("sbe41" %in% SensorsToPlot){
    data<-dataprofile$data$sbe41
    plotCTD(data,technical=technical,
            ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_01"))
  
    mydate<-max(as.POSIXct(dataprofile$data$sbe41[,"Date"],origin = "1970-01-01",tz="UTC"))
    mtext(paste("float:",login,", cycle:",CycleNumber,", pattern:",PatternNumber,", date:",mydate),side=3,line=-1,outer=T,cex=0.6,adj=0.95)
    
    # if (!is.null(dataprofile$technical$GPS)){
    #   mtext(paste("Lat: ",dataprofile$technical$GPS$`lat (deg)`,
    #               ", Lon:",dataprofile$technical$GPS$`lon (deg)`,sep = ""),side=3,line=1,outer=T,cex=0.6,adj=0.95)
    #   
    # }
  }
  
  

  
  
  #PlotEcoStd
  if ("eco" %in% SensorsToPlot){
    if ("chlorophyll-a_ug/l" %in% colnames(dataprofile$data$eco)){
      data<-dataprofile$data$eco
      try(PlotEcoStd(data,technical=technical,
                 ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_04")))
    }
  }
  
  #PlotOCR
  if ("ocr" %in% SensorsToPlot){  
    if ("Downwelling-irradiance-380nm" %in% colnames(dataprofile$data$ocr)){
      data<-dataprofile$data$ocr
      try(PlotOCR4(data,technical=technical,
               ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_03")))
    }
    
    if ("RAD665" %in% colnames(dataprofile$data$ocr)){
      data<-dataprofile$data$ocr
      try(PlotOCR507IR(data,technical=technical,
                   ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_03")))
    }
  }
    
  #PlotcRover
  if ("crover" %in% SensorsToPlot){  
    if ("c-uncalibrated_1/m" %in% colnames(dataprofile$data$crover)){
      data<-dataprofile$data$crover
      try(PlotCROVER(data,technical=technical,
                 ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_06")))
    }
  }
  
  #PlotOptode
  if ("do" %in% SensorsToPlot){  
    if ("doxy_uncalibrated" %in% colnames(dataprofile$data$do)){
      data<-dataprofile$data$do
      try(PlotOptode(data,technical=technical,
                 ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_02")))
    }
  }  
  
  #PlotSuna
  if ("suna" %in% SensorsToPlot){  
    data<-dataprofile$data$suna
    try(PlotSUNA(data,technical=technical,
             ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_07")))
  }

  
  #PlotsbepH
  if ("sbeph" %in% SensorsToPlot){  
    if ("pH_Uncal" %in% colnames(dataprofile$data$sbeph)){
      data<-dataprofile$data$sbeph
      try(PlotSbepH(data,technical=technical,
                ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_05")))
    }
  }    
  
  #PlotUVP_lpm
  if ("uvp6_lpm" %in% SensorsToPlot){  
    data<-dataprofile$data$uvp6_lpm
    try(PlotUVP_lpm(data,technical=technical,
                ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_08")))
  }    
  
  #PlotUVP_txo
  if ("uvp6_txo" %in% SensorsToPlot){  
    data<-dataprofile$data$uvp6_txo
    try(PlotUVP_txo(data,technical=technical,
                ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_08")))
  }   
    
  #PlotUVP_blk
  if ("uvp6_blk" %in% SensorsToPlot){  
    data<-dataprofile$data$uvp6_blk
    try(PlotUVP_blk(data,technical=technical,
                ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_08")))
  }        
  
  #PlotRamses
  if ("ramses" %in% SensorsToPlot){  
      data<-dataprofile$data$ramses
      try(PlotRamses(data,technical=technical,
                 ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_14")))
  }   
  
  if ("ramses2" %in% SensorsToPlot){  
    data<-dataprofile$data$ramses2
    try(PlotRamses(data,technical=technical,
               ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_14")))
  } 
  
  #IMU
  if ("imu" %in% SensorsToPlot){  
    data<-dataprofile$data$imu
    try(PlotIMU(data,technical=technical,
               ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_20")))
  } 
  
  #Wave 
  if ("wave" %in% SensorsToPlot){  
    data<-dataprofile$data$wave
    try(PlotWave(data))
  } 
  
  
  #ext_trig
  if ("ext_trig" %in% SensorsToPlot){  
    data<-dataprofile$data$ext_trig
    try(Plotext_trig(data,technical=technical,
                 ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_13")))
  }   
  
  #mpe
  if ("mpe" %in% SensorsToPlot){  
    data<-dataprofile$data$mpe
    try(PlotMPE(data,technical=technical,
            ZoneDepth=FindZoneDepth(dataprofile$inifile,"SENSOR_17")))
  }   
  
  #Plot RAD sensors together
  try(PlotCompRad(dataprofile))
  
  
  if (!add){
    cat("Close pdf","\n")
    dev.off()
  }
}
else {
  warning("Null data")
}
}
