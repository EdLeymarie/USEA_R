###########################################
#' save UVP6 data to EcoPart csv format
#'
#' @description
#' save UVP6 data to EcoPart csv format
#'
#'
#' @param login login of the float used as prefix
#' @param dataMerged Merged data obtained by \code{\link{cts5_concatProfile}}
#' @param subdir sub directory where to save the data
#' @param GPS GPS data to be included in the csv. obtained by \code{\link{cts5_readtechnical}}
#' 
#' @details 
#' 
#' GPS could also be set as a vector:
#' GPS<-c(Lat,Lon)
#' 
#' 
#' @export
#'
#SaveToCTS5 : Entete normalisee CTS5, sep="\t"
Save_UVP62EcoTaxa<-function(login,dataMerged,UVP6_HW_CONF=NULL,subdir=".",GPS=NULL){
  
  if (!is.null(dim(dataMerged))){
    
    ## Enregistrement 
    NAME<-paste(login,formatC(unique(dataMerged$`Number Cycle`),width=3,flag="0"),
                formatC(unique(dataMerged$`Number Profil`),width=2,flag="0"),sep="-")
    
    
    ## Ajout GPS
    Lat<-NA
    Lon<-NA
    if (!is.null(GPS)){
      Lat<-as.numeric(GPS[1])
      Lon<-as.numeric(GPS[2])
    }
    
    ## Formatage des colonnes
    dataTemp<-cbind(dataMerged[,2:1],Lat,Lon,dataMerged[,-(1:2)])
    dataTemp[,1]<-format(dataTemp[,1],format="%Y%m%dT%H%M%S")
    colnames(dataTemp)[1:2]<-c("DATE_TIME","PRES_decibar")
    colnames(dataTemp)[3:4]<-c("LATITUDE_decimal_degree","LONGITUDE_decimal_degree")
    
    ## Formatage UVP6_HW_CONF
    UVP6_HW_CONF<-strsplit(UVP6_HW_CONF,split = ",")[[1]]
    UVP6_MetaNames<-c("Camera_ref","Acquisition_mode","Default_acquisition_configuration","Delay_after_power_up_on_time_mode",
    "Light_ref","Correction_table_activation","Time_between_lighting_power_up_and_trigger",
    "Time_between_lighting_trigger_and_acquisition","Pressure_sensor_ref","Pressure_offset",
    "Storage_capacity","Minimum_remaining_memory_for_thumbnail_saving","Baud_Rate",
    "IP_adress","Black_level","Shutter","Gain","Threshold","Aa","Exp","Pixel_Size",
    "Image_volume","Calibration_date","Last_parameters_modification","Operator_email",
    paste("Lower_limit_size_class_",1:18,sep=""))
    
    UVP6SN<-UVP6_HW_CONF[1]
    
    
    #####################################################################
    ## 1- data LPM
    ind<-(dataTemp[,"SensorType"]==109)
    if (sum(ind)>0){
      
      ### Fichier META que si il y a des données.
      UVP6_Meta<-cbind(UVP6_MetaNames,UVP6_HW_CONF)
      cat("writing:",paste(NAME,"_",UVP6SN,"_DEPTH_META.txt",sep=""),"\n")
      write.table(UVP6_Meta,file = paste(NAME,"_",UVP6SN,"_DEPTH_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
      
      
      indcol<-c(1:4,7,grep("NSamples",colnames(dataTemp)),grep("UVP6_Temp",colnames(dataTemp))
                ,grep("NP_.*(um)",colnames(dataTemp))
                ,grep("MG_.*(um)",colnames(dataTemp)))
      
      dataUVP<-dataTemp[ind,indcol]
      
      #Formatage Colonnes
      
      colnames(dataUVP)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      
      colnames(dataUVP)[8:(8+17)]<-paste("AB_SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")
      colnames(dataUVP)[(8+18):(8+35)]<-paste("GREY_ SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")

      ## LPM Ascent
      ind<-dataUVP$`Number Phase`=="ASC"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
      ## LPM Parking
      ind<-dataUVP$`Number Phase`=="PAR"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
    }
    
    #####################################################################
    ## 2- data BLACK
    ind<-(dataTemp[,"SensorType"]==110)
    if (sum(ind)>0){
      
      indcol<-c(1:4,7,grep("uvp-blk_Internal_temp",colnames(dataTemp))
                ,grep("uvp-blk_Count",colnames(dataTemp)))
      
      dataUVP<-dataTemp[ind,indcol]
      
      #Formatage Colonnes
      
      # On ajoute une colonne de 1 car le black est en raw
      dataUVP<-cbind(dataUVP[,1:5],1,dataUVP[,-(1:5)])
      
      #Names
      colnames(dataUVP)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      colnames(dataUVP)[8:(8+4)]<-paste("AB_SIZE_SPECTRA_PARTICLES_class_",1:5,sep="")
      
      ## Black Ascent
      ind<-dataUVP$`Number Phase`=="ASC"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
      ## Black Parking
      ind<-dataUVP$`Number Phase`=="PAR"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
    }
    
  }
  
}
