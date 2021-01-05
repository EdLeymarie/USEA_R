#***************************************************************************
#' save UVP6 data to EcoPart csv format
#'
#' @description
#' save UVP6 data to EcoPart csv format
#'
#'
#' @param login login of the float used as prefix
#' @param dataprofile Merged data obtained by \code{\link{cts5_readProfile}}
#' @param subdir sub directory where to save the data
#' 
#' @examples 
#' 
#' Save_UVP62EcoTaxa(login,dataprofile,Meta$SENSORS$SENSOR_UVP6$HW_CONF,subdir="./uvp6/")
#' 
#' 
#' 
#' @export
#'
#SaveToCTS5 : Entete normalisee CTS5, sep="\t"
Save_UVP62EcoTaxa<-function(login,dataprofile,UVP6_HW_CONF=NULL,subdir=""){
  
  if (!is.null(dim(dataprofile$data$uvp6_lpm))){
    
    ## Enregistrement 
    NAME<-paste(subdir,login,"-",formatC(unique(dataprofile$CycleNumber),width=3,flag="0"),"-",
                formatC(unique(dataprofile$PatternNumber),width=2,flag="0"),sep="")
    
    
    ## Ajout GPS
    Lat<-NA
    Lon<-NA
    if (!is.null(dataprofile$technical)){
      Lat<-as.numeric(dataprofile$technical$GPS$`lat (deg)`)
      Lon<-as.numeric(dataprofile$technical$GPS$`lon (deg)`)
    }
    
    
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
    
    UVP6_Meta<-cbind(UVP6_MetaNames,UVP6_HW_CONF)
    
    #**************************************************************
    ## 1- data LPM
    if (nrow(dataprofile$data$uvp6_lpm)>0){
      
      ## Formatage des colonnes
      dataUVP<-cbind(dataprofile$data$uvp6_lpm[,1:2],Lat,Lon,dataprofile$data$uvp6_lpm[,-(1:2)])
      dataUVP[,1]<-format(dataUVP[,1],format="%Y%m%dT%H%M%S")
      colnames(dataUVP)[1:2]<-c("DATE_TIME","PRES_decibar")
      colnames(dataUVP)[3:4]<-c("LATITUDE_decimal_degree","LONGITUDE_decimal_degree")
      
      
      indcol<-c(1:5,grep("NSamples",colnames(dataUVP)),grep("UVP6_Temp",colnames(dataUVP))
                ,grep("NP_",colnames(dataUVP))
                ,grep("MG_",colnames(dataUVP)))
      
      dataUVP<-dataUVP[,indcol]
      
      #Formatage Colonnes
      
      colnames(dataUVP)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      
      colnames(dataUVP)[8:(8+17)]<-paste("NB_SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")
      colnames(dataUVP)[(8+18):(8+35)]<-paste("GREY_ SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")
      
      ## LPM DESCENT
      ind<-dataUVP$PhaseName=="DES"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
      }

      ## LPM Ascent
      ind<-dataUVP$PhaseName=="ASC"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
        }
      
      ## LPM Parking
      ind<-dataUVP$PhaseName=="PAR"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-PAR_",UVP6SN,"_TIME_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
        }
      
    }
    
    #**************************************************************
    ## 2- data BLACK
    if (nrow(dataprofile$data$uvp6_blk)>0){
      
      ## Formatage des colonnes
      dataUVP<-cbind(dataprofile$data$uvp6_blk[,1:2],Lat,Lon,dataprofile$data$uvp6_blk[,-(1:2)])
      dataUVP[,1]<-format(dataUVP[,1],format="%Y%m%dT%H%M%S")
      colnames(dataUVP)[1:2]<-c("DATE_TIME","PRES_decibar")
      colnames(dataUVP)[3:4]<-c("LATITUDE_decimal_degree","LONGITUDE_decimal_degree")
      
      indcol<-c(1:5,grep("NSamples",colnames(dataUVP)),grep("uvp-blk_Internal_temp",colnames(dataUVP))
                ,grep("uvp-blk_Count",colnames(dataUVP)))
      
      dataUVP<-dataUVP[,indcol]
      
      #Formatage Colonnes
      
      # On ajoute une colonne de 1 car le black est en raw
      dataUVP<-cbind(dataUVP[,1:5],1,dataUVP[,-(1:5)])
      
      #Names
      colnames(dataUVP)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      colnames(dataUVP)[8:(8+4)]<-paste("NB_SIZE_SPECTRA_PARTICLES_class_",1:5,sep="")
      
      ## Black Descent
      ind<-dataUVP$PhaseName=="DES"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
      ## Black Ascent
      ind<-dataUVP$PhaseName=="ASC"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
      ## Black Parking
      ind<-dataUVP$PhaseName=="PAR"
      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      }
      
    }
    
  }
  
}
