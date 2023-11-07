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
#' Save_UVP62EcoTaxa(login,dataprofile,UVP6_HW_CONF=Meta$SENSORS$SENSOR_UVP6$HW_CONF,subdir="./uvp6/")
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
    if (!is.null(dataprofile$technical$GPS)){
      Lat<-as.numeric(dataprofile$technical$GPS$`lat (deg)`)
      Lon<-as.numeric(dataprofile$technical$GPS$`lon (deg)`)
    }
    
    
    ## Formatage UVP6_HW_CONF
    UVP6SN<-UVP6_HW_CONF[1]
    UVP6_Meta<-cbind(names(UVP6_HW_CONF),UVP6_HW_CONF)

    
    
    #**************************************************************
    ## 1- data LPM
    if ((nrow(dataprofile$data$uvp6_lpm)>0) & (nrow(dataprofile$data$uvp6_blk)>0) & !is.na(Lat)){
      
      ## Formatage des colonnes lpm
      dataUVP<-cbind(dataprofile$data$uvp6_lpm[,c("Date","Pressure_dbar")],Lat,Lon,dataprofile$data$uvp6_lpm[,-(1:2)])
      
      #Supression donnees calibrees
      indPro<-grep("_Size_",colnames(dataUVP))
      if (length(indPro)>0){
        dataUVP<-dataUVP[,-indPro]
      }
      
      dataUVP[,1]<-format(dataUVP[,1],format="%Y%m%dT%H%M%S")
      colnames(dataUVP)[1:2]<-c("DATE_TIME","PRES_decibar")
      colnames(dataUVP)[3:4]<-c("LATITUDE_decimal_degree","LONGITUDE_decimal_degree")
      
      dataUVP<-dataUVP[,-grep("processing",colnames(dataUVP))]
      
      ## Formatage class lpm & Grey en fonction de la version uvp
      if (as.numeric(UVP6_HW_CONF["Lower_limit_size_class_1"])>50){
        ## UVP6 Taxo On doit decaler les colonnes lpm
        datatemp_lpm<-dataUVP[,grep("NP_Class",colnames(dataUVP))]
        datatemp_grey<-dataUVP[,grep("MG_Class",colnames(dataUVP))]
        
        #moyenne de grey pondéré par lpm
        for (r in 1:nrow(datatemp_grey)){
          Num <- datatemp_lpm[r,17]+datatemp_lpm[r,18]
          if (Num>0){
            datatemp_grey[r,17]<-(datatemp_lpm[r,17]*datatemp_grey[r,17]+datatemp_lpm[r,18]*datatemp_grey[r,18])/Num
          }
        }
        
        #decalage et somme lpm
        datatemp_lpm<-cbind(rep(NA,nrow(datatemp_lpm)),datatemp_lpm)
        datatemp_lpm[,18]<-apply(datatemp_lpm[,18:19],1,sum)
        
        dataUVP[,grep("NP_Class",colnames(dataUVP))] <- datatemp_lpm[,1:18]
        
        #decalage grey
        datatemp_grey<-cbind(rep(NA,nrow(datatemp_grey)),datatemp_grey)
        dataUVP[,grep("MG_Class",colnames(dataUVP))] <- datatemp_grey[,1:18]
        
      }
      
      #Formatage Colonnes
      
      colnames(dataUVP)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      
      colnames(dataUVP)[8:(8+17)]<-paste("NB_SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")
      colnames(dataUVP)[(8+18):(8+35)]<-paste("GREY_ SIZE_SPECTRA_PARTICLES_class_",1:18,sep="")
      
      
      ## Formatage des colonnes blk
      dataUVPblk<-cbind(dataprofile$data$uvp6_blk[,c("Date","Pressure_dbar")],Lat,Lon,dataprofile$data$uvp6_blk[,-(1:2)])
      dataUVPblk[,1]<-format(dataUVPblk[,1],format="%Y%m%dT%H%M%S")
      colnames(dataUVPblk)[1:2]<-c("DATE_TIME","PRES_decibar")
      colnames(dataUVPblk)[3:4]<-c("LATITUDE_decimal_degree","LONGITUDE_decimal_degree")
      
      # indcol<-c(1:5,grep("NSamples",colnames(dataUVPblk)),grep("uvp-blk_Internal_temp",colnames(dataUVPblk))
      #           ,grep("uvp-blk_Count",colnames(dataUVPblk)))
      # 
      # dataUVPblk<-dataUVPblk[,indcol]
      
      dataUVPblk<-dataUVPblk[,-grep("processing",colnames(dataUVPblk))]
      
      #Formatage Colonnes
      
      # On ajoute une colonne de 1 car le black est en raw
      dataUVPblk<-cbind(dataUVPblk[,1:5],1,dataUVPblk[,-(1:5)])
      
      #Names
      colnames(dataUVPblk)[6:7]<-c("IMAGE_NUMBER_PARTICLES","TEMP_PARTICLES")
      colnames(dataUVPblk)[8:(8+4)]<-paste("NB_SIZE_SPECTRA_PARTICLES_class_",1:5,sep="")
      
      ## LPM DESCENT
      ind<-dataUVP$PhaseName=="DES"
      indblk<-dataUVPblk$PhaseName=="DES"

      if ((sum(ind)>0) & (sum(indblk)>0)){
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),"\n")
        write.table(dataUVPblk[indblk,-5],file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-DES_",UVP6SN,"_DEPTH_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-DES_",UVP6SN,"_DEPTH_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
      }

      ## LPM Ascent
      ind<-dataUVP$PhaseName=="ASC"
      indblk<-dataUVPblk$PhaseName=="ASC"

      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),"\n")
        write.table(dataUVPblk[indblk,-5],file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-ASC_",UVP6SN,"_DEPTH_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-ASC_",UVP6SN,"_DEPTH_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
        }
      
      ## LPM Parking
      ind<-dataUVP$PhaseName=="PAR"
      indblk<-dataUVPblk$PhaseName=="PAR"

      if (sum(ind)>0){
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),"\n")
        write.table(dataUVP[ind,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_LPM.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
      
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),"\n")
        write.table(dataUVPblk[indblk,-5],file = paste(NAME,"-PAR_",UVP6SN,"_TIME_BLACK.txt",sep=""),col.names = T,row.names = F,quote = F,sep="\t")
        
        ### Fichier META que si il y a des données.
        cat("writing:",paste(NAME,"-PAR_",UVP6SN,"_TIME_META.txt",sep=""),"\n")
        write.table(UVP6_Meta,file = paste(NAME,"-PAR_",UVP6SN,"_TIME_META.txt",sep=""),col.names = F,row.names = F,quote = F,sep="\t")
        
        
        }
      
    }
    
    
  }
  
}
