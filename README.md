# USEA_R

This package helps to decode the raw files transmitted by a Provor CTS5 and allows them to be assembled into an easily usable R-list.

## Installation:

install.packages("remotes") remotes::install_github("EdLeymarie/USEA_R")

install nke decoder in a "nke-decoder" directory

## use:

This package is used by simply mentioning the cycle number you want to process (here 124 for example).

``` R
require(USEAR) 
require(oce) 
require(XML) 
require(chron) 
require(fields) 

Meta<-cts5_readMetaSensor() 
login<-Meta$TELECOM["Login"]
```

### trajectory:

``` R
Navdata<-cts5_system_process(AutoSaveLoad = T,userdefine="",Nke_ProgPath=your "nke-decoder" directory) 

cts5_system_Plot(Navdata) 

cts5_system_Plot(Navdata,cycle = 7,pattern = 1)#,ylim = c(-200,-100))
```

## Decode and plot Data

download all files in a directory and create a csv directory

``` R
#Decode hex files and copy Ascii output in ./csv
cts5_decode(floatname = floatname,CycleNumber=c,PatternNumber = p, subdir="./csv",Nke_ProgPath="D:/Data/Provor_USEA/USEA_R")   
 
#read and concat all Ascii data files
dataprofile<-cts5_readProfile(CycleNumber=c,PatternNumber = p,include.inifile=T)

#process data
dataprofile<-cts5_ProcessData(metadata=Meta$SENSORS,dataprofile,ProcessUncalibrated = T)   
 
#Check that all data have been decoded
check<-cts5_CheckDataCount(dataprofile)$check   
cat("check data:",check,"\n")

#Plot and save data
setwd("./csv")  
PlotCTS5(login=login, dataprofile)  
cts5_save2RData(dataprofile,login = login)
```
