# USEA_R

This package helps to decode the raw files transmitted by a Provor CTS5 and allows them to be assembled into an easily usable R-list.
This package is used by simply mentioning the cycle number you want to process (here 124 for exemple)

login="lovuse001a"
Meta<-cts5_readMetaSensor()
cts5_decode(CycleNumber=124)
dataprofile<-cts5_readProfile(CycleNumber=124,include.inifile=T)
dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)
PlotCTS5(login=login,dataprofile)
