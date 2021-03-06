% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS5_USEA_technical.R
\name{cts5_CheckDataCount}
\alias{cts5_CheckDataCount}
\title{Compare the number of available data point}
\usage{
cts5_CheckDataCount(dataprofile)
}
\arguments{
\item{dataprofile}{data and technical files read from \code{\link{cts5_readProfile}}}
}
\value{
list containing check (= True if the number of data point are the same);
and DataCheck a data.frame with the full comparison.
}
\description{
cts5_CheckDataCount compare the number of data point per sensor and per phase
between the technical file and the dataMerged file
}
\examples{
cts5_decode(floatname=floatname,CycleNumber=c,PatternNumber = p,subdir="./CSV",sensors=c("sbe41","do","eco","ocr"))

dataprofile<-cts5_readProfile(floatname=floatname,CycleNumber=c,PatternNumber = p)
    
dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)

dataprofile<-cts5_ProcessData(Meta$SENSORS,dataprofile)

if (!cts5_CheckDataCount(dataprofile)$check){
 cat("!! Warning, data count error \n")
}


}
