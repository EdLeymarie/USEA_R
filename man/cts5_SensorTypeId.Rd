% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS5_USEA.R
\name{cts5_SensorTypeId}
\alias{cts5_SensorTypeId}
\title{provide sensor Id used un csv file}
\usage{
cts5_SensorTypeId(pattern = "", exact = F)
}
\arguments{
\item{pattern}{name of the sensor to look for. if "", provide the full list.}

\item{exact}{if True, look for pattern==CTS5_supported_sensors. If false, look for grep(pattern,CTS5_supported_sensors,ignore.case = T)}
}
\value{
a vector containing the id of the sensors
}
\description{
provide sensor Id used un csv file \code{\link{cts5_readcsv}} .
}
\details{
SensorType
  0 : CTD
  3 : DO Aanderaa
  9 : ECOpuck
 12 : OCR504
 18 : cROVER
 21 : SUNA
 22 : PHSEABIRD
109 : UVP6 lpm
110 : UVP6 blk
111 : UVP6 txo
112 : UVP6 TAXO2 (not used)
113 : Ramses
114 : opus_lgt
115 : opus_blk
116 : ext_trig
117 : mpe
118 : ramses2
119 : HydroC
120 : imu
121 : wave
122 : fltrider
123 : PAL
124 : Tridente
}
\examples{
cts5_SensorTypeId("")

cts5_SensorTypeId("uvp6")

cts5_SensorTypeId("uvp6_blk")

}
