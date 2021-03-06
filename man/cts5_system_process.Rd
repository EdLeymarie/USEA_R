% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS5_System.R
\name{cts5_system_process}
\alias{cts5_system_process}
\title{decode, parse and Merge system file}
\usage{
cts5_system_process(
  Navdata = NULL,
  floatname = "",
  OnlyNew = T,
  sysfile_number = NULL,
  subdir = ".",
  userdefine = "",
  AutoSaveLoad = F,
  Nke_ProgPath = ""
)
}
\arguments{
\item{Navdata}{already processed system files}

\item{floatname}{hexa name of the float. If "" then name will be found automatically.}

\item{OnlyNew}{if true and if Navdata is not null, only system files older than files already
in Navdata will be processed}

\item{sysfile_number}{numeric : vector of system file numbers. If null, all files available
in the directory}

\item{subdir}{not used}

\item{userdefine}{Key word to look for in the system file}

\item{AutoSaveLoad}{If True, Navdata are saved and load automatically}

\item{Nke_ProgPath}{path to the nke decoder (APMTDecrypt.exe). This path is stored to Sys.getenv("USEAR_Nke_ProgPath") !! Attention, on linux : required
the installation of wine}
}
\value{
list containing the parsed system information as from \code{\link{cts5_system_parse}}
}
\description{
call \code{\link{cts5_system_decode}}, \code{\link{cts5_system_parse}} and \code{\link{cts5_system_merge}}.
}
\details{
this function must be call in the directory where are .hex files
}
\examples{
Navdata<-cts5_system_process(floatname = "3ab3")
Navdata<-cts5_system_process(floatname = "3ab3",userdefine="ICE")
Navdata<-cts5_system_process(userdefine="ICE")
Navdata<-cts5_system_process(userdefine="ICE",sysfile_number=125:130)


}
