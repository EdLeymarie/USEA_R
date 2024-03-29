% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS5_USEA.R
\name{findfloatname}
\alias{findfloatname}
\title{find the floatname within a float directory}
\usage{
findfloatname(
  pattern = "^[[:alnum:]]{4}_[[:digit:]]{3}_[[:digit:]]{2}.*",
  CycleNumber = NULL,
  PatternNumber = 1
)
}
\description{
take the last file with the pattern "ffff_ccc_pp.*" a return the hexafloatname
}
\examples{

findfloatname(pattern="[[:alnum:]]{4}_[[:digit:]]{3}_[[:digit:]]{2}.*")

}
