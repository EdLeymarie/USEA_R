% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS5_USEA.R
\name{cts5_create_command}
\alias{cts5_create_command}
\title{create a _command.txt files by comparing inifiles}
\usage{
cts5_create_command(
  newini = "apmt_update.ini",
  oldini = "",
  sections = c("PATTERN", "SENSOR"),
  output = "_command.txt"
)
}
\arguments{
\item{newini}{filename of the new ini file}

\item{oldini}{filename of the old ini file. Used the latest ini file if equal to ""}

\item{sections}{section scanned to create command}

\item{output}{name of the command file}
}
\value{
vector of command and _command.txt file
}
\description{
create a _command.txt files by comparing newini and oldini files
}
\details{
CTS5_create_command compare the two ini files only for sections given in the section parameter. Difference in other sections are ignored.
}
\examples{

cts5_create_command("apmt_update.ini",output = "_command.txt")

cts5_create_command(newini="apmt_uvp6_standard.ini",sections=c("SENSOR_08","SENSOR_09","SENSOR_10"))

}
