\name{createDemandSite}
\alias{createDemandSite}
\title{Creates A Demand Site}
\description{A function to create a demand site using demand pattern variation or time series}
\usage{
createDemandSite(type = "agricultural",
                 demandName = "Agri1", 
                 demandCode, 
                 annualUseRate,
                 annualVariation, 
                 area, 
                 cycle = FALSE,
                 numberOfCycles = NULL,
                 supplierCode,
                 downstreamCode,
                 priority = 1,
                 start = 1900)
}
\arguments{
  \item{type}{domestic, agricultural, and environmental. the hydropower is not available in this version}
  \item{demandName}{a string name of the desired demand site}
  \item{demandCode}{a uniqe integer number}
  \item{annualUseRate}{the amount of water consumption per unit of area during a year}
  \item{annualVariation}{a vector, in percent, of demand site variation during a year. the summation of vector's ellemnt should be equal to 100}
  \item{area}{for agricultual type: the area of demand site}
  \item{cycle}{logical, cycle the time series?}
  \item{numberOfCycles}{if cycle is TRUE, number of cycles}
  \item{supplierCode}{the code of reservoir which is going to supply this demand site}
  \item{downstreamCode}{the downstream of demand site}
  \item{priority}{the priority of supply}
  \item{start}{the start year}
}
\value{an object inherited from class create DemandSite}
\author{Rezgar Arabzadeh, Parisa Aberi}
\keyword{ models }