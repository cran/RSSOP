\name{hydrometeorology}
\alias{hydrometeorology}
\title{Creates A Meteorological Object}
\description{this function creates an hydrometeorological object include stream flow and evaporation time series}
\usage{
hydrometeorology(Inflow,
                 netEvaporation,
                 cycleEvaporation = FALSE,
                 numberOfCycles = NULL,
                 startDate = c(1900, 1),
                 reservoirCode)
}
\arguments{
  \item{Inflow}{Inflow time series (MCM)}
  \item{netEvaporation}{Net evaporation in (m)}
  \item{cycleEvaporation}{logicl, the net evaporation should be cycled or not}
  \item{numberOfCycles}{an integer nuber: if cycleEvaporation is TRUE, the number of cycles. if is not specified it would be calculated based on the Inflow time serires}
  \item{startDate}{a vector of two elemnts whose elements include start year and strart month respectively }
  \item{reservoirCode}{the code of reservoir which this hydrometeorological object belongs to that}
}
\value{an object from class of hydrometeorology}
\author{Rezgar Arabzadeh, Parisa Aberi}
\keyword{ models }