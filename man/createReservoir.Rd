\name{createReservoir}
\alias{createReservoir}
\title{Creates A Reservoir Object}
\description{this function creates a reservoir object usning geometrical specifications}
\usage{
createReservoir(type = "storage",
                name = "resrvoir1",
                reservoirCode = 1,
                downstreamReservoirCode,
                geometry = list(sMin=NULL, sMax=NULL, volumeArea=NULL))
}
\arguments{
  \item{type}{the type of reservoir, "storage" and "hydropower". hydropower is not available in this version}
  \item{name}{a string representing the name of reservoir object}
  \item{reservoirCode}{an unique integer code}
  \item{downstreamReservoirCode}{the code of reservoir downstream}
  \item{geometry}{a list, include maximum and minimum volumes of reservoir and a data frame for volume-area rating curve whose first column includes area (square KM) and the second column is Volume (MCM)}
}
\value{an object inherited from class createReservoir}
\author{Rezgar Arabzadeh, Parisa Aberi, Kaveh Panaghi}
\keyword{ models }