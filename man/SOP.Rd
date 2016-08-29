\name{SOP}
\alias{SOP}
\title{Standard Operation Policy}
\description{function for reservoir(s) system(s) operation using Standard Operation Policy}
\usage{
SOP(object)
}
\arguments{
  \item{object}{an object from class of createArea}
}
\value{an object inherited from class SOP}
\references{
Yeh, W. W. G. (1985). Reservoir management and operations models: A state of the art review. Water resources research, 21(12), 1797-1818.
}
\author{Parisa Aberi, Rezgar Arabzadeh, Shahab Araghinejad}
\examples{
# loading an area
data (object)
\dontrun{res<-SOP(object)}
\dontrun{plot(res)}
\dontrun{Yeild(res)}
}
\keyword{ models }