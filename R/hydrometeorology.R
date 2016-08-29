hydrometeorology <-
function(Inflow,netEvaporation,cycleEvaporation=FALSE,numberOfCycles=NULL,startDate=c(1900,1),reservoirCode)
{
   if(missing(Inflow))
   {
      stop("Inflow is not specified !")
   }
   if(missing(netEvaporation))
   {
      stop("Evaporation is not specified !")
   }
   if(missing(reservoirCode))
   {
      stop("Reservoir code is not specified !")
   }
   if(cycleEvaporation==TRUE)
   {
      if(is.null(numberOfCycles))
      {
         numberOfCycles<-length(Inflow)/12
      }
      netEvaporation<-rep(netEvaporation,numberOfCycles)
   }
   if(length(Inflow)!=length(netEvaporation))
   {
      stop("net evaporation and inflow durations differ!")
   }
   Inflow<-ts(Inflow,start=startDate,frequency=12)
   netEvaporation<-ts(netEvaporation,start=startDate,frequency=12)
   hydromet<-list(Inflow=Inflow,
                  netEvaporation=netEvaporation,
                  reservoirCode=reservoirCode)
   class(hydromet)<-"Hydrometorology"
   return(hydromet)
}
