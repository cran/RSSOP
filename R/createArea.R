createArea <-
function(name="unknown",location="unknown",start=c(),end=c())
{
   if(is.null(start))
   {
      stop("Simulation start date is not specified !")
   }
   if(is.null(end))
   {
      stop("Simulation end date is not specified !")
   }
   reservoirs        <-list()
   hydrometeorologies<-list()
   demands           <-list()
   duration<-(end[1]-start[1])*12+(end[2]-start[2])
   if(duration<0)
   {
      stop("The enterd dates are not accuarte !")
   }
   area<-list(name=name,
              location=location,
              reservoirs=reservoirs,
              hydrometeoroloies=hydrometeorologies,
              demands=demands,
              simulationPeriod=duration)
   class(area)<-"area"
   return(area)
}
