addObjectToArea <-
function(area,object,type)
{
   if(missing(area))
   {
      stop("Area is missing !")
   }
   if(missing(object))
   {
      stop("The object you want to be added to area is missing !")
   }
   if(missing(type))
   {
      stop("The type of object is missing !")
   }
   
   if(type=="reservoir")
   {
      i<-length(area$reservoirs)+1
      area$reservoirs[[i]]<-object
   }

   if(type=="hydrometeorology")
   {
      if(length(object$Inflow)!=area$simulationPeriod)
      {
         stop("The simulation period and hydrometorology time series durations miss match !")
      }
      i<-length(area$hydrometeorologies)+1
      area$hydrometeorologies[[i]]<-object
   }

   if(type=="demand")
   {
      if(object$type=="agricultural" || object$type=="environmental" || object$type=="domestic")
      {
         if(length(object$demand)!=area$simulationPeriod)
         {
            stop("The simulation period and demand time series durations miss match !")
         }
      }      
      i<-length(area$demands)+1
      area$demands[[i]]<-object
   }
   
   return(area)
}
