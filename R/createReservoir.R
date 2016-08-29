createReservoir <-
function(type         ="storage"            ,
         name         ="resrvoir1"          ,
         reservoirCode=1                    ,
         downstreamReservoirCode            ,
         geometry     =list(sMin      =NULL ,
                            sMax      =NULL ,
                            volumeArea=NULL))
{
   if(missing(downstreamReservoirCode))
   {
      stop("downstream is not delineated !")
   }
   if(exists("geometry$volumeArea"))
   {
      stop("Volume area curve is not specified !")
   }
   if(ncol(geometry$volumeArea)!=2)
   {
      stop("Volume or Area of reservoir geometry is missing!")
   }
   if(exists("geometry$sMin"))
   {
      stop("Minimum storage is not specified !")
   }
   if(exists("geometry$sMax"))
   {
      stop("Maximum storage is not specified !")
   }
   if(geometry$sMin>geometry$sMax)
   {
      stop("Minimum storage cannot be greater than maximum storage!")
   }

   if(type=="storage")
   {
      typeCode<-1
      reservoir<-list(type=type,
                      name=name,
                      reservoirCode=reservoirCode,
                      typeCode=typeCode,
                      downstream=downstreamReservoirCode,
                      geometry=geometry)
   }   
   class(reservoir)<-"reservoir"
   return(reservoir)
}
