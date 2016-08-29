createDemandSite <-
function(type              ="agricultural",
         demandName        ="Agri1"       ,
         demandCode                       ,
         annualUseRate                    ,
         annualVariation                  ,
         area                             ,
         cycle             =FALSE         ,
         numberOfCycles    =NULL          ,
         supplierCode                     ,
         downstreamCode                   ,
         priority          =1             ,
         start             =1900)
{
   if(missing(supplierCode))
   {
      stop("Supplier code is not specified !")
   }
   if(missing(downstreamCode))
   {
      stop("downstream code is not specified !")
   }
   if(missing(demandCode))
   {
      stop("Demand code is not specified !")
   }

   if(type=="agricultural")
   {
      if(missing(annualUseRate))
      {
         stop("Annual use rate is not specified !")
      }
      if(missing(annualVariation))
      {
         stop("Annual variation is not specified !")
      }
      if(missing(area))
      {
         stop("Coverage area is not specified !")
      }
      if(sum(annualVariation)!=100)
      {
         stop("Sum of annual variation must be equal to 100 percent !")
      }
      if(cycle)
      {
         if(length(annualUseRate)!=1)
         {
            stop("Annual use rate must be constant while cycling is assumed !")
         }
         demand<-rep(annualUseRate*annualVariation*area/100,numberOfCycles)
      }else{
         demand<-c(t(as.matrix(annualUseRate)%*%t(as.matrix(annualVariation)/100)*area))
      }
      demandTypeCode<-1
   }

   if(type=="industerial")
   {
      if(missing(annualUseRate))
      {
         stop("Annual use rate is not specified !")
      }
      if(missing(annualVariation))
      {
         stop("Annual variation is not specified !")
      }
      if(sum(annualVariation)!=100)
      {
         stop("Sum of annual variation must be equal to 100 percent !")
      }
      if(cycle)
      {
         if(length(annualUseRate)!=1)
         {
            stop("Annual use rate must be constant while cycling is assumed !")
         }
         demand<-rep(annualUseRate*annualVariation/100,numberOfCycles)
      }else{
         demand<-c(t(as.matrix(annualUseRate)%*%t(as.matrix(annualVariation)/100)))
      }
      demandTypeCode<-2
   }

   if(type=="domestic")
   {
      if(missing(annualUseRate))
      {
         stop("Annual use rate is not specified !")
      }
      if(missing(annualVariation))
      {
         stop("Annual variation is not specified !")
      }
      if(sum(annualVariation)!=100)
      {
         stop("Sum of annual variation must be equal to 100 percent !")
      }
      if(cycle)
      {
         if(length(annualUseRate)!=1)
         {
            stop("Annual use rate must be constant while cycling is assumed !")
         }
         demand<-rep(annualUseRate*annualVariation/100,numberOfCycles)
      }else{
         demand<-c(t(as.matrix(annualUseRate)%*%t(as.matrix(annualVariation)/100)))
      }
      demandTypeCode<-3
   }


   if(type=="environmental")
   {
      if(missing(annualUseRate))
      {
         stop("Annual use rate is not specified !")
      }
      if(missing(annualVariation))
      {
         stop("Annual variation is not specified !")
      }
      if(sum(annualVariation)!=100)
      {
         stop("Sum of annual variation must be equal to 100 percent !")
      }
      if(cycle)
      {
         if(length(annualUseRate)!=1)
         {
            stop("Annual use rate must be constant while cycling is assumed !")
         }
         demand<-rep(annualUseRate*annualVariation/100,numberOfCycles)
      }else{
         demand<-c(t(as.matrix(annualUseRate)%*%t(as.matrix(annualVariation)/100)))
      }
      demandTypeCode<-4
   }
   demand<-ts(demand,start=start,frequency=12)
   demandSite<-list(type=type,
                    demandTypeCode    =demandTypeCode    ,
                    demandName        =demandName        ,
                    demandCode        =demandCode        ,
                    supplierCode      =supplierCode      ,
                    downstreamCode    =downstreamCode    ,
                    priority          =priority          ,
                    demand            =demand)
   class(demandSite)<-"demand site"
   return(demandSite)
}
