SOP.base <-
function(object)
{
#-----------Initialization-----------
   hydrometeorologies<-object$hydrometeorologies
   demands<-object$demands
   reservoirs<-object$reservoirs
   simulationPeriod<-object$simulationPeriod
   nReservoir<-length(reservoirs)
   nDemand<-length(demands)

#-----------Creating a reference code matrix (RCM)-----------
   demandCode<-matrix(NA,8,nDemand)
   rownames(demandCode)<-c("SupplierCode"         ,
                           "ReservoirDownstream"  ,
                           "hydrometReservoirCode",
                           "ReturnFlowFraction"   ,
                           "demandCodeNO."        ,
                           "Priority"             ,
                           "DemandTypeCode"       ,
                           "DemandDownstreamCode")
   demandName<-c()
   for(i in 1:nDemand)
   {
      demandName<-c(demandName,demands[[i]]$demandName)
   }
   colnames(demandCode)<-demandName
   for (i in 1:nReservoir)
   {
      for(j in 1:nDemand)
      {
         if(reservoirs[[i]]$reservoirCode==demands[[j]]$supplierCode)
         {
            demandCode[1,j]<-reservoirs[[i]]$reservoirCode
            demandCode[2,j]<-reservoirs[[i]]$downstream
            demandCode[4,j]<-NA
            demandCode[5,j]<-demands[[j]]$demandCode
            demandCode[6,j]<-demands[[j]]$priority
            demandCode[7,j]<-demands[[j]]$demandTypeCode
            demandCode[8,j]<-demands[[j]]$downstreamCode
            for(h in 1:length(hydrometeorologies))
            {
               if(hydrometeorologies[[h]]$reservoirCode==demands[[j]]$supplierCode)
               {
                  demandCode[3,j]<-h
               }
            }
         }
      }
   }

#-----------Sorting RCM accoring to supplier code and priorities -----------
   demandCode<-demandCode[,sort(demandCode[6,],index.return=TRUE)$ix]
   demandCode<-demandCode[,sort(demandCode[1,],index.return=TRUE)$ix]

#-----------Initialize reservoirs state variable-----------
   V <-matrix(0,simulationPeriod,nReservoir)
   Sp<-matrix(0,simulationPeriod,nReservoir)
   EV<-matrix(0,simulationPeriod,nReservoir)
   Re<-matrix(NA,simulationPeriod,nDemand)
   colnames(V)<-colnames(Sp)<-colnames(EV)<-1:nReservoir
   colnames(Re)<-colnames(demandCode)
   allDemands<-Re
   for(i in 1:nDemand)
   {
      for(j in 1:nDemand)
      {
         if(demandCode[5,i]==demands[[j]]$demandCode)
         {
            allDemands[,i]<-demands[[j]]$demand
         }
      }
   }

#-----------Determinzation of number of demand site per each supplier reservoir-----------
   nDems_Res<-c(0,cumsum(c(as.matrix(table(demandCode[1,])))))

#-----------Interpolation/Extrapolation function definition-----------
   interpolate<-function (x, y, xout) 
   {
      n <- 50
      rule <- 2
      f <- 0
      ties <- "ordered"
      d <- !duplicated(x)
      x <- x[d]
      y <- y[d]
      d <- order(x)
      x <- x[d]
      y <- y[d]
      res <- approx(x, y, xout = xout, n = n, rule = 2, f = f, ties = ties)$y
      r <- range(x)
      d <- xout < r[1]
      if (any(d)) {res[d] <- (y[2] - y[1])/(x[2] - x[1]) * (xout[d] - x[1]) + y[1]}
      d <- xout > r[2]
      n <- length(y)
      if (any(d)) {res[d] <- (y[n] - y[n - 1])/(x[n] - x[n - 1]) * (xout[d] - x[n - 1]) + y[n - 1]}
      list(x = xout, y = res)
}

#-----------Operation phase-----------
   for(t in 2:simulationPeriod) #
   {
      for(r in 1:nReservoir)
      {

#-----------Extraction of RCM for a reservoir's demands under operation-----------
         DemCode<-as.matrix(demandCode[,(1+nDems_Res[r]):(nDems_Res[r+1])])

#-----------Extraction of demand sites time series related to operating reservoir-----------
         De<-matrix(NA,simulationPeriod,diff(nDems_Res)[r])
         for(i in 1:nDemand)
         {
            if(demands[[i]]$supplierCode==DemCode[1,1])
            {
               for(j in 1:diff(nDems_Res)[r])
               {
                  if(demands[[i]]$priority==DemCode[6,j])
                  {
                     De[,j]<-demands[[i]]$demand
                  }
               }
            }
         }

#-----------Extraction of hydrometeorological time series and geometrical specifications of operating reservoir-----------
         for(i in 1:nReservoir)
         {
            if(DemCode[1,1]==hydrometeorologies[[i]]$reservoirCode)
            {
               I<-hydrometeorologies[[i]]$Inflow
               E<-hydrometeorologies[[i]]$netEvaporation
            }
         }
         for(i in 1:nReservoir)
         {
            if(DemCode[1,1]==reservoirs[[i]]$reservoirCode)
            {
               AV<-reservoirs[[i]]$geometry$volumeArea
               Smax<-reservoirs[[i]]$geometry$sMax
               Smin<-reservoirs[[i]]$geometry$sMin
               downStream<-reservoirs[[i]]$downstream
            }
         }

#-----------Operation using balance equation-----------
         if(t == 2)
         {
            V[1,r]<-Smax
         }

         if((V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1])>Smax)
         {
            Re[t-1,(1+nDems_Res[r]):(nDems_Res[r+1])]<-De[t-1,]
            EV[t-1,r]<-E[t-1]*interpolate(x=AV[,2],y=AV[,1],xout=V[t-1,r])$y
            Sp[t-1,r]<-ifelse((V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1]-Smax-EV[t-1,r])>0,(V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1]-Smax-EV[t-1,r]),0)
            V[t,r] <-Smax-EV[t-1,r]
         }

         if(((V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1])<Smax) && ((V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1])>Smin))
         {
            Re[t-1,(1+nDems_Res[r]):(nDems_Res[r+1])]<-De[t-1,]
            EV[t-1,r]<-E[t-1]*interpolate(x=AV[,2],y=AV[,1],xout=V[t-1,r])$y
            Sp[t-1,r]<-0
            V[t,r]<-V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1]-EV[t-1,r]
         }

         if((V[t-1,r]+I[t-1]-apply(De,1,sum)[t-1])<Smin)
         {
            id<-which(diff(duplicated((V[t-1,r]+I[t-1]-cumsum(De[t-1,])-Smin)<0))==1)
            if (length(id) == 0)
            {
               id<-ncol(DemCode)
            }
            i<-0
            Vtemp <-V[t-1,r]+I[t-1]
            while(i<id)
            {
               if(Vtemp-De[t-1,i+1]>Smin)
               {
                  Re[t-1,i+(1+nDems_Res[r])]<-De[t-1,i+1]
               }else
               {
                  Re[t-1,i+(1+nDems_Res[r])]<-Vtemp-Smin
               }
               Vtemp <-Vtemp-Re[t-1,i+(1+nDems_Res[r])]
               i<-i+1
            }
            EV[t-1,r]<-E[t-1]*interpolate(x=AV[,2],y=AV[,1],xout=Vtemp)$y
            Sp[t-1,r]<-0
            if(id < ncol(DemCode))
            {
               Re[t-1,(id+nDems_Res[r]+1):(nDems_Res[r+1])]<-0
            }
            V[t,r] <-Vtemp-EV[t-1,r]
         }

         if(downStream != 0)
         {
            for(i in 1:nReservoir)
            {
               if(downStream==hydrometeorologies[[i]]$reservoirCode)
               {
                  hydrometeorologies[[i]]$Inflow[t-1]<-hydrometeorologies[[i]]$Inflow[t-1]+Sp[t-1,r]
               }
            }
         }
      }
   }

#-----------Final time step operation-----------
   for(r in 1:nReservoir)
   {

#-----------Extraction of RCM for a reservoir's demands under operation-----------
      DemCode<-as.matrix(demandCode[,(1+nDems_Res[r]):(nDems_Res[r+1])])

#-----------Extraction of demand sites time series related to operating reservoir-----------
      De<-matrix(NA,simulationPeriod,diff(nDems_Res)[r])
      for(i in 1:nDemand)
      {
         if(demands[[i]]$supplierCode==DemCode[1,1])
         {
            for(j in 1:diff(nDems_Res)[r])
            {
               if(demands[[i]]$priority==DemCode[6,j])
               {
                  De[,j]<-demands[[i]]$demand
               }
            }
         }
      }

#-----------Extraction of hydrometeorological time series and geometrical specifications of operating reservoir-----------
      for(i in 1:nReservoir)
      {
         if(DemCode[1,1]==hydrometeorologies[[i]]$reservoirCode)
         {
            I<-hydrometeorologies[[i]]$Inflow
            E<-hydrometeorologies[[i]]$netEvaporation
         }
      }
      for(i in 1:nReservoir)
      {
         if(DemCode[1,1]==reservoirs[[i]]$reservoirCode)
         {
            AV<-reservoirs[[i]]$geometry$volumeArea
            Smax<-reservoirs[[i]]$geometry$sMax
            Smin<-reservoirs[[i]]$geometry$sMin
            downStream<-reservoirs[[i]]$downstream
         }
      }

#-----------Operation using balance equation-----------

      if((V[t,r]+I[t]-apply(De,1,sum)[t])>Smax)
      {
         Re[t,(1+nDems_Res[r]):(nDems_Res[r+1])]<-De[t,]
         EV[t,r]<-E[t]*interpolate(x=AV[,2],y=AV[,1],xout=V[t,r])$y
         Sp[t,r]<-ifelse((V[t,r]+I[t]-apply(De,1,sum)[t]-Smax-EV[t,r])>0,(V[t,r]+I[t]-apply(De,1,sum)[t]-Smax-EV[t,r]),0)
         V[t,r] <-Smax-EV[t,r]
      }

      if(((V[t,r]+I[t]-apply(De,1,sum)[t])<Smax) && ((V[t,r]+I[t]-apply(De,1,sum)[t])>Smin))
      {
         Re[t,(1+nDems_Res[r]):(nDems_Res[r+1])]<-De[t,]
         EV[t,r]<-E[t]*interpolate(x=AV[,2],y=AV[,1],xout=V[t,r])$y
         Sp[t,r]<-0
         V[t,r]<-V[t,r]+I[t]-apply(De,1,sum)[t]-EV[t,r]
      }

      if((V[t,r]+I[t]-apply(De,1,sum)[t])<Smin)
      {
         id<-which(diff(duplicated((V[t,r]+I[t]-cumsum(De[t,])-Smin)<0))==1)
         if (length(id) == 0)
         {
            id<-ncol(DemCode)
         }
         i<-0
         Vtemp <-V[t,r]+I[t]
         while(i<id)
         {
            if(Vtemp-De[t,i+1]>Smin)
            {
               Re[t,i+(1+nDems_Res[r])]<-De[t,i+1]
            }else
            {
               Re[t,i+(1+nDems_Res[r])]<-Vtemp-Smin
            }
            Vtemp <-Vtemp-Re[t,i+(1+nDems_Res[r])]
            i<-i+1
         }
         EV[t,r]<-E[t]*interpolate(x=AV[,2],y=AV[,1],xout=Vtemp)$y
         Sp[t,r]<-0
         if(id < ncol(DemCode))
         {
            Re[t,(id+nDems_Res[r]+1):(nDems_Res[r+1])]<-0
         }
         V[t,r] <-Vtemp-EV[t,r]
      }

      if(downStream != 0)
      {
         for(i in 1:nReservoir)
         {
            if(downStream==hydrometeorologies[[i]]$reservoirCode)
            {
               hydrometeorologies[[i]]$Inflow[t]<-hydrometeorologies[[i]]$Inflow[t]+Sp[t,r]
            }
         }
      }
   }

   return(list(Storage     = V         ,
               Spill       = Sp        ,
               Evaporation = EV        ,
               Release     = Re        ,
               Demand      = allDemands))
}
