Yeild <-
function(object,s.const=0.95)
{
   R<-object$operation$Release
   D<-object$operation$Demand
   Base<-function(Re,De)
   {
      Criaterian<-rep(0,3)
      names(Criaterian)<-c("Vulnerability","Reliability","Resiliency")
  
      if(sum(De==0)>0)
      {
        De[which(De==0)]<-0.001
        cat("some demands were equal to zero, they were set to 0.001 !!","\n")
      }
      T<-length(Re)
      failure<-rep(NA,T)
      
      # Vulnerability
      Criaterian[1]<-sum((De-Re)/De)
      
      # Reliability
      Criaterian[2]<-1-sum(Re<s.const*De)/T
      
      # Resiliency
      failure[which(Re<s.const*De)]<-0
      failure[which(Re>s.const*De)]<-1
      f<-sum(diff(failure)==1)
      F<-sum(failure==0)
      Criaterian[3]<-f/F
      
      return (Criaterian)
   }
   C<-matrix(NA,3,ncol(R))
   rownames(C)<-c("Vulnerability","Reliability","Resiliency")
   colnames(C)<-colnames(R)
   for(i in 1:ncol(R))
   {
      C[,i]<-Base(Re=R[,i],De=D[,i])
   }
   return(C)
}
