SOP.default <-
function(object)
{
   if (missing(object))
   {
      stop("Missing SOP object!")
   }
   result<-list()
   operation<-SOP.base(object)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"SOP"
   return(result)
}
