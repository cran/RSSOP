plot.SOP <-
function(x,...)
{
   readkeygraph <- function(prompt)
   {
      getGraphicsEvent(prompt = prompt, 
                       onMouseDown = NULL,
                       onMouseMove = NULL,
                       onMouseUp = NULL,
                       onKeybd = onKeybd,
                       consolePrompt = "[press any key to continue.....]")
      Sys.sleep(0.01)
      return(keyPressed)
   }
   onKeybd <- function(key) keyPressed <<- key
   
   release    <-x$operation$Release
   demand     <-x$operation$Demand
   evaporation<-x$operation$Evaporation
   spill      <-x$operation$Spill
   storage    <-x$operation$Storage
   nReservoir <-ncol(storage)
   nDemand    <-ncol(release)
   for (i in 1:nDemand)
   {
      plot(release[,i]                ,
           xlab="Time interval"       ,
           ylab="Release volume (MCM)",
           type="l"                   ,
           main="Release")
      grid()
      keyPressed = readkeygraph("[press any key to continue.....]")    
   }
   for(i in 1:nReservoir)
   {
      plot(evaporation[,i]                ,
           xlab="Time interval"           ,
           ylab="Evaporation volume (MCM)",
           type="l"                       ,
           main="Evaporation")
      grid()
      keyPressed = readkeygraph("[press any key to continue.....]")    
   }
   for(i in 1:nReservoir)
   {
      plot(spill[,i]                  ,
           xlab="Time interval"       ,
           ylab="Spill volume (MCM)"  ,
           type="l"                   ,
           main="Spill")
      grid()
      keyPressed = readkeygraph("[press any key to continue.....]")    
   }
   for(i in 1:nReservoir)
   {
      plot(storage[,i]                ,
           xlab="Time interval"       ,
           ylab="Storage volume (MCM)",
           type="l"                   ,
           main="Storage")
      grid()
      keyPressed = readkeygraph("[press any key to continue.....]")
   }
}
