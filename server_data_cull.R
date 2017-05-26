Measure.Cull <- function(Data.Set.Cull, Groups.F1, Groups.F4){
  #Begin selection of groups for processing
  #Create Template for culled data insertion
  Data.Set.Temp <- matrix(data = NA, nrow = 1, ncol = ncol(Data.Set.Cull))
  Data.Set.Temp <- data.frame(Data.Set.Temp)
  names(Data.Set.Temp) <- names(Data.Set.Cull)
  
  #Determine groups wanted in graphing
  Treatment.Sel <- c(Groups.F1, Groups.F4)
  
  ##Build new data set with the groups specified
  for(i in 1:length(Treatment.Sel)){
    Data.Set.Temp <- rbind(Data.Set.Temp, Data.Set.Cull[Data.Set.Cull$Treatment == Treatment.Sel[i],])
  }
  
  #Replace the active data set with culled data set
  Data.Set.Out <- Data.Set.Temp
  Data.Set.Out <- Data.Set.Temp[-1,]
  
  #Replace data set with NAs removed
  Data.Set.Out <- Data.Set.Out[rowSums(is.na(Data.Set.Out[,]))==0,]
  
  return(Data.Set.Out)
}