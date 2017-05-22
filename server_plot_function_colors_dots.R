gplotFunction.Colors <- function(data = "", generation = "", YLabel = "", YLimit = "", ColInput = "", NameInput = "", dotdata = "", plotdot = F){
  Summary.Data <- data
  Data.Set <- dotdata
  
  if(generation %in% Summary.Data$Generation == F){
    return("No Data Available")
  }
  if(generation %in% Summary.Data$Generation == T){
    source("server_plot_theme.R")
    
    Summary.Data <- Summary.Data[Summary.Data$Generation == generation,]
    Data.Set <- Data.Set[Data.Set$Generation == generation,]
    
    g<-ggplot(data = Summary.Data, aes(x=Sex, y=Mean, fill=GraphID))
    g<-g + geom_bar(position=position_dodge(), stat="identity")
    g<-g + geom_bar(position=position_dodge(), color = 'black', stat="identity", show.legend = F)
    
    if(plotdot == F){
    } else if (plotdot == T) {
      g<-g + geom_point(data = Data.Set, aes(x=Sex, y=Data.Set[,YLabel], fill = Data.Set$GraphID), position = position_jitterdodge(dodge.width=0.9, jitter.width =.05), shape = 20, size = 1, na.rm = T, show.legend = FALSE) 
    }
    
    g<-g + geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width = .2, position=position_dodge(.9))
    g<-g + theme_graph
    g<-g + ylab(YLabel)
    
    if(YLimit > 0){
      g<-g + scale_y_continuous(expand = c(0,0), limits = c(0, YLimit))
    }
    if(YLimit < 0){
      g<-g + scale_y_continuous(expand = c(0,0), limits = c(YLimit,0))
    }
    
    
    g<-g + scale_fill_manual(name = "", values = ColInput, labels = NameInput)
  }
}