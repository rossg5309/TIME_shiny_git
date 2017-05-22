gplotFunction.Colors <- function(data = "", generation = "", YLabel = "", YLimit = "", ColInput = "", NameInput = ""){
  Summary.Data <- data
  
  if(generation %in% Summary.Data$Generation == F){
    return("No Data Available")
  }
  if(generation %in% Summary.Data$Generation == T){
    source("server_plot_theme.R")
    
    Summary.Data <- Summary.Data[Summary.Data$Generation == generation,]
    g<-ggplot(data = Summary.Data, aes(x=Sex, y=Mean, fill=GraphID))
    g<-g + geom_bar(position=position_dodge(), stat="identity")
    g<-g + geom_bar(position=position_dodge(), color = 'black', stat="identity", show.legend = F)
    g<-g + geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width = .2, position=position_dodge(.9))
    g<-g + theme_graph
    g<-g + ylab(YLabel)
    g<-g + scale_y_continuous(expand = c(0,0), limits = c(0, YLimit))
    g<-g + scale_fill_manual(name = "", values = ColInput, labels = NameInput)
  }
}