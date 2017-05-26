gplotFunction.Correl <- function(xDat, yDat, genID = "", sexID = "", ColInput, NameInput, xLim = "", yLim = ""){
  xDat <- xDat[which(xDat$Animal %in% yDat$Animal),]
  yDat <- yDat[which(yDat$Animal %in% xDat$Animal),]
  
  xDat <- xDat[order(xDat$Animal),]
  yDat <- yDat[order(yDat$Animal),]
  
  xDat <- xDat[xDat$Generation == genID,]
  yDat <- yDat[yDat$Generation == genID,]
  
  xDat.Name <- names(xDat)[7]
  yDat.Name <- names(yDat)[7]
  
  if(sexID == ""){
  } else if (sexID != ""){
    xDat <- xDat[xDat$Sex == sexID,]
    yDat <- yDat[yDat$Sex == sexID,]
  }
  
  if(sexID == ""){
    sexName <- "Combined Sexe"
  } else if (sexID != ""){
    sexName <- sexID
  }
  
  Data.Plot <- cbind(xDat[,1:7], yDat[,7], xDat[,8])
  names(Data.Plot)[8:9] <- c(names(yDat[7]), names(xDat[8]))
  
  #Begin plotting
  #Set regression data set
  gM <- ggplot(Data.Plot ,aes(x = Data.Plot[,xDat.Name], y = Data.Plot[,yDat.Name], color = Data.Plot$GraphID))
  gM <- gM + scale_color_manual(name = "", values = ColInput, labels = NameInput)
  gM <- gM + geom_smooth(method = lm, se = F, linetype = 8)
  #Set point data set
  gM <- gM + geom_point(mapping = aes(x = Data.Plot[,xDat.Name], y = Data.Plot[,yDat.Name], fill = Data.Plot$GraphID),shape = 21, size = 3, color = "black")
  gM <- gM + scale_fill_manual(name = "", values = ColInput, labels = NameInput)
  #Alter Aesthetics
  gM <- gM + theme_graph_correlation
  gM <- gM + ylab(yDat.Name) + xlab(xDat.Name) + ggtitle(paste(sexName, "s", sep = ""))
  gM <-gM + scale_y_continuous(expand = c(0,0), limits = yLim)
  gM <-gM + scale_x_continuous(expand = c(0,0), limits = xLim)
  
  return(gM)
}