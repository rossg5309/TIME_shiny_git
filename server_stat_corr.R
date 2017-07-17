Correl.stat <- function(xDat, yDat, genID = "", sexID = "", ColInput, NameInput, xLim = "", yLim = "", Corr.Method = "pearson"){
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
  
  Unique.Groups <- as.character(sort(unique(Data.Plot$GraphID)))
  
  Corr.Results <- matrix(data = NA, nrow = length(Unique.Groups), ncol = 4)
  Corr.Results <- data.frame(Corr.Results)
  names(Corr.Results) <- c('Group', 'r', 'N', 'p-value')
  
  for(i in 1:length(Unique.Groups)){
    Group <- Unique.Groups[i]
    
    Corr.Results[i,1] <- Group
    
    Cur.xDat <- Data.Plot[which(Data.Plot$GraphID == Group),7]
    Cur.yDat <- Data.Plot[which(Data.Plot$GraphID == Group),8]
    
    if(length(Cur.xDat >= 5)){
      Cur.rcorr <- rcorr(Cur.xDat, Cur.yDat, type = Corr.Method)
      Corr.Results[i, 2] <- round(rcorr(Cur.xDat, Cur.yDat)$r[2], digits = 4)
      Corr.Results[i, 3] <- round(rcorr(Cur.xDat, Cur.yDat)$n[2], digits = 4)
      Corr.Results[i, 4] <- round(rcorr(Cur.xDat, Cur.yDat)$P[2], digits = 4)
    } else if (length(Cur.xDat <= 4)) {
      Corr.Results[i, 2] <- "NA"
      Corr.Results[i, 3] <- "NA"
      Corr.Results[i, 4] <- "NA"
    }
  }
  
  return(Corr.Results)
}