ANOVA.shiny<-function(Data.Org, genID = "", sexID = "", Measure = "", Factor1, Factor2 = "", Factor3 = "", Min.Count = "",SSType = 3){
  #Alter environment variables needed for analysis, record current for reset
  Data.Org <- Data.Org[Data.Org$Generation == genID,]
  if(sexID != ""){
    Data.Org <- Data.Org[Data.Org$Sex == sexID,]
    Factor1 <- Factor2
    Factor2 <- ""
    
    SSType <- 2
  }
  
  Cur.Option<-getOption("contrasts")
  options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
  
  #Determine Column of factors entered
  N.Fact.1<-which(names(Data.Org) == Factor1)
  N.Fact.2<-which(names(Data.Org) == Factor2)
  N.Fact.3<-which(names(Data.Org) == Factor3)
  
  #Determine which data to perform analysis on
  Col.Use <- which(names(Data.Org) == Measure)
  Col.Names <- names(Data.Org)[Col.Use]
  N.Col<-length(Col.Use)
  
  #Find number of factors
  Factors<-cbind(Factor1, Factor2, Factor3, "")
  Factors<-Factors[-which(!nzchar(Factors))]
  N.Factors<-length(Factors)
  
  #Create vector of unique group names
  if(N.Factors == 3){
    Bart.Groups<-paste(Data.Org[,Factor1], Data.Org[,Factor2], Data.Org[,Factor3])
    Plot.Groups<-paste(Data.Org[,Factor1], Data.Org[,Factor2], Data.Org[,Factor3], sep = "\n")
    Data.Org$Groups.QQ.3<-paste(Data.Org[,Factor1], Data.Org[,Factor2])
  } else if(N.Factors == 2){
    Bart.Groups<-paste(Data.Org[,Factor1], Data.Org[,Factor2])
    Plot.Groups<-paste(Data.Org[,Factor1], Data.Org[,Factor2], sep = "\n")
  } else if(N.Factors == 1){
    Bart.Groups<-paste(Data.Org[,Factor1])
    Plot.Groups<-paste(Data.Org[,Factor1])
  }
  
  #Determine the combined unique groups
  Un.Groups<-unique(Bart.Groups)
  Un.Plot.Groups<-unique(Plot.Groups)
  Data.Org$Groups.Plot <- Plot.Groups
  Data.Org$Groups.QQ <- Bart.Groups
  
  #Build results sheet depending on input
  Rows.Results<-N.Col*2^N.Factors
  Results.AOV<-matrix(data = NA, nrow = Rows.Results, ncol = 11)
  Results.AOV<-data.frame(Results.AOV)
  
  #Add names to the table
  names(Results.AOV)<-c("Comparison", "Deg-F", sprintf("Sum of Squares (Type %s)", SSType), "Mean of Squares", "F-Value", "P-Value", "Measure", "Shapiro-Wilk", "Bartlett's", "Part-Eta-Sq", "Stat Report")
  
  #Add comparisons to table
  if(N.Factors == 3){
    Results.AOV[seq(from = 1, to = Rows.Results, by = 2^N.Factors),1]<-Factors[1]
    Results.AOV[seq(from = 2, to = Rows.Results, by = 2^N.Factors),1]<-Factors[2]
    Results.AOV[seq(from = 3, to = Rows.Results, by = 2^N.Factors),1]<-Factors[3]
    Results.AOV[seq(from = 4, to = Rows.Results, by = 2^N.Factors),1]<-paste(Factors[1], Factors[2], sep = ":")
    Results.AOV[seq(from = 5, to = Rows.Results, by = 2^N.Factors),1]<-paste(Factors[1], Factors[3], sep = ":")
    Results.AOV[seq(from = 6, to = Rows.Results, by = 2^N.Factors),1]<-paste(Factors[2], Factors[3], sep = ":")
    Results.AOV[seq(from = 7, to = Rows.Results, by = 2^N.Factors),1]<-paste(Factors[1], Factors[2], Factors[3], sep = ":")
    Results.AOV[seq(from = 8, to = Rows.Results, by = 2^N.Factors),1]<-"Residuals"
  } else if(N.Factors == 2){
    Results.AOV[seq(from = 1, to = Rows.Results, by = 2^N.Factors),1]<-Factors[1]
    Results.AOV[seq(from = 2, to = Rows.Results, by = 2^N.Factors),1]<-Factors[2]
    Results.AOV[seq(from = 3, to = Rows.Results, by = 2^N.Factors),1]<-paste(Factors[1], Factors[2], sep = ":")
    Results.AOV[seq(from = 4, to = Rows.Results, by = 2^N.Factors),1]<-"Residuals"
  } else if(N.Factors == 1){
    Results.AOV[seq(from = 1, to = Rows.Results, by = 2^N.Factors),1]<-Factors[1]
    Results.AOV[seq(from = 2, to = Rows.Results, by = 2^N.Factors),1]<-"Residuals"
  }
  
  #Build the model depending on the number of factors
  if(N.Factors == 3){
    AOV.Form<-Data.Org[,Cur.Measure] ~ Data.Org[,Factor1]*Data.Org[,Factor2]*Data.Org[,Factor3]
    Plot.Form<-Data.Org[,Cur.Measure] ~ interaction(Data.Org[,Factor1], Data.Org[,Factor2], Data.Org[,Factor3])
  } else if(N.Factors == 2){
    AOV.Form<-Data.Org[,Cur.Measure] ~ Data.Org[,Factor1]*Data.Org[,Factor2]
    Plot.Form<-Data.Org[,Cur.Measure] ~ interaction(Data.Org[,Factor1], Data.Org[,Factor2])
  } else if(N.Factors == 1){
    AOV.Form<-Data.Org[,Cur.Measure] ~ Data.Org[,Factor1]
    Plot.Form<-Data.Org[,Cur.Measure] ~ Data.Org[,Factor1]
  }
  
  #Begin Analysis
  Cur.Measure <- Measure
  Cur.Row<-seq(from = 1, to = Rows.Results, by = 2^N.Factors)[1]
  End.Row<-2^N.Factors + Cur.Row - 1
  
  Cur.AOV<-Anova(lm(AOV.Form, data = Data.Org), type = SSType)
  
  if(SSType == 2 | SSType == "II"){
    Results.AOV[Cur.Row:End.Row,2]<-Cur.AOV[2][[1]]
    Results.AOV[Cur.Row:End.Row,3]<-round(Cur.AOV[1][[1]], digits = 3)
    Results.AOV[Cur.Row:End.Row,4]<-round(Cur.AOV[1][[1]], digits = 3)
    Results.AOV[Cur.Row:End.Row,5]<-round(Cur.AOV[3][[1]], digits = 4)
    Results.AOV[Cur.Row:End.Row,6]<-round(Cur.AOV[4][[1]], digits = 4)
    Results.AOV[Cur.Row:End.Row,7]<-Cur.Measure
  }
  if(SSType == 3 | SSType == "III"){
    Results.AOV[Cur.Row:End.Row,2]<-Cur.AOV[2][[1]][-1]
    Results.AOV[Cur.Row:End.Row,3]<-round(Cur.AOV[1][[1]][-1], digits = 3)
    if(N.Factors == 1){
      Results.AOV[Cur.Row:End.Row,4]<-round(Cur.AOV[1][[1]][c(-1, -5)], digits = 3)
    }
    if(N.Factors == 2 | N.Factors == 3){
      Results.AOV[Cur.Row:End.Row,4]<-c(round(Cur.AOV[1][[1]][c(-1, -5)], digits = 3), NA)
    }
    Results.AOV[Cur.Row:End.Row,5]<-round(Cur.AOV[3][[1]][-1], digits = 4)
    Results.AOV[Cur.Row:End.Row,6]<-round(Cur.AOV[4][[1]][-1], digits = 4)
    Results.AOV[Cur.Row:End.Row,7]<-Cur.Measure
  }
  
  if(Min.Count >= 4){
    Cur.Shapiro<-shapiro.test(Data.Org[,Cur.Measure])$p.value
    Results.AOV[Cur.Row,8]<-round(Cur.Shapiro, digits = 4)
    
    Cur.Bart<-bartlett.test(Data.Org[,Cur.Measure] ~ Bart.Groups, data = Data.Org)$p.value
    Results.AOV[Cur.Row,9]<-round(Cur.Bart, digits = 4)
    
    Cur.EtaSq<-as.numeric(etaSquared(lm(AOV.Form, Data.Org), type = SSType)[,2])
    Cur.EtaSq.Need<-length(Results.AOV[Cur.Row:End.Row,10])
    Cur.EtaSq.Len<-length(Cur.EtaSq)
    Cur.EtaSq.Rep<-Cur.EtaSq.Need - Cur.EtaSq.Len
    Results.AOV[Cur.Row:End.Row,10]<-c(round(Cur.EtaSq, digits = 4), rep(NA, Cur.EtaSq.Rep)) 
  }
  
  #Reset environment options
  options(contrasts=Cur.Option)
  
  #Output
  return(Results.AOV)
}
