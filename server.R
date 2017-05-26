#Load Necessary Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RMySQL)
library(lsr)
library(car)

#Source local scripts
#source(file = "server_plot_theme.R", local = T)
source(file = "server_plot_function_colors_dots.R", local = T)
source(file = "server_SQL_query.R", local = T)
source(file = "server_ANOVA.R", local = T)
source(file = "server_data_cull.R", local = T)
source(file = "server_plot_correlation.R", local = T)
source(file = "server_plot_correlation_theme.R", local = T)

#Find available tables on the database
Table.List <- SQL_db_query(query = "SHOW TABLES")

#Begin the shiny sever session
shinyServer(function(input, output, session) {
  
  #Update Data Set selection input
  updateSelectInput(
    session,
    inputId = "table.sel",
    choices = Table.List[[1]],
    selected = Table.List[[1]][1]
  )
  
  #Retrieve the data set selected
  Data.Set.Sel <- reactive({
    if(input$table.sel == ""){
    } else {
      #Retrieve the dataset
      Data.Set.Sel <- SQL_db_query(sprintf("SELECT * FROM `%s`", input$table.sel))
    }
  })
  
  #Render Correlations Menu and send to UI
  observe({
    if(input$correl.act == TRUE){
      output$correl.menu <- renderMenu({
        fluidRow(
          radioButtons(
            inputId = "correl.sex.sel",
            label = "Choose Sex:",
            choices = c("Combined Sex" = "", "Males" = "Male", "Females" = "Female"),
            inline = FALSE
          ),
          selectInput(
            inputId <- "correl.table.sel",
            label = "Data Set",
            choices = Table.List[[1]],
            selected = Table.List[[1]][1]
          ),
          selectInput(
            inputId <- "correl.measure.sel",
            label = "Measure",
            choices = "",
            selected = ""
          )
        )
      })
    } else {
      output$correl.menu <- renderMenu({
        fluidRow()
      })
    }
  })
  
  #Retrieve the data set for correlations
  Data.Set.Sel.Correl <- reactive({
    if(is.null(input$correl.table.sel)){
    } 
    if(!is.null(input$correl.table.sel)){
      #Retrieve the dataset
      Data.Set.Sel.Correl <- SQL_db_query(sprintf("SELECT * FROM `%s`", input$correl.table.sel))
    }
  })
  
  #Update measures selections based on input dataset
  observe({
    if(input$table.sel == ""){
    } else {
      #Obtain the data set
      Data.Set <- Data.Set.Sel()
      
      #Find the relevant columns for graphing
      Col.Class<-lapply(Data.Set, class)
      Col.Data<-min(which(Col.Class == 'numeric' | Col.Class == 'integer'))
      Col.Use<-c(Col.Data:ncol(Data.Set))
      Measures.All<-names(Data.Set)[Col.Use]
      
      #Update the measures selection choices
      updateSelectInput(
        session,
        inputId =  "measure.sel",
        choices = Measures.All
      )
    }
  })
  
  #Update measure selection for correlations based on input dataset
  observe({
    if(!is.null(input$correl.table.sel)){
      if(input$correl.table.sel == ""){
      } else if (input$correl.act){
        #Obtain the data set
        Data.Set.Correl <- Data.Set.Sel.Correl()

        #Find the relevant columns for graphing
        Col.Class<-lapply(Data.Set.Correl, class)
        Col.Data<-min(which(Col.Class == 'numeric' | Col.Class == 'integer'))
        Col.Use<-c(Col.Data:ncol(Data.Set.Correl))
        Measures.All.Correl<-names(Data.Set.Correl)[Col.Use]

        #Update the measures selection choices
        updateSelectInput(
          session,
          inputId =  "correl.measure.sel",
          choices = Measures.All.Correl
        )
      }
    }
  })
  
  #Begin manipulation of the data and graphing
  observe({
    #Determine and Save Color Scheme for F1-F3
    Colors.F1.Sel <- reactive({
      Col.Val <- input$col.sel
      if(Col.Val == "color"){
        Colors.F1 <- read.csv(file = "F1_Color_Pal.csv", colClasses = "character")
      } else if(Col.Val == "grey"){
        Colors.F1 <- read.csv(file = "F1_Color_Pal_Grey.csv", colClasses = "character")
      } else {
      }
    })
    
    Colors.F1 <- Colors.F1.Sel()
    
    #Determine and Save Color Scheme for F4-F6
    Colors.F4.Sel <- reactive({
      Col.Val <- input$col.sel
      if(Col.Val == "color"){
        Colors.F4 <- read.csv(file = "F4_Color_Pal.csv", colClasses = "character")
      } else if(Col.Val == "grey"){
        Colors.F4 <- read.csv(file = "F4_Color_Pal_Grey.csv", colClasses = "character")
      } else {
      }
    })
    
    Colors.F4 <- Colors.F4.Sel()
    
    #Beging Data Culling and Manipulation
    if(input$measure.sel %in% names(Data.Set.Sel())){
      #Save Data set for Download
      Data.Set.dl <- Data.Set.Sel()
      
      #Determine measure based on selection and begin processing
      Cur.Measure.Sel <- input$measure.sel
      
      Cur.Measure <- Data.Set.Sel()[,Cur.Measure.Sel]
      Data.Set <- cbind(Data.Set.Sel()[,1:6], as.numeric(Cur.Measure))
      names(Data.Set)[7] <- input$measure.sel
      
      #Cull data set based on lineage input
      if(input$lineage.sel == ""){
      } else if(input$lineage.sel != ""){
        Data.Set <- Data.Set[Data.Set$Lineage == input$lineage.sel | Data.Set$Lineage == "F1",]
      }
      
      ##############################
      ########Correlations##########
      ##############################
      if(is.null(input$correl.measure.sel)){
      } else if(input$correl.measure.sel %in% names(Data.Set.Sel.Correl())){
        #Determine measure based on selection and begin processing
        Cur.Measure.Sel.Correl <- input$correl.measure.sel
        
        Cur.Measure.Correl <- Data.Set.Sel.Correl()[,Cur.Measure.Sel.Correl]
        Data.Set.Correl <- cbind(Data.Set.Sel.Correl()[,1:6], as.numeric(Cur.Measure.Correl))
        names(Data.Set.Correl)[7] <- input$correl.measure.sel
        
        #Cull data set based on lineage input
        if(input$lineage.sel == ""){
        } else if(input$lineage.sel != ""){
          Data.Set.Correl <- Data.Set.Correl[Data.Set.Correl$Lineage == input$lineage.sel | Data.Set.Correl$Lineage == "F1",]
        }
      }
      
      #Rename groups for graphing order
      Data.Set$GraphID<-NA
      Data.Set$GraphID[which(Data.Set$Treatment == '6% DMSO')] <- '1_6% DMSO'
      Data.Set$GraphID[which(Data.Set$Treatment == '100% DMSO')] <- '2_100% DMSO'
      Data.Set$GraphID[which(Data.Set$Treatment == 'Estradiol')] <- '3_ESTRADIOL'
      Data.Set$GraphID[which(Data.Set$Treatment == 'A1221')] <- '4_A1221'
      Data.Set$GraphID[which(Data.Set$Treatment == 'Low Vin')] <- '5_LOW VIN'
      Data.Set$GraphID[which(Data.Set$Treatment == 'High Vin')] <- '6_HIGH VIN'
      Data.Set$GraphID[which(Data.Set$Treatment == 'DMSO')] <- '7_DMSO'
      Data.Set$GraphID[which(Data.Set$Treatment == 'A1221/A1221')] <- '8_A1221/A1221'
      Data.Set$GraphID[which(Data.Set$Treatment == 'A1221/Vin')] <- '9_A1221/VIN'
      Data.Set$GraphID[which(Data.Set$Treatment == 'Vin/Vin')] <- 'X10_VIN/VIN'
      Data.Set$GraphID[which(Data.Set$Treatment == 'Vin/A1221')] <- 'X11_VIN/A1221'
      Data.Set$GraphID[which(Data.Set$Treatment == 'Flutamide')] <- 'X12_Flutamide'
      
      #Coerce Graphind ID to factor
      Data.Set$GraphID <- as.factor(Data.Set$GraphID)
      
      #Cull data sets based on input
      Data.Set <- Measure.Cull(Data.Set, input$groups.sel.F1, input$groups.sel.F4)
      
      if(exists("Data.Set.Correl")){
        #Rename groups for graphing order
        Data.Set.Correl$GraphID<-NA
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == '6% DMSO')] <- '1_6% DMSO'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == '100% DMSO')] <- '2_100% DMSO'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'Estradiol')] <- '3_ESTRADIOL'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'A1221')] <- '4_A1221'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'Low Vin')] <- '5_LOW VIN'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'High Vin')] <- '6_HIGH VIN'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'DMSO')] <- '7_DMSO'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'A1221/A1221')] <- '8_A1221/A1221'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'A1221/Vin')] <- '9_A1221/VIN'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'Vin/Vin')] <- 'X10_VIN/VIN'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'Vin/A1221')] <- 'X11_VIN/A1221'
        Data.Set.Correl$GraphID[which(Data.Set.Correl$Treatment == 'Flutamide')] <- 'X12_Flutamide'
        
        #Coerce Graphind ID to factor
        Data.Set.Correl$GraphID <- as.factor(Data.Set.Correl$GraphID)
        
        #Cull data sets based on input
        Data.Set.Correl <- Measure.Cull(Data.Set.Correl, input$groups.sel.F1, input$groups.sel.F4)
      }
      
      #Set Y-axis label
      Graph.Measure <- input$measure.sel
      YAxis.Lab <- input$measure.sel
      
      #Set Y-axis label for correlation
      Graph.Measure.Correl <- input$correl.measure.sel
      YAxis.Lab.Correl <- input$correl.measure.sel
      
      #Save data set for download output
      Measure.Set.dl <- Data.Set
      
      #Set group names per generation
      gID.F1 <- unique(Data.Set$GraphID[Data.Set$Generation == "F1"])
      gID.F3 <- unique(Data.Set$GraphID[Data.Set$Generation == "F3"])
      gID.F4 <- unique(Data.Set$GraphID[Data.Set$Generation == "F4"])
      gID.F6 <- unique(Data.Set$GraphID[Data.Set$Generation == "F6"])
      
      #Set Graph Colors based on select groups
      gg.Colors.F1 <- Colors.F1[which(Colors.F1$Group %in% gID.F1),2]
      gg.Colors.F3 <- Colors.F1[which(Colors.F1$Group %in% gID.F3),2]
      gg.Colors.F4 <- Colors.F4[which(Colors.F4$Group %in% gID.F4),2]
      gg.Colors.F6 <- Colors.F4[which(Colors.F4$Group %in% gID.F6),2]
      
      #Set Graph Legend names to replace coded ones
      gg.Guide.Names.F1 <- Colors.F1[which(Colors.F1$Group %in% gID.F1),3]
      gg.Guide.Names.F3 <- Colors.F1[which(Colors.F1$Group %in% gID.F3),3]
      gg.Guide.Names.F4 <- Colors.F4[which(Colors.F4$Group %in% gID.F4),3]
      gg.Guide.Names.F6 <- Colors.F4[which(Colors.F4$Group %in% gID.F6),3]
      
      #Build Correlation Graphs
      if(exists("Data.Set.Correl")){
        #Set Correlation Graph Limits
        YLimit.Correl <- c(
          min(Data.Set.Correl[,input$correl.measure.sel], na.rm = T) - .1*min(Data.Set.Correl[,input$correl.measure.sel], na.rm = T),
          max(Data.Set.Correl[,input$correl.measure.sel], na.rm = T) + .1*max(Data.Set.Correl[,input$correl.measure.sel], na.rm = T)
        )
        XLimit.Correl <- c(
          min(Data.Set[,input$measure.sel], na.rm = T) - .1*min(Data.Set[,input$measure.sel], na.rm = T),
          max(Data.Set[,input$measure.sel], na.rm = T) + .1*max(Data.Set[,input$measure.sel], na.rm = T)
        )
        
        #Build Figures
        gF1.Correl <- gplotFunction.Correl(
          xDat = Data.Set,
          yDat = Data.Set.Correl,
          genID = "F1",
          sexID = input$correl.sex.sel,
          ColInput = gg.Colors.F1,
          NameInput = gg.Guide.Names.F1,
          xLim = XLimit.Correl,
          yLim = YLimit.Correl
        )
        gF3.Correl <- gplotFunction.Correl(
          xDat = Data.Set,
          yDat = Data.Set.Correl,
          genID = "F3",
          sexID = input$correl.sex.sel,
          ColInput = gg.Colors.F3,
          NameInput = gg.Guide.Names.F3,
          xLim = XLimit.Correl,
          yLim = YLimit.Correl
        )
        gF4.Correl <- gplotFunction.Correl(
          xDat = Data.Set,
          yDat = Data.Set.Correl,
          genID = "F4",
          sexID = input$correl.sex.sel,
          ColInput = gg.Colors.F4,
          NameInput = gg.Guide.Names.F4,
          xLim = XLimit.Correl,
          yLim = YLimit.Correl
        )
        gF6.Correl <- gplotFunction.Correl(
          xDat = Data.Set,
          yDat = Data.Set.Correl,
          genID = "F6",
          sexID = input$correl.sex.sel,
          ColInput = gg.Colors.F6,
          NameInput = gg.Guide.Names.F6,
          xLim = XLimit.Correl,
          yLim = YLimit.Correl
        )
      }
      
      ##Begin summarization of data set
      #Add group means
      Summary.Data<-aggregate(
        Data.Set[,Graph.Measure],
        by=Data.Set[c('GraphID', 'Sex', 'Generation')],
        FUN=mean,
        na.rm = T
      )
      #Add column name to summary data
      names(Summary.Data)[4]<-'Mean'
      
      #Add the standard deviation to the summary data
      Summary.Data<-cbind(
        Summary.Data,
        aggregate(
          Data.Set[,Graph.Measure],
          by=Data.Set[c('GraphID', 'Sex', 'Generation')],
          FUN=sd,
          na.rm = T
        )[,4]
      )
      #Add column name to summary data
      names(Summary.Data)[5]<-'SD'
      
      #Calculate what the count of each group is minus NAs for correction
      Summary.Data$Count<-0
      for(j in 1:nrow(Summary.Data)){
        Cur.Treatment<-as.character(Summary.Data[j,1])
        Cur.Sex<-as.character(Summary.Data[j,2])
        Cur.Generation<-as.character(Summary.Data[j,3])
        CurCount<-sum(!is.na(Data.Set[Data.Set$GraphID == Cur.Treatment & Data.Set$Sex == Cur.Sex & Data.Set$Generation == Cur.Generation,Graph.Measure]))
        Summary.Data[j,6]<-round(CurCount, digits = 0)
      }
      
      #Calculate the group with the smallest N
      Min.Group.N.F1 <- min(Summary.Data[Summary.Data$Generation == "F1","Count"])
      Min.Group.N.F3 <- min(Summary.Data[Summary.Data$Generation == "F3","Count"])
      Min.Group.N.F4 <- min(Summary.Data[Summary.Data$Generation == "F4","Count"])
      Min.Group.N.F6 <- min(Summary.Data[Summary.Data$Generation == "F6","Count"])
      
      #Calculate the SEM with the corrected N, without NAs
      Summary.Data$SEM<-Summary.Data$SD/sqrt(Summary.Data$Count)
      Summary.Data <- data.frame(Summary.Data)
      
      #Calculate what the max Y value should be
      #Depend on data points shown or not
      Plot.Points <- input$points.add
      
      #Determine if YLimit needs to account for data points
      #Determine if YLimit needs to account for negative numbers
      #Set YLimit and pass to graphing functions
      if(Plot.Points == F){
        if(min(Summary.Data$Mean, na.rm = T) < 0){
          YLimit <- min(Summary.Data$Mean, na.rm = T) - max(Summary.Data$SEM, na.rm = T) - .15*max(Summary.Data$SEM, na.rm = T)
        }
        if(min(Summary.Data$Mean, na.rm = T) >= 0){
          YLimit <- max(Summary.Data$Mean, na.rm = T) + max(Summary.Data$SEM, na.rm = T) + .15*max(Summary.Data$SEM, na.rm = T)
        }
      } else if (Plot.Points == T){
        if(min(Summary.Data$Mean, na.rm = T) < 0){
          YLimit <- min(Data.Set[,Graph.Measure], na.rm = T) + .10*min(Data.Set[,Graph.Measure], na.rm = T)
        }
        if(min(Summary.Data$Mean, na.rm = T) >= 0){
          YLimit <- max(Data.Set[,Graph.Measure], na.rm = T) + .10*max(Data.Set[,Graph.Measure], na.rm = T)
        }
      }
      
      
      #Build Summary Data Tables for download
      F1.summary.data <- Summary.Data[Summary.Data$Generation == "F1" | Summary.Data$Generation == "F3",]
      F4.summary.data <- Summary.Data[Summary.Data$Generation == "F4" | Summary.Data$Generation == "F6",]
      
      #Build Graphs based on input
      gF1 <- gplotFunction.Colors(
        Summary.Data,
        generation = "F1",
        YLimit = YLimit,
        YLabel = YAxis.Lab,
        ColInput = gg.Colors.F1,
        NameInput = gg.Guide.Names.F1,
        dotdata = Data.Set,
        plotdot = Plot.Points
      )
      gF3 <- gplotFunction.Colors(
        Summary.Data,
        generation = "F3",
        YLimit = YLimit,
        YLabel = YAxis.Lab,
        ColInput = gg.Colors.F3,
        NameInput = gg.Guide.Names.F3,
        dotdata = Data.Set,
        plotdot = Plot.Points
      )
      gF4 <- gplotFunction.Colors(
        Summary.Data,
        generation = "F4",
        YLimit = YLimit,
        YLabel = YAxis.Lab,
        ColInput = gg.Colors.F4,
        NameInput = gg.Guide.Names.F4,
        dotdata = Data.Set,
        plotdot = Plot.Points
      )
      gF6 <- gplotFunction.Colors(
        Summary.Data,
        generation = "F6",
        YLimit = YLimit,
        YLabel = YAxis.Lab,
        ColInput = gg.Colors.F6,
        NameInput = gg.Guide.Names.F6,
        dotdata = Data.Set,
        plotdot = Plot.Points
      )
      
      
      #Prepare ANOVA results
      if(input$correl.act == F){
        if("F1" %in% Summary.Data[,"Generation"]){
          if(0 %in% Summary.Data[Summary.Data$Generation == "F1","SD"]){
          } else {
            AOV.F1 <- ANOVA.shiny(
              Data.Org = Data.Set,
              genID = "F1",
              sexID = input$f1.sex.sel,
              Measure = input$measure.sel,
              Factor1 = "Sex",
              Factor2 = "Treatment",
              Min.Count = Min.Group.N.F1
            )
            if(input$f1.sex.sel == ""){
              AOV.F1.anova <- AOV.F1[-4, c(1,5,6,10)]
              AOV.F1.diag <- AOV.F1[1, c(8,9)]
            }
            if(input$f1.sex.sel != ""){
              AOV.F1.anova <- AOV.F1[-2, c(1,5,6,10)]
              AOV.F1.diag <- AOV.F1[1, c(8,9)]
            }
          }
        }
        if("F3" %in% Summary.Data[,"Generation"]){
          if(0 %in% Summary.Data[Summary.Data$Generation == "F3","SD"]){
          } else {
            AOV.F3 <- ANOVA.shiny(
              Data.Org = Data.Set,
              genID = "F3",
              sexID = input$f3.sex.sel,
              Measure = input$measure.sel,
              Factor1 = "Sex",
              Factor2 = "Treatment",
              Min.Count = Min.Group.N.F3
            )
            if(input$f3.sex.sel == ""){
              AOV.F3.anova <- AOV.F3[-4, c(1,5,6,10)]
              AOV.F3.diag <- AOV.F3[1, c(8,9)]
            }
            if(input$f3.sex.sel != ""){
              AOV.F3.anova <- AOV.F3[-2, c(1,5,6,10)]
              AOV.F3.diag <- AOV.F3[1, c(8,9)]
            }
          }
        }
        if("F4" %in% Summary.Data[,"Generation"]){
          if(0 %in% Summary.Data[Summary.Data$Generation == "F4","SD"]){
          } else {
            AOV.F4 <- ANOVA.shiny(
              Data.Org = Data.Set,
              genID = "F4",
              sexID = input$f4.sex.sel,
              Measure = input$measure.sel,
              Factor1 = "Sex",
              Factor2 = "Treatment",
              Min.Count = Min.Group.N.F4
            )
            if(input$f4.sex.sel == ""){
              AOV.F4.anova <- AOV.F4[-4, c(1,5,6,10)]
              AOV.F4.diag <- AOV.F4[1, c(8,9)]
            }
            if(input$f4.sex.sel != ""){
              AOV.F4.anova <- AOV.F4[-2, c(1,5,6,10)]
              AOV.F4.diag <- AOV.F4[1, c(8,9)]
            }
          }
        }
        if("F6" %in% Summary.Data[,"Generation"]){
          if(0 %in% Summary.Data[Summary.Data$Generation == "F6","SD"]){
          } else {
            AOV.F6 <- ANOVA.shiny(
              Data.Org = Data.Set,
              genID = "F6",
              sexID = input$f6.sex.sel,
              Measure = input$measure.sel,
              Factor1 = "Sex",
              Factor2 = "Treatment",
              Min.Count = Min.Group.N.F6
            )
            if(input$f6.sex.sel == ""){
              AOV.F6.anova <- AOV.F6[-4, c(1,5,6,10)]
              AOV.F6.diag <- AOV.F6[1, c(8,9)]
            }
            if(input$f6.sex.sel != ""){
              AOV.F6.anova <- AOV.F6[-2, c(1,5,6,10)]
              AOV.F6.diag <- AOV.F6[1, c(8,9)]
            }
          }
        }
      }
      
      #Prepare plots for download
      output$dlgF1.plot <- downloadHandler(
        filename = function(){
          sprintf("TIME_F1_%s_%s_%s.eps", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file) {
          gH <- 7
          gW <- length(which(F1.summary.data$Generation == "F1")) * .625 + 2
          ggsave(file, plot = gF1, width = gW, height = gH)
        })
      
      output$dlgF3.plot <- downloadHandler(
        filename = function(){
          sprintf("TIME_F3_%s_%s_%s.eps", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file) {
          gH <- 7
          gW <- length(which(F1.summary.data$Generation == "F3")) * .625 + 2
          ggsave(file, plot = gF3, width = gW, height = gH)
        })
      
      output$dlgF4.plot <- downloadHandler(
        filename = function(){
          sprintf("TIME_F4_%s_%s_%s.eps", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file) {
          gH <- 7
          gW <- length(which(F4.summary.data$Generation == "F4")) * .625 + 2
          ggsave(file, plot = gF4, width = gW, height = gH)
        })
      
      output$dlgF6.plot <- downloadHandler(
        filename = function(){
          sprintf("TIME_F6_%s_%s_%s.eps", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file) {
          gH <- 7
          gW <- length(which(F4.summary.data$Generation == "F6")) * .625 + 2
          ggsave(file, plot = gF6, width = gW, height = gH)
        })
      
      #Load data of summary tables for download
      output$dlF1.Summary <- downloadHandler(
        filename = function(){
          sprintf("TIME_F1-F3_%s_%s_%s.csv", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file){
          write.csv(F1.summary.data, file, row.names = F)
        }
      )
      
      #Load data of summary tables for download
      output$dlF4.Summary <- downloadHandler(
        filename = function(){
          sprintf("TIME_F4-F6_%s_%s_%s.csv", input$lineage.sel, input$table.sel, input$measure.sel)
        },
        content = function(file){
          write.csv(F4.summary.data, file, row.names = F)
        }
      )
      
      #Load whole data set for download
      output$dltotal.data <- downloadHandler(
        filename = function(){
          sprintf("TIME_%s_%s.csv", input$table.sel, Sys.Date())
        },
        content = function(file){
          write.csv(Data.Set.dl, file, row.names = F)
        }
      )
      
      #Load culled measure set for download
      output$dltotal.measure <- downloadHandler(
        filename = function(){
          sprintf("TIME_%s_%s_%s_Culled.csv", input$table.sel, input$measure.sel, Sys.Date())
        },
        content = function(file){
          write.csv(Measure.Set.dl, file, row.names = F)
        }
      )
      
      #Deliver files to download buttons
      outputOptions(output, 'dltotal.data', suspendWhenHidden=FALSE)
      outputOptions(output, 'dltotal.measure', suspendWhenHidden=FALSE)
      
      #Render ANOVA table and diag outputs
      output$f1.anova <- renderTable(AOV.F1.anova)
      output$f1.diag <- renderTable(AOV.F1.diag)
      output$f3.anova <- renderTable(AOV.F3.anova)
      output$f3.diag <- renderTable(AOV.F3.diag)
      output$f4.anova <- renderTable(AOV.F4.anova)
      output$f4.diag <- renderTable(AOV.F4.diag)
      output$f6.anova <- renderTable(AOV.F6.anova)
      output$f6.diag <- renderTable(AOV.F6.diag)
      
      #Render summary tables for output to UI
      output$F1.summary.data <- renderTable(F1.summary.data)
      output$F4.summary.data <- renderTable(F4.summary.data)
      
      if(input$correl.act == F) {
        output$F1.Plot <- renderPlot(gF1)
        output$F3.Plot <- renderPlot(gF3)
        output$F4.Plot <- renderPlot(gF4)
        output$F6.Plot <- renderPlot(gF6)
      }
      if(input$correl.act == T) {
        if(exists("gF1.Correl")) {
          output$F1.Plot <- renderPlot(gF1.Correl)
        }
        if(exists("gF3.Correl")) {
          output$F3.Plot <- renderPlot(gF3.Correl)
        }
        if(exists("gF4.Correl")) {
          output$F4.Plot <- renderPlot(gF4.Correl)
        }
        if(exists("gF6.Correl")) {
          output$F6.Plot <- renderPlot(gF6.Correl)
        }
      }
      
      #Debug Output
      output$Debug <- renderTable(Measures.All.Correl)
      # output$Debug2 <- renderTable(XLimit.Correl)
      
    }
  })
})