library(shiny)
library(shinydashboard)
library(ggplot2)

##################################
##################################
##################################
##################################
header <- dashboardHeader(
  title = "TIME Data Portal",
  titleWidth = 250
)
##################################
##################################
##################################
##################################
sidebar <- dashboardSidebar(
  width = "250",
  sidebarMenu(
    selectInput(
      inputId <- "table.sel",
      label = "Data Set",
      choices = "",
      selected = ""
    ),
    selectInput(
      inputId <- "measure.sel",
      label = "Measure",
      choices = "",
      selected = ""
    ),
    menuItem(
      "Aesthetics",
      tabName = "behav",
      icon = icon("paint-brush"),
      radioButtons(
        inputId = "col.sel",
        label = "Choose Color Palette:",
        choices = c("Color Blind Safe" = "color", "Grey Scale" = "grey")
      ),
      checkboxInput(
        inputId = "points.add",
        label = "Add Data Points"
      )
    ),
    menuItem(
      "Group Selection",
      tabName = "grouptab",
      icon = icon("group"),
      radioButtons(
        inputId = "lineage.sel",
        label = "Choose Lineage:",
        choices = c("Combined Lineages" = "", "Paternal Lineage" = "Paternal", "Maternal Lineage" = "Maternal")
      ),
      checkboxGroupInput(
        inputId = "groups.sel.F1",
        label = "Select F1-F3 Groups",
        choices = c("6% DMSO", "100% DMSO", "Estradiol", "A1221", "Low Vin", "High Vin", "Flutamide"),
        selected = c("6% DMSO", "A1221", "Low Vin", "High Vin")
      ),
      checkboxGroupInput(
        inputId = "groups.sel.F4",
        label = "Select F4-F6 Groups",
        choices = c("DMSO", "A1221/A1221", "A1221/Vin", "Vin/Vin", "Vin/A1221"),
        selected = c("DMSO", "A1221/A1221", "A1221/Vin", "Vin/Vin", "Vin/A1221")
      )
    ),
    menuItem(
      "Data Output",
      tabName = "datoutmenu",
      icon = icon("database"),
      fluidRow(
        downloadButton('dltotal.data', "Download Data Set")
      ),
      fluidRow(
        downloadButton('dltotal.measure', "Download Measure Data")
      )
    ),
    menuItem(
      "Correlations",
      tabName = "correlmenu",
      icon = icon("line-chart"),
      fluidRow(
        checkboxInput("correl.act", label = "Activate Correlations", value = FALSE)
      ),
      menuItemOutput("correl.menu")
    )
  )
)
##################################
##################################
##################################
##################################
body <- dashboardBody(
  navbarPage(
    title = "Endpoints:",
    tabPanel(
      title = "Behavioral Data",
      fluidRow(
        box(title = "F1 Generation",
            solidHeader = T,
            status = "primary",
            plotOutput("F1.Plot", height = "500px")),
        box(title = "F3 Generation",
            solidHeader = T,
            status = "primary",
            plotOutput("F3.Plot", height = "500px"))
      ),
      fluidRow(
        tabBox(
          title = "F1 Statistics",
          width = 6,
          tabPanel("Download", downloadButton('dlgF1.plot', "Download F1 Plot (.eps)")),
          tabPanel("ANOVA",
                   fluidRow(
                     column(12, align = "center",
                            radioButtons(
                              inputId = "f1.sex.sel",
                              label = "Choose Sex:",
                              choices = c("Combined Sex" = "", "Males" = "Male", "Females" = "Female"),
                              inline = TRUE
                            )
                     )
                   ),
                   fluidRow(
                     tableOutput("f1.anova")
                   ),
                   fluidRow(
                     tableOutput("f1.diag")
                   )
          ),
          side = "left"
        ),
        tabBox(
          title = "F3 Statistics",
          width = 6,
          tabPanel("Download", downloadButton('dlgF3.plot', "Download F3 Plot (.eps)")),
          tabPanel("ANOVA",
                   fluidRow(
                     column(12, align = "center",
                            radioButtons(
                              inputId = "f3.sex.sel",
                              label = "Choose Sex:",
                              choices = c("Combined Sex" = "", "Males" = "Male", "Females" = "Female"),
                              inline = TRUE
                            )
                     )
                   ),
                   fluidRow(
                     tableOutput("f3.anova")
                   ),
                   fluidRow(
                     tableOutput("f3.diag")
                   )
          ),
          side = "left"
        )
      ),
      fluidRow(
        box(title = "F4 Generation",
            solidHeader = T,
            status = "primary",
            plotOutput("F4.Plot", height = "500px")),
        box(title = "F6 Generation",
            solidHeader = T,
            status = "primary",
            plotOutput("F6.Plot", height = "500px"))
      ),
      fluidRow(
        tabBox(
          title = "F4 Statistics",
          tabPanel("Download", downloadButton('dlgF4.plot', "Download F4 Plot (.eps)")),
          tabPanel("ANOVA",
                   fluidRow(
                     column(12, align = "center",
                            radioButtons(
                              inputId = "f4.sex.sel",
                              label = "Choose Sex:",
                              choices = c("Combined Sex" = "", "Males" = "Male", "Females" = "Female"),
                              inline = TRUE
                            )
                     )
                   ),
                   fluidRow(
                     tableOutput("f4.anova")
                   ),
                   fluidRow(
                     tableOutput("f4.diag")
                   )
          ),
          side = "left"
        ),
        tabBox(
          title = "F6 Statistics",
          tabPanel("Download", downloadButton('dlgF6.plot', "Download F6 Plot (.eps)")),
          tabPanel("ANOVA",
                   fluidRow(
                     column(12, align = "center",
                            radioButtons(
                              inputId = "f6.sex.sel",
                              label = "Choose Sex:",
                              choices = c("Combined Sex" = "", "Males" = "Male", "Females" = "Female"),
                              inline = TRUE
                            )
                     )
                   ),
                   fluidRow(
                     tableOutput("f6.anova")
                   ),
                   fluidRow(
                     tableOutput("f6.diag")
                   )
          ),
          side = "left"
        )
      ),
      fluidRow(
        box(
          title = "F1-F3 Generation Summary Data",
          solidHeader = T,
          status = "warning",
          tableOutput("F1.summary.data"),
          downloadButton('dlF1.Summary', "Download F1-F3 Summary Data")
        ),
        box(
          title = "F4-F6 Generation Summary Data",
          solidHeader = T,
          status = "warning",
          tableOutput("F4.summary.data"),
          downloadButton('dlF4.Summary', "Download F4-F6 Summary Data")
        )
      )
      # fluidRow(
      #   box(
      #     title = "Debug",
      #     solidHeader = T,
      #     status = "danger",
      #     tableOutput("Debug")
      #   ),
      #   box(
      #     title = "Debug 2",
      #     solidHeader = T,
      #     status = "danger",
      #     tableOutput("Debug2")
      #   )
      # )
    ),
    tabPanel(
      title = "Longitudinal Data",
      fluidRow(
        box(title = "Body Weight and AGI Data",
            solidHeader = T,
            status = "warning",
            "Coming Soon"
        )
      )
    )
  )
)
##################################
##################################
##################################
##################################
dashboardPage(header, sidebar, body)
##################################
##################################