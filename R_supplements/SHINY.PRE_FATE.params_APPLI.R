
rm(list = ls())

## load required libraries
library(shiny)
library(shinyFiles)
library(shinyalert)
library(shinyDirectoryInput)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyhelper)
library(markdown)
library(RFate)
library(data.table)
library(foreach)
library(zip)
library(ggplot2)
library(ggthemes)
library(raster)
library(viridis)
library(RColorBrewer)
library(dplyr)

###################################################################################################################################

source("R_supplements/SHINY.PRE_FATE.params_FUNCTIONS.R", local = TRUE)

names.PFG = c()
mat.PFG.ALL = data.frame()
mat.PFG.disp = data.frame()
mat.PFG.dist = data.frame()
mat.PFG.soil = data.frame()
mat.changing = data.frame()
button.color = "rgba(96, 129, 150, 0.5)"
button.style = paste0("background-color: ", button.color, "; border-width:0px;")
panel.style = "color:#FFFFFF; background-color:rgba(96, 129, 150, 0.5); border-width:0px;"
panel.style.hover = "color:#FFFFFF; background-color:#3a7da8; border-width:0px;"
help.color = "#dee2e8"

###################################################################################################################################
###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  
  tags$body(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One|Slabo+27px|Francois+One');
                    h1 {
                    font-family: 'Londrina Solid', cursive;
                    font-weight: 300;
                    line-height: 1.1;
                    background-color: #3a7da8;
                    padding: 20px;
                    margin-top: 20px;
                    margin-bottom: 20px;
                    border-radius: 0px;
                    color: #FFFFFF;
                    }
                    
                    .tabbable > .nav > li > a {
                    background-color: rgba(96, 129, 150, 0.5);
                    color: #FFFFFF;
                    border-radius: 0px;
                    }
                    .tabbable > .nav > li > a:hover {
                    background-color: #3a7da8;
                    color:#FFFFFF;
                    border-radius: 0px;
                    }
                    .tabbable > .nav > li[class=active] > a {
                    background-color: #3a7da8;
                    color:#FFFFFF;
                    border-radius: 0px;
                    }
                    p.panel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    p.tabPanel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    p.tabPanel_subtitle {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 18px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    .radioGroupButtons .btn {
                    background-color: rgba(96, 129, 150, 0.5);
                    color: #FFFFFF;
                    border-radius: 5px;
                    }
                    .radioGroupButtons .btn:hover {
                    background-color: #3a7da8;
                    }
                    .radioGroupButtons .btn-panelgraph.active {
                    background-color: #3a7da8;
                    }
                    "))
    ),
  
  headerPanel("FATE", windowTitle = "FATE"),
  
  # Sidebar layout with a input and output definitions
  mainPanel(
    width = 12,
    # radioGroupButtons(inputId = "show.panels"
    #                   , label = ""
    #                   , choices = c("A. Simulation folder & parameter files"
    #                                 , "B. Run simulation"
    #                                 , "C. Simulation outputs & graphics")
    #                   , selected = 0
    #                   , justified = TRUE
    #                   , status = "panelgraph"
    #                   , checkIcon = NULL
    # )
    tabsetPanel(
      source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.R", local = TRUE)$value,
      tabPanel(title =  HTML("<p class='panel_title'>B. Run simulation</p>")),
      source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.R", local = TRUE)$value
    )
  )
    ) ## END fluidPage


###################################################################################################################################
###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe_helpers(withMathJax = TRUE)

  ####################################################################
  
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel1.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel1.tab1.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel1.tab2.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel1.tab3.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel1.tab4.R", local = TRUE)$value
  
  ####################################################################
  
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel3.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel3.tab2.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.panel3.tab3.R", local = TRUE)$value
  
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

