
rm(list = ls())

## load required libraries
library(shiny)
library(shinyFiles)
library(shinyalert)
# devtools::install_github('wleepang/shiny-directory-input')
library(shinyDirectoryInput)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyhelper)
# library(shinymaterial)
library(shinybusy)
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
library(rintrojs)
library(dplyr)
library(plotly)

###################################################################################################################################

source("R_supplements/SHINY.PRE_FATE.params_FUNCTIONS.R", local = TRUE)
source("R_supplements/SHINY.PRE_FATE.params_FUNCTIONS.extern.R", local = TRUE)

names.PFG = c()
mat.PFG.ALL = data.frame()
mat.PFG.disp = data.frame()
mat.PFG.dist = data.frame()
mat.PFG.soil = data.frame()
mat.changing = data.frame()


###################################################################################################################################
###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  introjsUI(),
  
#   tags$script("
#               $(document).on('shiny:busy', function() {
#   var inputs = document.getElementsByTagName('button');
#   console.log(inputs);
# for (var i = 0; i < inputs.length; i++) {
# inputs[i].disabled = true;
# }
# });
# 
# $(document).on('shiny:idle', function() {
#   var inputs = document.getElementsByTagName('button');
#   console.log(inputs);
# for (var i = 0; i < inputs.length; i++) {
# inputs[i].disabled = false;
# }
# });
#               "),
  
  # tags$link(rel="stylesheet", type="text/css", href="app.css"),
  tags$body(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One|Slabo+27px|Francois+One');
                    .icon-helpd { color: Darkblue; }
                    h1 {
                    font-family: 'Londrina Solid', cursive;
                    font-weight: 300;
                    line-height: 1.1;
                    background-color: #3a7da8;
                    padding: 20px;
                    margin-top: 0px;
                    margin-bottom: 0px;
                    border-radius: 0px;
                    color: #FFFFFF;
                    }
                    .tabbable > .nav > li > a {
                    background-color: #e0dbd9;
                    color: #8c8582;
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
                    .navbar-default .navbar-nav > not(.active) > a {
                    color: #8c8582;
                    }
                    .navbar-default .navbar-nav > .active > a, 
                    .navbar-default .navbar-nav > .active > a:focus, 
                    .navbar-default .navbar-nav > .active > a:hover {
                    background-color: #e0dbd9;
                    }
                    .panel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    .tabPanel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    .tabPanel_subtitle {
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
  
  fluidRow(
    style = HTML(paste0("background-color: #3a7da8; margin-top: 20px; margin-bottom: 20px;")),
    column(10,
           headerPanel("FATE", windowTitle = "FATE")
    )
    , column(2,
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$img(src = 
                                         #"http://www.grobelny.pl/kola.gif"
                                         #"https://i.pinimg.com/originals/18/42/81/184281f0fe87517a950beb8112c308dd.gif"
                                         #"https://cdn-images-1.medium.com/max/2400/1*F_5AEXIfr1AXuShXhYT4zg.gif"
                                         #"http://thinkfuture.com/wp-content/uploads/2013/10/loading_spinner.gif"
                                         #"https://cdn.dribbble.com/users/1169971/screenshots/3553587/graphloader.gif"
                                         "https://loading.io/spinners/equalizer/lg.equalizer-bars-loader.gif"
                                       , height = "80px")
             )
    )
  ),
  
  # Sidebar layout with a input and output definitions
  mainPanel(
    width = 12,
    navbarPage(""
               , id = "navbar"
               , navbarMenu(title = HTML("<span class='panel_title'><i class='fa fa-copy'></i> Simulation parameter files</span>")
                            , source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.R", local = TRUE)$value
                            , tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-folder-open'></i> Open</span>")))
               , tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-cogs'></i> Run simulation</span>"))
               , source("R_supplements/SHINY.PRE_FATE.params_UI.panel3.R", local = TRUE)$value
    )
  )
    ) ## END fluidPage


###################################################################################################################################
###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe_helpers(withMathJax = TRUE)
  
  # Function to toggle input elements. 
  # input_list: List of inputs, reactiveValuesToList(input)
  # enable_inputs: Enable or disable inputs?
  # Only buttons: Toggle all inputs, or only buttons?
  # toggle_inputs = function(input_list
  #                          , enable_inputs = TRUE
  #                          , only_buttons = FALSE)
  # {
  #   # Subset if only_buttons is TRUE.
  #   if(only_buttons)
  #   {
  #     buttons = which(sapply(input_list, function(x) {any(grepl('Button', attr(x,"class")))}))
  #     input_list = input_list[buttons]
  #   }
  #   
  #   # Toggle elements
  #   for(x in names(input_list))
  #   {
  #     if(enable_inputs)
  #     {
  #       shinyjs::enable(x)
  #     } else
  #     {
  #       shinyjs::disable(x)
  #     }
  #   }
  # }
  
 
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

