
rm(list = ls())

## SHINY packages
list.packages = c("shiny", "shinyFiles", "shinyalert", "shinyDirectoryInput", "shinyjs", "shinyWidgets", "shinyhelper")
## DATA MANIPULATION packages
list.packages = c(list.packages
                  , "data.table", "foreach", "dplyr", "DT", "reshape2")
## GRAPHICS packages
list.packages = c(list.packages
                  , "ggplot2", "ggthemes", "ggdendro", "ggrepel", "ggExtra", "gridExtra"
                  , "viridis", "RColorBrewer", "plotly", "cowplot")
## OTHER packages
list.packages = c(list.packages
                  , "markdown", "zip", "raster", "rintrojs"
                  , "SPOT" ## designLHD
                  , "ade4" ## quasieuclid
)

load.shinyDirectoryInput = requireNamespace("shinyDirectoryInput")
load.devtools = requireNamespace("devtools")
if (!load.shinyDirectoryInput)
{
  if (!load.devtools)
  {
    install.packages("devtools")
  }
  library(devtools)
  devtools::install_github('wleepang/shiny-directory-input')
}
check.packages = sapply(list.packages, .loadPackage)
load.packages = sapply(list.packages, require, character.only = TRUE)

# library(shinycssloaders)
# library(shinymaterial)
# library(shinybusy)

###################################################################################################################################

# setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES/RFate/")
source("SHINY.PRE_FATE.params_FUNCTIONS.R", local = TRUE)

###################################################################################################################################
###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  extendShinyjs("www/js/app-shinyjs.js", functions = c("getInputType")),
  introjsUI(),
  
  # tags$link(rel="stylesheet", type="text/css", href="app.css"),
  tags$body(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One|Slabo+27px|Francois+One');
           #loadmessage {
           position: fixed;
           top: 0px;
           left: 0px;
           width: 100%;
           padding: 5px 0px 5px 0px;
           text-align: center;
           font-family: 'Londrina Solid', cursive;
           font-weight: 300;
           line-height: 1.1;
           color: #000000;
           background-color: #CCFF66;
           z-index: 105;
           }
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
                                         "https://cdn.dribbble.com/users/1169971/screenshots/3553587/graphloader.gif"
                                         # "https://loading.io/spinners/equalizer/lg.equalizer-bars-loader.gif"
                                       , height = "80px")
             )
             # conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
             #                  tags$div("Loading...", id = "loadmessage"))
    )
  ),

  # Sidebar layout with a input and output definitions
  mainPanel(
    width = 12,
    navbarPage(""
               , id = "navbar"
               , source("SHINY.PRE_FATE.params_UI.panel0.R", local = TRUE)$value
               , source("SHINY.PRE_FATE.params_UI.panel1.R", local = TRUE)$value
               # , source("SHINY.PRE_FATE.params_UI.panel2.R", local = TRUE)$value
               , navbarMenu(title = HTML("<span class='panel_title'><i class='fa fa-copy'></i> Simulation parameter files</span>")
                            , source("SHINY.PRE_FATE.params_UI.panel2.menu1.R", local = TRUE)$value
                            , source("SHINY.PRE_FATE.params_UI.panel2.menu2.R", local = TRUE)$value)
                            # , tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-clone'></i> Create multiple set</span>")))
               , source("SHINY.PRE_FATE.params_UI.panel3.R", local = TRUE)$value
               , source("SHINY.PRE_FATE.params_UI.panel4.R", local = TRUE)$value
    )
  )
    ) ## END fluidPage


###################################################################################################################################
###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe_helpers(withMathJax = TRUE)
  
  RV = reactiveValues(names.PFG = c()
                      , mat.PFG.ALL = data.frame()
                      , mat.PFG.disp = data.frame()
                      , mat.PFG.dist = data.frame()
                      , mat.PFG.soil = data.frame()
                      , mat.changing = data.frame()
                      , compt.global.nb = 0
                      , compt.global.files = c()
                      , compt.save.nb = 0
                      , compt.save.files = c()
                      , compt.succ.nb = 0
                      , compt.succ.files = c()
                      , compt.light.nb = 0
                      , compt.light.files = c()
                      , compt.disp.nb = 0
                      , compt.disp.files = c()
                      , compt.dist.nb = 0
                      , compt.dist.files = c()
                      , compt.soil.nb = 0
                      , compt.soil.files = c()
                      , compt.changing.nb = 0
                      , compt.changing.files = c()
                      , compt.dist.by_type = FALSE
                      , compt.dist.by_pfg = FALSE
                      , compt.browser = 1
                      , compt.browser.max = 1
                      , compt.browser.pfg = 1
                      , compt.browser.pfg.max = 1
                      , pfg.graph = c()
  )
  
  ####################################################################

  source("SHINY.PRE_FATE.params_SERVER.panel0.R", local = TRUE)$value
  
  ####################################################################

  source("SHINY.PRE_FATE.params_SERVER.panel1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel1.tab1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel1.tab2.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel1.tab3.R", local = TRUE)$value
  
  ####################################################################
  
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab2.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab3.tab0.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab3.tab1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab3.tab2.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab3.tab3.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab3.tab4.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu1.tab4.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel2.menu2.R", local = TRUE)$value
  
  ####################################################################
  
  source("SHINY.PRE_FATE.params_SERVER.panel3.R", local = TRUE)$value

  ####################################################################
  
  source("SHINY.PRE_FATE.params_SERVER.panel4.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel4.tab1.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel4.tab2.R", local = TRUE)$value
  source("SHINY.PRE_FATE.params_SERVER.panel4.tab3.R", local = TRUE)$value
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      # params <- list(n = input$slider)
      params.names = c("name.simul"
                       , "create.skeleton"
                       , "required.no_PFG"
                       , "required.no_STRATA"
                       , "required.simul_duration"
                       , "opt.no_CPU"
                       , "required.seeding_duration"
                       , "required.seeding_timestep"
                       , "required.seeding_input"
                       , "required.max_by_cohort"
                       , "required.max_abund_low"
                       , "required.max_abund_medium"
                       , "required.max_abund_high"
                       , "doDispersal"
                       , "doHabSuitability"
                       , "doDisturbances"
                       , "doLight"
                       , "doSoil"
                       , "DISPERSAL.mode"
                       , "HABSUIT.ref_option"
                       , "DIST.no"
                       , "DIST.no_sub"
                       , "DIST.freq"
                       , "LIGHT.thresh_medium"
                       , "LIGHT.thresh_low"
      )
      RV.names = c("compt.global.nb"
                   , "compt.global.files")
      
      params = vector("list")
      for (i in params.names)
      {
        eval(parse(text = paste0("params[[i]] = input$", i)))
      }
      for (i in RV.names)
      {
        eval(parse(text = paste0("params[[i]] = RV$", i)))
      }

      print(params)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport
                        , output_file = file
                        , params = params
                        , envir = new.env(parent = globalenv())
      )
    }
  )
  
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

