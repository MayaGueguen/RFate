
rm(list = ls())

## load required libraries
library(shiny)
library(shinyFiles)
library(shinythemes)
# library(shinycssloaders)
# library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(markdown)
library(RFate)
library(data.table)
library(foreach)
library(zip)

###################################################################################################################################

source("R_supplements/SHINY.PRE_FATE.params_FUNCTIONS.R", local = TRUE)

names.PFG = c()
mat.PFG.succ = data.frame()
mat.PFG.disp = data.frame()
mat.PFG.dist = data.frame()
mat.changing = data.frame()
button.color = "#dee2e8"

###################################################################################################################################
###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  
  # theme = "cosmo",
  tags$body(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One');
                    h1 {
                    font-family: 'Londrina Solid', cursive;
                    font-weight: 300;
                    line-height: 1.1;
                    background-color: #068f96;
                    padding: 10px;
                    margin-bottom: 50px;
                    border-radius: 2px;
                    color: #FFFFFF;
                    }
                    p.tabPanel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    background-color: #068f96;
                    padding: 10px;
                    margin-top: 0px;
                    border-radius: 2px;
                    color: #FFFFFF;
                    }
                    p.tabPanel_subtitle {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 15px;
                    font-weight: 200;
                    background-color: #068f96;
                    padding: 6px;
                    margin-top: 0px;
                    border-radius: 2px;
                    color: #FFFFFF;
                    }
                    "))
    ),
  
  headerPanel("FATE : create simulation folder & parameter files", windowTitle = "FATE"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 3,
      style = "border-solid:solid; border-width:2px; border-color:#068f96;",
      withMathJax(),
      
      br(),
      br(),
      br(),
      fluidRow(
        column(12
               , textInput(inputId = "name.simul"
                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>Enter the simulation name :</span>")
                           , value = "FATE_simulation"
                           , width = "100%")
        )
      ),
      fluidRow(
        column(12
               , actionButton(inputId = "create.skeleton"
                              , label = "Create folder"
                              , icon = icon("folder")
                              , width = "100%"
                              , style = HTML(paste0("background-color: ", button.color, ";"))
               )
        )
      ),
      br(),
      helpText(HTML("
                    <ul>
                    <li>DATA
                    <ul>
                    <li>GLOBAL_PARAMETERS</li>
                    <li>MASK</li>
                    <li>SCENARIO</li>
                    <li>SAVE</li>
                    <li>PFGS :
                    <ul>
                    <li>SUCC</li>
                    <li>DISP</li>
                    <li>HABSUIT</li>
                    <li>LIGHT</li>
                    <li>SOIL</li>
                    <li>DIST</li>
                    </ul>
                    </li>
                    </ul>
                    </li>
                    <li>PARAM_SIMUL</li>
                    <li>RESULTS</li>
                    </ul>
                    "
      )),
      br(),
      br(),
      fluidRow(
        column(12
               , shinyjs::hidden(
                 actionButton(inputId = "create.simul"
                              , label = HTML("Create Simulation <br/>parameters file")
                              , icon = icon("file")
                              , width = "100%"
                              , style = HTML(paste0("background-color: ", button.color, ";"))
                 )
               )
        )
      ),
      br(),
      fluidRow(
        column(12
               , uiOutput(outputId = "UI.download.folder")
        )
      ),
      br(),
      fluidRow(
        column(12
               , shinyjs::hidden(
                 actionButton(inputId = "refresh"
                              , label = "Start new folder"
                              , icon = icon("refresh")
                              , width = "100%"
                              , style = HTML(paste0("background-color: ", button.color, ";"))
                 )
               )
        )
      )
      ) ## END sidebarPanel
    
    # Output
    , mainPanel(
      shinyjs::hidden(
        wellPanel(id = "main.panel",
                  style = "border-solid:solid; border-width:2px; border-color:#068f96;",
                  tabsetPanel(
                    source("R_supplements/SHINY.PRE_FATE.params_UI.tab1.R", local = TRUE)$value,
                    source("R_supplements/SHINY.PRE_FATE.params_UI.tab2.R", local = TRUE)$value,
                    source("R_supplements/SHINY.PRE_FATE.params_UI.tab3.R", local = TRUE)$value,
                    source("R_supplements/SHINY.PRE_FATE.params_UI.tab4.R", local = TRUE)$value
                  ) ## END tabsetPanel
        ) ## END wellPanel
      ) ## END hidden
    ) ## END mainPanel
  ) ## END sidebarLayout
) ## END fluidPage


###################################################################################################################################
###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # output$mat.PFG.succ = data.frame()
  session$onSessionEnded(stopApp)
  
  ####################################################################
  
  observeEvent(input$create.skeleton, {
    print_messages(as.expression(
      PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
    ))
    
    shinyjs::show("main.panel")
    shinyjs::show("create.simul")
    shinyjs::show("UI.download.folder")
    shinyjs::show("refresh")
    
    output$UI.download.folder = renderUI({
      downloadButton(outputId = "FATE_simulation.zip"
                     , label = "Download folder"
                     , icon = icon("download")
                     , width = "100%"
                     , style = HTML(paste0("background-color: ", button.color, ";"))
      )
    })
  })
  
  
  observeEvent(input$create.simul, {
    if (input$create.skeleton > 0)
    {
      mask.file = list.files(path = paste0(input$name.simul, "/DATA/MASK")
                             , pattern = "^MASK_")
      if (input$upload.mask > 0 && length(mask.file) > 0)
      {
        get_res = print_messages(as.expression(
          PRE_FATE.params_simulParameters(name.simulation = input$name.simul
                                          , name.mask = mask.file
          )
        ))
      } else
      {
        shinyalert(type = "warning", text = "You must upload a simulation mask first !")
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  
  output$FATE_simulation.zip = downloadHandler(
    filename = function(){
      paste0(input$name.simul, "_", Sys.Date(), ".zip")
    },
    content = function(file){
      zip(zipfile = file, input$name.simul)
      file.copy(file, file)
    },
    contentType = "application/zip"
  )
  
  
  observeEvent(input$refresh, {
    system(command = paste0("rm -r ", input$name.simul))
    shinyjs::hide("main.panel")
    shinyjs::hide("create.simul")
    shinyjs::hide("UI.download.folder")
    shinyjs::hide("refresh")
  })
  
  
  ####################################################################
  
  output$UI.doHabSuitability = renderUI({
    if (input$doHabSuitability)
    {
      selectInput(inputId = "HABSUIT.ref_option"
                  , label = ""
                  , choices = c("(1) random", "(2) PFG specific")
                  , selected = "(1) random"
                  , multiple = FALSE
                  , width = "100%")
    } 
  })
  
  ####################################################################
  
  output$UI.doDisturbances = renderUI({
    if (input$doDisturbances)
    {
      column(12
             , numericInput(inputId = "DIST.no"
                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.no</span>")
                            , min = 1
                            , value = 1
                            , width = "100%")
             , numericInput(inputId = "DIST.no_sub"
                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.no_sub</span>")
                            , min = 4
                            , max = 4
                            , value = 1
                            , width = "100%")
             , numericInput(inputId = "DIST.freq"
                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>DIST.freq</span>")
                            , min = 1
                            , value = 1
                            , width = "100%")
      )
    }
  })
  
  ####################################################################
  
  output$UI.doLight = renderUI({
    if (input$doLight)
    {
      column(12
             , numericInput(inputId = "LIGHT.thresh_medium"
                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>LIGHT.thresh_medium</span>")
                            , min = 1
                            , value = 1
                            , width = "100%")
             , numericInput(inputId = "LIGHT.thresh_low"
                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>LIGHT.thresh_low</span>")
                            , min = 4
                            , max = 4
                            , value = 1
                            , width = "100%")
      )
    }
  })
  
  ####################################################################
  
  observeEvent(input$create.global, {
    if (input$create.skeleton > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_globalParameters(name.simulation = input$name.simul
                                         , opt.no_CPU = input$opt.no_CPU
                                         , required.no_PFG = input$required.no_PFG
                                         , required.no_STRATA = input$required.no_STRATA
                                         , required.simul_duration = input$required.simul_duration
                                         , required.seeding_duration = input$required.seeding_duration
                                         , required.seeding_timestep = input$required.seeding_timestep
                                         , required.seeding_input = input$required.seeding_input
                                         , required.max_by_cohort = input$required.max_by_cohort
                                         , required.max_abund_low = input$required.max_abund_low
                                         , required.max_abund_medium = input$required.max_abund_medium
                                         , required.max_abund_high = input$required.max_abund_high
                                         , doDispersal = input$doDispersal
                                         , doHabSuitability = input$doHabSuitability
                                         , HABSUIT.ref_option = ifelse(input$HABSUIT.ref_option == "(1) random", 1, 2)
                                         , doLight = input$doLight
                                         , LIGHT.thresh_medium = input$LIGHT.thresh_medium
                                         , LIGHT.thresh_low = input$LIGHT.thresh_low
                                         , doSoil = input$doSoil
                                         , doDisturbances = input$doDisturbances
                                         , DIST.no = input$DIST.no
                                         , DIST.no_sub = input$DIST.no_sub
                                         , DIST.freq = input$DIST.freq
        )
      ))
      
      if(get_res)
      {
        output$created_table.global = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
          return(get_files(path_folder))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  observeEvent(input$add.PFG.name, {
    names.PFG <<- c(names.PFG, input$name.PFG)
    output$names.PFG = renderText({
      paste0("PFG list : ", paste0(names.PFG, collapse = " "))
    })
    shinyjs::show("add.PFG.succ")
    shinyjs::show("create.succ")
  })
  
  output$UI.succ.PFG = renderUI({
    if (input$add.PFG.name || input$delete.names.PFG)
    {
      selectInput(inputId = "succ.PFG"
                  , label = NULL
                  , choices = names.PFG
                  , selected = NULL
                  , multiple = F
                  , width = "100%"
      )
    }
  })
  
  observeEvent(input$delete.names.PFG, {
    names.PFG <<- c()
    output$names.PFG = renderText({ names.PFG })
    shinyjs::hide("add.PFG.succ")
    shinyjs::hide("create.succ")
  })
  
  ####################################################################
  
  # observeEvent(input$refresh, {
  #   system(command = paste0("rm -r ", input$name.simul))
  #   shinyjs::hide("main.panel")
  #   shinyjs::hide("create.simul")
  #   shinyjs::hide("UI.download.folder")
  #   shinyjs::hide("refresh")
  # })
  
  observeEvent(input$add.PFG.succ, {
    mat.PFG.succ <<- rbind(mat.PFG.succ
                           , data.frame(PFG = input$succ.PFG
                                        , type = input$succ.type
                                        , height = as.numeric(input$succ.height)
                                        , maturity = as.numeric(input$succ.maturity)
                                        , longevity = as.numeric(input$succ.longevity)
                                        , light = as.numeric(input$succ.light)))
    output$mat.PFG.succ = renderTable({ mat.PFG.succ })
  })
  
  observeEvent(input$delete.PFG.succ, {
    mat.PFG.succ <<- data.frame()
    output$mat.PFG.succ = renderTable({ mat.PFG.succ })
  })
  
  ####################################################################
  
  observeEvent(input$create.succ, {
    if (input$create.skeleton > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_PFGsuccession(name.simulation = input$name.simul
                                      , mat.PFG.succ = mat.PFG.succ
        )
      ))
      
      if(get_res)
      {
        output$created_table.succ = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
          return(get_files(path_folder))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  output$UI.disp.PFG = renderUI({
    if (input$create.succ > 0)
    {
      names.PFG = list.files(path = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
                             , pattern = "^SUCC_")
      names.PFG = sub("^SUCC_", "", names.PFG)
      names.PFG = sub(".txt$", "", names.PFG)
      selectInput(inputId = "disp.PFG"
                  , label = NULL
                  , choices = names.PFG
                  , multiple = FALSE
                  , width = "100%")
    } else
    {
      textInput(inputId = "disp.PFG"
                , label = NULL
                , width = "100%")
    }
  })
  
  ####################################################################
  
  observeEvent(input$add.PFG.disp, {
    mat.PFG.disp <<- rbind(mat.PFG.disp
                           , data.frame(PFG = input$disp.PFG
                                        , MODE = as.numeric(input$disp.mode)
                                        , d50 = as.numeric(input$disp.d50)
                                        , d99 = as.numeric(input$disp.d99)
                                        , ldd = as.numeric(input$disp.ldd)))
    output$mat.PFG.disp = renderTable({ mat.PFG.disp })
  })
  
  observeEvent(input$delete.PFG.disp, {
    mat.PFG.disp <<- data.frame()
    output$mat.PFG.disp = renderTable({ mat.PFG.disp })
  })
  
  ####################################################################
  
  observeEvent(input$create.disp, {
    if (input$create.skeleton > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_PFGdispersal(name.simulation = input$name.simul
                                     , mat.PFG.disp = mat.PFG.disp
        )
      ))
      
      if(get_res)
      {
        output$created_table.disp = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/PFGS/DISP/")
          return(get_files(path_folder))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  output$UI.dist.grouping = renderUI({
    if (input$dist.grouping == "by type")
    {
      fluidRow(
        column(6
               , br()
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML(""))
                 , column(4, HTML("<strong>H</strong>"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Stage 1</strong>"))
                 , column(4, HTML("<strong>Killed</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.1.kill.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Resprout</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.1.resprout.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Stage 2</strong>"))
                 , column(4, HTML("<strong>Killed</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.2.kill.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Resprout</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.2.resprout.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Stage 3</strong>"))
                 , column(4, HTML("<strong>Killed</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.3.kill.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Resprout</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.3.resprout.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML("<strong> Stage 4</strong>"))
                 , column(4, HTML("<strong>Killed</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.4.kill.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               )
               , fluidRow(
                 column(4, HTML(""))
                 , column(4, HTML("<strong>Resprout</strong>"))
                 , column(4
                          , selectInput(inputId = "dist.4.resprout.H"
                                        , label = NULL
                                        , choices = seq(0,100,10)
                                        , multiple = FALSE
                                        , width = "100%"))
               ))
        , column(2
                 , br()
                 , HTML("<strong>C</strong>")
                 , selectInput(inputId = "dist.1.kill.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.1.resprout.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.2.kill.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.2.resprout.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.3.kill.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.3.resprout.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.4.kill.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.4.resprout.C"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
        )
        , column(2
                 , br()
                 , HTML("<strong>P</strong>")
                 , selectInput(inputId = "dist.1.kill.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.1.resprout.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.2.kill.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.2.resprout.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.3.kill.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.3.resprout.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.4.kill.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
                 , selectInput(inputId = "dist.4.resprout.P"
                               , label = NULL
                               , choices = seq(0,100,10)
                               , multiple = FALSE
                               , width = "100%")
        )
      )
    } else
    {
      # names.PFG = list.files(path = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
      #                        , pattern = "^SUCC_")
      # names.PFG = sub("^SUCC_", "", names.PFG)
      # names.PFG = sub(".txt$", "", names.PFG)
    }
  })
  
  ####################################################################
  
  observeEvent(input$add.PFG.dist, {
    if (input$dist.grouping == "by type")
    {
      mat.PFG.dist <<- rbind(mat.PFG.dist
                             , data.frame(name = input$dist.name
                                          , responseStage = 1:4
                                          , KilledIndiv_H = as.numeric(c(input$dist.1.kill.H
                                                                         , input$dist.2.kill.H
                                                                         , input$dist.3.kill.H
                                                                         , input$dist.4.kill.H)) / 10
                                          , KilledIndiv_C = as.numeric(c(input$dist.1.kill.C
                                                                         , input$dist.2.kill.C
                                                                         , input$dist.3.kill.C
                                                                         , input$dist.4.kill.C)) / 10
                                          , KilledIndiv_P = as.numeric(c(input$dist.1.kill.P
                                                                         , input$dist.2.kill.P
                                                                         , input$dist.3.kill.P
                                                                         , input$dist.4.kill.P)) / 10
                                          , ResproutIndiv_H = as.numeric(c(input$dist.1.resprout.H
                                                                           , input$dist.2.resprout.H
                                                                           , input$dist.3.resprout.H
                                                                           , input$dist.4.resprout.H)) / 10
                                          , ResproutIndiv_C = as.numeric(c(input$dist.1.resprout.C
                                                                           , input$dist.2.resprout.C
                                                                           , input$dist.3.resprout.C
                                                                           , input$dist.4.resprout.C)) / 10
                                          , ResproutIndiv_P = as.numeric(c(input$dist.1.resprout.P
                                                                           , input$dist.2.resprout.P
                                                                           , input$dist.3.resprout.P
                                                                           , input$dist.4.resprout.P)) / 10
                             ))
    } else
    {
      
    }
    output$mat.PFG.dist = renderTable({ mat.PFG.dist })
  })
  
  observeEvent(input$delete.PFG.dist, {
    mat.PFG.dist <<- data.frame()
    output$mat.PFG.dist = renderTable({ mat.PFG.dist })
  })
  
  ####################################################################
  
  observeEvent(input$create.dist, {
    if (input$create.skeleton > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_PFGdisturbance(name.simulation = input$name.simul
                                       , mat.PFG.dist = mat.PFG.dist
        )
      ))
      
      if(get_res)
      {
        output$created_table.dist = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
          return(get_files(path_folder))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  observeEvent(input$create.save.maps, {
    if (input$create.skeleton > 0)
    {
      opt.folder.name = ifelse(nchar(input$save.maps.folder) > 0, input$save.maps.folder, "")
      get_res = print_messages(as.expression(
        PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                  , years.maps = round(seq(input$save.maps.year1
                                                           , input$save.maps.year2
                                                           , length.out = input$save.maps.no))
                                  , years.objects = NULL
                                  , opt.folder.name = opt.folder.name
        )
      ))
      
      if(get_res)
      {
        output$created_table.save = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/SAVE/", opt.folder.name)
          if (nchar(opt.folder.name) > 0) path_folder = paste0(path_folder, "/")
          return(get_files(path_folder, skip.no = 0))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  
  observeEvent(input$create.save.objects, {
    if (input$create.skeleton > 0)
    {
      opt.folder.name = ifelse(nchar(input$save.objects.folder) > 0, input$save.objects.folder, "")
      get_res = print_messages(as.expression(
        PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                  , years.maps = NULL
                                  , years.objects = c(input$save.objects.year1
                                                      , input$save.objects.year2
                                                      , input$save.objects.year3)
                                  , opt.folder.name = opt.folder.name
        )
      ))
      
      if(get_res)
      {
        output$created_table.save = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/SAVE/", opt.folder.name)
          if (nchar(opt.folder.name) > 0) path_folder = paste0(path_folder, "/")
          return(get_files(path_folder, skip.no = 0))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  observeEvent(input$upload.mask, {
    if (input$create.skeleton > 0)
    {
      if(!is.null(input$simul.mask))
      {
        file1 = input$simul.mask$datapath
        file2 = input$simul.mask$name
        file2 = paste0("MASK_", file2)
        file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
        get_res = file.copy(from = file1, to = file2)
        
        if(get_res)
        {
          shinyalert(type = "success", text = paste0("The file ", input$simul.mask$name
                                                     , " has been correctly uploaded and renamed as "
                                                     , file2, " !"))
        } else
        {
          shinyalert(type = "error", text = "Oops. Something went wrong !")
        }
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  observeEvent(input$upload.habsuit.mask, {
    if (input$create.skeleton > 0)
    {
      if(!is.null(input$habsuit.mask))
      {
        if (nchar(input$habsuit.folder) > 0)
        {
          dir.create(paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", input$habsuit.folder))
          opt.folder.name = paste0(input$habsuit.folder, "/")
        } else
        {
          opt.folder.name = ""
        }
        
        file1 = input$habsuit.mask$datapath
        file2 = input$habsuit.mask$name
        file2 = paste0("HABSUIT_", file2)
        file2 = paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", opt.folder.name, file2)
        get_res = file.copy(from = file1, to = file2)
        
        if(get_res)
        {
          shinyalert(type = "success", text = paste0("The file ", input$habsuit.mask$name
                                                     , " has been correctly uploaded and renamed as "
                                                     , file2, " !"))
        } else
        {
          shinyalert(type = "error", text = "Oops. Something went wrong !")
        }
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  ####################################################################
  
  observeEvent(input$upload.dist.mask, {
    if (input$create.skeleton > 0)
    {
      if(!is.null(input$dist.mask))
      {
        file1 = input$dist.mask$datapath
        file2 = input$dist.mask$name
        file2 = paste0("DIST_", file2)
        file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
        get_res = file.copy(from = file1, to = file2)
        
        if(get_res)
        {
          shinyalert(type = "success", text = paste0("The file ", input$dist.mask$name
                                                     , " has been correctly uploaded and renamed as "
                                                     , file2, " !"))
        } else
        {
          shinyalert(type = "error", text = "Oops. Something went wrong !")
        }
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
  observeEvent(input$add.changing, {
    # if(!is.null(input$changing.file))
    # {
    mat.changing <<- rbind(mat.changing
                           , data.frame(opt.folder.name = input$changing.folder
                                        , type.changing = input$type.changing
                                        , year = input$changing.year
                                        , order = input$changing.order
                                        , file.name = input$changing.file))
    # , file.name = input$changing.file$name))
    output$mat.changing = renderTable({ mat.changing })
    # }
  })
  
  observeEvent(input$delete.changing, {
    mat.changing <<- data.frame()
    output$mat.changing = renderTable({ mat.changing })
  })
  
  ####################################################################
  
  observeEvent(input$create.changing, {
    if (input$create.skeleton > 0)
    {
      if (nrow(mat.changing) > 0)
      {
        mat.changing.split = split(mat.changing, list(mat.changing$type.changing, mat.changing$opt.folder.name))
        print(length(mat.changing.split))
        for(i in 1:length(mat.changing.split))
        {
          print(i)
          tab = mat.changing.split[[i]]
          tab$opt.folder.name = as.character(tab$opt.folder.name)
          tab$file.name = as.character(tab$file.name)
          print(nrow(tab))
          print(tab)
          print(tab$opt.folder.name)
          if (nrow(tab) > 0 && sum(nchar(tab$file.name) == 0) == 0)
          {
            if (nchar(tab$opt.folder.name) > 0)
            {
              dir.create(paste0(input$name.simul, "/DATA/scenario/", tab$opt.folder.name))
            }
            get_res = print_messages(as.expression(
              PRE_FATE.params_changingYears(name.simulation = input$name.simul
                                            , type.changing = tab$type.changing
                                            , mat.changing = data.frame(year = tab$year
                                                                        , order = tab$year
                                                                        , file.name = tab$file.name)
                                            , opt.folder.name = tab$opt.folder.name
              )
            ))
            
            if(get_res)
            {
              output$created_table.changing = renderDataTable({
                path_folder = paste0(input$name.simul, "/DATA/SCENARIO/")
                return(get_files(path_folder, skip.no = 0))
              })
            }
          }
        }
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

