
rm(list = ls())

## load required libraries
# library(foreach)
# library(reshape2)
# library(ggplot2)
# library(ggthemes)
# library(plotly)
library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(markdown)
library(RFate)
# library(RFate)
# library(DT)
library(data.table)
# library(zip)

###################################################################################################################################

# <link rel = "stylesheet" href = "https://use.fontawesome.com/releases/v5.5.0/css/solid.css" integrity =
#   "sha384-rdyFrfAIC05c5ph7BKz3l5NG5yEottvO/DQ0dCrwD8gzeQDjYBHNr1ucUpQuljos" crossorigin =
#     "anonymous" >
# < link rel = "stylesheet" href = "https://use.fontawesome.com/releases/v5.5.0/css/fontawesome.css" integrity =
#   "sha384-u5J7JghGz0qUrmEsWzBQkfvc8nK3fUT7DCaQzNQ+q4oEXhGSx+P2OqjWsfIRB8QT" crossorigin =
#     "anonymous" >

# button.color = "#e2ac8c"

get_messages = function(out_info)
{
  ind_empty = which(nchar(out_info) == 0)
  ind_Warning = grep("^Warning", out_info)
  ind_Warning_mess = vector()
  for(ind in ind_Warning){
    if (length(ind_empty) > 0)
    {
      i = ind + 1
      next_empty = FALSE
      while(!next_empty)
      {
        if (i %in% ind_empty){
          next_empty = TRUE
        } else {
          i = i + 1
        }
      }
    } else
    {
      i = length(out_info) + 1
    }
    ind_Warning_mess = c(ind_Warning_mess, (ind+1):(i-1))
  }
  ind_mess = 1:length(out_info)
  ind_mess = ind_mess[-c(ind_empty, ind_Warning, ind_Warning_mess)]
  return(list(warning = out_info[ind_Warning_mess]
              , message = out_info[ind_mess]))
}

print_messages = function(fun)
{
  out_fun = tryCatch(
    capture.output(type = "message", expr = { fun })
    , error = function(e) { e })
  if (inherits(out_fun, "simpleError"))
  {
    shinyalert(type = "error", text = out_fun$message)
    return(0)
  } else
  {
    out_info = get_messages(out_fun)
    if (length(out_info$warning) > 0)
    {
      showNotification(out_info$warning, type = "warning")
    }
    if (length(out_info$message) > 0)
    {
      shinyalert(type = "success", text = out_info$message)
    }
    return(1)
  }
}


###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  
  # theme = "cosmo",
  titlePanel(HTML("<p style='background-color:#068f96; padding:10px; margin-bottom:50px; border-radius:2px;
                  font-family:Serif; color:#FFFFFF'>
                  FATE : create simulation folder</p>")
             , windowTitle = "FATE"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 3,
      style = "border-solid:solid; border-width:2px; border-color:#068f96;",
      withMathJax(),
      fluidRow(
        column(12
               , ""
               , actionButton(inputId = "refresh"
                              , label = "Start new simulation"
                              , icon = icon("refresh")
                              , width = "100%")
        )
      ),
      br(),
      br(),
      br(),
      fluidRow(
        column(12
               , ""
               , textInput(inputId = "name.simul"
                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>Enter the simulation name :</span>")
                           , value = "FATE_simulation"
                           , width = "100%")
        )
      ),
      fluidRow(
        column(12
               , ""
               , actionButton(inputId = "create.skeleton"
                              , label = "Create folder"
                              , icon = icon("folder")
                              , width = "100%")
               # , style = HTML(paste0("background-color: ", button.color, ";")))
        )
      ),
      br(),
      helpText(HTML("
<ul>
<li>DATA
<ul>
  <li>GLOBAL_PARAMETERS</li>
  <li>NAMESPACE_CONSTANTS</li>
  <li>MASK</li>
  <li>SCENARIO</li>
  <li>SAVE</li>
  <li>PFGS :
  <ul>
    <li>SUCC</li>
    <li>DISP</li>
    <li>DIST</li>
    <li>HABSUIT</li>
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
               , ""
               , actionButton(inputId = "download.folder"
                              , label = "Download folder"
                              , icon = icon("download")
                              , width = "100%")
        )
      )
    ),
    
    
    
    # Output
    mainPanel(
      wellPanel(
      style = "border-solid:solid; border-width:2px; border-color:#068f96;",
      tabsetPanel(
        # type = "pills",
        tabPanel(title = HTML("<p style='background-color:#068f96; padding:10px; margin-top:0px; border-radius:2px;
                  font-family:Serif; color:#FFFFFF'>Internal settings</p>")
                 , value = "create.namespace"
                 , br()
                 , helpText(HTML("
                                 <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_namespaceConstants.html' target='_blank'>
                                 See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                 <table style='width:100%;'>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;vertical-align:top;'>global.abund.low</td>
                                 <td style='width:70%;'><em>Not used for now. To be removed ?</em></td>
                                 </tr>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;vertical-align:top;'>global.abund.med</td>
                                 <td style='width:70%;'><em>Not used for now. To be removed ?</em></td>
                                 </tr>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;vertical-align:top;'>global.abund.high</td>
                                 <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values in each pixel</td>
                                 </tr>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;'>global.max.by.cohort</td>
                                 <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of each cohort in each pixel</td>
                                 </tr>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;vertical-align:top;'>global.resource.thresh.med</td>
                                 <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
                                 It corresponds to the limit of abundances above which light resources are medium.
                                 PFG abundances lower than this threshold imply high amount of light.
                                 It is consequently lower than <span style='font-family:Monospace;'>global.resource.thresh.low</span>.</td>
                                 </tr>
                                 <tr>
                                 <td style='width:30%;font-family:Monospace;vertical-align:top;'>global.resource.thresh.low</td>
                                 <td style='width:70%;'>an integer in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
                                 It corresponds to the limit of abundances above which light resources are low.
                                 PFG abundances higher than <span style='font-family:Monospace;'>global.resource.thresh.med</span> and lower than this threshold imply medium amount of light.</td>
                                 </tr>
                                 </table>
                                 " 
                 ))
                 , fluidRow(
                   column(6
                          , br()
                          , br()
                          , numericInput(inputId = "global.abund.low"
                                         , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.abund.low</span>")
                                         , value = 1000000
                                         , width = "100%")
                          , numericInput(inputId = "global.abund.med"
                                         , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.abund.med</span>")
                                         , value = 5000000
                                         , width = "100%")
                          , numericInput(inputId = "global.abund.high"
                                         , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.abund.high</span>")
                                         , value = 8000000
                                         , width = "100%")
                   )
                   , column(6
                            , br()
                            , br()
                            , numericInput(inputId = "global.max.by.cohort"
                                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.max.by.cohort</span>")
                                           , value = 5000000
                                           , width = "100%")
                            , numericInput(inputId = "global.resource.thresh.med"
                                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.resource.thresh.med</span>")
                                           , value = 13000000
                                           , width = "100%")
                            , numericInput(inputId = "global.resource.thresh.low"
                                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>global.resource.thresh.low</span>")
                                           , value = 19000000
                                           , width = "100%")
                   )
                 )
                 , fluidRow(
                   column(6
                          , br()
                          , actionButton(inputId = "create.namespaceConstants"
                                         , label = "Create Namespace constants file"
                                         , icon = icon("file")
                                         , width = "100%")
                          # , style = HTML(paste0("background-color: ", button.color, ";")))
                          , br())
                 )
                 ),
        tabPanel(title = HTML("<p style='background-color:#068f96; padding:10px; margin-top:0px; border-radius:2px;
                  font-family:Serif; color:#FFFFFF'>PFG files</p>")
                 , value = "create.PFG"
                 , br()
                 , tabsetPanel(
                   tabPanel(title = "Succession"
                            , value = "panel.succ"
                            , br()
                            , helpText(HTML("
                                            <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html' target='_blank'>
                                            See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                            <table style='width:100%;'>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>type</td>
                                            <td style='width:70%;'>or life-form, based on Raunkier.
                                            It should be either <span style='font-family:Monospace;'>H</span> (herbaceous),
                                            <span style='font-family:Monospace;'>C</span> (chamaephyte) or <span style='font-family:Monospace;'>P</span> (phanerophyte) for now</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>height</td>
                                            <td style='width:70%;'>the maximum or average height that reach the PFG</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>maturity</td>
                                            <td style='width:70%;'>the age from which the PFG can reproduce</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>longevity</td>
                                            <td style='width:70%;'>the maximum or average lifespan of the PFG</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>light</td>
                                            <td style='width:70%;'>a value between 0 and 10 corresponding to the Ellenberg value of the PFG</td>
                                            </tr>
                                            </table>
                                            "
                            ))
                            , fluidRow(
                              column(6
                                     , br()
                                     , actionButton(inputId = "add.PFG.succ"
                                                    , label = "Add PFG"
                                                    , icon = icon("plus")
                                                    , width = "100%"))
                              , column(6
                                       , br()
                                       , actionButton(inputId = "create.succ"
                                                      , label = "Create PFG succession files"
                                                      , icon = icon("file")
                                                      , width = "100%"))
                              # , style = HTML(paste0("background-color: ", button.color, ";")))
                            )
                            , fluidRow(
                              column(2
                                     , br()
                                     , br()
                                     , HTML("<strong>PFG</strong>")
                                     , textInput(inputId = "succ.PFG"
                                                 , label = NULL
                                                 , width = "100%"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>type</strong>")
                                       , selectInput(inputId = "succ.type"
                                                     , label = NULL
                                                     , choices = c("H", "C", "P")
                                                     , selected = NULL
                                                     , multiple = F
                                                     , width = "100%"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>height</strong>")
                                       , numericInput(inputId = "succ.height"
                                                      , label = NULL
                                                      , value = 0
                                                      , min = 0
                                                      , width = "100%"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>maturity</strong>")
                                       , numericInput(inputId = "succ.maturity"
                                                      , label = NULL
                                                      , value = 0
                                                      , min = 0
                                                      , width = "100%"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>longevity</strong>")
                                       , numericInput(inputId = "succ.longevity"
                                                      , label = NULL
                                                      , value = 0
                                                      , min = 0
                                                      , width = "100%"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>light</strong>")
                                       , selectInput(inputId = "succ.light"
                                                     , label = NULL
                                                     , choices = 0:10
                                                     , selected = NULL
                                                     , multiple = F
                                                     , width = "100%"))
                            )
                            )
                   , tabPanel(title = "Dispersal"
                              , value = "panel.disp"
                              , br()
                              , helpText(HTML("
                                            <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html' target='_blank'>
                                            See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                            <table style='width:100%;'>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>MODE :</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>[1] uniform kernel</td>
                                            <td style='width:70%;'>homogeneous dispersal within the d50, d99 and ldd circles</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>[2] exponential kernel</td>
                                            <td style='width:70%;'>seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1)</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>[3] exponential kernel <br/>with probability</td>
                                            <td style='width:70%;'>seeds are dispersed within each concentric circle according to a decreasing exponential density law (lambda = 1) and a continuous decreasing probability with distance</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>d50</td>
                                            <td style='width:70%;'>the distance at which 50% of seeds are dispersed</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>d99</td>
                                            <td style='width:70%;'>the distance at which 49% of seeds are dispersed</td>
                                            </tr>
                                            <tr>
                                            <td style='width:30%;font-family:Monospace;vertical-align:top;'>ldd</td>
                                            <td style='width:70%;'>the long dispersal distance at which 1% of seeds are dispersed</td>
                                            </tr>
                                            </table>
                               "
                              ))
                              , fluidRow(
                                column(6
                                       , br()
                                       , actionButton(inputId = "add.PFG.disp"
                                                      , label = "Add PFG"
                                                      , icon = icon("plus")
                                                      , width = "100%"))
                                , column(6
                                         , br()
                                         , actionButton(inputId = "create.disp"
                                                        , label = "Create PFG dispersal files"
                                                        , icon = icon("file")
                                                        , width = "100%"))
                                         # , style = HTML(paste0("background-color: ", button.color, ";")))
                                )
                                , fluidRow(
                                  column(2
                                         , br()
                                         , br()
                                         , HTML("<strong>PFG</strong>")
                                         , textInput(inputId = "disp.PFG"
                                                     , label = NULL
                                                     , width = "100%"))
                                  , column(2
                                           , br()
                                           , br()
                                           , HTML("<strong>MODE</strong>")
                                           , selectInput(inputId = "disp.light"
                                                         , label = NULL
                                                         , choices = 1:3
                                                         , selected = NULL
                                                         , multiple = F
                                                         , width = "100%"))
                                  , column(2
                                           , br()
                                           , br()
                                           , HTML("<strong>d50</strong>")
                                           , numericInput(inputId = "disp.d50"
                                                          , label = NULL
                                                          , value = 0
                                                          , min = 0
                                                          , width = "100%"))
                                  , column(2
                                           , br()
                                           , br()
                                           , HTML("<strong>d99</strong>")
                                           , numericInput(inputId = "disp.d99"
                                                          , label = NULL
                                                          , value = 0
                                                          , min = 0
                                                          , width = "100%"))
                                  , column(2
                                           , br()
                                           , br()
                                           , HTML("<strong>ldd</strong>")
                                           , numericInput(inputId = "disp.ldd"
                                                          , label = NULL
                                                          , value = 0
                                                          , min = 0
                                                          , width = "100%"))
                                )
                              )
                              , tabPanel(title = "Disturbances"
                                         , value = "create.dist")
                   )
                   ),
                 tabPanel(title = HTML("<p style='background-color:#068f96; padding:10px; margin-top:0px; border-radius:2px;
                  font-family:Serif; color:#FFFFFF'>Scenario files</p>")
                          , value = "create.scenario"
                          , fluidRow(
                            column(6
                                   , checkboxInput(inputId = "save.maps"
                                                   , label = "Save maps ?"
                                                   , value = FALSE)
                            )
                            , column(6
                                     , checkboxInput(inputId = "save.objects"
                                                     , label = "Save objects ?"
                                                     , value = FALSE)
                            )
                          )
                 ),
                 tabPanel(title = HTML("<p style='background-color:#068f96; padding:10px; margin-top:0px; border-radius:2px;
                  font-family:Serif; color:#FFFFFF'>Simulation files</p>")
                          , value = "create.simul"
                 )
        )
        # , wellPanel(dataTableOutput(outputId = "created_table"))
          # textOutput("out_message"))
        )
      , wellPanel(dataTableOutput(outputId = "created_table"))
    )
  )
)


###################################################################################################################################
cat <- message
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  ####################################################################
  
  observeEvent(input$create.skeleton, {
    print_messages(as.expression(
      PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
    ))
  })
  
  ####################################################################
  
  observeEvent(input$create.namespaceConstants, {
    if (input$create.skeleton > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_namespaceConstants(name.simulation = input$name.simul
                                           , global.abund.low = input$global.abund.low
                                           , global.abund.med = input$global.abund.med
                                           , global.abund.high = input$global.abund.high
                                           , global.max.by.cohort = input$global.max.by.cohort
                                           , global.resource.thresh.med = input$global.resource.thresh.med
                                           , global.resource.thresh.low = input$global.resource.thresh.low
        )
      ))
      
      if(get_res)
      {
        output$created_table = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/NAMESPACE_CONSTANTS/")
          tab_name = list.files(path = path_folder)
          tab = fread(paste0(path_folder, tab_name), header = FALSE)
          return(tab)
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  # ####################################################################
  # output$dirRes_selector = renderUI({
  # 
  # })
  # 
  # ####################################################################
  # get_zipfiles = eventReactive(input$refresh, {
  #   zip(zipfile = paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
  #       , list.files(path = get_dirSave(), full.names = T))
  # })
  # 
  # observeEvent(input$doZip, {
  #   get_zipfiles()
  # })
  # 
  # output$zip_selector = renderUI({
  #   if (input$doZip)
  #   {
  #     selectInput(inputId = "zip"
  #                 , label = "Sélectionner un dossier"
  #                 , choices = c("",list.files(pattern = "^SAUVEGARDE_ORCHAMP_"))
  #                 , selected = NULL
  #                 , multiple = TRUE
  #     )
  #   }
  # })
  # 
  # output$downloadSelection = downloadHandler(
  #   filename = function(){
  #     paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip")
  #   },
  #   content = function(file){
  #     file.copy(paste0("SAUVEGARDE_ORCHAMP_selection_", Sys.Date(), ".zip"), file)
  #   },
  #   contentType = "application/zip"
  # )
  
  
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

