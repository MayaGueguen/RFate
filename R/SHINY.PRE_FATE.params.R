
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
library(foreach)
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

get_files = function(path_folder, skip.no = 2)
{
  tab_names = list.files(path = path_folder, include.dirs = FALSE)
  tab = foreach(tab_name = tab_names, .combine = "cbind") %do%
  {
    fread(file = paste0(path_folder, tab_name), header = FALSE, skip = skip.no, sep = "\t")
  }
  colnames(tab) = tab_names
  return(tab)
}


mat.PFG.succ = data.frame()
mat.PFG.disp = data.frame()

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
               , uiOutput(outputId = "UI.download.folder")
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
                                   <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
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
                   , fluidRow(
                     br(),
                     wellPanel(dataTableOutput(outputId = "created_table.namespace"))
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
                              , fluidRow(
                                column(10
                                       , br()
                                       , tableOutput(outputId = "mat.PFG.succ"))
                                , column(2
                                         , br()
                                         , actionButton(inputId = "delete.PFG.succ"
                                                        , label = NULL
                                                        , icon = icon("trash")))
                              )
                              , fluidRow(
                                br(),
                                wellPanel(dataTableOutput(outputId = "created_table.succ"))
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
                                                <td style='width:70%;'>homogeneous dispersal within the <span style='font-family:Monospace;'>d50</span>,
                                                <span style='font-family:Monospace;'>d99</span> and <span style='font-family:Monospace;'>ldd</span> circles</td>
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
                                         , uiOutput(outputId = "UI.disp.PFG"))
                                  , column(2
                                           , br()
                                           , br()
                                           , HTML("<strong>MODE</strong>")
                                           , selectInput(inputId = "disp.mode"
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
                                , fluidRow(
                                  column(10
                                         , br()
                                         , tableOutput(outputId = "mat.PFG.disp"))
                                  , column(2
                                           , br()
                                           , actionButton(inputId = "delete.PFG.disp"
                                                          , label = NULL
                                                          , icon = icon("trash")))
                                )
                                , fluidRow(
                                  br(),
                                  wellPanel(dataTableOutput(outputId = "created_table.disp"))
                                )
                                )
                     , tabPanel(title = "Disturbances"
                                , value = "panel.dist"
                                , br()
                                , helpText(HTML("
                                                <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html' target='_blank'>
                                                See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                                <table style='width:100%;'>
                                                <tr>
                                                <td style='width:30%;font-family:Monospace;vertical-align:top;'>[1] uniform kernel</td>
                                                <td style='width:70%;'>homogeneous dispersal within the <span style='font-family:Monospace;'>d50</span>,
                                                <span style='font-family:Monospace;'>d99</span> and <span style='font-family:Monospace;'>ldd</span> circles</td>
                                                </tr>
                                                </table>
                                                "
                                ))
                                , fluidRow(
                                  column(6
                                         , br()
                                         , actionButton(inputId = "add.PFG.dist"
                                                        , label = "Add disturbance"
                                                        , icon = icon("plus")
                                                        , width = "100%"))
                                  , column(6
                                           , br()
                                           , actionButton(inputId = "create.dist"
                                                          , label = "Create PFG disturbance files"
                                                          , icon = icon("file")
                                                          , width = "100%"))
                                  # , style = HTML(paste0("background-color: ", button.color, ";")))
                                )
                                , fluidRow(
                                  column(4
                                         , br()
                                         , br()
                                         , HTML("<strong>Disturbance</strong>")
                                         , textInput(inputId = "dist.name"
                                                     , label = NULL
                                                     , width = "100%"))
                                  , column(4
                                           , br()
                                           , br()
                                           , HTML("<strong>Disturbance</strong>")
                                           , radioButtons(inputId = "dist.grouping"
                                                                , label = NULL
                                                                , choices = c("by type", "by PFG")
                                                                , selected = "by type"
                                                                , width = "100%"))
                                )
                                ,fluidRow(
                                  uiOutput(outputId = "UI.dist.grouping")
                                )
                                , fluidRow(
                                  column(10
                                         , br()
                                         , tableOutput(outputId = "mat.PFG.dist"))
                                  , column(2
                                           , br()
                                           , actionButton(inputId = "delete.PFG.dist"
                                                          , label = NULL
                                                          , icon = icon("trash")))
                                )
                                , fluidRow(
                                  br(),
                                  wellPanel(dataTableOutput(outputId = "created_table.dist"))
                                )
                                )
                     )
                     )
          , tabPanel(title = HTML("<p style='background-color:#068f96; padding:10px; margin-top:0px; border-radius:2px;
                                  font-family:Serif; color:#FFFFFF'>Scenario files</p>")
                     , value = "create.scenario"
                     , br()
                     , helpText(HTML("
                                     <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html' target='_blank'>
                                     See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                                     <table style='width:100%;'>
                                     <tr>
                                     <td style='width:30%;font-family:Monospace;vertical-align:top;'>years.maps</td>
                                     <td style='width:70%;'>a <span style='font-family:Monospace;'>vector</span> of simulation years at which PFG abundance maps will be saved</td>
                                     </tr>
                                     <tr>
                                     <td style='width:30%;font-family:Monospace;vertical-align:top;'>years.objects</td>
                                     <td style='width:70%;'>a <span style='font-family:Monospace;'>vector</span> of simulation years at which FATE-HD simulation state will be saved</td>
                                     </tr>
                                     <tr>
                                     <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.folder.name</td>
                                     <td style='width:70%;'><em>(optional) a <span style='font-family:Monospace;'>string</span> that corresponds to the name of the folder that will 
                                     be created into the <span style='font-family:Monospace;'>name.simulation/DATA/SAVE/</span> directory to store the results</em></td>
                                     </tr>
                                     </table>
                                     "
                     ))
                     , fluidRow(
                       column(6
                              , br()
                              , wellPanel(
                                HTML("<strong>Save maps ?</strong>")
                                , br()
                                , br()
                                , textInput(inputId = "save.maps.folder"
                                            , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.folder.name</span>")
                                            , value = NULL
                                            , width = "100%")
                                , br()
                                , br()
                                , numericInput(inputId = "save.maps.year1"
                                               , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.start</span>")
                                               , value = 0
                                               , min = 0
                                               , width = "100%")
                                , numericInput(inputId = "save.maps.year2"
                                               , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.end</span>")
                                               , value = 0
                                               , min = 0
                                               , width = "100%")
                                , numericInput(inputId = "save.maps.no"
                                               , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.maps.number</span>")
                                               , value = 0
                                               , min = 0
                                               , max = 100
                                               , step = 10
                                               , width = "100%")
                                , br()
                                , br()
                                , actionButton(inputId = "create.save.maps"
                                               , label = "Create SAVE maps files"
                                               , icon = icon("file")
                                               , width = "100%")
                              )
                       )
                       , column(6
                                , br()
                                , wellPanel(
                                  HTML("<strong>Save simulation ?</strong>")
                                  , br()
                                  , br()
                                  , textInput(inputId = "save.objects.folder"
                                              , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>opt.folder.name</span>")
                                              , value = NULL
                                              , width = "100%")
                                  , br()
                                  , br()
                                  , numericInput(inputId = "save.objects.year1"
                                                 , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>years.objects</span>")
                                                 , value = 0
                                                 , min = 0
                                                 , width = "100%")
                                  , numericInput(inputId = "save.objects.year2"
                                                 , label = NULL
                                                 , value = 0
                                                 , min = 0
                                                 , width = "100%")
                                  , numericInput(inputId = "save.objects.year3"
                                                 , label = NULL
                                                 , value = 0
                                                 , min = 0
                                                 , width = "100%")
                                  , br()
                                  , br()
                                  , actionButton(inputId = "create.save.objects"
                                                 , label = "Create SAVE objects files"
                                                 , icon = icon("file")
                                                 , width = "100%")
                                )
                       )
                     )
                     , fluidRow(
                       br(),
                       wellPanel(dataTableOutput(outputId = "created_table.save"))
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
      # , wellPanel(dataTableOutput(outputId = "created_table"))
      )
)
)


###################################################################################################################################
cat <- message
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # output$mat.PFG.succ = data.frame()
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
        output$created_table.namespace = renderDataTable({
          path_folder = paste0(input$name.simul, "/DATA/NAMESPACE_CONSTANTS/")
          return(get_files(path_folder))
        })
      }
    } else
    {
      shinyalert(type = "warning", text = "You must create a simulation folder first !")
    }
  })
  
  ####################################################################
  
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
      
      # names.PFG = list.files(path = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
      #                        , pattern = "^SUCC_")
      # names.PFG = sub("^SUCC_", "", names.PFG)
      # names.PFG = sub(".txt$", "", names.PFG)
      # selectInput(inputId = "disp.PFG"
      #             , label = NULL
      #             , choices = names.PFG
      #             , multiple = FALSE
      #             , width = "100%")
    } else
    {
      # textInput(inputId = "disp.PFG"
      #           , label = NULL
      #           , width = "100%")
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
  
  output$UI.download.folder = renderUI({
    if (input$create.skeleton > 0)
    {
      downloadButton(outputId = "FATE_simulation.zip"
                     , label = "Download folder"
                     , icon = icon("download")
                     , width = "100%")
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
  
  
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

