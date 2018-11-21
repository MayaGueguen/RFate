
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
library(markdown)
# library(DT)
# library(data.table)
library(zip)

###################################################################################################################################

# <link rel = "stylesheet" href = "https://use.fontawesome.com/releases/v5.5.0/css/solid.css" integrity =
#   "sha384-rdyFrfAIC05c5ph7BKz3l5NG5yEottvO/DQ0dCrwD8gzeQDjYBHNr1ucUpQuljos" crossorigin =
#     "anonymous" >
# < link rel = "stylesheet" href = "https://use.fontawesome.com/releases/v5.5.0/css/fontawesome.css" integrity =
#   "sha384-u5J7JghGz0qUrmEsWzBQkfvc8nK3fUT7DCaQzNQ+q4oEXhGSx+P2OqjWsfIRB8QT" crossorigin =
#     "anonymous" >



###################################################################################################################################

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # theme = "cosmo",
  titlePanel("FATE : create simulation folder"
             , windowTitle = "FATE"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 3,
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
               # , p(em("Enter the simulation name :"))
               , textInput(inputId = "name.simul"
                           , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>Enter the simulation name :</span>")
                           , value = "FATE_simulation"
                           , width = "100%")
               # , style = "font-style: italic;")
        )
      ),
      fluidRow(
        column(12
               , ""
               , actionButton(inputId = "create.skeleton"
                              , label = "Create folder"
                              , icon = icon("folder")
                              , width = "100%")
        )
      ),
      br(),
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
      tabsetPanel(
        tabPanel(title = "Internal settings"
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
                          , br())
                 )
        ),
        tabPanel(title = "PFG files"
                 , value = "create.PFG"
                 , br()
                 , tabsetPanel(
                   tabPanel(title = "Succession"
                            , value = "panel.succ"
                            , fluidRow(
                              column(6
                                     , br()
                                     , actionButton(inputId = "add.PFG"
                                                    , label = "Add PFG"
                                                    , icon = icon("plus")
                                                    , width = "100%"))
                              , column(6
                                       , br()
                                       , actionButton(inputId = "create.succ"
                                                      , label = "Create PFG succession files"
                                                      , icon = icon("file")
                                                      , width = "100%"))
                            )
                            , fluidRow(
                              column(2
                                     , br()
                                     , br()
                                     , HTML("<strong>PFG</strong>"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>type</strong>"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>height</strong>"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>maturity</strong>"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>longevity</strong>"))
                              , column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>light</strong>"))
                            )
                   )
                   , tabPanel(title = "Dispersal"
                              , value = "panel.disp"
                              , fluidRow(
                                column(6
                                       , br()
                                       , actionButton(inputId = "add.PFG"
                                                      , label = "Add PFG"
                                                      , icon = icon("plus")
                                                      , width = "100%"))
                                , column(6
                                         , br()
                                         , actionButton(inputId = "create.disp"
                                                        , label = "Create PFG dispersal files"
                                                        , icon = icon("file")
                                                        , width = "100%"))
                              )
                              , fluidRow(
                                column(2
                                       , br()
                                       , br()
                                       , HTML("<strong>PFG</strong>"))
                                , column(2
                                         , br()
                                         , br()
                                         , HTML("<strong>MODE</strong>"))
                                , column(2
                                         , br()
                                         , br()
                                         , HTML("<strong>d50</strong>"))
                                , column(2
                                         , br()
                                         , br()
                                         , HTML("<strong>d99</strong>"))
                                , column(2
                                         , br()
                                         , br()
                                         , HTML("<strong>ldd</strong>"))
                              )
                   )
                   , tabPanel(title = "Disturbances"
                              , value = "create.dist")
                 )
        ),
        tabPanel(title = "Scenario files"
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
        tabPanel(title = "Simulation files"
                 , value = "create.simul"
        )
      )
      , wellPanel()
    )
  )
)


###################################################################################################################################

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  ####################################################################
  # get_allParams = eventReactive(input$refresh, {
  # 
  # })
  # 
  # get_params1 = eventReactive(input$refresh, {
  #   year.win = 6
  #   params = paste0(input$year.range[1], "_"
  #                   , input$year.range[2], "_"
  #                   , year.win, "_"
  #                   , input$prob.decrease.sampThisYear, "_"
  #                   , input$prob.decrease.sampSuccYears, "_"
  #                   , input$prob.increase.sampXYears, "_"
  #                   , input$prob.decrease.notWorking
  #   )
  #   params
  # })
  # 
  # get_version = eventReactive(input$refresh, {
  #   if (input$saveResults)
  #   {
  #     params = paste0(input$year.range[1], "_", input$year.range[2])
  #     version = length(list.files(path = getwd(), pattern = paste0("^ORCHAMP_selection_.*", params), recursive = F)) + 1
  #     version
  #   }
  # })
  # 
  # get_params2 = eventReactive(input$refresh, {
  #   paste0("version", get_version(), "_", input$year.range[1], "_", input$year.range[2])
  # })
  # 
  # get_params3 = eventReactive(input$refresh, {
  #   paste0("version", get_version(), "_", get_params1())
  # })
  # 
  # get_dirSave = eventReactive(input$refresh, {
  #   dirSave = paste0("ORCHAMP_selection_", get_params2())
  #   if (input$saveResults) dir.create(dirSave)
  #   dirSave
  # })
  # 
  # ####################################################################
  # get_comb.ALL.vec = eventReactive(input$refresh, {
  # })
  # 
  # ####################################################################
  # get_RES = eventReactive(input$refresh, {
  #   
  #   ## --------------------------------------------------------------------------
  #   withProgress(message = "CALCUL DE L'ECHANTILLONNAGE EN COURS"
  #                , min = 0, max = year.end - year.start + 1, {
  #                  RES = foreach(ye.end = seq(year.start + (year.win - 1), year.end, 1)) %do%
  #                  {
  # 
  #                    return(RES)
  #                  }
  #                })
  #   
  #   return(RES)
  # })
  # 
  # ####################################################################
  # get_RES_SEL = eventReactive(input$refresh, {
  #   RES
  # })
  # 
  # output$RES_SEL = renderDataTable({
  #   get_RES_SEL()
  # })
  # 
  # 
  # ####################################################################
  # get_plot2 = eventReactive(input$refresh, {
  # 
  # })
  # 
  # output$plot2 = renderPlot({
  #   print(get_plot2())
  # })
  # 
  # ####################################################################
  # get_plot4 = eventReactive(input$refresh, {
  # 
  # })
  # 
  # output$plot4 = renderPlot({
  #   print(get_plot4())
  # })
  # 
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

