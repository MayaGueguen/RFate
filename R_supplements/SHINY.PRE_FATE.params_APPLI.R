
rm(list = ls())

## load required libraries
library(shiny)
library(shinyFiles)
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
mat.PFG.ALL = data.frame()
mat.PFG.disp = data.frame()
mat.PFG.dist = data.frame()
mat.PFG.soil = data.frame()
mat.changing = data.frame()
button.color = "rgba(96, 129, 150, 0.5)"
button.style = paste0("background-color: ", button.color, "; border-width:0px;")
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
                    font-family: 'Francois One', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 00px;
                    margin-top: 0px;
                    }
                    p.tabPanel_title {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 20px;
                    font-weight: 200;
                    padding: 10px;
                    margin-top: 0px;
                    }
                    p.tabPanel_subtitle {
                    font-family: 'Londrina Solid', cursive;
                    font-size: 18px;
                    font-weight: 200;
                    padding: 0px;
                    margin-top: 0px;
                    }
                    "))
    ),
  
  headerPanel("FATE", windowTitle = "FATE"),
  
  # Sidebar layout with a input and output definitions
  mainPanel(
    width = 12,
    tabsetPanel(
      source("R_supplements/SHINY.PRE_FATE.params_UI.panel1.R", local = TRUE)$value,
      tabPanel(title =  HTML("<p class='panel_title'>B. Run simulation</p>")),
      tabPanel(title =  HTML("<p class='panel_title'>C. Create simulation ouputs & graphics</p>"))
    )
  )
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
    shinyjs::show("FATE_simulation.zip")
    shinyjs::show("refresh")
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
    shinyjs::hide("FATE_simulation.zip")
    shinyjs::hide("refresh")
  })
  
  
  ####################################################################
  
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.tab1.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.tab2.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.tab3.R", local = TRUE)$value
  source("R_supplements/SHINY.PRE_FATE.params_SERVER.tab4.R", local = TRUE)$value
  
  ####################################################################
}

###################################################################################################################################
# Create a Shiny app object
shinyApp(ui = ui, server = server)

