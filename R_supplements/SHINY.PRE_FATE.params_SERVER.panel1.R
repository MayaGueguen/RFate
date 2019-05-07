
####################################################################

observeEvent(input$HELP.panel1, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c(paste0("#help1_", 1:2),"#main.panel",paste0("#help1_", 4:5))#paste0("#help1_", 1:5)
                                                # , position = c("auto", "auto", "bottom-middle-aligned", "auto", "auto")
                                                , intro = c("<p><code>FATE-HD</code> requires only one input parameter, which is a file containing
                                                            the names of parameter files, which may themselves contain parameters or other
                                                            file names. The point is : the user could give names of files stored everywhere
                                                            on a machine, and does not have to put them all in one same place.</p>
                                                            <p>But as this is more practical, this panel proposes a way to organize all
                                                            those files or parameter files that will or could be used by a <code>FATE-HD</code>
                                                            simulation.</p>"
                                                            , "<p><code>PRE_FATE.skeletonDirectory</code> function creates a user-friendly directory tree to run a <code>FATE-HD</code> simulation.
                                                            <p>The tree structure is detailed below the button.</p>"
                                                            , "<p><em>1. Simulation parameterization</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>Global parameters</strong> : related to the simulation definition<br>
                                                            (number of PFG and strata, simulation duration, computer resources, manage abundance values, modules loaded…)<br>
                                                            with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_globalParameters.html'>PRE_FATE.params_globalParameters</a>
                                                            </li>
                                                            <li>
                                                            <strong>Years to save abundance rasters and simulation outputs</strong> with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_saveYears.html'>PRE_FATE.params_saveYears</a>
                                                            </li>
                                                            <li>
                                                            <strong>Years and files to change rasters</strong> for the succession, habitat suitability or disturbance modules<br>
                                                            with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_changingYears.html'>PRE_FATE.params_changingYears</a>
                                                            </li>
                                                            </ul>
                                                            <p><em>2. For each PFG : behavior and characteristics</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>Succession files</strong> : related to the life history with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsuccession.html'>PRE_FATE.params_PFGsuccession</a>
                                                            </li>
                                                            <li>
                                                            <strong>Dispersal files</strong> : related to the dispersal ability with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdispersal.html'>PRE_FATE.params_PFGdispersal</a>
                                                            </li>
                                                            <li>
                                                            <strong>Light files</strong> : related to the light competition with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGlight.html'>PRE_FATE.params_PFGlight</a>
                                                            </li>
                                                            <li>
                                                            <strong>Soil files</strong> : related to the soil competition with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGsoil.html'>PRE_FATE.params_PFGsoil</a>
                                                            </li>
                                                            <li>
                                                            <strong>Disturbance files</strong> : related to the response to perturbations in terms of resprouting and mortality<br>
                                                            with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_PFGdisturbance.html'>PRE_FATE.params_PFGdisturbance</a>
                                                            </li>
                                                            </ul>"
                                                            , "<p><em>3. Parameter management</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                                                            This is the file that will be given as the only argument to the <code>FATE-HD</code> executable file into the command line.<br>
                                                            It can be created with the function <a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>
                                                            </li>
                                                            </ul>"
                                                            , "<p>Download the complete simulation folder as an archive file (<code>FATE_simulation.zip</code>).</p>"))
          )
                           )
})


####################################################################

observeEvent(input$create.skeleton, {
  print_messages(as.expression(
    PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
  ))
  
  shinyjs::show("main.panel")
  shinyjs::enable("create.simul")
  shinyjs::enable("FATE_simulation.zip")
  shinyjs::enable("refresh")
})

####################################################################

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

####################################################################

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

####################################################################

observeEvent(input$refresh, {
  system(command = paste0("rm -r ", input$name.simul))
  shinyjs::hide("main.panel")
  shinyjs::disable("create.simul")
  shinyjs::disable("FATE_simulation.zip")
  shinyjs::disable("refresh")
})

