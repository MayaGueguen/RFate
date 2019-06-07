
####################################################################

observeEvent(input$HELP.panel2, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c(paste0("#help2_", 1:2),"#main.panel",paste0("#help2_", 4:5))
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
  get_res = print_messages(as.expression(
    PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
  ))
  
  if (as.character(get_res) != "0")
  {
    shinyjs::show("main.panel")
    shinyjs::enable("load.file")
    shinyjs::enable("load.param")
    shinyjs::enable("create.simul")
    shinyjs::enable("FATE_simulation.zip")
    shinyjs::enable("refresh")
  }
})

####################################################################

observeEvent(input$name.simul, {
  file.simulParam = get_files.names(path_folder = paste0(input$name.simul, "/PARAM_SIMUL/"))
  if (length(file.simulParam) > 0)
  {
    newFiles = basename(file.simulParam)
    newFiles = sub("paramSimul_", "", newFiles)
    newFiles = sub(".txt", "", newFiles)
    updateSelectInput(session = session
                      , inputId = "load.file"
                      , choices = newFiles
                      , selected = newFiles[1]
    )
  }
})

####################################################################

observeEvent(input$load.param, {
  if (nchar(input$load.file) > 0)
  {
    file.simulParam = paste0(input$name.simul, "/PARAM_SIMUL/paramSimul_", input$load.file, ".txt")
    file.globalParam = .getParam(params.lines = file.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
  } else
  {
    file.globalParam = get_files.names(path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
  }
  # print(file.globalParam)
  
  if (length(file.globalParam) > 0)
  {
    file.globalParam = file.globalParam[1]
    # print(file.globalParam)
    
    update.param = list(
      "required.no_PFG" = .getParam(params.lines = file.globalParam
                                    , flag = "NB_FG"
                                    , flag.split = " "
                                    , is.num = TRUE)
      , "required.no_STRATA" = .getParam(params.lines = file.globalParam
                                         , flag = "NB_STRATUM"
                                         , flag.split = " "
                                         , is.num = TRUE)
      , "required.simul_duration" = .getParam(params.lines = file.globalParam
                                              , flag = "SIMULATION_DURATION"
                                              , flag.split = " "
                                              , is.num = TRUE)
      , "opt.no_CPU" = .getParam(params.lines = file.globalParam
                                 , flag = "NB_CPUS"
                                 , flag.split = " "
                                 , is.num = TRUE)
      , "required.seeding_duration" = .getParam(params.lines = file.globalParam
                                                , flag = "SEEDING_DURATION"
                                                , flag.split = " "
                                                , is.num = TRUE)
      , "required.seeding_timestep" = .getParam(params.lines = file.globalParam
                                                , flag = "SEEDING_TIMESTEP"
                                                , flag.split = " "
                                                , is.num = TRUE)
      , "required.seeding_input" = .getParam(params.lines = file.globalParam
                                             , flag = "SEEDING_INPUT"
                                             , flag.split = " "
                                             , is.num = TRUE)
      , "required.max_by_cohort" = .getParam(params.lines = file.globalParam
                                             , flag = "MAX_BY_COHORT"
                                             , flag.split = " "
                                             , is.num = TRUE)
      , "required.max_abund_low" = .getParam(params.lines = file.globalParam
                                             , flag = "MAX_ABUND_LOW"
                                             , flag.split = " "
                                             , is.num = TRUE)
      , "required.max_abund_medium" = .getParam(params.lines = file.globalParam
                                                , flag = "MAX_ABUND_MEDIUM"
                                                , flag.split = " "
                                                , is.num = TRUE)
      , "required.max_abund_high" = .getParam(params.lines = file.globalParam
                                              , flag = "MAX_ABUND_HIGH"
                                              , flag.split = " "
                                              , is.num = TRUE)
      , "doDispersal" = .getParam(params.lines = file.globalParam
                                  , flag = "DO_DISPERSAL"
                                  , flag.split = " "
                                  , is.num = TRUE)
      , "doHabSuitability" = .getParam(params.lines = file.globalParam
                                       , flag = "DO_HAB_SUITABILITY"
                                       , flag.split = " "
                                       , is.num = TRUE)
      , "HABSUIT.ref_option" = .getParam(params.lines = file.globalParam
                                         , flag = "HABSUIT_OPTION"
                                         , flag.split = " "
                                         , is.num = TRUE)
      , "doDisturbances" = .getParam(params.lines = file.globalParam
                                     , flag = "DO_DISTURBANCES"
                                     , flag.split = " "
                                     , is.num = TRUE)
      , "DIST.no" = .getParam(params.lines = file.globalParam
                              , flag = "NB_DISTURBANCES"
                              , flag.split = " "
                              , is.num = TRUE)
      , "DIST.no_sub" = .getParam(params.lines = file.globalParam
                                  , flag = "NB_SUBDISTURBANCES"
                                  , flag.split = " "
                                  , is.num = TRUE)
      , "DIST.freq" = .getParam(params.lines = file.globalParam
                                , flag = "FREQ_DISTURBANCES"
                                , flag.split = " "
                                , is.num = TRUE)
      , "doLight" = .getParam(params.lines = file.globalParam
                              , flag = "DO_LIGHT_COMPETITION"
                              , flag.split = " "
                              , is.num = TRUE)
      , "LIGHT.thresh_medium" = .getParam(params.lines = file.globalParam
                                          , flag = "LIGHT_THRESH_MEDIUM"
                                          , flag.split = " "
                                          , is.num = TRUE)
      , "LIGHT.thresh_low" = .getParam(params.lines = file.globalParam
                                       , flag = "LIGHT_THRESH_LOW"
                                       , flag.split = " "
                                       , is.num = TRUE)
      , "doSoil" = .getParam(params.lines = file.globalParam
                             , flag = "DO_SOIL_COMPETITION"
                             , flag.split = " "
                             , is.num = TRUE)
    )
    print(update.param)
    updateShinyInputs(session = session
                      , updates = update.param)
  }
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
  shinyalert(type = "warning"
             , text = paste0("The current simulation folder will be removed !\n"
                             , "Make sure you saved your previous work.")
             , showCancelButton = TRUE
             , showConfirmButton = TRUE
             , callbackR = function(x)
             {
               if (x)
               {
                 system(command = paste0("rm -r ", input$name.simul))
                 shinyjs::hide("main.panel")
                 shinyjs::disable("load.file")
                 shinyjs::disable("load.param")
                 shinyjs::disable("create.simul")
                 shinyjs::disable("FATE_simulation.zip")
                 shinyjs::disable("refresh")
               }
             })
})

