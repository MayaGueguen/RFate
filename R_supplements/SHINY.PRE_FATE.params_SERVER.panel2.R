
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

get_files_simulParam = function(simulParam.fi, simulParam.val, flag, flag.split = "^--.*--$", is.num = FALSE)
{
  res = ifelse(length(grep(flag, simulParam.val)) > 0
               , list(.getParam(params.lines = simulParam.fi
                                , flag = flag
                                , flag.split = flag.split
                                , is.num = is.num))
               , "")
  return(unlist(res))
}

get_val_param = function(filename)
{
  if (file.exists(filename))
  {
    val.file = readLines(filename)
    ind.comment = grep("^#", val.file)
    if (length(ind.comment) > 0)
    {
      val.file = val.file[-ind.comment]
    }
  } else
  {
    val.file = ""
  }
  return(val.file)
}

####################################################################

observeEvent(input$load.param, {
  if (nchar(input$load.file) > 0)
  {
    file.simulParam = paste0(input$name.simul, "/PARAM_SIMUL/paramSimul_", input$load.file, ".txt")
    val.simulParam = readLines(file.simulParam)
    # val.simulParam = get_val_param(file.simulParam)
    
    file.globalParam = get_files_simulParam(file.simulParam, val.simulParam, flag = "GLOBAL_PARAMS")
    file.saveArrays = get_files_simulParam(file.simulParam, val.simulParam, flag = "ARRAYS_SAVING_YEARS")
    file.saveObjects = get_files_simulParam(file.simulParam, val.simulParam, flag = "OBJECTS_SAVING_YEARS")
    file.PFGsucc = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_LIFE_HISTORY_PARAMS")
    file.PFGlight = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_LIGHT_PARAMS")
    file.PFGdisp = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_DISPERSAL_PARAMS")
    file.PFGdist = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_DISTURBANCES_PARAMS")
    file.PFGsoil = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_SOIL_PARAMS")
  } else
  {
    file.globalParam = get_files.names(path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
    file.globalParam = file.globalParam[1]
    file.saveArrays = get_files.names(path_folder = paste0(input$name.simul, "/DATA/SAVE/"))
    file.saveArrays = file.saveArrays[1]
    file.saveObjects = get_files.names(path_folder = paste0(input$name.simul, "/DATA/SAVE/"))
    file.saveObjects = file.saveObjects[1]
  }
  
  if (length(file.globalParam) > 0)
  {
    val.saveArrays = get_val_param(file.saveArrays)
    val.saveObjects = get_val_param(file.saveObjects)
    
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
      , "save.maps.folder" = sub(paste0(input$name.simul, "/DATA/SAVE"), "", dirname(file.saveArrays))
      , "save.maps.year1" = val.saveArrays[1]
      , "save.maps.year2" = val.saveArrays[length(val.saveArrays)]
      , "save.maps.no" = length(val.saveArrays)
      , "save.objects.folder" = sub(paste0(input$name.simul, "/DATA/SAVE"), "", dirname(file.saveObjects))
      , "save.objects.year1" = val.saveObjects[1]
      , "save.objects.year2" = ifelse(length(val.saveObjects) > 1, val.saveObjects[2], "")
      , "save.objects.year3" = ifelse(length(val.saveObjects) > 2, val.saveObjects[3], "")
      , "PFG.folder" = sub(paste0(input$name.simul, "/DATA/PFGS/SUCC"), "", unique(dirname(file.PFGsucc)))
    )
    
    ## update shiny input parameters
    updateShinyInputs(session = session
                      , updates = update.param)
    
    ## update shiny reactiveValues
    RV$names.PFG = sub(".txt", "", sub("SUCC_", "", basename(file.PFGsucc)))
    RV$mat.PFG.ALL = foreach(fi = file.PFGsucc, .combine = 'rbind') %do%
    {
      PFG = .getParam(params.lines = fi
                      , flag = "NAME"
                      , flag.split = " "
                      , is.num = FALSE)
      type = .getParam(params.lines = fi
                       , flag = "TYPE"
                       , flag.split = " "
                       , is.num = FALSE)
      height = .getParam(params.lines = fi
                         , flag = "HEIGHT"
                         , flag.split = " "
                         , is.num = TRUE)
      maturity = .getParam(params.lines = fi
                           , flag = "MATURITY"
                           , flag.split = " "
                           , is.num = TRUE)
      longevity = .getParam(params.lines = fi
                            , flag = "LONGEVITY"
                            , flag.split = " "
                            , is.num = TRUE)
      light = 0
      
      return(data.frame(PFG = ifelse(is.null(PFG), "", PFG)
                        , type = ifelse(is.null(type), "", type)
                        , height = ifelse(is.null(height), "", height)
                        , maturity = ifelse(is.null(maturity), "", maturity)
                        , longevity = ifelse(is.null(longevity), "", longevity)
                        , light = 0
      ))
    }
    if (length(file.PFGlight) > 0)
    {
      for(fi in file.PFGlight)
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        light = .getParam(params.lines = fi
                          , flag = "LIGHT"
                          , flag.split = " "
                          , is.num = TRUE)
        light = ifelse(is.null(light), NULL, light)
        if (PFG %in% RV$mat.PFG.ALL$PFG && !is.null(light))
        {
          RV$mat.PFG.ALL$light[which(RV$mat.PFG.ALL$PFG == PFG)] = light
        }
      }
    }
    ## Dispersal
    if (length(file.PFGdisp) > 0)
    {
      RV$mat.PFG.disp = foreach(fi = file.PFGdisp, .combine = 'rbind') %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        MODE = .getParam(params.lines = fi
                         , flag = "MODE_DISPERS"
                         , flag.split = " "
                         , is.num = TRUE)
        dd = .getParam(params.lines = fi
                       , flag = "DISPERS_DIST"
                       , flag.split = " "
                       , is.num = TRUE)
        
        return(data.frame(PFG = ifelse(is.null(PFG), "", PFG)
                          , MODE = ifelse(is.null(MODE), "", MODE)
                          , d50 = ifelse(is.null(dd), "", dd[1])
                          , d99 = ifelse(is.null(dd), "", dd[2])
                          , ldd = ifelse(is.null(dd), "", dd[3])
        ))
      }
    }
    ## Disturbances
    if (length(file.PFGdist) > 0)
    {
      res = foreach(fi = file.PFGdist) %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        FATES = .getParam(params.lines = fi
                          , flag = "FATES"
                          , flag.split = " "
                          , is.num = TRUE)
        
        no.dist = length(FATES) / (4 * 2)
        ind_Kill = seq(1, 4 * 2, 2)
        res = foreach(di = 1:no.dist, .combine = "rbind") %do%
        {
          res = data.frame(name = paste0("DIST_", di), responseStage = 1:4)
          eval(parse(text = paste0("res$KilledIndiv_", PFG, " = FATES[ind_Kill + (di -1) * 8]")))
          eval(parse(text = paste0("res$ResproutIndiv_", PFG, " = FATES[ind_Kill + 1 + (di -1) * 8]")))
          return(res)
        }
        return(res)
      }
      RV$mat.PFG.dist = Reduce(f = function(x, y) merge(x, y, by = c("name", "responseStage")), x = res)
    }
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

