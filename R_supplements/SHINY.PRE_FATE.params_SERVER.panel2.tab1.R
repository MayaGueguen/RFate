

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

output$created_table.global = renderDataTable({
  path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
  return(get_files(path_folder))
})

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
                                       , DIST.freq = rep(input$DIST.freq, input$DIST.no)
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
    
    if (as.character(get_res) != "0")
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

