
# introBox(
tabPanel(title = HTML("<span class='tabPanel_title'>Global parameters</span>")
         , value = "panel.global"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             fluidRow(
               column(4
                      , br()
                      , numericInput(inputId = "required.no_PFG"
                                     , label = param.style("required.no_PFG")
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
                      , numericInput(inputId = "required.no_STRATA"
                                     , label = param.style("required.no_STRATA")
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
                      , numericInput(inputId = "required.simul_duration"
                                     , label = param.style("required.simul_duration")
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
                      , numericInput(inputId = "opt.no_CPU"
                                     , label = param.style("opt.no_CPU")
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
               )
               , column(4
                        , br()
                        , numericInput(inputId = "required.seeding_duration"
                                       , label = param.style("required.seeding_duration")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.seeding_timestep"
                                       , label = param.style("required.seeding_timestep")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.seeding_input"
                                       , label = param.style("required.seeding_input")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
               )
               , column(4
                        , br()
                        , numericInput(inputId = "required.max_by_cohort"
                                       , label = param.style("required.max_by_cohort")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.max_abund_low"
                                       , label = param.style("required.max_abund_low")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.max_abund_medium"
                                       , label = param.style("required.max_abund_medium")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.max_abund_high"
                                       , label = param.style("required.max_abund_high")
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
               )
             )
             , fluidRow(
               column(4
                      , br()
                      , checkboxInput(inputId = "doDispersal"
                                      , label = param.style("doDispersal")
                                      , value = TRUE
                                      , width = "100%")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doHabSuitability"
                                        , label = param.style("doHabSuitability")
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doHabSuitability")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doDisturbances"
                                        , label = param.style("doDisturbances")
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doDisturbances")
               )
             )
             , fluidRow(
               column(4
                      , br()
                      , checkboxInput(inputId = "doLight"
                                      , label = param.style("doLight")
                                      , value = FALSE
                                      , width = "100%")
                      , uiOutput(outputId = "UI.doLight")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doSoil"
                                        , label = param.style("doSoil")
                                        , value = FALSE
                                        , width = "100%")
               )
             )
             , fluidRow(
               column(6
                      , br()
                      , actionButton(inputId = "create.global"
                                     , label = "Create Global parameters file"
                                     , icon = icon("file")
                                     , width = "100%"
                                     , style = HTML(button.style)
                      ) %>% helper(type = "inline"
                                   , title = "Global parameter file"
                                   , size = "l"
                                   , content = help.full(param.web = "https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_globalParameters.html"
                                                         , param.name.vec = c("<hr/>"
                                                                              , "opt.no_CPU"
                                                                              , "required.no_PFG"
                                                                              , "required.no_STRATA"
                                                                              , "required.simul_duration"
                                                                              , "required.seeding_duration"
                                                                              , "required.seeding_timestep"
                                                                              , "required.seeding_input"
                                                                              , "required.max_by_cohort"
                                                                              , "required.max_abund_low"
                                                                              , "required.max_abund_medium"
                                                                              , "required.max_abund_high"
                                                                              , "<hr/>"
                                                                              , "doDispersal"
                                                                              , "<hr/>"
                                                                              , "doHabSuitability"
                                                                              , "HABSUIT.ref_option"
                                                                              , "<hr/>"
                                                                              , "doLight"
                                                                              , "LIGHT.thresh_medium"
                                                                              , "LIGHT.thresh_low"
                                                                              , "<hr/>"
                                                                              , "doSoil"
                                                                              , "<hr/>"
                                                                              , "doDisturbances"
                                                                              , "DIST.no"
                                                                              , "DIST.no_sub"
                                                                              , "DIST.freq"
                                                         )
                                                         , param.desc.vec = c("<hr/>"
                                                                              , "default 1 <em>(optional). The number of resources that can be used to parallelize the FATE-HD simulation</em>"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of PFG"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of height strata"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the duration of simulation (in years)"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the duration of seeding (in years)"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the time interval at which occurs the seeding, and until the seeding duration is not over (in years)"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> corresponding to the number of seeds attributed to each PFG at each time step, and until the seeding duration is not over"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of each cohort in each pixel (carrying capacity equivalent)"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of small PFG"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of intermediate PFG"
                                                                              , "an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of tall PFG"
                                                                              , "<hr/>"
                                                                              , "default <span style='font-family:Monospace;'>FALSE</span>.
                                                                              <em>If <span style='font-family:Monospace;'>TRUE</span>, seed dispersal is activated in the FATE-HD simulation, and associated parameters are required</em>"
                                                                              , "<hr/>"
                                                                              , "default <span style='font-family:Monospace;'>FALSE</span>.
                                                                              <em>If <span style='font-family:Monospace;'>TRUE</span>, habitat suitability is activated in the FATE-HD simulation, and associated parameters are required<e/m>"
                                                                              , "<em>an <span style='font-family:Monospace;'>integer</span> corresponding to the way of simulating the habitat suitability variation between years for each PFG, either random (1) or PFG specific (2)</em>"
                                                                              , "<hr/>"
                                                                              , "default <span style='font-family:Monospace;'>FALSE</span>.
                                                                              <em>If <span style='font-family:Monospace;'>TRUE</span>, light competition is activated in the FATE-HD simulation, and associated parameters are required</em>"
                                                                              , "<em>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
                                                                              It corresponds to the limit of abundances above which light resources are <span style='font-family:Monospace;'>medium</span>. PFG abundances lower than this threshold
                                                                              imply high amount of light. It is consequently lower than <span style='font-family:Monospace;'>LIGHT.thresh_low</span>.</em>"
                                                                              , "<em>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
                                                                              It corresponds to the limit of abundances above which light resources are <span style='font-family:Monospace;'>low</span>. PFG abundances higher than
                                                                              <span style='font-family:Monospace;'>LIGHT.thresh_medium</span> and lower than this threshold imply medium amount of light.</em>"
                                                                              , "<hr/>"
                                                                              , "default <span style='font-family:Monospace;'>FALSE</span>.
                                                                              <em>If <span style='font-family:Monospace;'>TRUE</span>, soil competiion is activated in the FATE-HD simulation, and associated parameters are required</em>"
                                                                              , "<hr/>"
                                                                              , "default <span style='font-family:Monospace;'>FALSE</span>.
                                                                              <em>If <span style='font-family:Monospace;'>TRUE</span>, disturbances are applied in the FATE-HD simulation, and associated parameters are required</em>"
                                                                              , "<em>the number of disturbances</em>"
                                                                              , "<em>the number of way a PFG could react to a disturbance</em>"
                                                                              , "<em>the frequency of each disturbance (in years)</em>"
                                                         )))
                      , br())
             )
             , fluidRow(
               br(),
               column(12,
                      wellPanel(style = "overflow-x:scroll;"
                                , dataTableOutput(outputId = "created_table.global"))
               )
             )
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## END tabPanel (Global parameters)
#   data.step = 3,
#   data.position = "auto",
#   data.intro = "<strong>Step 2</strong><hr>
#            <p><code>PRE_FATE.params_globalParameters</code> function creates a <code>.txt</code> file into the
#            <code>name.simulation/DATA/GLOBAL_PARAMETERS</code> directory."
# ) ## END introBox