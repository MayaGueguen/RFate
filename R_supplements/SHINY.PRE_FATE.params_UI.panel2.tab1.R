
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
                                   , content = help.HTML("docs/reference/PRE_FATE.params_globalParameters.html")
                      )
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
