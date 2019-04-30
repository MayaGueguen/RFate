
tabPanel(title = HTML("<p class='tabPanel_title'>Specific year...</p>")
         , value = "panel.specific_year"
         , br()
         , wellPanel(
           style = HTML(paste0("background-color: ", help.color, ";")),
           helpText(HTML("
                         <p><a href='https://mayagueguen.github.io/RFate/reference/PRE_FATE.params_globalParameters.html' target='_blank'>
                         See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a></p>
                         <table style='width:100%;'>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>opt.no_CPU</td>
                         <td style='width:70%;'>default 1 <em>(optional). The number of resources that can be used to parallelize the FATE-HD simulation</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.no_PFG</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the number of PFG</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.no_STRATA</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the number of height strata</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.simul_duration</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the duration of simulation (in years)</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.seeding_duration</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the duration of seeding (in years)</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.seeding_timestep</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the time interval at which occurs the seeding, and until the seeding duration is not over (in years)</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.seeding_input</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> corresponding to the number of seeds attributed to each PFG at each time step, and until the seeding duration is not over</td>
                         </tr>
                         
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.max_by_cohort</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of each cohort in each pixel (carrying capacity equivalent)</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.max_abund_low</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of small PFG</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.max_abund_medium</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of intermediate PFG</td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>required.max_abund_high</td>
                         <td style='width:70%;'>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to rescale abundance values of tall PFG</td>
                         </tr>
                         <tr><td><br/></td></tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>doDispersal</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>FALSE</span>.
                         <em>If <span style='font-family:Monospace;'>TRUE</span>, seed dispersal is activated in the FATE-HD simulation, and associated parameters are required</em></td>
                         </tr>
                         <tr><td><br/></td></tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>doHabSuitability</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>FALSE</span>.
                         <em>If <span style='font-family:Monospace;'>TRUE</span>, habitat suitability is activated in the FATE-HD simulation, and associated parameters are required<e/m></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>HABSUIT.ref_option</td>
                         <td style='width:70%;'><em>an <span style='font-family:Monospace;'>integer</span> corresponding to the way of simulating 
                         the habitat suitability variation between years for each PFG, either random (1) or PFG specific (2)</em></td>
                         </tr>
                         <tr><td><br/></td></tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>doLight</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>FALSE</span>.
                         <em>If <span style='font-family:Monospace;'>TRUE</span>, light competition is activated in the FATE-HD simulation, and associated parameters are required</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>LIGHT.thresh_medium</td>
                         <td style='width:70%;'><em>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources.
                         It corresponds to the limit of abundances above which light resources are <span style='font-family:Monospace;'>medium</span>. PFG abundances lower than this threshold
                         imply high amount of light. It is consequently lower than <span style='font-family:Monospace;'>LIGHT.thresh_low</span>.</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>LIGHT.thresh_low</td>
                         <td style='width:70%;'><em>an <span style='font-family:Monospace;'>integer</span> in the order of 1 000 000 to convert PFG abundances in each strata into light resources. 
                         It corresponds to the limit of abundances above which light resources are <span style='font-family:Monospace;'>low</span>. PFG abundances higher than 
                         <span style='font-family:Monospace;'>LIGHT.thresh_medium</span> and lower than this threshold imply medium amount of light.</em></td>
                         </tr>
                         <tr><td><br/></td></tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>doSoil</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>FALSE</span>.
                         <em>If <span style='font-family:Monospace;'>TRUE</span>, soil competiion is activated in the FATE-HD simulation, and associated parameters are required</em></td>
                         </tr>
                         <tr><td><br/></td></tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>doDisturbances</td>
                         <td style='width:70%;'>default <span style='font-family:Monospace;'>FALSE</span>.
                         <em>If <span style='font-family:Monospace;'>TRUE</span>, disturbances are applied in the FATE-HD simulation, and associated parameters are required</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>DIST.no</td>
                         <td style='width:70%;'><em>the number of disturbances</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>DIST.no_sub</td>
                         <td style='width:70%;'><em>the number of way a PFG could react to a disturbance</em></td>
                         </tr>
                         <tr>
                         <td style='width:30%;font-family:Monospace;vertical-align:top;'>DIST.freq</td>
                         <td style='width:70%;'><em>the frequency of each disturbance (in years)</em></td>
                         </tr>
                         </table>
                         "
           )))
         , fluidRow(
           column(4
                  , br()
                  , numericInput(inputId = "required.no_PFG"
                                 , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>required.no_PFG</span>")
                                 , value = 1
                                 , min = 1
                                 , width = "100%")
                  , numericInput(inputId = "required.no_STRATA"
                                 , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>required.no_STRATA</span>")
                                 , value = 1
                                 , min = 1
                                 , width = "100%")
           )
         )
         , fluidRow(
           column(4
                  , br()
                  , checkboxInput(inputId = "doDispersal"
                                  , label = HTML("<span style = 'font-style: italic; font-weight: normal;'>doDispersal</span>")
                                  , value = TRUE
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
                  )
                  , br())
         )
         , fluidRow(
           br(),
           wellPanel(style = "overflow-x:scroll;"
                     , dataTableOutput(outputId = "created_table.global"))
         )
) ## END tabPanel (Global parameters)
