#ui function
#Dir <-"C:/Users/narendra.sahu/Documents/BayesianNetwork"
#setwd(Dir)

source('dependencies.R')

#options(repos = c(getOption("repos"), BiocInstaller::biocinstallRepos()))
#getOption("repos")
options(repos = c(getOption("repos"), BiocManager::repositories()))
#packrat:::appDependencies()
#grep("Rcpp", list.files(recursive = TRUE), fixed = TRUE, value = TRUE)

dashboardPage(skin = 'blue',
              dashboardHeader(title = 'Bayesian Network'
                              ),
              dashboardSidebar(
                sidebarMenu(id = 'side_tab',
                            tags$head(
                              tags$style(
                                HTML('.shiny-notification')
                              )
                            ),
                            menuItem("Structure", tabName = 'model_structure', icon = icon('globe')),
                            menuItem('Define States', tabName = 'model_state', icon = icon('files-o')),
                            menuItem('Parametrise State', tabName = 'quantify', icon = icon('table')),
                            menuItem('Execution', tabName = 'report', icon = icon('calculator'))
                )),
              dashboardBody(
                # Include introjs UI
                rintrojs::introjsUI(),
                tabItems(
                  tabItem(tabName = 'model_structure',
                          hidden(div(id = 'text_div', textOutput('model_name')) ),
                          fluidRow(column(width = 3,
                                          box(title = 'Model Input', status = 'success', collapsible = T, width = NULL,
                                              helpText('Select a model template or uplaod your saved model'),
                                              radioGroupButtons(inputId = 'dataInput', choices = c('Sample Model' = 1, 'Upload Model' = 2),
                                                                selected = 1, justified = T),
                                              conditionalPanel(condition = 'input.dataInput == 1',
                                                               selectInput(inputId = 'net', h5('Bayesian Network:'),
                                                                           c('Credit Fraud Model' = 1,
                                                                             'Building Destruction Model' = 2,
                                                                             'Cyber Attack Model' =3 ))),
                                              conditionalPanel(condition = 'input.dataInput == 2',
                                                               p('Note: Upload previously saved model in .RData format:'),
                                                               fileInput('load_model_from_file',
                                                                         strong('Model Input:')))),
                                          box(title = 'Save Model', status = 'success', collapsible = T, width = NULL,
                                              actionBttn('clear_model', 'Clear Model', icon = icon('times-circle'), style = 'jelly', color = 'danger', size = 'sm'),
                                              useShinyalert(),
                                              downloadBttn('save_model_to_file', 'Save Model', style = 'jelly', color = 'primary', size = 'sm'),
                                              useShinyalert()),
                                          box(title = 'Add Nodes and Relationship', status = 'success', collapsible = T, width = NULL,
                                              uiOutput('parent_node'),
                                              uiOutput('child_node'),
                                              actionBttn('add_child_parent', 'Add New Nodes', icon = icon('floppy-o'), color = 'primary', style = 'pill', size = 'sm'),
                                              actionBttn('delete_nodes_edges', 'Delete Nodes', icon = icon('trash-o'), style = 'pill', color = 'warning', size = 'sm')),
                                          # Add introjs btn
                                          shiny::actionButton("structureIntro", "Help", icon = icon('info-circle'))),
                                   column(width = 9,
                                          box(title = 'Model Structure', status = 'success', collapsible = T, width = NULL,
                                              grVizOutput('model_plot')),
                                          #helpText('Define other deterministic Nodes (if any)'),
                                          #uiOutput('formula_node'),
                                          #uiOutput('node_formula'),
                                          #div(style="display:inline-block;vertical-align:bottom;",actionButton(inputId = 'check_formula', label = 'Check', icon = icon('check'))),
                                          #hidden(div(id = 'text_div', verbatimTextOutput('output_formula')) ),
                                          div(style="display:inline-block",textInputAddon(inputId = 'exposure', label = 'Define Exposure', placeholder = 'Enter Formula', addon = icon('expand-arrows-alt'))),
                                          div(style="display:inline-block;vertical-align:bottom;",actionButton(inputId = 'checkx', label = 'Check', icon = icon('check'))),
                                          hidden(div(id = 'text_div', verbatimTextOutput('outputx')) ),
                                          div(style="display:inline-block",textInputAddon(inputId = 'occurence', label = 'Define Occurence', placeholder = 'Enter Formula', addon = icon('opera'))),
                                          div(style="display:inline-block;vertical-align:bottom;",actionButton(inputId = 'checko', label = 'Check', icon = icon('check'))),
                                          hidden(div(id = 'text_div', verbatimTextOutput('outputo')) ),
                                          div(style="display:inline-block",textInputAddon(inputId = 'impact', label = 'Define Impact', placeholder = 'Enter Formula', addon = icon('italic'))),
                                          div(style="display:inline-block;vertical-align:bottom;",actionButton(inputId = 'checki', label = 'Check', icon = icon('check'))),
                                          hidden(div(id = 'text_div', verbatimTextOutput('outputi')) )
                                   )
                          )
                          ),
                  tabItem(tabName = 'model_state',
                          fluidRow(column(width = 4,
                                          box(title = 'Add States', status = 'success', collapsible = T, width = NULL,
                                              helpText('Define or Modify states for each Node'),
                                              uiOutput('select_node_for_states'),
                                              uiOutput('node_type'),
                                              # selectizeInput(inputId = 'node_type', h5('Node type'),
                                              #                c('Level', 'Numeric', 'Boolean')),
                                              # conditionalPanel(condition="input.node_type=='Numeric'",
                                              #                  selectInput(inputId ='determ_node',h5('Deterministic'),
                                              #                              c('No','Yes'))),
                                              # conditionalPanel(condition="input.node_type != 'Numeric'",
                                              #                  selectInput(inputId ='determ_node',h5('Deterministic'),
                                              #                              c('No'))),
                                              uiOutput('determ_node'),
                                              conditionalPanel(condition="input.is_determ=='No' ", uiOutput('define_states')),
                                              #uiOutput('define_states'),
                                              conditionalPanel(condition="input.is_determ=='Yes' ", textInputAddon(inputId = 'node_formula', label = paste0('Define '),
                                                                                                                   placeholder = 'Enter formula', addon = icon('equals')) ),
                                              #useShinyjs(),
                                              conditionalPanel(condition="input.is_determ=='Yes' ", actionButton(inputId = 'check_formula', label = 'Check', icon = icon('check'))),
                                              #hidden(div(id = 'text_div', verbatimTextOutput('output_formula')) ), 
                                              useShinyalert(),
                                              useShinyalert(),
                                              hr(),
                                              actionBttn('add_state', 'Add new state', icon = icon('floppy-o'), color = 'primary', style = 'pill', size = 'sm'),
                                              actionBttn('delete_state', 'Delete State', icon = icon('trash-o'), color = 'warning', style = 'pill', size = 'sm'),
                                              br(),
                                              br(),
                                              actionBttn('clear_state', 'Clear all States', icon = icon('trash-alt'), style= 'pill', color = 'danger', size = 'sm'),
                                              actionBttn('validate_state', 'Validate states', icon = icon('tasks'), style= 'pill', color = 'primary', size = 'sm'),
                                              useShinyalert()
                                          )),
                                   column(width = 4,
                                          DT::dataTableOutput(outputId = 'node_state_tab'),
                                          helpText(strong('Node type')),
                                          uiOutput('type'),
                                          helpText(strong('Is deterministic ?')),
                                          uiOutput('determ'),
                                          helpText(strong('Formula')),
                                          uiOutput('formula'))
                          ),
                          # Add introjs btn
                          shiny::actionButton("stateIntro", "Help", icon = icon('info-circle'))),
                  tabItem(tabName = 'quantify',
                          fluidRow(column(width = 5,
                                          helpText('Parametrise the node probabilities'),
                                          uiOutput('select_node_for_probs'),
                                          # box(title = 'Conditional Probabilities', collapsible = T, width = 10,
                                          #     uiOutput('define_probs_for_node')),
                                          actionBttn('add_CPT_to_node', 'Add CPT to Node', icon = icon('link'), style = 'unite', color = 'success', size = 'sm'),
                                          useShinyalert(),  # Set up shinyalert
                                          useShinyalert(),
                                          # Add introjs btn
                                          shiny::actionButton("parameterIntro", "Help", icon = icon('info-circle')),
                                          #dataTableOutput(outputId = 'CPT')
                                          DT::DTOutput('CPT')),
                                   column(width = 7, plotOutput('condplot'),
                                          plotOutput('margplot'))
                          ) ),
                  tabItem(tabName = 'report',
                          numericInput(inputId = 'n_sims', label = 'Number of Simulation', value = 1000000),
                          useSweetAlert(),
                          actionBttn('calculate', 'Launch Simulation', icon = icon('bar-chart-o'), style = 'unite', color = 'primary', size = 'sm'),
                          useShinyalert(),
                          # Add introjs btn
                          shiny::actionButton("reportIntro", "Help", icon = icon('info-circle')),
                          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                          downloadBttn('generate_report', 'Generate Report', color = 'primary', style = 'unite', size = 'sm'),
                          downloadBttn('download_data', 'Download Data', color = 'primary', style = 'unite', size = 'sm'),
                          fluidRow(column(width = 7, plotOutput('histogram'),
                                          plotOutput('density')),
                                   column(width = 4, htmlOutput('text_out'))) 
                          )
                ))
)