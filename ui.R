#ui function
library(shiny)
library(dplyr)
library(shinycssloaders)
library(DT)
library(shinydashboard)
library(HydeNet)
library(bnlearn)
library(Rgraphviz)
library(BiocManager)
library(shinyWidgets)
library(shinytest)
library(DiagrammeR)
library(backports)
library(arrayhelpers)
library(abind)
# list.of.packages <- c("shiny","dplyr","bnlearn","DiagrammeR","backports","shinycssloaders","DT",
#                       "shinydashboard","HydeNet","BiocManager","shinyWidgets","shinytest",'Rgraphviz')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

getOption("repos")
options(repos = BiocManager::repositories())

dashboardPage(skin = 'green',
              dashboardHeader(title = 'Bayesian Network',
                              tags$li(a(href = 'http://google.com',
                                        style = 'padding-top:2px;padding-bottom:2px;'),
                                      class = 'dropdown'),
                              tags$li (a(href = 'http://google.com', icon('send'),title = 'visit us'), class = 'dropdown')),
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
              dashboardBody(tabItems(
                tabItem(tabName = 'model_structure',
                        fluidRow(column(width = 4,
                                        box(title = 'Model Input', status = 'success', collapsible = T, width = NULL,
                                            helpText('Select a model template or uplaod your saved model'),
                                            radioGroupButtons(inputId = 'dataInput', choices = c('Sample Model' = 1, 'Upload Model' = 2),
                                                              selected = 1, justified = T),
                                            conditionalPanel(condition = 'input.dataInput == 1',
                                                             selectInput(inputId = 'net', h5('Bayesian Network:'),
                                                                         c('sample model' = 1,
                                                                           'Cyber Attack Model' = 2,
                                                                           'Credit Fraud Model' = 3,
                                                                           'Building Destruction Model' = 4))),
                                            conditionalPanel(condition = 'input.dataInput == 2',
                                                             p('Note: Upload previously saved model in .RData format:'),
                                                             fileInput('load_model_from_file',
                                                                       strong('Model Input:')))),
                                        box(title = 'Save Model', status = 'success', collapsible = T, width = NULL,
                                            actionBttn('clear_model', 'Clear Model', icon = icon('times-circle'), style = 'jelly', color = 'danger', size = 'sm'),
                                            downloadBttn('save_model_to_file', 'Save Model', style = 'jelly', color = 'primary', size = 'sm')),
                                        box(title = 'Add Nodes and Relationship', status = 'success', collapsible = T, width = NULL,
                                            uiOutput('parent_node'),
                                            uiOutput('child_node'),
                                            actionBttn('add_child_parent', 'Add New Nodes', icon = icon('floppy-o'), color = 'primary', style = 'pill', size = 'sm'),
                                            actionBttn('delete_nodes_edges', 'Delete Nodes', icon = icon('trash-o'), style = 'pill', color = 'warning', size = 'sm'))),
                                 column(width = 8,
                                        box(title = 'Model Structure', status = 'success', collapsible = T, width = NULL,
                                            grVizOutput('model_plot'))))),
                tabItem(tabName = 'model_state',
                        fluidRow(column(width = 4,
                                        box(title = 'Add States', status = 'success', collapsible = T, width = NULL,
                                            helpText('Define or Modify states for each Node'),
                                            uiOutput('select_node_for_states'),
                                            uiOutput('define_states'),
                                            hr(),
                                            actionBttn('add_state', 'Add new state', icon = icon('floppy-o'), color = 'primary', style = 'pill', size = 'sm'),
                                            actionBttn('delete_state', 'Delete State', icon = icon('trash-o'), color = 'warning', style = 'pill', size = 'sm'),
                                            br(),
                                            br(),
                                            actionBttn('clear_state', 'Clear all States', icon = icon('trash-alt'), style= 'pill', color = 'danger', size = 'sm'))),
                                 column(width = 4,
                                        DT::dataTableOutput(outputId = 'node_state_tab')))),
                tabItem(tabName = 'quantify',
                        fluidRow(column(width = 4,
                                        helpText('Parametrise the node probabilities'),
                                        uiOutput('select_node_for_probs'),
                                        uiOutput('define_probs_for_node'))),
                       fluidRow(column(width = 6,
                               DT::dataTableOutput(outputId = 'CPT'))),
                        fluidRow(column(width = 4,
                                        actionBttn('add_CPT_to_node', 'Add CPT to Node', icon = icon('link'), style = 'unite', color = 'success', size = 'sm')))),
                tabItem(tabName = 'report',
                        actionBttn('calculate', 'Launch Simulation', icon = icon('bar-chart-o'), style = 'unite', color = 'primary', size = 'sm'),
                        DT::dataTableOutput(outputId = 'model_report'),
                        downloadBttn('generate_report', 'Generate Report', color = 'primary', style = 'unite', size = 'sm'))
              ))
)