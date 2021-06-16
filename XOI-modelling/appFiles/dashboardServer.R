

if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}
network <- list(
)

network_data <- shiny::reactiveValues(
  node_list = NULL,
  Hyde_plot = NULL,
  node_states = NULL,
  CPT = NULL,
  model = NULL,
  node_type = NULL,
  is_determ = NULL,
  formula = NULL
)

HydeNet::HydePlotOptions(
  variable = list(shape = "ellipse", fillcolor = "#A6DBA0"),
  determ = list(
    shape = "rect",
    fillcolor = "#E7D4E8",
    fontcolor = "#1B7837",
    linecolor = "#1B7837"
  ),
  decision = list(
    shape = "triangle",
    fillcolor = "#1B7837",
    linecolor = "white"
  ),
  utility = list(
    shape = "rect",
    fillcolor = "#762A83",
    fontcolor = "white"
  )
)

output$dashboard = shiny::renderUI({
  
  argonR::argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = TRUE,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("home"),
      icon("globe"),
      icon("laptop-code"),
      icon("chart-line"),
      icon("calculator")
    ),
    argonR::argonTab(
      tabName = "Home",
      active = T,
      argonR::argonRow(
        argonR::argonColumn(
          width = 4,
          img(src = 'structure.jpg', width = "100%"),
          h6(
            "Source: Wikipedia",
            style = 'text-align:center;
             font-style: italic;font-weight: bold;
             '
          )
        ),
        argonR::argonColumn(
          width = 5,
          h4(
            "The XOI method allows a structured assessment of scenario through:",
            style = 'text-align:justify;'
          ),
          p(
            "-The use of 3 common dimensions for each scenario: Exposure, Occurrence, Impact",
            style = 'text-align:justify;'
          ),
          p(
            "-The use of specific drivers for each dimension (number of units exposed, time to detection, time to recovery, market conditions, etc.)",
            style = 'text-align:justify;'
          ),
          h4(
            "The experts are prompted to provide or confirm an assessment (value, range, set of ranges) on each driver.
            The assessments can be informed by external statistical analysis.",
            style = 'text-align:justify;'
          ),
          h4(
            "The XOI method does not add any assumption to expert opinions and generates the implicit distribution of potential losses through
          probabilistic calculation using:",
            style = 'text-align:justify;'
          ),
          p("-Bayesian inference", style = 'text-align:justify;'),
          p("-Monte Carlo simulation", style = 'text-align:justify;'),
          h4(
            "The use of distributions in scenario assessment is generally focused on combining observed losses with single point
            projections to assess the tail of a distribution.",
            style = 'text-align:justify;'
          ),
          h4(
            "The XOI approach focuses rather on generating a distribution of potential tail events.",
            style = 'text-align:justify;'
          )
        ),
        argonR::argonColumn(
          width = 3,
          img(
            src = 'network.gif',
            width = "100%",
            height = "80%"
          ),
          h6("Source: Wikipedia", style = 'text-align:center;font-style: italic;font-weight: bold;')
        )
        
      ),
      p(
        "This platform has 4 tabs: Model Structure, Define States, Parametrisation  and Execution. User define the structure of the XOI model
      at first from the Model Structure tab.
        The states of each node are defined on the next tab while the probability distribution of each nodes are defined in Parametrisation tab.
        Finally, model can be executed from Execution tab",
        style = 'text-align:justify;'
      ),
      p(
        "The XOI model can be used for modelling of different risk categories such as Climate risk (physical risk), Legal risk, Conduct risk
        and other OpRisk models."
      ),
      tags$br(),
      h4("What are XOI?:", style = 'color:Red;font-size:15px;text-align:Left;'),
      p(
        "1. A unit of Exposureis a resource used by the firm's business.",
        style = 'color:Red;font-size:13px;text-align:Left;'
      ),
      p(
        "2. The Occurrence of an event creates a loss when striking a resource.",
        style = 'color:Red;font-size:13px;text-align:Left;'
      ),
      p("3. The Impactis the amount of the loss." , style = 'color:Red;font-size:13px;text-align:Left;')
      
      
    ),
    # Define structure tab -----
    argonR::argonTab(
      tabName = "Model Structure",
      active = F,
      tags$head(
        tags$style(
          type = "text/css",
          "
             #loadmessage {
                           position: fixed;
                           top: 150px;
                           left: 50px;
                           width: 93%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
  ")
      ),
      
      argonR::argonRow(
        argonR::argonColumn(
          width = 12,
          shiny::uiOutput(outputId = "chartUI") %>% shinycssloaders::withSpinner()
        )
      ),
      shiny::conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$div("Loading Page!!! Please wait...", id = "loadmessage")
      )
    ),
    argonR::argonTab(
      tabName = "Define States",
      active = F,
      argonR::argonRow(
        argonR::argonColumn(
          width = 4,
          bs4Dash::box(
            title = 'Add States',
            collapsible = F,
            width = NULL,
            helpText('Define or Modify states for each Node'),
            tags$br(),
            shiny::uiOutput(outputId = 'select_node_for_states'),
            shiny::uiOutput(outputId = 'node_type'),
            shiny::uiOutput(outputId = 'determ_node'),
            shiny::conditionalPanel(condition = "input.is_determ=='No' ", shiny::uiOutput(outputId = 'define_states')),
            shiny::conditionalPanel(
              condition = "input.is_determ=='Yes' ",
              shinyWidgets::textInputAddon(
                inputId = 'node_formula',
                label = paste0('Define '),
                placeholder = 'Enter formula',
                addon = icon('equals')
              )
            ),
            conditionalPanel(
              condition = "input.is_determ=='Yes' ",
              shiny::actionButton(
                inputId = 'check_formula',
                label = 'Check',
                icon = icon('check')
              )
            ),
            tags$br(),
            shinyWidgets::actionBttn(
              inputId = 'add_state',
              label = 'Add new state',
              icon = icon('floppy-o'),
              color = 'primary',
              style = 'pill',
              size = 'sm'
            ),
            shinyWidgets::actionBttn(
              inputId = 'delete_state',
              label = 'Delete State',
              icon = icon('trash-o'),
              color = 'warning',
              style = 'pill',
              size = 'sm'
            ),
            tags$br(),
            tags$br(),
            shinyWidgets::actionBttn(
              inputId = 'clear_state',
              label = 'Clear all States',
              icon = icon('trash-alt'),
              style = 'pill',
              color = 'danger',
              size = 'sm'
            ),
            shinyWidgets::actionBttn(
              inputId = 'validate_state',
              label = 'Validate states',
              icon = icon('tasks'),
              style = 'pill',
              color = 'primary',
              size = 'sm'
            )
          )
        ),
        argonR::argonColumn(
          width = 4,
          DT::dataTableOutput(outputId = 'node_state_tab'),
          tags$br(),
          helpText(strong('Node type')),
          shiny::uiOutput(outputId = 'type'),
          tags$br(),
          helpText(strong('Is deterministic ?')),
          shiny::uiOutput(outputId = 'determ'),
          tags$br(),
          helpText(strong('Formula')),
          shiny::uiOutput(outputId = 'formula')
        )
      ),
      # Add introjs btn
      shiny::actionButton(inputId = "stateIntro", "Help", icon = icon('info-circle'))
    ),
    
    argonR::argonTab(
      tabName = "Parametrisation",
      active = FALSE,
      argonR::argonRow(
        argonR::argonColumn(
          width = 5,
          helpText(h2('Parametrise the node probabilities')),
          shiny::uiOutput(outputId = 'select_node_for_probs'),
          actionBttn(
            'add_CPT_to_node',
            inputId = 'Add CPT to Node',
            icon = icon('link'),
            style = 'unite',
            color = 'success',
            size = 'sm'
          ),
          tags$br(),
          tags$br(),
          DT::DTOutput(outputId = 'CPT')
        ),
        argonR::argonColumn(
          width = 7,
          shiny::plotOutput(outputId = 'condplot') %>% shinycssloaders::withSpinner(),
          shiny::plotOutput(outputId = 'margplot') %>% shinycssloaders::withSpinner()
        )
      ),
      # Add introjs btn
      shiny::actionButton(
        inputId = "parameterIntro",
        label = "Help",
        icon = icon('info-circle')
      )
    ),
    
    argonR::argonTab(
      tabName = "Execution",
      active = FALSE,
      argonR::argonRow(
        argonR::argonColumn(
          width = 4,
          bs4Dash::box(
            title = '',
            collapsible = F,
            width = NULL,
      shiny::numericInput(
        inputId = 'n_sims',
        label = 'Number of Simulation',
        value = 1000000
      ),
      shinyWidgets::actionBttn(
        inputId = 'calculate',
        label = 'Launch Simulation',
        icon = icon('bar-chart-o'),
        style = 'unite',
        color = 'primary',
        size = 'sm'
      ),
      tags$br(),
      shiny::radioButtons(
        inputId = 'format',
        label = 'Document format',
        c('PDF', 'HTML', 'Word'),
        inline = TRUE
      ),
      shinyWidgets::downloadBttn(
        outputId = 'generate_report',
        label = 'Generate Report',
        color = 'primary',
        style = 'unite',
        size = 'sm'
      ),
      shinyWidgets::downloadBttn(
        outputId = 'download_data',
        label = 'Download Data',
        color = 'primary',
        style = 'unite',
        size = 'sm'
      ),
      tags$br(),
      tags$br(),
      textInput(inputId = 'prob', label = "Quantiles(probabilities) separated by comma", 
                value = '0.5, 0.75, 0.8, 0.85, 0.90, 0.95, 0.99, 0.995, 0.999, 0.9999'),
      shinyWidgets::actionBttn(
        inputId = 'quantile_plot',
        label = 'Update Quantile Plot',
        icon = icon('update'),
        style = 'unite',
        color = 'primary',
        size = 'sm'
      ))
      ),
      argonR::argonColumn(
        width = 4,
        bs4Dash::box(
          title = '',
          collapsible = F,
          width = NULL,
        shiny::htmlOutput(outputId = 'text_out')
        )
      )
      ),
      tags$br(),
      argonR::argonRow(
        argonR::argonColumn(
          width = 12,
          #shiny::plotOutput(outputId = 'histogram') 
          highchartOutput("histogram",width = "100%")
          
          # shiny::plotOutput(outputId = 'density')
        )
        # argonR::argonColumn(width = 4,
        #                     shiny::htmlOutput(outputId = 'text_out'))
      ),
      tags$br(),
      # Add introjs btn
      shiny::actionButton(
        inputId = "reportIntro",
        label = "Help",
        icon = icon('info-circle')
      )
    )
  )
})

outputOptions(output, "dashboard", suspendWhenHidden = FALSE)


output$chartUI = shiny::renderUI({
  
  tagList(
    div(
      id = 'text_div1',
      textOutput('model_name') %>% tagAppendAttributes(style = 'color:blue;padding-left: 800px;font-weight: bold;')
    ) ,
    argonR::argonRow(
      argonR::argonColumn(width = 3,
                          argonR::argonRow(
                            argonR::argonColumn(
                              width = 12,
                              bs4Dash::box(
                                title = 'Model Input',
                                collapsible = F,
                                width = NULL,
                                helpText('Select a model template or uplaod your saved model'),
                                shinyWidgets::radioGroupButtons(
                                  inputId = 'dataInput',
                                  choices = c('Sample Model' = 1, 'Upload Model' = 2),
                                  selected = 1,
                                  justified = T
                                ),
                                shiny::conditionalPanel(
                                  condition = 'input.dataInput == 1',
                                  p('Select Model Template::'),
                                  shiny::selectInput(
                                    inputId = 'net',
                                    label = "",
                                    choices =
                                      c(
                                        'Credit Fraud Model' = 1,
                                        'Building Destruction Model' = 2,
                                        'Cyber Attack Model' = 3
                                      )
                                  )
                                ),
                                shiny::conditionalPanel(
                                  condition = 'input.dataInput == 2',
                                  p('Note: Upload previously saved model in .RData format:'),
                                  shiny::fileInput('load_model_from_file',
                                                   '')
                                )
                              ),
                              #tags$br(),
                              bs4Dash::box(
                                title = '',
                                collapsible = FALSE,
                                width = NULL,
                                shinyWidgets::actionBttn(
                                  inputId = 'clear_model',
                                  label = 'Clear Model',
                                  icon = icon('times-circle'),
                                  style = 'jelly',
                                  color = 'danger',
                                  size = 'sm'
                                ),
                                shinyWidgets::downloadBttn(
                                  outputId = 'save_model_to_file',
                                  label = 'Save Model',
                                  style = 'jelly',
                                  color = 'primary',
                                  size = 'sm'
                                )
                              ),
                              #tags$br(),
                              bs4Dash::box(
                                title = 'Add Nodes and Relationship',
                                collapsible = FALSE,
                                width = NULL,
                                shiny::uiOutput(outputId = 'parent_node'),
                                shiny::uiOutput(outputId = 'child_node'),
                                shinyWidgets::actionBttn(
                                  inputId = 'add_child_parent',
                                  label = 'Add New Nodes',
                                  icon = icon('floppy-o'),
                                  color = 'primary',
                                  style = 'pill',
                                  size = 'sm'
                                ),
                                shinyWidgets::actionBttn(
                                  inputId = 'delete_nodes_edges',
                                  label = 'Delete Nodes',
                                  icon = icon('trash-o'),
                                  style = 'pill',
                                  color = 'warning',
                                  size = 'sm'
                                )
                              ),
                              # Add introjs btn
                              shiny::actionButton(
                                inputId = "structureIntro",
                                label = "Help",
                                icon = icon('info-circle')
                              )
                            )
                          )),
      argonR::argonColumn(
        width = 9,
        
        bs4Dash::box(
          title = '',
          collapsible = F,
          width = NULL,
          DiagrammeR::grVizOutput(outputId = 'model_plot')  %>% shinycssloaders::withSpinner()
        ),
        argonR::argonRow(
          argonR::argonColumn(
            width = 3,
            shinyWidgets::textInputIcon(
              inputId = 'exposure',
              label = 'Define Exposure',
              placeholder = 'Enter Formula',
              icon = icon('expand-arrows-alt')
            ),
            div(
              style = "display:inline-block;vertical-align:bottom;",
              shiny::actionButton(
                inputId = 'checkx',
                label = 'Check',
                icon = icon('check')
              )
            ),
            div(id = 'text_div2', shiny::verbatimTextOutput('outputx'))
          ),
          argonR::argonColumn(
            width = 3,
            shinyWidgets::textInputIcon(
              inputId = 'occurence',
              label = 'Define Occurence',
              placeholder = 'Enter Formula',
              icon = icon('opera')
            ),
            div(
              style = "display:inline-block;vertical-align:bottom;",
              shiny::actionButton(
                inputId = 'checko',
                label = 'Check',
                icon = icon('check')
              )
            ),
            div(id = 'text_div3', shiny::verbatimTextOutput('outputo'))
          ),
          argonR::argonColumn(
            width = 3,
            shinyWidgets::textInputIcon(
              inputId = 'impact',
              label = 'Define Impact',
              placeholder = 'Enter Formula',
              icon = icon('italic')
            ),
            div(
              style = "display:inline-block;vertical-align:bottom;",
              shiny::actionButton(
                inputId = 'checki',
                label = 'Check',
                icon = icon('check')
              )
            ),
            div(id = 'text_div4', shiny::verbatimTextOutput('outputi'))
          )
        )
      )
    )
  )
})

################################################################################################################
###### #######################                      Structure            #####################################
#############################################################################################################

#### Parent Node  ####
output$parent_node <- shiny::renderUI({
  remove <- c('Exposure', 'Occurence', 'Impact')
  shiny::selectizeInput(
    inputId = 'Parent_ID',
    choices = c(Choose = '', as.list(
      setdiff(network_data$node_list, remove)
    )),
    multiple = FALSE,
    label = 'Parent Node',
    options = list(placeholder = 'Add Parent Node', create = T)
  )
})

#### Child Node ####
output$child_node <- shiny::renderUI({
  add <- c('Exposure', 'Occurence', 'Impact')
  shiny::selectizeInput(
    inputId = 'Child_ID',
    choices = c(Choose = '', as.list(union(
      network_data$node_list, add
    ))),
    multiple = FALSE,
    label = 'Child Node',
    options = list(placeholder = 'Add Child Node', create = T)
  )
})

#### Add parent and  parent and their relationship ####
shiny::observeEvent(input$add_child_parent, {
  if (!is.null(input$add_child_parent) & input$add_child_parent > 0) {
    shiny::isolate({
      network <<-
        insert_node(network, input$Parent_ID, invalidate_CPT = FALSE)
      network <<- insert_node(network, input$Child_ID)
      network <<-
        add_parent_child_rel(network,
                             parent_ID  = input$Parent_ID,
                             child_ID = input$Child_ID)
    })
    #### node ordering using BNlearn and plot using Hydenet ####
    network_data$node_list <-
      network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
    network_data$Hyde_plot <-
      network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
    plot1 <-
      network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
    save_html(plot1, file = 'file.html')
    webshot::webshot("file.html", file = "temp.png", cliprect = "viewport")
    model_name <<- model_name
    #plot2 <<- export_svg(plot1)
    #plot2 <- html_print(HTML(plot2))
  }
})

#### Save model ####
#network <- network
output$save_model_to_file <- shiny::downloadHandler(
  #shinyalert("Note: Save model in .RData format", "for e.g. 'model_name.RData' ", type = "info"),
  filename = function() {
    paste0(model_name, '.RData')
  },
  content = function(file) {
    save(network, model_name, file = file)
  }
)


## Load pre-defined demo models
network <- shiny::observe({
  req(input$dataInput)
  if (input$dataInput == 1) {
    if (input$net == 1) {
      load(file = 'data/Fraudmodel.RData')
      network <<- network
      model_name <<- model_name
    } else if (input$net == 2) {
      load(file = 'data/BuildingDestruction.RData')
      network <<- network
      model_name <<- model_name
    } else if (input$net == 3) {
      load(file = 'data/CyberAttack.RData')
      network <<- network
      model_name <<- model_name
    }
  } else if (input$dataInput == 2) {
    inFile <- input$load_model_from_file
    if (is.null(inFile))
      return(NULL)
    load(file = inFile$datapath)
    network <<- network
    model_name <<- model_name
  }
  #rm(network_saved)
  network_data$node_list <-
    network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
  network_data$Hyde_plot <-
    network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
  network_data$model <- model_name
})

#### clear the model ####
observeEvent(input$clear_model, {
  network <<- list()
  network_data$node_list <- NULL
  network_data$node_states <- NULL
  network_data$Hyde_plot <- NULL
  network_data$CPT <- NULL
  network_data$model <- NULL
  network_data$node_type <- NULL
  network_data$is_determ <- NULL
  network_data$formula <- NULL
  shinyalert('New model name', type = 'input', callbackR = mycallback)
  
})

mycallback <- function(value) {
  model_name <<- value
  network_data$model <- model_name
}

output$model_name <- renderText({
  paste("Model Name: ", network_data$model)
})

#### delete nodes ####
observeEvent(input$delete_nodes_edges, {
  if (!is.null(input$delete_nodes_edges) &
      input$delete_nodes_edges > 0) {
    isolate({
      network <<- delete_node(network, input$Child_ID)
    })
    network_data$node_list <-
      network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
    network_data$Hyde_plot <-
      network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
    
  }
})

#### plot network ####
output$model_plot <- renderGrViz({
  if (is.null(network))
    return(NULL)
  network_data$Hyde_plot
})

observeEvent(input$checkx, {
  #toggle('text_div2')
  formula_exposure <- input$exposure
  output$outputx <-
    renderPrint({
      check_formula(formula_exposure, network, 'Exposure')
    })
})

observeEvent(input$checko, {
  #toggle('text_div3')
  formula_occurence <- input$occurence
  output$outputo <-
    renderPrint({
      check_formula(formula_occurence, network, 'Occurence')
    })
})

observeEvent(input$checki, {
  #toggle('text_div4')
  formula_impact <- input$impact
  output$outputi <-
    renderPrint({
      check_formula(formula_impact, network, 'Impact')
    })
})


###############################################################################################################
###############################          Parametrise the nodes    ####################################
#########################################################################################################

#### Define states of the nodes  ####
output$select_node_for_states <- shiny::renderUI({
  remove <- c('Exposure', 'Occurence', 'Impact')
  if (!is.null(network_data$node_list)) {
    selectInput(
      inputId = 'selected_node_for_states',
      label = 'Select Node',
      selectize = F,
      choices = c(Choose = '', as.list(
        setdiff(network_data$node_list, remove)
      ))
    )
  } else {
    return('No nodes defined in network')
  }
})

output$node_type <- shiny::renderUI({
  if (!is.null(network_data$node_list)) {
    selectInput(
      inputId = 'node_type',
      label = 'Node Type',
      selectize = F,
      choices = c('Labels', 'Numeric', 'Boolean')
    )
  } else {
    return('No nodes defined in network')
  }
})

output$determ_node <- shiny::renderUI({
  node_ID <- input$selected_node_for_states
  node_type <- input$node_type
  if (!is.null(node_ID) & !is.null(node_type)) {
    network_data$node_type <- get_node_type(network, node_ID)
    network_data$is_determ <- get_if_determ(network, node_ID)
    network_data$formula <-  get_node_formula(network, node_ID)
    if (node_type == 'Numeric') {
      selectizeInput(
        inputId = 'is_determ',
        choices = c('No', 'Yes'),
        label = 'Is this a deterministic node?',
        multiple = F
      )
    } else {
      selectizeInput(
        inputId = 'is_determ',
        choices = c('No'),
        label = 'Is this a deterministic node?',
        multiple = F
      )
    }
  } else {
    network_data$node_type <- NULL
    network_data$is_determ <- NULL
    network_data$formula <- NULL
    return('No nodes defined in network')
  }
})

output$define_states <- shiny::renderUI({
  #req(input$is_determ == 'No')
  node_ID <- input$selected_node_for_states
  if (!is.null(node_ID)) {
    network_data$node_states <- get_node_states(network, node_ID)
    selectizeInput(
      inputId = 'state_to_add',
      choices = c(Choose = '', as.list(network_data$node_states)),
      multiple = F,
      label = 'Define State',
      options = list(placeholder = 'Add state to the node', create = T)
    )
  } else {
    network_data$node_states <- NULL
    return('No node selected')
  }
})


observeEvent(input$check_formula, {
  #toggle('text_div')
  node_ID <- input$selected_node_for_states
  network[[node_ID]]$formula <<- input$node_formula
  network_data$node_type <- get_node_type(network, node_ID)
  network_data$is_determ <- get_if_determ(network, node_ID)
  network_data$formula <-  get_node_formula(network, node_ID)
  #formula_of_node <- input$node_formula
  output_formula <-
    check_formula(input$node_formula, network, node_ID)
  if (output_formula == 'OK') {
    shinyalert('Formula OK', type = 'success')
  } else {
    shinyalert('Formula error, plese re-enter the formula',
               output_formula,
               type = 'error')
  }
  #output$output_formula <- renderPrint({ check_formula(input$node_formula, network, node_ID) })
})

#### Add state button ####
observeEvent(input$add_state, {
  node_ID <- input$selected_node_for_states
  if(!is.null(node_ID)) {
    if(!is.null(input$add_state) & input$add_state > 0) {
      network <<- add_node_type(network, node_ID, input$node_type, input$is_determ)
      if(input$is_determ == 'Yes' && !is.null(input$node_formula) ) {
        network <<- define_determ_node(network, node_ID, input$node_formula)
      }
      if(input$is_determ == 'No'  ){
        network <<- add_state_to_node(network, node_ID, input$state_to_add)
        network <<- test_determ_node(network)
      }
      network_data$node_states <- get_node_states(network, node_ID)
      network_data$node_type <- get_node_type(network, node_ID)
      network_data$is_determ <- get_if_determ(network, node_ID)
      network_data$formula <-  get_node_formula(network,node_ID)
    }
  }
})

# Observe intro btn and start the intro
shiny::observeEvent(input$structureIntro,
                    rintrojs::introjs(session, options = list(steps = structureHelp)))

#### show state table ####
output$node_state_tab <- DT::renderDataTable(
  rownames = F,
  options = list(
    pageLength = 20,
    selection = list(State = 'row'),
    width = 'auto',
    spacing = 'xs'
  ),
  expr = {
    if (!is.null(input$selected_node_for_states)) {
      stats <- network_data$node_states
      if (!is.null(stats)) {
        stats %>% as.matrix
      }
    }
  }
)

### show node type
output$type <- shiny::renderUI(network_data$node_type)
output$determ <- shiny::renderUI(network_data$is_determ)
output$formula <- shiny::renderUI(network_data$formula)

#### delete state button ####
observeEvent(input$delete_state, {
  node_ID <- input$selected_node_for_states
  if (!is.null(node_ID)) {
    if (!is.null(input$delete_state) & input$delete_state > 0) {
      isolate({
        network <<-
          remove_state_from_node(network, node_ID, input$state_to_add)
      })
      network_data$node_states <- get_node_states(network, node_ID)
      network_data$node_type <- get_node_type(network, node_ID)
      network_data$is_determ <- get_if_determ(network, node_ID)
      network_data$formula <-  get_node_formula(network, node_ID)
    }
  }
})

#### clear all state button ####
observeEvent(input$clear_state, {
  node_ID <- input$selected_node_for_states
  if (!is.null(node_ID)) {
    isolate({
      network <<- clear_all_state_from_node(network, node_ID)
    })
    network_data$node_states <- get_node_states(network, node_ID)
    network_data$node_type <- get_node_type(network, node_ID)
    network_data$is_determ <- get_if_determ(network, node_ID)
    network_data$formula <-  get_node_formula(network, node_ID)
  }
}) 

#### Validate the states and show error message if any
observeEvent(input$validate_state, {
  check <- check_state(network)
  network <<- update_check_state(network)
  if (!is.null(check)) {
    shinyalert('Error',
               paste(check, '.', 'Some of the nodes have been reset'),
               type = 'error')
  } else {
    shinyalert("OK", type = 'success')
  }
})

# Observe intro btn and start the intro
shiny::observeEvent(input$stateIntro,
                    rintrojs::introjs(session, options = list(steps = stateHelp)))


####################################################################################################
#######################  Define proobalities of the node  ########################################
########################################################################################

# Observe intro btn and start the intro
shiny::observeEvent(input$parameterIntro,
                    rintrojs::introjs(session, options = list(steps = parameterHelp)))

output$select_node_for_probs <- shiny::renderUI({
  if (!is.null(network_data$node_list)) {
    remove <- c('Exposure', 'Occurence', 'Impact')
    selectInput(
      inputId = 'selected_node_for_probs',
      label = 'Select Node',
      selectize = F,
      choices = c(Choose = '', as.list(
        setdiff(network_data$node_list, remove)
      ))
    )
  } else {
    return('No nodes defined in network')
  }
})

observeEvent(c(input$CPT_cell_edit), {
  node_ID <- input$selected_node_for_probs
  shiny::validate(need(node_ID, ''))
  shiny::validate(need(network[[node_ID]]$CPT, 'Add probabilities to the node'))
  if (network[[node_ID]]$Is_determ == 'Yes') {
    #network <<- network
    shinyalert("Deterministic Node!!",
               "You can't change the probabilities",
               type = "error")
  }
  if (!is.null(node_ID) & (network[[node_ID]]$Is_determ == 'No')) {
    CPT <- network[[node_ID]]$CPT
    table <- CPT %>% transform_CPT
    table <- editData(table, input$CPT_cell_edit, 'CPT')
    network <<- update_cpt_to_network(network, node_ID, table)
  }
  
})

observeEvent(input$add_CPT_to_node, {
  node_ID <- input$selected_node_for_probs
  if (!is.null(node_ID)) {
    CPT <- network[[node_ID]]$CPT
    CPT <- CPT %>% transform_CPT
    network <<- update_cpt_to_network(network, node_ID, CPT)
    if (network[[node_ID]]$CPT %>% as.data.frame %>% colSums %>% sum != prod(dim(network[[node_ID]]$CPT)[-1])) {
      network <<- normalize_prob(network, node_ID)
      shinyalert("Oops! probabilities do not sum up to 1",
                 "Normalizing probablities",
                 type = "success")
    }
  }
})

observeEvent(c(input$selected_node_for_probs), {
  node_ID <- input$selected_node_for_probs
  shiny::validate(need(node_ID, ''))
  CPT <- network[[node_ID]]$CPT
  if (is.null(CPT)) {
    #{ & network[[node_ID]]$Is_determ =='No'
    CPT <- calc_CPT_structure_for_node(network, node_ID)
  }
  table <-
    CPT %>% transform_CPT  #%>%  datatable( rownames = FALSE) %>%
  # } else if( network[[node_ID]]$Is_determ =='Yes') {
  #   network <<- define_determ_node(network, node_ID, network[[node_ID]]$formula)
  #   table <- network[[node_ID]]$CPT %>% transform_CPT
  # }
  network <<- update_cpt_to_network(network, node_ID, table)
  output$CPT <-
    DT::renderDT(
      DT::datatable(
        table,
        selection = list (mode = 'single', target = 'cell'),
        editable = TRUE,
        extensions = c('FixedColumns', 'FixedHeader'),
        options = list(
          searching = F,
          scrollX = TRUE,
          fixedHeader = TRUE,
          fixedColumns =
            list(leftColumns = 2, rightColumns = 0)
        )
      )
    )
})

observeEvent(c(input$selected_node_for_probs, input$add_CPT_to_node), {
  output$condplot <- renderPlot({
    node_ID <- input$selected_node_for_probs
    non_CPT_node <- node_without_CPT(network) %>% is.null
    shiny::validate(need(node_ID, ''))
    shiny::validate(
      need(
        !is.null(network[[node_ID]]$CPT),
        'Please adjust conditional probabilities of the node and click "ADD CPT to Node" '
      )
    )
    shiny::validate(need((non_CPT_node),
                         'Please define conditional probabilities for all the nodes'
    ))
    bnlearn_net <-
      network %>% net_transform %>% mapping_bnlearn_network %>% model2network
    bnlearn_net_fit <-
      custom.fit(bnlearn_net,
                 dist = network %>% net_transform %>% lapply(function(v)
                   v$CPT))
    bn.fit.barchart(
      bnlearn_net_fit[[node_ID]],
      cex.axis = 1.5,
      cex.names = 1.5,
      cex.lab = 1.5,
      ylab = node_ID
    )
  })
})

observeEvent(c(input$selected_node_for_probs, input$add_CPT_to_node), {
  output$margplot <- renderPlot({
    node_ID <- input$selected_node_for_probs
    shiny::validate(need(node_ID, ''))
    shiny::validate(
      need(
        !is.null(network[[node_ID]]$CPT),
        'Please adjust conditional probabilities of the node and click "ADD CPT to Node" '
      )
    )
    network_sim <- network  ## copy network
    if (c('Exposure', 'Occurence', 'Impact')  %in%  names(network_sim) %>% unique == TRUE) {
      network_sim[["Impact"]] <-
        network_sim[["Exposure"]] <- network_sim[["Occurence"]] <- NULL
    }
    plot_marinal(network, node_ID)
  })
})

# Observe intro btn and start the intro
shiny::observeEvent(input$reportIntro,
                    rintrojs::introjs(session, options = list(steps = reportHelp)))

observeEvent(input$calculate, {
  
  withProgress(message = 'Calculation in progress...', {
    n_sims <- input$n_sims
    formula_exposure <- input$exposure
    formula_occurence <- input$occurence
    formula_impact <- input$impact
    if (!(formula_exposure > 0 &
          formula_impact > 0 & formula_occurence > 0)) {
      shinyalert("Define formula for XOI nodes from 'Structure'tab", type = 'error')
    }
    shiny::validate(need((
      c(formula_exposure > 0 &
          formula_impact > 0 &
          formula_occurence > 0)
    ), "Define formula for XOI nodes from 'Structure'tab"))
    #shiny::validate(need(any(formula_exposure, formula_impact, formula_occurence), "Define formula for XOI nodes from ' Structure'tab"))
    for (node_ID in setdiff(names(network), c('Exposure', 'Occurence', 'Impact'))) {
      if (is.null(network[[node_ID]]$CPT)) {
        shinyalert('Probabilities missing for', node_ID, type = 'error')
        shiny::validate(need(network[[node_ID]]$CPT , 'Probabilities missing'))
      }
    }
    ##
    progressSweetAlert(
      session,
      id = "runProgress",
      value = 10,
      total = NULL,
      display_pct = T,
      size = NULL,
      status = "success",
      striped = T,
      title = paste0("Setting up Execution data...")
    )
    exposure_net <- net_transform_node(network, 'Exposure')
    exposure_bnlearn <-
      exposure_net %>% mapping_bnlearn_network %>% model2network
    exposure_bnlearn_net_fit <-
      custom.fit(exposure_bnlearn, dist = exposure_net %>% lapply(function(v)
        v$CPT))
    exposure_sim <- rbn(exposure_bnlearn_net_fit, n_sims)
    exposure_sim <-
      sapply(exposure_sim, function(x)
        as.numeric(as.vector(x)))
    exposure_sim <-
      exposure_sim %>% as.data.frame %>%  mutate(Exposure =  eval(parse(text =
                                                                          formula_exposure))) %>%
      .$Exposure %>% as.character %>% as.numeric
    ###
    updateProgressBar(
      session = session,
      id = "runProgress",
      value = 30,
      title = "Running Simulation..."
    )
    sim <-
      run_simulation(network,
                     exposure_sim,
                     n_sims,
                     formula_occurence,
                     formula_impact)
    updateProgressBar(
      session = session,
      id = "runProgress",
      value = 75,
      title = "Calculating losses"
    )
    Sys.sleep(10)

    cummlLoss <<-
      aggregate(sim$Loss_tot,
                by = list(Iter = sim$Iter_vec),
                FUN = sum) %>% .$x %>% round
    updateProgressBar(
      session = session,
      id = "runProgress",
      value = 90,
      title = "Generating loss distribution"
    ) 
    output$text_out <- shiny::renderUI({
      str1 <-
        paste("Number of Simulations: ",
              format(n_sims, big.mark = ',', format = 'd'),
              '\n')
      str2 <- paste("<B>Single Event</B>")
      str3 <-
        paste("Minimum Loss: ", paste0(
          '$',
          format(
            sim %>% filter(Loss_tot != 0) %>% .$Loss_tot %>%
              min %>% round,
            big.mark = ',',
            format = 'd',
            scientific = F
          )
        ), '\n')
      str4 <-
        paste("Average Loss: ", paste0(
          '$',
          format(
            sim %>% filter(Loss_tot != 0) %>% .$Loss_tot %>%
              mean %>% round,
            big.mark = ',',
            format = 'd',
            scientific = F
          )
        ), '\n')
      str5 <-
        paste("Maximum Loss: ", paste0(
          '$',
          format(
            sim %>% filter(Loss_tot != 0) %>% .$Loss_tot %>%
              max %>% round,
            big.mark = ',',
            format = 'd',
            scientific = F
          )
        ), '\n')
      str6 <- paste("<B>Frequency</B>")
      str7 <-
        paste("Average: ",
              (sim$Occurence %>% mean) * (exposure_sim %>% mean),
              '\n')
      str8 <- paste("<B>Cumulated Loss</B>")
      str9 <-
        paste("Min : ", paste0(
          '$',
          format(
            cummlLoss %>% min %>% round,
            big.mark = ',',
            format = 'd'
          )
        ), '\n')
      str10 <-
        paste("Max : ", paste0(
          '$',
          format(
            cummlLoss %>% max %>% round,
            big.mark = ',',
            format = 'd',
            scientific = F
          )
        ), '\n')
      str11 <-
        paste("Mean : ", paste0(
          '$',
          format(
            cummlLoss %>% mean %>% round,
            big.mark = ',',
            format = 'd',
            scientific = F
          )
        ), '\n')
      str12 <-
        paste("Median : ", paste0(
          '$',
          format(
            cummlLoss %>% median %>% round,
            big.mark = ',',
            format = 'd'
          )
        ), '\n')
      str13 <-
        paste("Standard Deviation : ", paste0(
          '$',
          format(
            cummlLoss  %>% sd %>% round,
            big.mark = ',',
            format = 'd'
          )
        ), '\n')
      str14 <-
        paste("Skewness : ", format(cummlLoss %>% skewness, digits = 4), '\n')
      str15 <-
        paste("Kurtosis : ", format(cummlLoss %>% kurtosis, digits = 4), '\n')
      backgroundColour <- 'blue'
      HTML(
        paste(
          str1,
          str2,
          str3,
          str4,
          str5,
          str6,
          str7,
          str8,
          str9,
          str10,
          str11,
          str12,
          str13,
          str14,
          str15,
          sep = '<br/>'
        ),
        class=backgroundColour
      )
    })
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = " Run completed !!!",
                   type = "success")
  })
})

observeEvent(c(input$calculate, input$quantile_plot),{
  probs = strsplit(input$prob," ") %>% unlist() %>% strsplit(",") %>% unlist %>% as.numeric()
  if (!is.numeric(probs)) {
    shinyalert("Check the quantiles probabilities", type = 'error')
    shiny::validate(need( is.numeric(probs), ''))
  }

  validate(need(exists('cummlLoss'), message = F))
  value <- quantile(cummlLoss, probs) %>% round(0) # %>%  as.data.frame() %>%  setDT( keep.rownames = T)
  value <- data.frame(Quantile = names(value), Loss=value)
  #colnames(value) <- c('Quantile', 'Loss')
  
  output$histogram <- renderHighchart({
    
    validate(need(exists('value'), message = F))
    
    hc <- value %>% 
      hchart(
        'column', dataLabels = list(enabled = TRUE, format = '${point.y}'
        ),
        hcaes(x = Quantile, y = Loss, color = Quantile) 
        
      ) %>%
      hc_subtitle(text = "Loss Distribution",
                  align = "left",
                  style = list(color = "#2b908f", fontWeight = "bold")) %>%
      hc_yAxis_multiples(
        plotLines = list(list(value =  mean(cummlLoss), color = "red", width = 2,
                              dashStyle = "shortdash")))
    hc %>% 
      hc_chart(borderColor = '#EBBA95',
               borderRadius = 10,
               borderWidth = 2
      ) %>%
      hc_exporting(
        enabled = TRUE
      ) %>%
      hc_colors("orange") %>%
      hc_tooltip(shared = TRUE, borderWidth = 5,table = F) 
    
  })
})

output$generate_report <- downloadHandler(
  filename = function() {
    paste(model_name, sep = '.', switch(
      input$format,
      PDF = 'pdf',
      HTML = 'html',
      Word = 'docx'
    ))
  },
  content = function(file) {
    src <- normalizePath('report.Rmd')
    src2 <- normalizePath('temp.png')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
    file.copy(src2, 'temp.png')
    
    # Set up parameters to pass to Rmd document
    params <- list(model_name = model_name, n = input$n_sims)
    out <- rmarkdown::render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(),
      HTML = html_document(),
      Word = word_document()
    ))
    file.rename(out, file)
  }
) 

output$download_data <- downloadHandler(
  filename = function() {
    paste0(model_name, 'LossDistrution.txt')
  },
  content = function(file) {
    write.csv2(cummlLoss, file, row.names = FALSE, col.names = FALSE)
    
  }
) 

