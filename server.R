###server logic

options(shiny.testmode = T)
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
network <- list()

function(input, output, session) {
  
  network_data <- reactiveValues(
    node_list = NULL,
    Hyde_plot = NULL,
    node_states = NULL,
    CPT = NULL,
    model = NULL,
    node_type = NULL,
    is_determ = NULL,
    formula = NULL
  )
  HydePlotOptions(variable = list(shape = "ellipse", fillcolor = "#A6DBA0"), 
                  determ = list(shape = "rect", fillcolor = "#E7D4E8", fontcolor = "#1B7837", linecolor = "#1B7837"),
                  decision = list(shape = "triangle", fillcolor = "#1B7837", linecolor = "white"),
                  utility = list(shape = "rect", fillcolor = "#762A83", fontcolor = "white"))
  ################################################################################################################
  ###### #######################                      Structure            #####################################
  #############################################################################################################
  
  #### Parent Node  ####
  output$parent_node <- renderUI({
    remove <- c('Exposure', 'Occurence', 'Impact')
    selectizeInput(inputId = 'Parent_ID',
                   choices = c(Choose = '', as.list(setdiff(network_data$node_list, remove))),
                   multiple = F,
                   label = 'Parent Node',
                   options = list(placeholder = 'Add Parent Node', create = T))
  })
  
  #### Child Node ####
  output$child_node <- renderUI({
    add <- c('Exposure', 'Occurence', 'Impact')
    selectizeInput(inputId = 'Child_ID',
                   choices = c(Choose = '', as.list(union(network_data$node_list, add))),
                   multiple = F,
                   label = 'Child Node',
                   options = list(placeholder = 'Add Child Node', create = T))
  })
  
  #### Add parent and  parent and their relationship ####
  observeEvent(input$add_child_parent, {
    if(!is.null(input$add_child_parent) & input$add_child_parent > 0) {
      isolate({
        network <<- insert_node(network, input$Parent_ID, invalidate_CPT = FALSE)
        network <<- insert_node(network, input$Child_ID)
        network <<- add_parent_child_rel(network, parent_ID  = input$Parent_ID, child_ID = input$Child_ID)
      })
      #### node ordering using BNlearn and plot using Hydenet ####
      network_data$node_list <- network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
      network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
      plot1 <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet 
      save_html(plot1,file='file.html')
      webshot::webshot("file.html",file = "temp.png",cliprect = "viewport")
      model_name <<- model_name
      #plot2 <<- export_svg(plot1) 
      #plot2 <- html_print(HTML(plot2))
    }
  })
  
  #### Save model ####
  #network <- network
  output$save_model_to_file <- downloadHandler(
    #shinyalert("Note: Save model in .RData format", "for e.g. 'model_name.RData' ", type = "info"),
    filename = function() {
      paste0(model_name, '.RData')
    },
    content = function(file){
      save(network,model_name, file = file)
    }
  )
  
  
  ## Load pre-defined demo models
  network <- shiny::observe({
    if(input$dataInput == 1) {
      if(input$net == 1) {
        load(file = 'Fraudmodel.RData')
        network <<- network
        model_name <<- model_name
      } else if (input$net == 2) {
        load(file = 'BuildingDestruction.RData')
        network <<- network
        model_name <<- model_name
      } else if (input$net == 3) {
        load(file = 'CyberAttack.RData')
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
    network_data$node_list <- network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
    network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
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
    shinyalert('New model name', type='input', callbackR = mycallback)
    
  })
  
  mycallback <- function(value) {
    model_name <<- value
    network_data$model <- model_name
    #toggle('text_div')
  }
  output$model_name <- renderText({ paste("Model Name: ", network_data$model ) })
  #### delete nodes ####
  observeEvent(input$delete_nodes_edges, {
    if(!is.null(input$delete_nodes_edges) & input$delete_nodes_edges > 0){
      isolate({
        network <<- delete_node(network, input$Child_ID)
      })
      network_data$node_list <- network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
      network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot_hydenet
      
    }
  })
  
  #### plot network ####
  output$model_plot <- renderGrViz({
    if(is.null(network))
      return(NULL)
    network_data$Hyde_plot
  })
  
  ###############################################################################################################
  ###############################          Parametrise the nodes    ####################################
  #########################################################################################################
  
  #### Define states of the nodes  ####
  output$select_node_for_states <- renderUI({
    remove <- c('Exposure', 'Occurence', 'Impact')
    if(!is.null(network_data$node_list)) {
      selectInput(inputId = 'selected_node_for_states',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose ='', as.list(setdiff(network_data$node_list, remove))))
    } else {
      return('No nodes defined in network')
    }
  })
  
  output$node_type <- renderUI({
    if(!is.null(network_data$node_list)) {
      selectInput(inputId = 'node_type',
                  label = 'Node Type',
                  selectize = F,
                  choices = c('Labels', 'Numeric', 'Boolean'))
    } else {
      return('No nodes defined in network')
    }
  })
  
  output$determ_node <- renderUI({
    node_ID <- input$selected_node_for_states
    node_type <- input$node_type
    if(!is.null(node_ID) & !is.null(node_type)) {
      network_data$node_type <- get_node_type(network,node_ID)
      network_data$is_determ <- get_if_determ(network,node_ID)
      network_data$formula <-  get_node_formula(network,node_ID)
      if(node_type =='Numeric') {
        selectizeInput(inputId = 'is_determ', choices = c('No', 'Yes'),
                       label = 'Is this a deterministic node?',
                       multiple = F)
      } else {
        selectizeInput(inputId = 'is_determ', choices = c('No'),
                       label = 'Is this a deterministic node?',
                       multiple = F)
      }
    } else {
      network_data$node_type <- NULL
      network_data$is_determ <- NULL
      network_data$formula <- NULL
      return('No nodes defined in network')
    }
  })
  
  output$define_states <- renderUI({
    #req(input$is_determ == 'No')
    node_ID <- input$selected_node_for_states
    if(!is.null(node_ID)) {
      network_data$node_states <- get_node_states(network, node_ID)
      selectizeInput(inputId = 'state_to_add',
                     choices = c(Choose = '', as.list(network_data$node_states)),
                     multiple = F,
                     label = 'Define State',
                     options = list(placeholder = 'Add state to the node', create = T))
    } else {
      network_data$node_states <- NULL
      return('No node selected')
    }
  })
  
  # output$node_formula <- renderUI({
  #   #req(input$is_determ == 'Yes')
  #   node_ID <- input$selected_node_for_formula
  #   #validate(need(node_ID, ''))
  #   if(!is.null(node_ID) ) {
  #   #validate(need(network[[node_ID]]$Is_determ == 'Yes'),"")
  #   textInputAddon(inputId = 'node_formula', label = paste0('Define ', node_ID),placeholder = 'Enter formula', addon = icon('equals'))
  #   network[[node_ID]]$formula <- input$node_formula
  #   } 
  # })
  
  # observe({
  #   shinyjs::hide("check_formula")
  #   if(input$is_determ == 'Yes')
  #     shinyjs::show("check_formula")
  # })
  
  # observeEvent(input$check_formula, {
  #   #shinyjs::hide("check_formula")
  #   #req(input$is_determ == 'Yes')
  #  # if(input$is_determ == 'Yes')
  #   #  shinyjs::show("check_formula")
  #   toggle('text_div')
  #   node_ID <- input$selected_node_for_states
  #   network[[node_ID]]$formula <<- input$node_formula
  #   formula_of_node <- input$node_formula
  #   output$output_formula <- renderPrint({ check_formula(formula_of_node, network, node_ID) })
  # })
  
  observeEvent(input$check_formula, {
    toggle('text_div')
    node_ID <- input$selected_node_for_states
    network[[node_ID]]$formula <<- input$node_formula 
    network_data$node_type <- get_node_type(network,node_ID)
    network_data$is_determ <- get_if_determ(network,node_ID)
    network_data$formula <-  get_node_formula(network,node_ID)
    #formula_of_node <- input$node_formula
    output_formula <-  check_formula(input$node_formula, network, node_ID)
    if(output_formula == 'OK') {
      shinyalert('Formula OK', type = 'success')
    } else {
      shinyalert('Formula error, plese re-enter the formula', output_formula, type = 'error')
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
                      rintrojs::introjs(session, options = list(steps = structureHelp))
  )
  
  #### show state table ####
  output$node_state_tab <- DT::renderDataTable(rownames = F,
                                               options = list(pageLength = 20, selection = list(State = 'row'), width = 'auto', spacing = 'xs'),
                                               expr = {
                                                 if(!is.null(input$selected_node_for_states)) {
                                                   stats <- network_data$node_states
                                                   if(!is.null(stats)) {
                                                     stats %>% as.matrix
                                                   }
                                                 }
                                               })
  
  ### show node type
  output$type <- renderUI(network_data$node_type)
  output$determ <- renderUI(network_data$is_determ)
  output$formula <- renderUI(network_data$formula)
  
  #### delete state button ####
  observeEvent(input$delete_state, {
    node_ID <- input$selected_node_for_states
    if(!is.null(node_ID)) {
      if(!is.null(input$delete_state) & input$delete_state > 0) {
        isolate({
          network <<- remove_state_from_node(network, node_ID, input$state_to_add)
        })
        network_data$node_states <- get_node_states(network, node_ID)
        network_data$node_type <- get_node_type(network, node_ID)
        network_data$is_determ <- get_if_determ(network, node_ID)
        network_data$formula <-  get_node_formula(network,node_ID)
      }
    }
  })
  
  #### clear all state button ####
  observeEvent(input$clear_state, {
    node_ID <- input$selected_node_for_states
    if(!is.null(node_ID)) {
      isolate({
        network <<- clear_all_state_from_node(network, node_ID)
      })
      network_data$node_states <- get_node_states(network, node_ID)
      network_data$node_type <- get_node_type(network, node_ID)
      network_data$is_determ <- get_if_determ(network, node_ID)
      network_data$formula <-  get_node_formula(network,node_ID)
    }
  }) 
  
  #### Validate the states and show error message if any
  observeEvent(input$validate_state, {
    check <- check_state(network)
    network <<- update_check_state(network)
    if(!is.null(check)) {
      shinyalert('Error',paste(check,'.','Some of the nodes have been reset'), type = 'error')
    } else {
      shinyalert("OK",type = 'success')
    }
  })
  
  # Observe intro btn and start the intro
  shiny::observeEvent(input$stateIntro,
                      rintrojs::introjs(session, options = list(steps = stateHelp))
  )
  ####################################################################################################
  #######################  Define proobalities of the node  ########################################
  ########################################################################################
  
  # Observe intro btn and start the intro
  shiny::observeEvent(input$parameterIntro,
                      rintrojs::introjs(session, options = list(steps = parameterHelp))
  )
  
  output$select_node_for_probs <- renderUI({
    if(!is.null(network_data$node_list)) {
      remove <- c('Exposure', 'Occurence', 'Impact')
      selectInput(inputId = 'selected_node_for_probs',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose = '', as.list(setdiff(network_data$node_list, remove))))
    } else {
      return('No nodes defined in network')
    }
  })
  
  
  observeEvent(c( input$CPT_cell_edit ), {
    node_ID <- input$selected_node_for_probs
    validate(need(node_ID, ''))
    validate(need(network[[node_ID]]$CPT, 'Add probabilities to the node'))
    if(network[[node_ID]]$Is_determ == 'Yes') {
      #network <<- network
      shinyalert("Deterministic Node!!", "You can't change the probabilities", type = "error")
    }
    if(!is.null(node_ID) & (network[[node_ID]]$Is_determ == 'No')) {
      CPT <- network[[node_ID]]$CPT
      table <- CPT %>% transform_CPT
      table <- editData(table, input$CPT_cell_edit, 'CPT')
      network <<- update_cpt_to_network(network, node_ID, table)
    }
    
  })
  
  observeEvent(input$add_CPT_to_node, {
    node_ID <- input$selected_node_for_probs
    if(!is.null(node_ID) ) {
      CPT <- network[[node_ID]]$CPT
      CPT <- CPT %>% transform_CPT
      network <<- update_cpt_to_network(network, node_ID, CPT)
      if(network[[node_ID]]$CPT %>% as.data.frame %>% colSums %>% sum != prod(dim(network[[node_ID]]$CPT)[-1]) ) {
        network <<- normalize_prob(network, node_ID)
        shinyalert("Oops! probabilities do not sum up to 1", "Normalizing probablities", type = "success")
      }
    } 
  })
  
  observeEvent(c(input$selected_node_for_probs), {
    node_ID <- input$selected_node_for_probs
    validate(need(node_ID, ''))
    CPT <- network[[node_ID]]$CPT
    if(is.null(CPT)  ){ #{ & network[[node_ID]]$Is_determ =='No' 
      CPT <- calc_CPT_structure_for_node(network, node_ID) }
    table <- CPT %>% transform_CPT  #%>%  datatable( rownames = FALSE) %>%
    # } else if( network[[node_ID]]$Is_determ =='Yes') {
    #   network <<- define_determ_node(network, node_ID, network[[node_ID]]$formula)
    #   table <- network[[node_ID]]$CPT %>% transform_CPT
    # }
    network <<- update_cpt_to_network(network, node_ID, table)
    output$CPT <- DT::renderDT(DT::datatable(table, selection = list (mode = 'single', target = 'cell'), editable = TRUE, 
                                             extensions = c('FixedColumns','FixedHeader'),
                                             options = list(searching = F, scrollX = TRUE, fixedHeader=TRUE,
                                                            fixedColumns=list(leftColumns=2,rightColumns=0))))
  })
  
  
  
  # observeEvent(c(input$clear_model,input$add_child_parent,input$delete_nodes_edges,input$load_model_from_file,input$dataInput,input$net), {
  # output$formula_node <- renderUI({
  #   formula_node_ID <- get_formula_node(network)
  #     selectInput(inputId = 'selected_node_for_formula',
  #                 label = 'Select Node',
  #                 selectize = F,
  #                 choices = c(Choose = '', formula_node_ID))
  #   })
  # })
  
  
  observeEvent(c(input$selected_node_for_probs,input$add_CPT_to_node),{
    output$condplot <- renderPlot({
      node_ID <- input$selected_node_for_probs
      non_CPT_node <- node_without_CPT(network) %>% is.null
      validate(need(node_ID, ''))
      validate(need(! is.null(network[[node_ID]]$CPT), 'Please adjust conditional probabilities of the node and click "ADD CPT to Node" '))
      validate(need((non_CPT_node), 'Please define conditional probabilities for all the nodes'))
      bnlearn_net <- network %>% net_transform %>% mapping_bnlearn_network %>% model2network
      bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network %>% net_transform %>% lapply(function(v) v$CPT))
      bn.fit.barchart(bnlearn_net_fit[[node_ID]],cex.axis=1.5, cex.names=1.5, cex.lab = 1.5, ylab = node_ID)
    })
  })
  
  observeEvent(c(input$selected_node_for_probs,input$add_CPT_to_node),{
    output$margplot <- renderPlot({
      node_ID <- input$selected_node_for_probs
      validate(need(node_ID, ''))
      validate(need(! is.null(network[[node_ID]]$CPT), 'Please adjust conditional probabilities of the node and click "ADD CPT to Node" '))
      network_sim <- network  ## copy network
      if( c('Exposure', 'Occurence', 'Impact')  %in%  names(network_sim) %>% unique == TRUE) {
        network_sim[["Impact"]] <- network_sim[["Exposure"]] <- network_sim[["Occurence"]] <- NULL
      }
      plot_marinal(network, node_ID)
    })
  })
  
  
  observeEvent(input$checkx, {
    toggle('text_div')
    formula_exposure <- input$exposure
    output$outputx <- renderPrint({ check_formula(formula_exposure, network, 'Exposure') })
  })
  
  observeEvent(input$checko, {
    toggle('text_div')
    formula_occurence <- input$occurence
    output$outputo <- renderPrint({ check_formula(formula_occurence, network, 'Occurence') })
  })
  
  observeEvent(input$checki, {
    toggle('text_div')
    formula_impact <- input$impact
    output$outputi <- renderPrint({ check_formula(formula_impact, network, 'Impact') })
  })
  
  # Observe intro btn and start the intro
  shiny::observeEvent(input$reportIntro,
                      rintrojs::introjs(session, options = list(steps = reportHelp))
  )
  
  observeEvent(input$calculate, {
   withProgress(message = 'Calculation in progress...', {
    # progressSweetAlert(
    #   session = session, id = "myprogress",
    #   title = "Work in progress",
    #   display_pct = TRUE, value = 0
    # )

      n_sims <- input$n_sims
      formula_exposure <- input$exposure
      formula_occurence <- input$occurence
      formula_impact <- input$impact
      if(!(formula_exposure >0 & formula_impact >0 & formula_occurence>0)) {
        shinyalert("Define formula for XOI nodes from 'Structure'tab", type = 'error')
      }
      validate(need((c(formula_exposure >0 & formula_impact >0 & formula_occurence>0)), "Define formula for XOI nodes from 'Structure'tab"))
      #validate(need(any(formula_exposure, formula_impact, formula_occurence), "Define formula for XOI nodes from ' Structure'tab"))
      for(node_ID in setdiff(names(network), c('Exposure','Occurence','Impact'))) {
        if(is.null(network[[node_ID]]$CPT)) {
          shinyalert('Probabilities missing for', node_ID, type = 'error')
          validate(need(network[[node_ID]]$CPT ,'Probabilities missing'))
        }
      }
      ##
      exposure_net <- net_transform_node(network, 'Exposure')
      exposure_bnlearn <- exposure_net %>% mapping_bnlearn_network %>% model2network
      exposure_bnlearn_net_fit <- custom.fit(exposure_bnlearn, dist = exposure_net %>% lapply(function(v) v$CPT))
      exposure_sim <- rbn(exposure_bnlearn_net_fit,n_sims)   
      exposure_sim <- sapply(exposure_sim, function(x) as.numeric(as.vector(x)))
      exposure_sim <- exposure_sim %>% as.data.frame %>%  mutate(Exposure =  eval(parse(text=formula_exposure))) %>%
                          .$Exposure %>% as.character %>% as.numeric
      ###
      sim <- run_simulation(network,exposure_sim, n_sims,formula_occurence,formula_impact)
      cummlLoss <<- aggregate(sim$Loss_tot, by=list(Iter = sim$Iter_vec), FUN = sum) %>% .$x %>% round
      #cummlLoss <- CumulLoss$x %>% sort
      output$histogram <- renderPlot({
        value <<- quantile(cummlLoss, probs = c(0.8, 0.85,0.90, 0.95, 0.99, 0.995, 0.999, 0.9999))
        plot <-  barplot(value %>% round, xlab = 'Quantile', ylab = ' Loss', col = c('#FFE5CC','#FFCC99','#FFB266','#FF9933','#FF8000','#CC6600','#994C00','#663300'),
                        main = 'Loss Distribution', border = 'red',
                        cex.axis=1.5, cex.names=1.5, cex.lab = 1.5)
        abline(h = cummlLoss %>% mean)
        text(plot, value, paste0('$',format(value, big.mark=',', format = 'd')), pos=3, offset=.1, xpd=TRUE, col='darkgreen',cex=1.5)
      })
      output$density <- renderPlot({
        plot(cummlLoss %>% sort, type='h', col='red', main='Cumulative Loss Density plot', ylab='Loss')
      })
      output$text_out <- renderUI({
        str1 <- paste("Number of Simulations: ", format(n_sims, big.mark=',', format = 'd'), '\n')
        str2 <- paste("<B>Single Event</B>")
        str3 <- paste("Minimum Loss: ", paste0('$',format(sim %>% filter(Loss_tot!=0) %>% .$Loss_tot %>% min %>% round, big.mark=',', format = 'd', scientific = F)), '\n')
        str4 <- paste("Average Loss: ", paste0('$',format(sim %>% filter(Loss_tot!=0) %>% .$Loss_tot %>% mean %>% round, big.mark=',', format = 'd', scientific = F)), '\n')
        str5 <- paste("Maximum Loss: ", paste0('$',format(sim %>% filter(Loss_tot!=0) %>% .$Loss_tot %>% max %>% round, big.mark=',', format = 'd', scientific = F)), '\n')
        str6 <- paste("<B>Frequency</B>")
        str7 <- paste("Average: ", (sim$Occurence %>% mean) * (exposure_sim %>% mean ), '\n')
        str8 <- paste("<B>Cumulated Loss</B>")
        str9 <- paste("Min : ", paste0('$',format(cummlLoss %>% min %>% round, big.mark=',', format = 'd')), '\n')
        str10 <- paste("Max : ", paste0('$',format(cummlLoss %>% max %>% round, big.mark=',', format = 'd', scientific = F)), '\n')
        str11 <- paste("Mean : ", paste0('$',format(cummlLoss %>% mean %>% round, big.mark=',', format = 'd', scientific = F)), '\n')
        str12 <- paste("Median : ", paste0('$',format(cummlLoss %>% median %>% round, big.mark=',', format = 'd')), '\n')
        str13 <- paste("Standard Deviation : ", paste0('$',format(cummlLoss  %>% sd %>% round, big.mark=',', format = 'd')), '\n')
        str14 <- paste("Skewness : ", format(cummlLoss %>% skewness, digits = 4), '\n')
        str15 <- paste("Kurtosis : ", format(cummlLoss %>% kurtosis, digits = 4), '\n')
        HTML(paste(str1, str2,str3, str4,str5, str6, str7, str8, str9,str10,str11,str12,str13,str14,str15, sep = '<br/>'))
        
      })
   })
     # closeSweetAlert(session = session)
      # sendSweetAlert(
      #   session = session,
      #   title =" Calculation completed !",
      #   type = "success"
      # )
  })
  
  output$generate_report <- downloadHandler(
    filename = function() {
      paste(model_name, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
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
      params <- list(model_name = model_name, n=input$n_sims)
      out <- rmarkdown::render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
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
  
}