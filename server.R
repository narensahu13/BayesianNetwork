###server logic

options(shiny.testmode = T)
network <- list()

function(input, output, session) {
  
  network_data <- reactiveValues(
    node_list = NULL,
    Hyde_plot = NULL,
    node_states = NULL,
    CPT = NULL
  )
  
  ################################################################################################################
  ###### #######################                      Structure            #####################################
  #############################################################################################################
  
  #### Parent Node  ####
  output$parent_node <- renderUI({
    selectizeInput(inputId = 'Parent_ID',
                   choices = c(Choose = '', as.list(network_data$node_list)),
                   multiple = F,
                   label = 'Parent Node',
                   options = list(placeholder = 'Add Parent Node', create = T))
  })
  
  #### Child Node ####
  output$child_node <- renderUI({
    selectizeInput(inputId = 'Child_ID',
                   choices = c(Choose = '', as.list(network_data$node_list)),
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
      network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot
    }
  })
  
  #### Save model ####
  # observeEvent(input$save_model_to_file, {
  #   network_saved <- network
  #   save(network_saved, file = 'network.RData')
  # })
  # 
  # network_saved <- reactiveValues()
  # observe({
  #   if(!is.null(network))
  #     isolate(
  #       network_saved <- network
  #     )
  # })
  
  network_saved <- network
  output$save_model_to_file <- downloadHandler(
    filename = function() {
      paste0('Model', '.RData')
    },
    content = function(file){
      save(network_saved, file)
    }
  )
  
  network <- shiny::observe({
    if(input$dataInput == 1) {
      if(input$net == 1) {
        load(file = 'network.RData')
        network <<- network_saved
      } else if (input$net == 2) {
        load(file = 'network2.RData')
        network <<- network_saved
      } else if (input$net == 3) {
        load(file = 'network3.RData')
        network <<- network_saved
      } else if (input$net == 4) {
        load(file = 'network4.RData')
        network <<- network_saved
      }
    } else if (input$dataInput == 2) {
      inFile <- input$load_model_from_file
      if (is.null(inFile))
        return(NULL)
      load(file = inFile$datapath)
      network <<- network_saved
    }
    rm(network_saved)
    network_data$node_list <- network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
    network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot
  })
  
  #### clear the model ####
  observeEvent(input$clear_model, {
    network <<- list()
    network_data$node_list <- NULL
    network_data$node_states <- NULL
    network_data$Hyde_plot <- NULL
  })
  
  #### delete nodes ####
  observeEvent(input$delete_nodes_edges, {
    if(!is.null(input$delete_nodes_edges) & input$delete_nodes_edges > 0){
      isolate({
        network <<- delete_node(network, input$Child_ID)
      })
      network_data$node_list <- network %>% mapping_bnlearn_network %>% model2network %>% node.ordering
      network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot
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
    if(!is.null(network_data$node_list)) {
      selectInput(inputId = 'selected_node_for_states',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose ='', as.list(network_data$node_list)))
    } else {
      return('No nodes defined in network')
    }
  })
  
  output$define_states <- renderUI({
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
  
  #### Add state button ####
  observeEvent(input$add_state, {
    node_ID <- input$selected_node_for_states
    if(!is.null(node_ID)) {
      if(!is.null(input$add_state) & input$add_state > 0) {
        isolate({
          network <<- add_state_to_node(network, node_ID, input$state_to_add)
        })
        network_data$node_states <- get_node_states(network, node_ID)
      }
    }
  })
  
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
  
  output$node_state_tab2 <- DT::renderDataTable(rownames = F,
                                                options = list(pageLength = 20, selection = list(State = 'row'), width = 'auto', spacing = 'xs'),
                                                expr = {
                                                  network_data$node_states %>% as.matrix
                                                })
  
  #### delete state button ####
  observeEvent(input$delete_state, {
    node_ID <- input$selected_node_for_states
    if(!is.null(node_ID)) {
      if(!is.null(input$delete_state) & input$delete_state > 0) {
        isolate({
          network <<- remove_state_from_node(network, node_ID, input$state_to_add)
        })
        network_data$node_states <- get_node_states(network, node_ID)
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
    }
  }) 
  
  ####################################################################################################
  #######################  Define proobalities of the node  ########################################
  ########################################################################################
  
  output$select_node_for_probs <- renderUI({
    if(!is.null(network_data$node_list)) {
      selectInput(inputId = 'selected_node_for_probs',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose = '', as.list(network_data$node_list)))
    } else {
      return('No nodes defined in network')
    }
  })
  
  output$define_probs_for_node <- renderUI({
    node_ID <- input$selected_node_for_probs
    if(!is.null(node_ID)) {
      CPT <- network[[node_ID]]$CPT
      if(is.null(CPT)){
        CPT <- calc_CPT_structure_for_node(network, node_ID)
      }
      dims <- dim(CPT)
      n_dims <- length(dims)
      network_data$CPT <- CPT
      if(n_dims == 1) {
        states <- dimnames(CPT)[[1]]
        lapply(1:length(states), function(ii) {
          sliderInput(inputId = paste0('n_', node_ID, '_s_', states[ii]),
                      label = paste0('State: ', states[ii]),
                      min = 0, max = 1, value = CPT[ii], step = 0.001)
        })
      } else if(n_dims == 2) {
        child_states <- dimnames(CPT)[[1]]
        parent_states <- dimnames(CPT)[[2]]
        lapply(1:length(parent_states), function(jj) {
          list(
            hr(),
            paste0("Parent State: ", parent_states[jj]),
            lapply(1:length(child_states), function(ii) {
              sliderInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states[jj]),
                          label = paste0('Child State: ', child_states[ii]),
                          min = 0, max = 1, value = CPT[ii,jj], step = 0.001)
            })
          )
        })
      }
    } else {
      return('No node selected')
    }
  })
  
  #### add CPT button ####
  observeEvent(input$add_CPT_to_node, {
    node_ID <- input$selected_node_for_probs
    if(!is.null(node_ID)) {
      CPT <- calc_CPT_structure_for_node(network,node_ID)
      dims <- dim(CPT)
      n_dims <- length(dims)
      if(n_dims == 1) {
        states <- dimnames(CPT)[[1]]
        CPT[] <- sapply(states, function(st) {
          input[[paste0('n_', node_ID, '_s_', st)]]
        })
      } else if(n_dims ==2 ){
        child_states <- dimnames(CPT)[[1]]
        parent_states <- dimnames(CPT)[[2]]
        CPT[] <- sapply(1:length(parent_states), function(jj) {
          sapply(1:length(child_states), function(ii) {
            input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states[jj])]]
          })
        })
      }
      network <<- add_CPT_to_node(network, node_ID, CPT)
      network_data$CPT <- CPT
    }
  })
  
  output$CPT <- renderDataTable({
    dims <- dim(network_data$CPT)
    n_dims <- length(dims)
    if(n_dims == 1) {
      network_data$CPT %>% as.matrix
    } else if(n_dims == 2) {
      network_data$CPT
    }
  })
  
  
}