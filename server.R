###server logic

options(shiny.testmode = T)
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
network <- list()

function(input, output, session) {
  
  network_data <- reactiveValues(
    node_list = NULL,
    Hyde_plot = NULL,
    node_states = NULL,
    CPT = NULL
  )
  HydePlotOptions(variable = list(shape = "ellipse", fillcolor = "#A6DBA0"), 
                  determ = list(shape = "rect", fillcolor = "#E7D4E8", fontcolor = "#1B7837", linecolor = "#1B7837"),
                  decision = list(shape = "triangle", fillcolor = "#1B7837", linecolor = "white"),
                  utility = list(shape = "diamond", fillcolor = "#762A83", fontcolor = "white"))
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
      network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot
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
      } else if (input$net == 2) {
        load(file = 'network2.RData')
        network <<- network
      } else if (input$net == 3) {
        load(file = 'network3.RData')
        network <<- network
      } else if (input$net == 4) {
        load(file = 'BuildingDestruction.RData')
        network <<- network
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
    network_data$Hyde_plot <- network %>% mapping_Hydenet_network %>% HydeNetwork %>% plot
  })
  
  #### clear the model ####
  observeEvent(input$clear_model, {
    network <<- list()
    network_data$node_list <- NULL
    network_data$node_states <- NULL
    network_data$Hyde_plot <- NULL
    network_data$CPT <- NULL
    shinyalert('New model name', type='input', callbackR = mycallback)
    
  })

    mycallback <- function(value) {
      model_name <<- value
      toggle('text_div')
      output$model_name <- renderText({ paste("Model Name: ", value ) })
    }

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
      remove <- c('Exposure', 'Occurence', 'Impact')
      selectInput(inputId = 'selected_node_for_probs',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose = '', as.list(setdiff(network_data$node_list, remove))))
    } else {
      return('No nodes defined in network')
    }
  })
  
  # output$define_probs_for_node <- renderUI({
  #   node_ID <- input$selected_node_for_probs
  #   if(!is.null(node_ID)) {
  #     CPT <- network[[node_ID]]$CPT
  #     if(is.null(CPT)){
  #       CPT <- calc_CPT_structure_for_node(network, node_ID)
  #     }
  #     dims <- dim(CPT)
  #     n_dims <- length(dims)
  #     network_data$CPT <- CPT
  #     if(n_dims == 1) {
  #       states <- dimnames(CPT)[[1]]
  #       lapply(1:length(states), function(ii) {
  #         numericInput(inputId = paste0('n_', node_ID, '_s_', states[ii]),
  #                      label = paste0('State: ', states[ii]),
  #                      min = 0, max = 1, value = CPT[ii], step = 0.001)
  #       })
  #     } else if(n_dims == 2) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states <- dimnames(CPT)[[2]]
  #       lapply(1:length(parent_states), function(jj) {
  #         list(
  #           hr(),
  #           paste0("Parent State: ", parent_states[jj]),
  #           lapply(1:length(child_states), function(ii) {
  #             numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states[jj]),
  #                          label = paste0('Child State: ', child_states[ii]),
  #                          min = 0, max = 1, value = CPT[ii,jj], step = 0.001)
  #           })
  #         )
  #       })
  #     } else if(n_dims ==3 ) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       lapply(1:length(parent_states_1), function(kk) {
  #         lapply(1:length(parent_states_2), function(jj) {
  #           list(
  #             hr(),
  #             paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]),
  #             lapply(1:length(child_states), function(ii) {
  #               numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj]),
  #                            label = paste0('Child State: ', child_states[ii]),
  #                            min = 0, max = 1, value = CPT[ii,kk,jj], step = 0.001)
  #             })
  #           )
  #         })
  #       })
  # 
  #     } else if(n_dims ==4 ) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       lapply(1:length(parent_states_1), function(kk) {
  #         lapply(1:length(parent_states_2), function(jj) {
  #           lapply(1:length(parent_states_3), function(ll) {
  #             list(
  #               hr(),
  #               paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]),
  #               lapply(1:length(child_states), function(ii) {
  #                 numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll]),
  #                              label = paste0('Child State: ', child_states[ii]),
  #                              min = 0, max = 1, value = CPT[ii,kk,jj,ll], step = 0.001)
  #               })
  #             )
  #           })
  #         })
  #       })
  # 
  #     } else if(n_dims ==5 ) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       parent_states_4 <- dimnames(CPT)[[5]]
  #       lapply(1:length(parent_states_1), function(kk) {
  #         lapply(1:length(parent_states_2), function(jj) {
  #           lapply(1:length(parent_states_3), function(ll) {
  #             lapply(1:length(parent_states_4), function(mm) {
  #               list(
  #                 hr(),
  #                 paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]," ,", parent_states_4[mm] ),
  #                 lapply(1:length(child_states), function(ii) {
  #                   numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll], parent_states_4[mm]),
  #                                label = paste0('Child State: ', child_states[ii]),
  #                                min = 0, max = 1, value = CPT[ii,kk,jj,ll,mm], step = 0.001)
  #                 })
  #               )
  #             })
  #           })
  #         })
  #       })
  # 
  #     } else if(n_dims ==6 ) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       parent_states_4 <- dimnames(CPT)[[5]]
  #       parent_states_5 <- dimnames(CPT)[[6]]
  #       lapply(1:length(parent_states_1), function(kk) {
  #         lapply(1:length(parent_states_2), function(jj) {
  #           lapply(1:length(parent_states_3), function(ll) {
  #             lapply(1:length(parent_states_4), function(mm) {
  #               lapply(1:length(parent_states_5), function(nn) {
  #                 list(
  #                   hr(),
  #                   paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]," ,", parent_states_4[mm]," ,", parent_states_5[nn] ),
  #                   lapply(1:length(child_states), function(ii) {
  #                     numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll], parent_states_4[mm], parent_states_5[nn]),
  #                                  label = paste0('Child State: ', child_states[ii]),
  #                                  min = 0, max = 1, value = CPT[ii,kk,jj,ll,mm,nn], step = 0.001)
  #                   })
  #                 )
  #               })
  #             })
  #           })
  #         })
  #       })
  # 
  #     }
  #   } else {
  #     return('No node selected')
  #   }
  # })
  
  #### add CPT button ####
  # observeEvent(input$add_CPT_to_node, {
  #   node_ID <- input$selected_node_for_probs
  #   if(!is.null(node_ID)) {
  #     CPT <- calc_CPT_structure_for_node(network,node_ID)
  #     dims <- dim(CPT)
  #     n_dims <- length(dims)
  #     if(n_dims == 1) {
  #       states <- dimnames(CPT)[[1]]
  #       CPT[] <- sapply(states, function(st) {
  #         input[[paste0('n_', node_ID, '_s_', st)]]
  #       })
  #     } else if(n_dims ==2 ){
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states <- dimnames(CPT)[[2]]
  #       CPT[] <- sapply(1:length(parent_states), function(jj) {
  #         sapply(1:length(child_states), function(ii) {
  #           input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states[jj])]]
  #         })
  #       })
  #     } else if(n_dims == 3) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       CPT[] <- sapply(1:length(parent_states_1), function(kk) {
  #         sapply(1:length(parent_states_2), function(jj) {
  #           sapply(1:length(child_states), function(ii) {
  #             input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj])]]
  #           })
  #         })
  #       })
  #     } else if(n_dims == 4) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       CPT[] <- sapply(1:length(parent_states_1), function(kk) {
  #         sapply(1:length(parent_states_2), function(jj) {
  #           sapply(1:length(parent_states_3), function(ll) {
  #             sapply(1:length(child_states), function(ii) {
  #               input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll])]]
  #             })
  #           })
  #         })
  #       })
  #     } else if(n_dims == 5) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       parent_states_4 <- dimnames(CPT)[[5]]
  #       CPT[] <- sapply(1:length(parent_states_1), function(kk) {
  #         sapply(1:length(parent_states_2), function(jj) {
  #           sapply(1:length(parent_states_3), function(ll) {
  #             sapply(1:length(parent_states_4), function(mm) {
  #               sapply(1:length(child_states), function(ii) {
  #                 input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll],parent_states_4[mm])]]
  #               })
  #             })
  #           })
  #         })
  #       })
  #     } else if(n_dims == 6) {
  #       child_states <- dimnames(CPT)[[1]]
  #       parent_states_1 <- dimnames(CPT)[[2]]
  #       parent_states_2 <- dimnames(CPT)[[3]]
  #       parent_states_3 <- dimnames(CPT)[[4]]
  #       parent_states_4 <- dimnames(CPT)[[5]]
  #       parent_states_5 <- dimnames(CPT)[[6]]
  #       CPT[] <- sapply(1:length(parent_states_1), function(kk) {
  #         sapply(1:length(parent_states_2), function(jj) {
  #           sapply(1:length(parent_states_3), function(ll) {
  #             sapply(1:length(parent_states_4), function(mm) {
  #               sapply(1:length(parent_states_5), function(nn) {
  #                 sapply(1:length(child_states), function(ii) {
  #                   input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll],parent_states_4[mm],parent_states_5[nn])]]
  #                 })
  #               })
  #             })
  #           })
  #         })
  #       })
  #     }
  #     network <<- add_CPT_to_node(network, node_ID, CPT)
  #     network_data$CPT <- CPT
  #       if(network[[node_ID]]$CPT %>% as.data.frame %>% colSums %>% sum != prod(dim(network[[node_ID]]$CPT)[-1]) ) {
  #         network <<- normalize_prob(network, node_ID)
  #         shinyalert("Oops! probabilities do not sum up to 1", "Normalizing probablities", type = "success")
  #       }
  #   }
  # })
  # values = reactiveValues()
  # 
  # table = reactive({
  #   node_ID <- input$selected_node_for_probs
  #   CPT <- network[[node_ID]]$CPT
  #   if(is.null(CPT)){
  #     CPT <- calc_CPT_structure_for_node(network, node_ID)
  #   }
  #   CPT <- CPT %>% transform_CPT 
  #   if (!is.null(input$CPT)) {
  #     DF = hot_to_r(input$CPT)
  #   } else {
  #     if (is.null(values[["DF"]]))
  #       DF = CPT
  #     else
  #       DF = values[["DF"]]
  #   }
  #   values[["DF"]] = DF
  #   DF
  # })
  
  observeEvent(c( input$CPT_cell_edit ), {
    node_ID <- input$selected_node_for_probs
    validate(need(node_ID, ''))
    validate(need(network[[node_ID]]$CPT, 'Add probabilities to the node'))
    CPT <- network[[node_ID]]$CPT
    # if(is.null(CPT)){
    #   CPT <- calc_CPT_structure_for_node(network, node_ID)
    # }
    # dims <- dim(network[[node_ID]]$CPT)
    # n_dims <- length(dims)
    #if(input$CPT_cell_edit > 0) {
      # if(n_dims == 1) {
      #   table <- CPT %>% as.matrix #network[[node_ID]]$
      # } else if(n_dims == 2) {
      #   table <- CPT
      # } else if(n_dims >= 3) {
       table <- CPT %>% transform_CPT
      # }
   # output$CPT <- renderDT(table, selection = 'none', editable = 'cell')
    # proxy = dataTableProxy('CPT')
    # info = input$CPT_cell_edit
    # str(info)
    # i = info$row
    # j = info$col
    # v = info$value
    # table[i, j] <- DT::coerceValue(v, table[i, j])
    # replaceData(proxy, table, resetPaging = FALSE)  # important
      #observe(input$CPT_cell_edit)
      table <- editData(table, input$CPT_cell_edit, 'CPT')
      
      network <<- update_cpt_to_network(network, node_ID, table)
      
      # isolate({
      # if(network[[node_ID]]$CPT %>% as.data.frame %>% colSums %>% sum != prod(dim(network[[node_ID]]$CPT)[-1]) ) {
      #   network <<- normalize_prob(network, node_ID)
      #   shinyalert("Oops! probabilities do not sum up to 1", "Normalizing probablities", type = "success")
      # }
      # })

      #network <<- add_CPT_to_node(network, node_ID, updated_cpt)
      #network[[node_ID]]$CPT <- NULL
      #network[[node_ID]]$CPT <<- update_cpt_to_network(network, node_ID, cpt_transformed)
      #network_data$CPT <- network[[node_ID]]$CPT
    #} else {
      #network <<- add_CPT_to_node(network, node_ID, CPT)
     # network_data$CPT <- CPT
    #}

  })
  
  observeEvent(input$add_CPT_to_node, {
      node_ID <- input$selected_node_for_probs
      if(!is.null(node_ID)) {
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
    if(is.null(CPT)){
      CPT <- calc_CPT_structure_for_node(network, node_ID)
    }
    # dims <- dim(network[[node_ID]]$CPT)
    # n_dims <- length(dims)
    # matrix_dim <- c(dims[1]+n_dims-1, prod(dims[-1])+1)
    # if(n_dims == 1) {
    #   table <- network[[node_ID]]$CPT %>% as.matrix
    # } else if(n_dims == 2) {
    #    table <- network[[node_ID]]$CPT
    # } else if(n_dims >= 3) {
    table <- CPT %>% transform_CPT  #%>%  datatable( rownames = FALSE) %>%
    # formatStyle(2:matrix_dim[2],
    #             background = styleEqual(c(4, 5), c(rep("lightblue", 2))))
    # }
    #table <- editData(table, input$CPT_cell_edit, 'CPT')
    
    network <<- update_cpt_to_network(network, node_ID, table)
    output$CPT <- DT::renderDT(DT::datatable(table, selection = list (mode = 'single', target = 'cell'), editable = TRUE, options = list(searching = F, scrollX = TRUE, dom = 't')))
    
    #output$CPT <-render_dt( table, 'all', rownames = T, options =list(searching = F, scrollX = TRUE))
    # output$CPT <- renderRHandsontable({
    #   #table = table()
    #   if (!is.null(table))
    #     rhandsontable(table, stretchH = "all")
    # })
  })
  
  
  # observeEvent(input$selected_node_for_probs, {
  #   output$CPT <- renderDataTable({ 
  #     node_ID <- input$selected_node_for_probs
  #     dims <- dim(network[[node_ID]]$CPT)
  #     n_dims <- length(dims)
  #     matrix_dim <- c(dims[1]+n_dims-1, prod(dims[-1])+1)
  #     if(n_dims == 1) {
  #       network[[node_ID]]$CPT %>% as.matrix
  #     } else if(n_dims == 2) {
  #        network[[node_ID]]$CPT 
  #     } else if(n_dims >= 3) {
  #        network[[node_ID]]$CPT %>% transform_CPT  #%>%  datatable( rownames = FALSE) %>%
  #         # formatStyle(2:matrix_dim[2],
  #         #             background = styleEqual(c(4, 5), c(rep("lightblue", 2))))
  #     }
  #   },  editable = T, server = T, options =list(searching = F, scrollX = TRUE))
  # })
  
  # proxy = dataTableProxy('CPT')
  # 
  # observeEvent(input$CPT_cell_edit, {
    # info = input$CPT_cell_edit
    # str(info)
    # i = info$row
    # j = info$col
    # v = info$value
    # table[i, j] <<- DT::coerceValue(v, table[i, j])
    # replaceData(proxy, table, resetPaging = FALSE)  # important
  # })
  
  observeEvent(c(input$clear_model,input$add_child_parent,input$delete_nodes_edges,input$load_model_from_file,input$dataInput,input$net), {
  output$formula_node <- renderUI({
    formula_node_ID <- get_formula_node(network)
      selectInput(inputId = 'selected_node_for_formula',
                  label = 'Select Node',
                  selectize = F,
                  choices = c(Choose = '', formula_node_ID))
    })
  })
  
  output$node_formula <- renderUI({
    node_ID <- input$selected_node_for_formula
    validate(need(node_ID, ''))
    div(style="display:inline-block",textInputAddon(inputId = paste0(node_ID, '_formula'), 
                                                    label = paste0('Define ', node_ID),placeholder = 'Enter formula', addon = icon('equals')))
  })
  
  observeEvent(input$check_formula, {
    toggle('text_div')
    node_ID <- input$selected_node_for_formula
    formula_of_node <- input[[paste0(node_ID, '_formula')]]
    output$output_formula <- renderPrint({ check_formula(formula_of_node, network, node_ID) })
  })
  
  
  observeEvent(c(input$selected_node_for_probs,input$add_CPT_to_node),{
    output$condplot <- renderPlot({
      node_ID <- input$selected_node_for_probs
      non_CPT_node <- node_without_CPT(network) %>% is.null
      validate(need(node_ID, ''))
      validate(need(! is.null(network[[node_ID]]$CPT), 'Please adjust conditional probabilities of the node and click "ADD CPT to Node" '))
      validate(need((non_CPT_node), 'Please define conditional probabilities for all the nodes'))
      bnlearn_net <- network %>% net_transform %>% mapping_bnlearn_network %>% model2network
      bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network %>% net_transform %>% lapply(function(v) v$CPT))
      bnlearn::bn.fit.barchart(bnlearn_net_fit[[node_ID]],cex.axis=1.5, cex.names=1.5, cex.lab = 1.5, ylab = node_ID)
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

  observeEvent(input$calculate, {
    withProgress(message = 'Calculationin progress...', style = 'old', {
      n_sims <- input$n_sims
      formula_exposure <- input$exposure
      formula_occurence <- input$occurence
      formula_impact <- input$impact
      network <- network
      sim <- run_simulation(network, n_sims)
      sim <- sim %>% mutate(Exposure = eval(parse(text=formula_exposure)), Occurence =  eval(parse(text=formula_occurence)),
                            Impact = eval(parse(text = formula_impact)) ) %>%
        mutate(Loss = Exposure * Occurence * Impact)
      output$histogram <- renderPlot({
        value <- quantile(sim$Loss, probs = c(0.90, 0.95, 0.99, 0.999, 0.9995, 0.9999))
        plot<-  barplot(value, xlab = 'Quantile', ylab = 'Expected Loss', col = 'blue', main = 'Loss Distribution', border = 'red',
                        cex.axis=1.5, cex.names=1.5, cex.lab = 1.5)
        abline(h = sim$Loss %>% mean)
        text(plot, value, paste0('$',format(value, big.mark=',', format = 'd')), pos=3, offset=.1, xpd=TRUE, col='darkgreen',cex=1.5)
      })
      output$text_out <- renderUI({
        str1 <- paste("Number of Simulations: ", format(n_sims, big.mark=',', format = 'd'), '\n')
        str2 <- paste("<B>Single Event</B>")
        str3 <- paste("Minimum Loss: ", paste0('$',format(sim %>% filter(Loss!=0) %>% .$Loss %>% min, big.mark=',', format = 'd', scientific = F)), '\n')
        str4 <- paste("Average Loss: ", paste0('$',format(sim %>% filter(Loss!=0) %>% .$Loss %>% mean, big.mark=',', format = 'd', scientific = F)), '\n')
        str5 <- paste("Maximum Loss: ", paste0('$',format(sim %>% filter(Loss!=0) %>% .$Loss %>% max, big.mark=',', format = 'd', scientific = F)), '\n')
        str6 <- paste("<B>Frequency</B>")
        str7 <- paste("Average: ", sim$Occurence %>% mean, '\n')
        str8 <- paste("<B>Cumulated Loss</B>")
        str9 <- paste("Min : ", paste0('$',format(sim$Loss %>% sort %>% cumsum %>% min, big.mark=',', format = 'd')), '\n')
        str10 <- paste("Max : ", paste0('$',format(sim$Loss %>% sort %>% cumsum %>% max, big.mark=',', format = 'd', scientific = F)), '\n')
        str11 <- paste("Mean : ", paste0('$',format(sim$Loss %>% sort %>% cumsum %>% mean, big.mark=',', format = 'd', scientific = F)), '\n')
        str12 <- paste("Median : ", paste0('$',format(sim$Loss %>% sort %>% cumsum %>% median, big.mark=',', format = 'd')), '\n')
        str13 <- paste("Standard Deviation : ", paste0('$',format(sim$Loss %>% sort %>% cumsum %>% sd, big.mark=',', format = 'd')), '\n')
        str14 <- paste("Skewness : ", format(sim$Loss %>% sort %>% cumsum %>% skewness, digits = 4), '\n')
        str15 <- paste("Kurtosis : ", format(sim$Loss %>% sort %>% cumsum %>% kurtosis, digits = 4), '\n')
        HTML(paste(str1, str2,str3, str4,str5, str6, str7, str8, str9,str10,str11,str12,str13,str14,str15, sep = '<br/>'))

      })
    })
  })
  
  
  
}