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
    filename = function() {
      paste0('Model', '.RData')
    },
    content = function(file){
      save(network, file = file)
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
        load(file = 'network4.RData')
        network <<- network
      }
    } else if (input$dataInput == 2) {
      inFile <- input$load_model_from_file
      if (is.null(inFile))
        return(NULL)
      load(file = inFile$datapath)
      network <<- network
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
          numericInput(inputId = paste0('n_', node_ID, '_s_', states[ii]),
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
              numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states[jj]),
                           label = paste0('Child State: ', child_states[ii]),
                           min = 0, max = 1, value = CPT[ii,jj], step = 0.001)
            })
          )
        })
      } else if(n_dims ==3 ) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        lapply(1:length(parent_states_1), function(kk) {
          lapply(1:length(parent_states_2), function(jj) {
            list(
              hr(),
              paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]),
              lapply(1:length(child_states), function(ii) {
                numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj]),
                             label = paste0('Child State: ', child_states[ii]),
                             min = 0, max = 1, value = CPT[ii,kk,jj], step = 0.001)
              })
            )
          })
        })
        
      } else if(n_dims ==4 ) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        lapply(1:length(parent_states_1), function(kk) {
          lapply(1:length(parent_states_2), function(jj) {
            lapply(1:length(parent_states_3), function(ll) {
              list(
                hr(),
                paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]),
                lapply(1:length(child_states), function(ii) {
                  numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll]),
                               label = paste0('Child State: ', child_states[ii]),
                               min = 0, max = 1, value = CPT[ii,kk,jj,ll], step = 0.001)
                })
              )
            })
          })
        })
        
      } else if(n_dims ==5 ) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        parent_states_4 <- dimnames(CPT)[[5]]
        lapply(1:length(parent_states_1), function(kk) {
          lapply(1:length(parent_states_2), function(jj) {
            lapply(1:length(parent_states_3), function(ll) {
              lapply(1:length(parent_states_4), function(mm) {
                list(
                  hr(),
                  paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]," ,", parent_states_4[mm] ),
                  lapply(1:length(child_states), function(ii) {
                    numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll], parent_states_4[mm]),
                                 label = paste0('Child State: ', child_states[ii]),
                                 min = 0, max = 1, value = CPT[ii,kk,jj,ll,mm], step = 0.001)
                  })
                )
              })
            })
          })
        })
        
      } else if(n_dims ==6 ) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        parent_states_4 <- dimnames(CPT)[[5]]
        parent_states_5 <- dimnames(CPT)[[6]]
        lapply(1:length(parent_states_1), function(kk) {
          lapply(1:length(parent_states_2), function(jj) {
            lapply(1:length(parent_states_3), function(ll) {
              lapply(1:length(parent_states_4), function(mm) {
                lapply(1:length(parent_states_5), function(nn) {
                  list(
                    hr(),
                    paste0("Parent State: ", parent_states_1[kk], " ," ,parent_states_2[jj]," ,", parent_states_3[ll]," ,", parent_states_4[mm]," ,", parent_states_5[nn] ),
                    lapply(1:length(child_states), function(ii) {
                      numericInput(inputId = paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj], parent_states_3[ll], parent_states_4[mm], parent_states_5[nn]),
                                   label = paste0('Child State: ', child_states[ii]),
                                   min = 0, max = 1, value = CPT[ii,kk,jj,ll,mm,nn], step = 0.001)
                    })
                  )
                })
              })
            })
          })
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
      } else if(n_dims == 3) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        CPT[] <- sapply(1:length(parent_states_1), function(kk) {
          sapply(1:length(parent_states_2), function(jj) {
            sapply(1:length(child_states), function(ii) {
              input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj])]]
            })
          })
        })
      } else if(n_dims == 4) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        CPT[] <- sapply(1:length(parent_states_1), function(kk) {
          sapply(1:length(parent_states_2), function(jj) {
            sapply(1:length(parent_states_3), function(ll) {
              sapply(1:length(child_states), function(ii) {
                input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll])]]
              })
            })
          })
        })
      } else if(n_dims == 5) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        parent_states_4 <- dimnames(CPT)[[5]]
        CPT[] <- sapply(1:length(parent_states_1), function(kk) {
          sapply(1:length(parent_states_2), function(jj) {
            sapply(1:length(parent_states_3), function(ll) {
              sapply(1:length(parent_states_4), function(mm) {
                sapply(1:length(child_states), function(ii) {
                  input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll],parent_states_4[mm])]]
                })
              })
            })
          })
        })
      } else if(n_dims == 6) {
        child_states <- dimnames(CPT)[[1]]
        parent_states_1 <- dimnames(CPT)[[2]]
        parent_states_2 <- dimnames(CPT)[[3]]
        parent_states_3 <- dimnames(CPT)[[4]]
        parent_states_4 <- dimnames(CPT)[[5]]
        parent_states_5 <- dimnames(CPT)[[6]]
        CPT[] <- sapply(1:length(parent_states_1), function(kk) {
          sapply(1:length(parent_states_2), function(jj) {
            sapply(1:length(parent_states_3), function(ll) {
              sapply(1:length(parent_states_4), function(mm) {
                sapply(1:length(parent_states_5), function(nn) {
                  sapply(1:length(child_states), function(ii) {
                    input[[paste0('n_', node_ID, '_s_', child_states[ii], '_s_', parent_states_1[kk], parent_states_2[jj],parent_states_3[ll],parent_states_4[mm],parent_states_5[nn])]]
                  })
                })
              })
            })
          })
        })
      }
      network <<- add_CPT_to_node(network, node_ID, CPT)
      network_data$CPT <- CPT
    }
  })
  
  observeEvent(input$selected_node_for_probs, {
    output$CPT <- renderDT({ 
      dims <- dim(network_data$CPT)
      n_dims <- length(dims)
      if(n_dims == 1) {
        network_data$CPT %>% as.matrix
      } else if(n_dims == 2) {
        network_data$CPT 
      } else if(n_dims >= 3) {
        network_data$CPT %>% transform_CPT  
      }
    })
    #options = list(editable = 'cell',selection = 'none')
  })
  observeEvent(c(input$clear_model,input$add_child_parent,input$delete_nodes_edges,input$load_model_from_file), {
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
      validate(need(node_ID, ''))
      #validate(need(! is.null(network[[node_ID]]$CPT), 'You have selected a deterministic node'))
      network_sim <- network %>% net_transform
      bnlearn_net <- network_sim %>% mapping_bnlearn_network %>% model2network
      bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network_sim %>% lapply(function(v) v$CPT))
      bnlearn::bn.fit.barchart(bnlearn_net_fit[[node_ID]],cex.axis=1.5, cex.names=1.5, cex.lab = 1.5, ylab = node_ID)
    })
  })
  
  observeEvent(c(input$selected_node_for_probs,input$add_CPT_to_node),{
    output$margplot <- renderPlot({
      node_ID <- input$selected_node_for_probs
      validate(need(node_ID, ''))
      #validate(need(! is.null(network[[node_ID]]$CPT), 'You have selected a deterministic node'))
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
        value <- quantile(sim$Loss, probs = c(0.90, 0.95, 0.99, 0.999, 0.9999))
        plot<-  barplot(value, xlab = 'Quantile', ylab = 'Expected Loss', col = 'blue', main = 'Loss Distribution', border = 'red',
                        cex.axis=1.5, cex.names=1.5, cex.lab = 1.5)
        text(plot, value, paste0('$',formatC(value, big.mark=',', format = 'd')), pos=3, offset=.1, xpd=TRUE, col='darkgreen',cex=1.5)
      })
    })
  })
  
  
  
}