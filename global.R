### global functions to create ,perform checks and do plots

### Get list of nodes in a network
get_nodes <- function(network) {
  names(network)
}

is_empty <- function(network) {
  is.null(names(network))
}

### insertion of a node
insert_node <- function(network, node_ID, invalidate_CPT = TRUE) {
  if ( !(node_ID %in% names(network)) ) {
    n1 <- list(ID = node_ID, IS_ROOT = TRUE)
    network <- c(list(n1), network)
    names(network)[1] <- n1$ID
  }
  if(invalidate_CPT) {
    network[[node_ID]]$CPT <-NULL
  }
  network
}

### insert parent-child relation
add_parent_child_rel <- function(network, parent_ID, child_ID) {
  if(all(c(parent_ID, child_ID) %in% names(network))) {
    network[[parent_ID]]$Children <- c(child_ID, network[[parent_ID]]$Children) %>% unique
    network[[child_ID]]$Parents   <- c(parent_ID, network[[child_ID]]$Parents) %>% unique
    network[[child_ID]]$IS_ROOT   <- FALSE
  }
  network
}

### Get list of children of a node
get_children <- function(network, node_ID) {
  network[[node_ID]]$Children
}

### Get table of list of parents and all children
get_parent_child_table <- function(network) {
  parents <- get_nodes(network = network)
  sapply(parents, function(parent_ID) {
    children <- get_children(network = network, parent_ID)
    tibble(Parent = rep(parent_ID, length(children), Child = children))
  }) %>% do.call(what = 'rbind')
}

### deletion of a node
##### the deletion should return a coherent  parent-child relation
delete_node <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    ##for each parent, it must remove node_ID from children
    for(parent in network[[node_ID]]$Parents) {
      network[[node_ID]]$Children <-network[[parent]]$Children %>% Filter(f = function(v) v != node_ID)
      if(0 == length(network[[parent]]$Children)) {
        network[[parent]]$Children <- NULL
      }
    }
    ## for each child, must remove node_ID from parent
    for(child in network[[node_ID]]$Children) {
      network[[child]]$Parents <- network[[child]]$Parents %>% Filter(f = function(v) v != node_ID)
      if(0 == length(network[[child]]$Parents)) {
        network[[child]]$Parents <- NULL
        network[[child]]$IS_ROOT <- TRUE
      }
    }
    ## now the node with name node_ID is completely unlinked
    network[[node_ID]] <- NULL
  }
  network
}

### mapping a node to a BNlearn and Hydenet object
mapping_node <- function(node, type ='bnlearn') {
  if(node$IS_ROOT) {
    retval <- node$ID
  } else {
    retval <- paste0(node$ID, '|', paste(node$Parents, collapse = ':'))
  }
  if(type == 'bnlearn') {
    retval <- paste0('[', retval, ']')
  }
  retval
}

mapping_bnlearn_network <- function(network) {
  sapply(network, mapping_node) %>% paste(collapse = '')
}

mapping_Hydenet_network <- function(network) {
  strng <- sapply(network, mapping_node, type = 'Hydenet') %>% paste(collapse = '+')
  strng <- paste('~', strng)  %>% formula
  gsub(":", "*", strng, fixed = T) %>% formula
}

### parametrising: adding states to a node
add_state_to_node <- function(network, node_ID, new_state) {
  if(node_ID %in% names(network)) {
    if(! (new_state %in% network[[node_ID]]$States)) {
      ## invalidate CPT of node and of all its children
      network[[node_ID]]$CPT <- NULL
      for(child_ID in network[[node_ID]]$Children) {
        network[[child_ID]]$CPT <- NULL
      }
      network[[node_ID]]$States <- union(new_state, network[[node_ID]]$States)
    }
  }
  network
}

### Removing a state from a node
remove_state_from_node <- function(network, node_ID, old_state) {
  if(node_ID %in% names(network)) {
    if(old_state %in% network[[node_ID]]$States) {
      ##invalidate CPT of node and of its children
      network[[node_ID]]$CPT <- NULL
      for(child_ID in network[[node_ID]]$Children) {
        network[[child_ID]]$CPT <-NULL
      }
      network[[node_ID]]$States <- setdiff(network[[node_ID]]$States, old_state)
    }
  }
  network
}

### clearing all states from a node selected
### Removing a state from a node
clear_all_state_from_node <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$CPT <- NULL
    network[[node_ID]]$States <- NULL
    for(child_ID in network[[node_ID]]$Children) {
      network[[child_ID]]$CPT <- NULL
    }
  }
  network
}

###get all tghe states in a node
get_node_states <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$States
  } else {
    NULL
  }
}

### adding states to a node
add_states_to_node <- function(network, node_ID, states) {
  if (node_ID %in% names(network)) {
    network[[node_ID]]$States <- states
  }
  network
}

### calculate CPT structure for the node
calc_CPT_structure_for_node <- function(network, node_ID) {
  CPT <- c()
  if(node_ID %in% names(network)) {
    node_n_states <- network[[node_ID]]$States %>% length
    if(network[[node_ID]]$IS_ROOT) {
      dimnams <- structure(list(network[[node_ID]]$States), .Names = node_ID)
      CPT     <- array(1/node_n_states, dim = node_n_states, dimnames = dimnams)
    } else {
      parent_states <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$States)
      parent_n_states <- parent_states %>% sapply(length)
      dimnams <- structure(list(network[[node_ID]]$States), .Names = node_ID)
      dimnams <- c(dimnams, structure(parent_states, .Names = network[[node_ID]]$Parents))
      CPT <- array(1/node_n_states, dim = c(node_n_states, parent_n_states), dimnames = dimnams)
    }
  }
  CPT
}

### Return the list of nodes whose CPT is not yet defined
node_without_CPT <- function(network) {
  remove <- c('Exposure', 'Occurence', 'Impact')
  non_CPT_node <- c()
  for(id in names(network) %>% setdiff(remove)) {if(is.null(network[[id]]$CPT)) {non_CPT_node <- c(non_CPT_node, id)}}
  return(non_CPT_node)
}

### Define deterministic nodes other than XOI and calculates its states and probs automatically
define_determ_node <- function(network, node_ID, formula) {
  if(node_ID %in% names(network)) {
    if(is.null(network[[node_ID]]$States)) {
      if(is.null(network[[node_ID]]$CPT)) {
        if(! is.null(network[[node_ID]]$Parents)) {
          parent_states <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$States %>% as.double)
          dimnams <- structure(parent_states, .Names = network[[node_ID]]$Parents) 
          check <- anyNA(dimnams %>% unlist)
          if(!check) {
            network[[node_ID]]$States <- dimnams %>% as.data.frame %>% mutate( states = eval(parse(text=formula))) %>% select(states) %>% unlist %>% as.character
            CPT <- calc_CPT_structure_for_node(network, node_ID) %>% transform_CPT
            
           
          }
        }
      }
    }
  }
}

### parametrising: adding CPT to a node
add_CPT_to_node <- function(network, node_ID, CPT) {
  perform_add <- FALSE
  if(node_ID %in% names(network)) {
    node_n_states <- network[[node_ID]]$States %>% length
    ##check coherence of CPT array dimension
    if(network[[node_ID]]$IS_ROOT) {
      if(length(CPT) == node_n_states) {
        perform_add <- TRUE
        dimnams <- structure(list(network[[node_ID]]$States), .Names = node_ID)
        CPT <- array(CPT, dim = node_n_states, dimnames = dimnams)
      }
    } else {
      parent_states <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$States)
      parent_n_states <- parent_states %>% sapply(length)
      if(length(CPT) == node_n_states * prod(parent_states %>% sapply(length))) {
        perform_add <- TRUE
        dimnams <- structure(list(network[[node_ID]]$States), .Names = node_ID)
        dimnams <- c(dimnams, structure(parent_states, .Names = network[[node_ID]]$Parents))
        CPT <- array(CPT, dim = c(node_n_states, parent_n_states), dimnames = dimnams)
      }
    }
    if(perform_add) {
      network[[node_ID]]$CPT <- CPT
    }
  }
  network
}

##Update CPT array dataframe to the network
update_cpt_to_network <- function(network, node_ID, CPT_array) {
  if(node_ID %in% names(network)) {      
    CPT <- network[[node_ID]]$CPT
  if(is.null(CPT)){
    CPT <- calc_CPT_structure_for_node(network, node_ID)
  }
    dims <- dim(CPT)
    n_dims <- length(dims)
    if(n_dims == 1 | n_dims == 2){
      transformed_cpt <- tail.matrix(CPT_array, length(network[[node_ID]]$States) )
    }
    if(n_dims >= 3){
      transformed_cpt <- tail.matrix(CPT_array[-1], -length(network[[node_ID]]$States) ) %>% as.array %>% unlist %>% as.double %>% c
    }
    #network[[node_ID]]$CPT[1:prod(dims)] <- transformed_cpt
    network <- add_CPT_to_node(network, node_ID,transformed_cpt )
  }
  return(network)
}

## Normalizing probabilities
normalize_prob <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    if(! (is.null(network[[node_ID]]))) {
      if(! (is.null(network[[node_ID]]$CPT))) {
        if(network[[node_ID]]$CPT %>% as.data.frame %>% colSums %>% sum != prod(dim(network[[node_ID]]$CPT)[-1]) ) {
          dims <- dim(network[[node_ID]]$CPT)
          if(length(dims) == 1) {
            network[[node_ID]]$CPT <- network[[node_ID]]$CPT / network[[node_ID]]$CPT %>% sum
          } else if(length(dims) == 2) {
            network[[node_ID]]$CPT <- (network[[node_ID]]$CPT %>% t / network[[node_ID]]$CPT %>% as.data.frame %>% colSums) %>% t
          }
          else if(length(dims) >= 3) {
            network[[node_ID]]$CPT[1:prod(dim(network[[node_ID]]$CPT))] <- (network[[node_ID]]$CPT %>% as.data.frame %>% t / 
                                                                              network[[node_ID]]$CPT %>% as.data.frame %>% colSums) %>% t
          }
        }
      }
    }
  }
  network
}

## Transform the CPT for greater than or equal to 3-D array to presentable dataframe
transform_CPT <- function(CPT) {
  dims <- dim(CPT)
  n_dims <- length(dims)
  if (n_dims == 1) {
    df <- CPT %>% as.matrix
  } else if (n_dims == 2) {
    df <- CPT 
  } else if (n_dims >=3) {
    child_states <- dimnames(CPT)[[1]]
    child_name <- names(dimnames(CPT)[1])
    for(i in 2: n_dims) { assign(paste0('parent_states_',i-1), dimnames(CPT)[[i]])}
    for(i in 2: n_dims) { assign(paste0('parent_name_',i-1), names(dimnames(CPT)[i]))}
    matrix_dim <- c(dims[1]+n_dims-1, prod(dims[-1])+1)
    df <- data.frame(matrix(NA, nrow = matrix_dim[1], ncol = matrix_dim[2]))
    if (n_dims  == 3) {
      df[,1] <- c(parent_name_2,parent_name_1,child_states)
      df[2,-1] <- rep(parent_states_1, time = prod(dims[-1])/dims[2])
      df[1,-1] <- rep(parent_states_2, each = prod(dims[-1])/dims[3]) 
    } else if (n_dims == 4) { 
      df[,1] <- c(parent_name_3,parent_name_2,parent_name_1,child_states)
      df[3,-1] <- rep(parent_states_1, time = prod(dims[-1])/dims[2])
      df[2,-1] <- rep(parent_states_2, each = prod(dims[-1])/dims[3]/dims[4]) %>% rep(time =dims[4] )
      df[1,-1] <- rep(parent_states_3, each = prod(dims[-1])/dims[4])
    } else if (n_dims == 5) {
      df[,1] <- c(parent_name_4,parent_name_3,parent_name_2,parent_name_1,child_states)
      df[4,-1] <- rep(parent_states_1, time = prod(dims[-1])/dims[2])
      df[3,-1] <- rep(parent_states_2, each = prod(dims[-1])/dims[3]/dims[4]) %>% rep(time = dims[4]/dims[5]) %>% rep(time = dims[5]) 
      df[2,-1] <- rep(parent_states_3, each = prod(dims[-1])/dims[4]/dims[5]) %>% rep(time = dims[5])
      df[1,-1] <- rep(parent_states_4, each = prod(dims[-1])/dims[5])
    } else if (n_dims == 6) {
      df[,1] <- c(parent_name_5,parent_name_4,parent_name_3,parent_name_2,parent_name_1,child_states)
      df[5,-1] <- rep(parent_states_1, time = prod(dims[-1])/dims[2])
      df[4,-1] <- rep(parent_states_2, each = prod(dims[-1])/dims[3]/dims[4]) %>% rep(time = dims[4]/dims[5]) %>% rep(time = dims[5]/dims[6]) %>% rep(time = dims[6])
      df[3,-1] <- rep(parent_states_3, each = prod(dims[-1])/dims[4]/dims[5]) %>% rep(time = dims[5]/dims[6]) %>% rep(time = dims[6])
      df[2,-1] <- rep(parent_states_4, each = prod(dims[-1])/dims[5]/dims[6]) %>% rep(time = dims[6])
      df[1,-1] <- rep(parent_states_5, each = prod(dims[-1])/dims[6])
    }
    
    df[is.na(df)] <- CPT %>% as.matrix
    row.names(df)<- colnames(df) <- NULL
  }
  #formattable(df, list(area(col = 2:matrix_dim[2]) ~ color_tile("transparent", "pink")))
  # datatable(df) %>% formatStyle(
  #   2:matrix_dim[2],
  #   #target = 'row',
  #   backgroundColor = styleInterval(c(0, 1), c('white', 'yellow','white'))
  #)
  # datatable(df, rownames = FALSE) %>%
  #   formatStyle(2:matrix_dim[2],
  #     background = styleEqual(c(4, 5), c(rep("lightblue", 2))))
  return(df)
}

dt_output = function(id) {
  #fluidRow(column(
   # 12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
     DTOutput(id)
 # ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

get_formula_node <- function(network) {
  formula_node_ID <- c()
  for(id in names(network)) { if(network[[id]]$IS_ROOT == FALSE) { formula_node_ID <- c(formula_node_ID, id)} }
  remove <- c('Exposure', 'Occurence', 'Impact')
  formula_node_ID <- setdiff(formula_node_ID, remove)
  return(formula_node_ID)
}

check_formula <- function(formula, network, node_ID) {
  if(node_ID %in% names(network)) {
    check <- strapplyc(gsub(" ", "", format(formula), fixed = T), "-?|[a-zA-Z_]+", simplify = T, ignore.case = T) %>%
      stri_remove_empty %in% network[[node_ID]]$Parents %>% unique
    if(length(check) == 1) {
      if(check == TRUE) {cat('OK')} else {cat('Check formula of node:', node_ID)}
    } else if(length(check) == 2) {cat('Check formula of node', node_ID)}
  }
}

net_transform <- function(network) {
  network_sim <- network  ## copy network
  if( c('Exposure', 'Occurence', 'Impact')  %in%  names(network_sim) %>% unique == TRUE) {
    parents_used <- c(network_sim$Exposure$Parents, network_sim$Occurence$Parents, network_sim$Impact$Parents)
    parent_occurence <- network_sim$Occurence$Parents
    network_sim[["Impact"]] <- network_sim[["Exposure"]] <- network_sim[["Occurence"]] <- NULL
  }
  return(network_sim)
}

run_simulation <- function(network, n_sims){
  parents_used <- c(network$Exposure$Parents, network$Occurence$Parents, network$Impact$Parents)
  parent_occurence <- network$Occurence$Parents
  network_sim <- network %>% net_transform
  bnlearn_net <- network_sim %>% mapping_bnlearn_network %>% model2network
  bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network_sim %>% lapply(function(v) v$CPT))
  sim <- rbn(bnlearn_net_fit,n_sims)
  sim[parent_occurence] <- ifelse(toupper(sim[,parent_occurence]) == TRUE, 1, 0)  
  sim[,parents_used] <- lapply(sim[,parents_used], function(x) as.double(as.vector(x)))
  return(sim)
}

plot_marinal <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    if(! is.null(network[[node_ID]]$CPT)) {
      if(is.null(network[[node_ID]]$Parents)) {
        plot <- barplot( network[[node_ID]]$CPT %>% as.double, names.arg = network[[node_ID]]$States, xlab = 'Probabilities', ylab = node_ID, 
                         main = 'Marginal Probabilities',col = 'darkolivegreen3',cex.axis=1, cex.names=1, cex.lab = 1.5, border = "red",
                         horiz = T, xlim = c(0, max(network[[node_ID]]$CPT)+0.2))
        text(y=plot, x=network[[node_ID]]$CPT %>% as.double, signif(network[[node_ID]]$CPT %>% as.double,4) ,pos=2,labels=network[[node_ID]]$CPT %>% as.double %>% percent(accuracy = 0.01))
      } else {
        state <- network[[node_ID]]$States
        prob <- calc_marinal_prob(network, node_ID)
        plot <- barplot( prob, names.arg = state, xlab = 'Probabilities', ylab = node_ID, main = 'Marginal Probabilities',
                         col = 'darkolivegreen3',cex.axis=1, cex.names=1, cex.lab = 1.5, border = "red", horiz = T, xlim = c(0, max(prob)+0.2))
        text(y=plot, x=prob, pos=4,labels=prob %>% percent(accuracy = 0.01))
      }
    }
  }
}

calc_marinal_prob <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    if(! is.null(network[[node_ID]]$CPT)) {
      dims <- dim(network[[node_ID]]$CPT)
      n_dims = length(dims)
      if(n_dims == 1 ) {
        return( network[[node_ID]]$CPT %>% as.double)
      } else {
        for(i in 1: length(network[[node_ID]]$Parents)) { assign(paste0('parent',i), network[[node_ID]]$Parents[i]) } 
        if(n_dims == 2) {
          prob <- data.frame(mapply(`*`,network[[node_ID]]$CPT %>% as.data.frame,
                                    rep(calc_marinal_prob(network, parent1),times=prod(dims[-1])/dims[2]) ,SIMPLIFY=FALSE)) %>% rowSums
        } else if(n_dims == 3){
          prob <- data.frame(mapply(`*`,network[[node_ID]]$CPT %>% as.data.frame,
                                    (rep(calc_marinal_prob(network, parent1),times=prod(dims[-1])/dims[2]) *rep(calc_marinal_prob(network, parent2), each=prod(dims[-1])/dims[3])) ),SIMPLIFY=FALSE) %>% rowSums
        } else if(n_dims == 4){
          prob <- data.frame(mapply(`*`,network[[node_ID]]$CPT %>% as.data.frame,
                                    (rep(calc_marinal_prob(network, parent1), times=prod(dims[-1])/dims[2]) *rep(calc_marinal_prob(network, parent2), each=prod(dims[-1])/dims[3]) *rep(calc_marinal_prob(network, parent3),each=prod(dims[-1])/dims[4])),SIMPLIFY=FALSE)) %>% rowSums
        } else if(n_dims == 5){
          prob <- data.frame(mapply(`*`,network[[node_ID]]$CPT %>% as.data.frame,
                                    (rep(calc_marinal_prob(network, parent1), times=prod(dims[-1])/dims[2]) *rep(calc_marinal_prob(network, parent2), each=prod(dims[-1])/dims[3]) *rep(calc_marinal_prob(network, parent3),each=prod(dims[-1])/dims[4])*rep(calc_marinal_prob(network, parent4),each=prod(dims[-1])/dims[5])),SIMPLIFY=FALSE)) %>% rowSums
        } else if(n_dims == 6){
          prob <- data.frame(mapply(`*`,network[[node_ID]]$CPT %>% as.data.frame,
                                    (rep(calc_marinal_prob(network, parent1), times=prod(dims[-1])/dims[2]) *rep(calc_marinal_prob(network, parent2), each=prod(dims[-1])/dims[3]) *rep(calc_marinal_prob(network, parent3),each=prod(dims[-1])/dims[4])*rep(calc_marinal_prob(network, parent4),each=prod(dims[-1])/dims[5])*rep(calc_marinal_prob(network, parent5),each=prod(dims[-1])/dims[6])),SIMPLIFY=FALSE)) %>% rowSums
        }
      }
    }
  }
  return(prob)
}

####

modFunction <- function(input, output, session, data,reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match(c("ratio","cost","updated_price"), names(v$data))) {
        print(match(c("ratio","cost", "updated_price"), names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
        if (j %in% match("cost", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
        if (j %in% match("ratio", names(v$data))) {
          v$data$updated_price <<- v$data$cost * v$data$ratio
        }
      } else {
        stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE)
    
  })
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

####

# network %>% mapping_bnlearn_network
# bnlearn_net <- network %>% mapping_bnlearn_network %>% model2network
# 
# bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network %>% lapply(function(v) v$CPT))
# sim <- rbn(bnlearn_net_fit,1000)
# sim$Impact <- with(sim, Amount  +  (Income  + Intensity ) *1000 )
# sim$Exposure <- with(sim, Contractors + Employees )
# sim$Occurence <- with(sim, Fraud ) 
# sim$Occurence <- with(sim, ifelse(toupper(Fraud) == "TRUE", 1, 0)  )
# sim$Loss <- Impact * Exposure * Occurence
# hist(quantile(sim["Loss"], probs = c(0.90, 0.95, 0.975, 0.99, 0.995, 0.999, 0.9999, 0.999999)))
# str_replace_all(formulaI,network[["Impact"]]$Parents, paste(network[["Impact"]]$Parents, paste('%>% as.character %>% as.integer')) )
# graphviz.chart(bnlearn_net_fit, type = 'barprob', layout = 'dot')
# bn.fit.barchart(bnlearn_net_fit[["Fraud"]],panel=function(x,y,subscripts,...){
#   panel.grid(h=-1,v=0) 
#   panel.barchart(...)
#   panel.text(bnlearn_net_fit[["Fraud"]]$prob,bnlearn_net_fit[["Fraud"]]$prob, labels=t$y, pos=3)
# })
