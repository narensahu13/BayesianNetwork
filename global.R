
######################################## Define Login Page functtion ########################################
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("copyright @2020."),
                     #tags$code("Username: myuser  Password: mypass"),
                     br(),
                     #tags$code("Username: myuser1  Password: mypass1")
                     tags$code("Unauthorized sharing of this application is illegal.")
                   ))
)

##################################### Define Credentials ##########################################################
credentials = data.frame(
  username_id = c("myuser", "bnuser"),
  passod   = sapply(c("mypass", "bnpass"),password_store),
  permission  = c("advanced", "advanced"), 
  stringsAsFactors = F
)

### global functions to create ,perform checks and do plots ###########################################################

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
      if(new_state != '') {
        ## invalidate CPT of node and of all its children
        network[[node_ID]]$CPT <- NULL
        for(child_ID in network[[node_ID]]$Children) {
          network[[child_ID]]$CPT <- NULL
        }
        network[[node_ID]]$States <- union(new_state, network[[node_ID]]$States)
      }
    }
  }
  network
}

### adding node-type (numeric,bool,character) and if node is deterministic
add_node_type <- function(network, node_ID, type, is_determ) {
  if(node_ID %in% names(network)) {
    if(is.null(network[[node_ID]]$Type)) {
      network[[node_ID]]$Type <- type
    }
    if(is.null(network[[node_ID]]$Is_determ)) {
      network[[node_ID]]$Is_determ <- is_determ
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
    network[[node_ID]]$Type <- NULL
    network[[node_ID]]$Is_determ <- NULL
    network[[node_ID]]$formula <- NULL
    for(child_ID in network[[node_ID]]$Children) {
      network[[child_ID]]$CPT <- NULL
    }
  }
  network
}

### Plot network using hydenet 
plot_hydenet <- function(net) {
  if(any(c('Exposure', 'Occurence','Impact') %in% net[['nodes']])) {
    net <- setUtilityNodes(net,'Exposure')
    net <- setUtilityNodes(net,'Occurence')
    net <- setUtilityNodes(net,'Impact')
  }
  return(plot(net))
  
}

###get all tghe states in a node
get_node_states <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$States
  } else {
    NULL
  }
}

## Not-in function
'%!in%' <- function(x,y)!('%in%'(x,y))

### adding states to a node
add_states_to_node <- function(network, node_ID, states) {
  if (node_ID %in% names(network)) {
    #if (network[[node_ID]]$Type != "Boolean") {
    if(states %!in% network[[node_ID]]$States) {
      if(states != '') {
        network[[node_ID]]$States <- states
      }# else if(is.null(states) && )
    }
    #} else if(network[[node_ID]]$Type == 'Boolean') {
    #  network[[node_ID]]$States <- NULL
    #  network[[node_ID]]$States <- c('True', 'False')
    # }
  }
  network
}

### Define states of boolean node automatically (True, False) ## NOT included
add_boolean_state <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$States <- NULL
    network[[node_ID]]$States <- c('True', 'False')
  }
}

### Check for defined state correctness in each node. The states should be defined according to the node_type i.e.
### node_type == 'Numeric -> the states defined should be only numeric
### node_type == 'Boolean' -> states defined should be only c('True','False')
### node_type == 'Level' -> states defined can be numeral/character
check_state <- function(network) {
  remove <- c('Exposure', 'Occurence', 'Impact')
  for(node_ID in setdiff(names(network),remove)) {
    node_type <- network[[node_ID]]$Type
    if(is.null(node_type)) {
      return(paste("Please define node type of nodeID:", node_ID))
    }
    if(node_type == 'Numeric') {
      check <- network[[node_ID]]$States %>% as.vector %>% as.double %>% is.na %>% unique
      if(length(check)==1) {
        if(check == TRUE) {
          return(paste("Numeric node contains non-numeric states, check NodeID",node_ID))
        }
      } else {
        return(paste("Numeric node contains non-numeric states, check NodeID",node_ID))
      }
    } else if(node_type == 'Boolean') {
      check <- network[[node_ID]]$States %>% toupper %!in% c('TRUE','FALSE') %>% unique
      if(length(check)==1) {
        if(check == TRUE) {
          return(paste("Boolean node contains undefined states, check Node ID:", node_ID))
        }
      } else {
        return(paste("Boolean node contains undefined states, check Node ID:", node_ID))
      }
    }
  }
}

### Clear state and CPT if check_state gives error 
clear_after_check <- function(network, node_ID, type ) {
  if(type == 'Numeric') {
    network[[node_ID]]$States <- NULL
    network[[node_ID]]$CPT <- NULL
  }
  if(type == 'Boolean') {
    network[[node_ID]]$States <- c("True", "False")
    network[[node_ID]]$CPT <- NULL
    
  }
  for(child_ID in network[[node_ID]]$Children){
    network[[child_ID]]$States <- NULL
    network[[child_ID]]$CPT <- NULL
  }
  return(network)
}

### update network according to check_state function and return updated network
update_check_state <- function(network) {
  remove <- c('Exposure', 'Occurence', 'Impact')
  for(node_ID in setdiff(names(network),remove)) {
    node_type <- network[[node_ID]]$Type
    if(!(is.null(node_type))) {
      if(node_type == 'Numeric') {
        check <- network[[node_ID]]$States %>% as.vector %>% as.double %>% is.na %>% unique
        if(length(check)==1) {
          if(check == TRUE) {
            network <- clear_after_check(network, node_ID, type = 'Numeric')
          }
        } else {
          network <- clear_after_check(network, node_ID, type = 'Numeric')
        }
      } else if(node_type == 'Boolean') {
        check <- network[[node_ID]]$States %>% toupper %!in% c("TRUE", "FALSE") %>% unique
        if(length(check)==1) {
          if(check == TRUE) {
            network <- clear_after_check(network,node_ID, type ='Boolean')
          }
        } else {
          network <- clear_after_check(network,node_ID, type ='Boolean')
        }
      }
    }
  }
  return(network)
}

### to be used: names(network) %>% lapply(function(node) is.null(network[[node_ID]]$States)) %>% unique
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

###Define states of deterministic nodes
# determ_node_state <- function(network, node_ID) {
#   
# }

### Define deterministic nodes other than XOI and calculates its states and probs automatically
define_determ_node <- function(network, node_ID, formula) {
  if(node_ID %in% names(network)) {
    if(! is.null(network[[node_ID]]$Parents)) {
      parents_type <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$Type) %>% unique
      if(length(parents_type)==1 ) {
        if(parents_type == 'Numeric'){
          network[[node_ID]]$States <- NULL
          network[[node_ID]]$CPT <- NULL
          parent_states <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$States %>% as.double)
          dimnams <- structure(parent_states, .Names = network[[node_ID]]$Parents) 
          check <- anyNA(dimnams %>% unlist)
          if(!check) {
            network[[node_ID]]$States <- dimnams %>% as.vector %>% expand.grid %>% as.data.frame %>% mutate( states = eval(parse(text=formula))) %>% 
              select(states) %>% as.vector %>% format(scientific=F) %>% unlist %>% unique %>% as.double %>% sort
            CPT <- calc_CPT_structure_for_node(network, node_ID) %>% transform_CPT
            #table <- dimnams %>% as.vector %>% expand.grid %>% mutate(states = eval(parse(text=formula)))
            n_parents <- network[[node_ID]]$Parents %>% length
            # CPT_copy<- CPT
            rownames(CPT) <- NULL
            if(ncol(CPT) ==1) {
              CPT <- CPT
            } else {
              for(i in 2: ncol(CPT)) {
                #table1 <- CPT[1:n_parents, 2:ncol(CPT)]
                for(j in 1:n_parents) { assign(rev(network[[node_ID]]$Parents)[j], CPT[j,i] %>% as.character %>% as.double)} %>% as.double(row.names=F)
                value <- eval(parse(text = formula))# %>% format(scientific=F) %>% as.double
                CPT[which(CPT[,1]==value),i] <- 1
              }
            }
            n_dims <- n_parents+1
            if(n_dims == 1 | n_dims == 2){
              CPT_array <- tail.matrix(CPT, length(network[[node_ID]]$States) )
            }
            if(n_dims >= 3){
              CPT_array <- tail.matrix(CPT[-1], length(network[[node_ID]]$States) ) %>% as.array %>% unlist %>% as.double %>% c
            }
            #CPT_array <- tail.matrix(CPT[-1], length(network[[node_ID]]$States) ) %>% as.array  %>% unlist %>% as.double %>% c 
            CPT_array[CPT_array != 1] <- 0
            network <- add_CPT_to_node(network, node_ID,CPT_array )
          }
        } else { network[[node_ID]]$CPT <- NULL }
      } else { network[[node_ID]]$CPT <- NULL }
    }
  }
  network
}

### Test determ node (if Parents of a deterministic nodes are changed, delete the states and CPT of the determ node)
test_determ_node <- function(network) {
  for(node_ID in names(network)) {
    if(!is.null(network[[node_ID]]$Is_determ)) {
      if(network[[node_ID]]$Is_determ == 'Yes') {
        parents_type <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$Type) %>% unique
        if(length(parents_type)==1 ) {
          if(parents_type != 'Numeric') {
            network[[node_ID]]$States <- NULL
            network[[node_ID]]$CPT <- NULL
            network[[node_ID]]$Type <- NULL
            network[[node_ID]]$formula <- NULL
            network[[node_ID]]$Is_determ <- NULL
            for(child_ID in network[[node_ID]]$Children) {
              network[[child_ID]]$States <- NULL
              network[[child_ID]]$CPT <-NULL
            }
          }
        } else if(length(parents_type) == 2) {
          network[[node_ID]]$States <- NULL
          network[[node_ID]]$CPT <- NULL
          network[[node_ID]]$Type <- NULL
          network[[node_ID]]$formula <- NULL
          network[[node_ID]]$Is_determ <- NULL
          for(child_ID in network[[node_ID]]$Children) {
            network[[child_ID]]$States <- NULL
            network[[child_ID]]$CPT <-NULL
          }
        }
      }
    }
  }
  network
}

### get node type
get_node_type <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$Type
  } else {
    NULL
  }
}

### get if determ
get_if_determ <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$Is_determ
  } else {
    NULL
  }
}

### get node formula
get_node_formula <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    network[[node_ID]]$formula
  } else {
    NULL
  }
}

### parametrising: adding CPT to a node
add_CPT_to_node <- function(network, node_ID, CPT) {
  perform_add <- FALSE
  if(node_ID %in% names(network)) {
    #if(network[[node_ID]]$Is_determ == 'No') {
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
    # }
  }
  network
}

##Update CPT array dataframe to the network
update_cpt_to_network <- function(network, node_ID, CPT_array) {
  if(node_ID %in% names(network)) {
    #if(network[[node_ID]]$Is_determ == 'No') {
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
      transformed_cpt <- tail.matrix(CPT_array[-1], length(network[[node_ID]]$States) ) %>% as.array %>% unlist %>% as.double %>% c
    }
    #network[[node_ID]]$CPT[1:prod(dims)] <- transformed_cpt
    network <- add_CPT_to_node(network, node_ID,transformed_cpt )
    #}
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


### Get all the nodes for which a deterministic formulae can be defined (NOT USED)
get_formula_node <- function(network) {
  formula_node_ID <- c()
  for(id in names(network)) { if(network[[id]]$IS_ROOT == FALSE) { formula_node_ID <- c(formula_node_ID, id)} }
  remove <- c('Exposure', 'Occurence', 'Impact')
  formula_node_ID <- setdiff(formula_node_ID, remove)
  return(formula_node_ID)
}

### Check formula of a deterministic node
check_formula <- function(formula, network, node_ID) {
  if(node_ID %in% names(network)) {
    parents_type <- network[[node_ID]]$Parents %>% lapply(function(nodenam) network[[nodenam]]$Type) %>% unique 
    # check <- strapply(gsub(" ", "", format(formula), fixed = T), "-?|[a-zA-Z_]+", simplify = T, ignore.case = T) %>%
    #  stri_remove_empty %in% network[[node_ID]]$Parents %>% unique
    res <- trimws(el(strsplit(formula, "\\+|\\-|\\*|\\/|\\,|\\pmin|\\pmax|\\log|\\^|\\(|\\)|\\(|\\)")))
    check <- res[is.na(suppressWarnings(as.numeric(res)))] %>% stri_remove_empty %in% network[[node_ID]]$Parents %>% unique
    if((length(check) == 1) & (length(parents_type)==1) ) {
      if((check == TRUE) & (parents_type == 'Numeric' | parents_type == 'Boolean')) {
        return('OK')
      } else if((check==FALSE)) {
        return(paste("Check formula of node", node_ID))
      } else {
        return(paste('Check node type of parents of node', node_ID))
      }
    } else if((length(check) == 2)) {
      return(paste("Check formula of node", node_ID))
    } else if((length(parents_type)==2) ) {
      return(paste('Check node type of parents of node', node_ID))
    } else if( (parents_type != 'Numeric')) {
      return(paste('Check node type of parents of node', node_ID))
    }
  }
}

### Transform the network before running the simulation so that network does not contain XOI nodes
net_transform <- function(network) {
  network_sim <- network  ## copy network
  if( c('Exposure', 'Occurence', 'Impact')  %in%  names(network_sim) %>% unique == TRUE) {
    #parent_occurence <- network_sim$Occurence$Parents
    #network_sim[[parent_occurence]]$States <- ifelse(toupper(network_sim[[parent_occurence]]$States) == TRUE, 1, 0)
    #rownames(network_sim[[parent_occurence]]$CPT) <- ifelse(toupper(rownames(network_sim[[parent_occurence]]$CPT)) == TRUE, 1, 0)
    parents_used <- c(network_sim$Exposure$Parents, network_sim$Occurence$Parents, network_sim$Impact$Parents)
   # for(node_ID in parents_used) {
   #   network_sim[[node_ID]]$States <- network_sim[[node_ID]]$States %>% as.vector %>% as.double
   # }
    network_sim[["Impact"]] <- network_sim[["Exposure"]] <- network_sim[["Occurence"]] <- NULL
  }
  return(network_sim)
}

net_transform_node <- function(network, node_ID) {
  network_sim <- network  ## copy network
  if( node_ID  %in%  names(network_sim) ) {
    parents_used <- network_sim[[node_ID]]$Parents
    for(nodenam in setdiff(names(network),parents_used)) {
    network_sim[[nodenam]] <- NULL
    }
  }
  return(network_sim)
}

RepParallel <- function(n, expr, simplify = "array",...) {
  answer <-
    mclapply(integer(n), eval.parent(substitute(function(...) expr)),...)
  if (!identical(simplify, FALSE) && length(answer)) 
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}

do_simulation <- function(network,bnlearn_net_fit,blocksize,formula_occurence,formula_impact) {
  parents_used <- c(network$Exposure$Parents, network$Occurence$Parents, network$Impact$Parents)
  parent_occurence <- network$Occurence$Parents
  sim <- rbn(bnlearn_net_fit, blocksize) #%>% mutate(Iter_vec)
  sim[,parent_occurence] <- ifelse(toupper(sim[,parent_occurence]) == TRUE, 1, 0)
  sim[,parents_used] <- lapply(sim[,parents_used], function(x) as.double(as.vector(x)))
  sim <- sim %>% mutate( Occurence =  eval(parse(text=formula_occurence)), Impact = eval(parse(text = formula_impact)) ) %>%   
    mutate(Loss_tot = Occurence * Impact) %>% transmute(Occurence, Loss_tot)
}

### Run the simulation and  return a dataframe containing the simulated values of the nodes 
run_simulation <- function(network, exposure_sim,n_sims,formula_occurence,formula_impact){
  network_sim <- network %>% net_transform
  bnlearn_net <- network_sim %>% mapping_bnlearn_network %>% model2network
  bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network_sim %>% lapply(function(v) v$CPT))
  ###
  Iter_vec <- rep(1:n_sims, exposure_sim)
  #Occ_vec <- sapply(exposure_sim, function(ee) 1:ee) %>% unlist %>% as.numeric
  tot_num_vec <- sum(exposure_sim)
  if(tot_num_vec > 5000000) {
    blocksize <- 1000000
    n_blocks <- (tot_num_vec/blocksize) %>% ceiling
  } else {
    blocksize <- tot_num_vec
    n_blocks <- 1
  }
  sim <- do.call("rbind", RepParallel(n_blocks, {
    do_simulation(network,bnlearn_net_fit,blocksize,formula_occurence,formula_impact)
  }, simplify = FALSE)) %>% as.data.frame
  sim <- sim[1:length(Iter_vec),] %>% mutate(Iter_vec)
  
  return(sim)
}

###Plot conditional graph ## modify bnlearn function
bn.fit.barchart <- function (fitted, xlab = "Probabilities", ylab = "Levels", main,  ...)  {
  if (is(fitted, "bn.fit")) 
    stop("only plots of single, discrete nodes are implemented.")
  if (!is(fitted, c("bn.fit.dnode", "bn.fit.onode"))) 
    stop("fitted must be an object of class 'bn.fit.dnode' or 'bn.fit.onode'.")
  if (missing(main)) 
    main = paste("Conditional Probabilities for Node", fitted$node)
  lattice.discrete.backend(fitted = fitted, type = "bar", xlab = xlab, 
                           ylab = ylab, main = main, ...)
  invisible(NULL)
}

## lattice.discrete.backend ## modify bnlearn plot

# lattice backend for plots aimed at gaussian bayesian networks.
lattice.discrete.backend = function(fitted, type, xlab, ylab, main, ...) {

  # check whether lattice is loaded, and try to load if it is not.
  #check.and.load.package("lattice")
  
  if (type == "bar") {
    
    if (length(fitted$parents) == 0) {
      
      p = lattice::barchart(fitted$prob, xlab = xlab, ylab = ylab, main = main,
                            panel = function(x, y, ...) {
                              lattice::panel.grid(h = 0, v = -1)
                              lattice::panel.barchart(x, y, ...)
                            })
      
    }#THEN
    else {
      ## set layout
      dims <-  fitted$prob %>% dim
      col <- dims[2]
      row <- (dims[-1] %>% prod)/col
      p = lattice::barchart(fitted$prob, groups = FALSE, as.table = TRUE, layout=c(col,row),
                            xlab = xlab, ylab = ylab, main = main,
                            panel = function(x, y, ...) {
                              lattice::panel.grid(h = 0, v = -1)
                              lattice::panel.barchart(x, y, ...)
                            })
      
    }#ELSE
    
  }#THEN
  else if (type == "dot") {
    
    if (length(fitted$parents) == 0) {
      
      p = lattice::dotplot(fitted$prob, xlab = xlab, ylab = ylab, main = main,
                           type = c("p", "h"),
                           panel = function(x, y, ...) {
                             lattice::panel.grid(h = 0, v = -1)
                             lattice::panel.dotplot(x, y, ...)
                           })
      
    }#THEN
    else {
      
      p = lattice::dotplot(fitted$prob, groups = FALSE, as.table = TRUE,
                           type = c("p", "h"), xlab = xlab, ylab = ylab, main = main,
                           panel = function(x, y, ...) {
                             lattice::panel.grid(h = 0, v = -1)
                             lattice::panel.dotplot(x, y, ...)
                           })
      
    }#ELSE
    
  }#THEN
  
  # print the plot explicitly, do not rely on auto-printing.
  print(p)
  
}#LATTICE.DISCRETE.BACKEND

### plot marginal probability graph of the selected node
plot_marinal <- function(network, node_ID) {
  if(node_ID %in% names(network)) {
    if(! is.null(network[[node_ID]]$CPT)) {
      if(is.null(network[[node_ID]]$Parents)) {
        plot <- barplot( network[[node_ID]]$CPT %>% as.double, names.arg = network[[node_ID]]$States, xlab = 'Probabilities', ylab = node_ID, 
                         main = paste('Marginal Probabilities for Node',node_ID),col = 'darkolivegreen3',cex.axis=1, cex.names=1, cex.lab = 1.5, border = "red",
                         horiz = T, xlim = c(0, max(network[[node_ID]]$CPT)+0.2))
        text(y=plot, x=network[[node_ID]]$CPT %>% as.double, signif(network[[node_ID]]$CPT %>% as.double,4) ,pos=2,labels=network[[node_ID]]$CPT %>% as.double %>% percent(accuracy = 0.01))
      } else {
        state <- network[[node_ID]]$States
        prob <- calc_marinal_prob(network, node_ID)
        plot <- barplot( prob, names.arg = state, xlab = 'Probabilities', ylab = node_ID, main = paste('Marginal Probabilities for Node',node_ID),
                         col = 'darkolivegreen3',cex.axis=0.75, cex.names=0.75, cex.lab = 1.5, border = "red", horiz = T, xlim = c(0, max(prob)+0.2))
        text(y=plot, x=prob, pos=4,labels=prob %>% percent(accuracy = 0.01))
      }
    }
  }
}

### calculate marginal probabilities of the node selected
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


### Help data for Structure tab
structureHelp <- data.frame(
  step = c(1,2,3,4,5),
  intro = c(
    "This is from where you can load few available template models. You can upload an existing model from Upload Model tab.",
    "Click this button to clear the loaded model and create/load another model. If you want to create a new model, it will ask for name
      of the new model.",
    "Clicking this button will download the model in .RData format to your desktop.",
    "To define the structure of the new model, add a parent node, a child node and click on this button. You can delete a relationship
      from delete button beside this.",
    "This is where you can see the strucure of the model."
  ),
  element = c("#dataInput", "#clear_model", "#save_model_to_file", "#add_child_parent", "#model_plot"),
  position = c("auto","auto","auto","auto","auto")
)

### Help data for State tab
stateHelp <- data.frame(
  step = c(1,2,3,4,5,6,7,8),
  intro = c(
    "Select a node and define states accordingly. The XOI nodes will not be available here.",
    "Choose type of the node. If Numeric -> the states should be numeric, if Level -> states should be character type or mix type (not 
    purely Numeric), if Boolean -> states should be True & False.",
    "If the states of a node derived through a formula containing it's parents, only then choose Yes. If selected Yes, a a textbox will
    appear to enter the formula for e.g. if a node C depends upon its parents A and B as A+B, then enter the formula as A+B. click on 
    Check button after that to evaluate correctness of the formula",
    "This appears only when the node is not deterministic. Enter the state and click Add State button. For deterministic node, this 
    place will be replaced by textbox for formula.",
    "To add the states, click this button. To delete a state, choose a node and a state and click Detete button.",
    "This button clears all the nodes from a node.",
    "This is where all the states of a node can be seen.",
    "This button validates all the nodes and the states of the node according to the node type. If the states do not match the node type,
    this may clear the states of the node automatically."
  ),
  element = c("#select_node_for_states", "#node_type", "#determ_node","#define_states", "#add_state", "#clear_state", "#node_state_tab",
              "#validate_state"),
  position = c("auto","auto","auto","auto","auto","auto","auto","auto")
)

### Help data for parameter tab
parameterHelp <- data.frame(
  step = c(1,2,3,4,5),
  intro = c(
    "Select a node for which conditional probabilities to be defined.",
    "This is conditional probability table for the chosen node. Adjust the probabilities accordingly.",
    "Clicking the button will updated the adjusted probabilities to the model.",
    "This is where Conditional probability plot of the selected node will appear.",
    "This is where Marginal probability plot of the selected node will appear."
  ),
  element = c("#select_node_for_probs", "#CPT", "#add_CPT_to_node", "#condplot", "#margplot"),
  position = c("auto","auto","auto","auto","auto")
)

### Help data for execution tab
reportHelp <- data.frame(
  step = c(1),
  intro = c(
    "Before launching simulation, first define the formulae of XOI nodes from Structure tab."
  ),
  element = c("#calculate"),
  position = c("auto")
)

##########  Below functions are NOT used

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

dt_output = function(id) {
  #fluidRow(column(
  # 12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
  DTOutput(id)
  # ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
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
library(htmlwidgets)

widgetThumbnail <- function(p, thumbName, width = 1024, height = 768) {
  phantom <- findPhantom()
  
  success <- FALSE
  if(phantom == "") {
    message("** phantomjs dependency could not be found - thumbnail cannot be generated (run phantomInstall() for details)")
  } else {
    res <- try({
      ff <- paste0(thumbName, ".html")
      ffjs <- paste0(thumbName, ".js")
      
      # don't want any padding
      p$sizingPolicy$padding <- 0
      suppressMessages(saveWidget(p, ff, selfcontained = FALSE))
      
      js <- paste0("var page = require('webpage').create();
                    page.viewportSize = { width: ", width,", height: ", height," };
                    page.clipRect = { top: 0, left: 0, width: ", width,", height: ", height," };
                    page.open('", ff, "', function(status) {
                    console.log(\"Status: \" + status);
                    if(status === \"success\") {
                    page.render('", thumbName, ".png');
                    }
                    phantom.exit();
                    });")
      cat(js, file = ffjs)
      system2(phantom, ffjs)
    })
    if(!inherits(res, "try-error")) {
      success <- TRUE
    }
    if(!file.exists(paste0(thumbName, ".png"))) {
      success <- FALSE
    }
  }
  
  if(!success) {
    message("** could not create htmlwidget thumbnail... creating an empty thumbnail...")
  }
}

#' Get instructions on how to install phantomjs
#' @export
phantomInstall <- function() {
  message("Please visit this page to install phantomjs on your system: http://phantomjs.org/download.html")
}

# similar to webshot
findPhantom <- function() {
  
  phantom <- Sys.which("phantomjs")
  
  if(Sys.which("phantomjs") == "") {
    if(identical(.Platform$OS.type, "windows")) {
      phantom <- Sys.which(file.path(Sys.getenv("APPDATA"), "npm", "phantomjs.cmd"))
    }
  }
  
  phantom
  
}
