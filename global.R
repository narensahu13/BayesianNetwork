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
    for(child in network[[node_Id]]$Children) {
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
  paste('~', strng) %>% formula
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
    if(network[[node_ID]]$I_ROOT) {
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

# network %>% mapping_bnlearn_network
# bnlearn_net <- network %>% mapping_bnlearn_network %>% model2network
# 
# bnlearn_net_fit <- custom.fit(bnlearn_net, dist = network %>% lapply(function(v) v$CPT))
# rbn(bnlearn_net_fit,10)
#

