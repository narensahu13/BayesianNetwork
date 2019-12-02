network <- list()
network <- insert_node(network,'Level',invalidate_CPT = FALSE)
network <- insert_node(network,'Employees',invalidate_CPT = FALSE)
network <- insert_node(network,'Contractors',invalidate_CPT = FALSE)
network <- insert_node(network,'Fraud')
network <- insert_node(network,'Income')
network <- insert_node(network,'Amount',invalidate_CPT = FALSE)
network <- insert_node(network,'Intensity',invalidate_CPT = FALSE)
network <- insert_node(network,'Exposure',invalidate_CPT = FALSE)
network <- insert_node(network,'Occurence',invalidate_CPT = FALSE)
network <- insert_node(network,'Impact',invalidate_CPT = FALSE)

network <- add_parent_child_rel(network, 'Level','Fraud')
network <- add_parent_child_rel(network, 'Level','Income')
network <- add_parent_child_rel(network, 'Employees','Exposure')
network <- add_parent_child_rel(network, 'Contractors','Exposure')
network <- add_parent_child_rel(network, 'Fraud','Occurence')
network <- add_parent_child_rel(network, 'Income','Impact')
network <- add_parent_child_rel(network, 'Amount','Impact')
network <- add_parent_child_rel(network, 'Intensity','Impact')


network <- add_state_to_node(network,'Employees','1000')
network <- add_state_to_node(network,'Employees','1100')
network <- add_state_to_node(network,'Employees','1200')
network <- add_state_to_node(network,'Contractors','100')
network <- add_state_to_node(network,'Level','Clerk')
network <- add_state_to_node(network,'Level','Manager')
network <- add_state_to_node(network,'Level','Executive')
network <- add_state_to_node(network,'Amount','1000')
network <- add_state_to_node(network,'Intensity','0.5')
network <- add_state_to_node(network,'Intensity','2.5')
network <- add_state_to_node(network,'Intensity','7.5')
network <- add_state_to_node(network,'Fraud','FALSE')
network <- add_state_to_node(network,'Fraud','TRUE')
network <- add_state_to_node(network,'Income','1')
network <- add_state_to_node(network,'Income','1.5')
network <- add_state_to_node(network,'Income','2.0')

# network <- add_state_to_node(network,'Impact','100')
# network <- add_state_to_node(network,'Impact','150')
# network <- add_state_to_node(network,'Occurence','FALSE')
# network <- add_state_to_node(network,'Occurence','TRUE')
# network <- add_state_to_node(network,'Exposure','20')
# network <- add_state_to_node(network,'Exposure','40')

network <- add_CPT_to_node(network,'Employees',c(0.25,0.5,0.25))
network <- add_CPT_to_node(network,'Contractors',c(1))
network <- add_CPT_to_node(network,'Level',c(0.25,0.5,0.25))
network <- add_CPT_to_node(network,'Intensity',c(0.05,0.15,0.8))
network <- add_CPT_to_node(network,'Amount',c(1))
network <- add_CPT_to_node(network,'Fraud',c(0.0015,0.9985,0.0010,0.999,5e-4,0.9995))
network <- add_CPT_to_node(network,'Income',c(1,0,0,0,1,0,0,0,1))
# network <- add_CPT_to_node(network,'Occurence',c(1,0,0,1))
# network <- add_CPT_to_node(network,'Exposure',c(0.8,0.2,0.5,0.5,0.6,0.4))
# network <- add_CPT_to_node(network,'Impact',c(0.2,0.8,0.1,0.9,0.15,0.85,0.2,0.8,0.3,0.7,0.4,0.6,0.5,0.5,0.4,0.6,0.1,0.9))

network$Level$Type <- 'Level'
network$Level$Is_determ <- 'No'
network$Contractors$Type <- 'Numeric'
network$Contractors$Is_determ <- 'No's
network$Employees$Type <- 'Numeric'
network$Employees$Is_determ <- 'No'
network$Intensity$Type <- 'Numeric'
network$Amount$Type <- 'Numeric'
network$Income$Type <- 'Numeric'
network$Intensity$Is_determ <- 'No'
network$Amount$Is_determ <- 'No'
network$Income$Is_determ <- 'No'
network$Fraud$Type <- 'Boolean'
network$Fraud$Is_determ <- 'No'

save(network,model_name, file = 'Fraudmodel.RData')
