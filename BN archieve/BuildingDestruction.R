network <- list()
network <- insert_node(network,'NbBuildings',invalidate_CPT = FALSE)
network <- insert_node(network,'ClimateScenario',invalidate_CPT = FALSE)
network <- insert_node(network,'Location',invalidate_CPT = FALSE)
network <- insert_node(network,'Disaster')
network <- insert_node(network,'EventIntensity')
network <- insert_node(network,'BuildingValue')
network <- insert_node(network,'DamageRate')
network <- insert_node(network,'PropertyDamage')
network <- insert_node(network,'AdditionalCost')
network <- insert_node(network,'DailyRevenue')
network <- insert_node(network,'NonAuto',invalidate_CPT = FALSE)
network <- insert_node(network,'SlowDown',invalidate_CPT = FALSE)
network <- insert_node(network,'SwitichingTime',invalidate_CPT = FALSE)
network <- insert_node(network,'RevenueLoss')
network <- insert_node(network,'Exposure',invalidate_CPT = FALSE)
network <- insert_node(network,'Occurence',invalidate_CPT = FALSE)
network <- insert_node(network,'Impact',invalidate_CPT = FALSE)

network <- add_parent_child_rel(network, 'ClimateScenario','Disaster')
network <- add_parent_child_rel(network, 'ClimateScenario','EventIntensity')
network <- add_parent_child_rel(network, 'Location','Disaster')
network <- add_parent_child_rel(network, 'Location','BuildingValue')
network <- add_parent_child_rel(network, 'Location','DailyRevenue')
network <- add_parent_child_rel(network, 'EventIntensity','DamageRate')
network <- add_parent_child_rel(network, 'EventIntensity','AdditionalCost')
network <- add_parent_child_rel(network, 'DamageRate','PropertyDamage')
network <- add_parent_child_rel(network, 'BuildingValue','PropertyDamage')
network <- add_parent_child_rel(network, 'NonAuto','RevenueLoss')
network <- add_parent_child_rel(network, 'DailyRevenue','RevenueLoss')
network <- add_parent_child_rel(network, 'SlowDown','RevenueLoss')
network <- add_parent_child_rel(network, 'SwitichingTime','RevenueLoss')
network <- add_parent_child_rel(network, 'NbBuildings','Exposure')
network <- add_parent_child_rel(network, 'Disaster','Occurence')
network <- add_parent_child_rel(network, 'PropertyDamage','Impact')
network <- add_parent_child_rel(network, 'AdditionalCost','Impact')
network <- add_parent_child_rel(network, 'RevenueLoss','Impact')

network <- add_state_to_node(network,'NbBuildings','10')
network <- add_state_to_node(network,'ClimateScenario','1.5C')
network <- add_state_to_node(network,'ClimateScenario','2C')
network <- add_state_to_node(network,'ClimateScenario','3C')
network <- add_state_to_node(network,'ClimateScenario','4C')
network <- add_state_to_node(network,'Location','NAM')
network <- add_state_to_node(network,'Location','EMEA')
network <- add_state_to_node(network,'Location','APAC')
network <- add_state_to_node(network,'Disaster','TRUE')
network <- add_state_to_node(network,'Disaster','FALSE')
network <- add_state_to_node(network,'EventIntensity','Medium')
network <- add_state_to_node(network,'EventIntensity','Catastrophic')
network <- add_state_to_node(network,'BuildingValue','100000')
network <- add_state_to_node(network,'BuildingValue','200000')
network <- add_state_to_node(network,'BuildingValue','500000')
network <- add_state_to_node(network,'DamageRate','0.05')
network <- add_state_to_node(network,'DamageRate','1')
network <- add_state_to_node(network,'DailyRevenue','10000')
network <- add_state_to_node(network,'DailyRevenue','20000')
network <- add_state_to_node(network,'DailyRevenue','30000')
network <- add_state_to_node(network,'DailyRevenue','40000')
network <- add_state_to_node(network,'NonAuto','0.1')
network <- add_state_to_node(network,'SlowDown','1')
network <- add_state_to_node(network,'SwitichingTime','20')
# network <- add_state_to_node(network,'PropertyDamage','10000')
# network <- add_state_to_node(network,'PropertyDamage','20000')
# network <- add_state_to_node(network,'PropertyDamage','50000')
# network <- add_state_to_node(network,'PropertyDamage','5000')
# network <- add_state_to_node(network,'PropertyDamage','25000')
# network <- add_state_to_node(network,'PropertyDamage','30000')
network <- add_state_to_node(network,'AdditionalCost','1000')
network <- add_state_to_node(network,'AdditionalCost','2000')
network <- add_state_to_node(network,'AdditionalCost','5000')
network <- add_state_to_node(network,'AdditionalCost','6000')
network <- add_state_to_node(network,'AdditionalCost','7000')
# network <- add_state_to_node(network,'RevenueLoss','1000')
# network <- add_state_to_node(network,'RevenueLoss','2000')
# network <- add_state_to_node(network,'RevenueLoss','3000')
# network <- add_state_to_node(network,'RevenueLoss','4000')

network <- add_CPT_to_node(network,'NbBuildings',c(1))
network <- add_CPT_to_node(network,'ClimateScenario',c(0.1,0.2,0.3,0.4))
network <- add_CPT_to_node(network,'Location',c(0.3,0.4,0.3))
network <- add_CPT_to_node(network,'EventIntensity',c(0.05,0.95,0.03,0.97,0.02,0.98,0.02,0.98))
network <- add_CPT_to_node(network,'Disaster',c(0.97,0.03,0.96,0.04,0.98,0.02,0.99,0.01,0.99,0.01,0.97,0.03,0.98,0.02,0.97,0.03,
                                                0.96,0.04,0.99,0.01,0.98,0.02,0.97,0.03))
network <- add_CPT_to_node(network,'BuildingValue',c(0,0,1,0,1,0,1,0,0))
network <- add_CPT_to_node(network,'DamageRate',c(1,0,0,1))
network <- add_CPT_to_node(network,'DailyRevenue',c(0,0.25,0.5,0.25,0,0.25,0.25,0.5,0.2,0.2,0.3,0.3))
network <- add_CPT_to_node(network,'NonAuto',c(1))
network <- add_CPT_to_node(network,'SlowDown',c(1))
network <- add_CPT_to_node(network,'SwitichingTime',c(1))
#network <- add_CPT_to_node(network,'PropertyDamage',c(0.1,0.2,0.1,0.1,0.2,0.3, 0.2,0.2,0.2,0.1,0.1,0.2, 0.3,0.3,0.1,0.1,0.1,0.1,
#                                                      0.1,0.2,0.1,0.1,0.2,0.3, 0.2,0.2,0.2,0.1,0.1,0.2, 0.3,0.3,0.1,0.1,0.1,0.1))
network <- add_CPT_to_node(network,'AdditionalCost',c(0.3,0.3,0.1,0.2,0.1,0.2,0.2,0.2,0.2,0.2))
#network <- add_CPT_to_node(network,'RevenueLoss',c(0,0.25,0.5,0.25, 0,0.25,0.25,0.5 ,0.2,0.2,0.3,0.3, 0.2,0.2,0.3,0.3))

network$NbBuildings$Type <- 'Numeric'
network$NbBuildings$Is_determ <- 'No'
network$ClimateScenario$Type <- 'Level'
network$ClimateScenario$Is_determ <- 'No'
network$Location$Type <- 'Level'
network$Location$Is_determ <- 'No'
network$Disaster$Type <- 'Boolean'
network$Disaster$Is_determ <- 'No'
network$EventIntensity$Type <- 'Level'
network$EventIntensity$Is_determ <- 'No'
network$BuildingValue$Type <- 'Numeric'
network$BuildingValue$Is_determ <- 'No'
network$DamageRate$Type <- 'Numeric'
network$DamageRate$Is_determ <- 'No'
network$PropertyDamage$Type <- 'Numeric'
network$PropertyDamage$Is_determ <- 'Yes'
network$PropertyDamage$formula <- 'DamageRate*BuildingValue'
network$AdditionalCost$Type <- 'Numeric'
network$AdditionalCost$Is_determ <- 'No'
network$DailyRevenue$Type <- 'Numeric'
network$DailyRevenue$Is_determ <- 'No'
network$NonAuto$Type <- 'Numeric'
network$NonAuto$Is_determ <- 'No'
network$SlowDown$Type <- 'Numeric'
network$SlowDown$Is_determ <- 'No'
network$SwitichingTime$Type <- 'Numeric'
network$SwitichingTime$Is_determ <- 'No'
network$RevenueLoss$Type <- 'Numeric'
network$RevenueLoss$Is_determ <- 'Yes'
network$RevenueLoss$formula <- 'SwitichingTime*SlowDown*NonAuto*DailyRevenue'

network <- define_determ_node(network,'PropertyDamage',network$PropertyDamage$formula)
network <- define_determ_node(network,'RevenueLoss',network$RevenueLoss$formula)

save(network,model_name, file = 'BuildingDestruction.RData')
