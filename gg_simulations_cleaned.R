########## goatgrass simulation output analysis code ########
#########  by Rebecca Nelson #############
##### for Nelson, Dritz, Aigner and Valdovinos ###
## created 8-21-24 ###
## last updated 5-1-25 ###
######################################

rm(list = ls())


######## load required packages ########
library(tidyverse)
library(bipartite)


## Load data ###

## plant data ###
plants <-read.csv('plant_output_9_24_24.csv') 

## pollinator data
pollinators <-read.csv('pollinator_output_9_24_24.csv') 


## animal info
func <-read.csv('paul_func_groups.csv') 

nest <-read.csv('bee_nesting_habits.csv') 

LACA <-read.csv('LACA_poll_info.csv') 

#clean
pollinators$Scenario[pollinators$Scenario == "Goatgrass_removed"] <- "Goatgrass_Removed"

### Total Floral Abundance & Visits #########

plants %>% dplyr::select(Scenario, Output, SUM) %>% filter(Output == "plantsf") %>%  ggplot(aes(Scenario, SUM, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Floral Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plants$Output[plants$Output == "SVisitsP"] <- "sVisitsP"
plants %>% select(Scenario, Output, SUM) %>% filter(Output == "sVisitsP") %>%  ggplot(aes(Scenario, SUM, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


plants %>% select(Scenario, Output, SUM) %>% filter(Output == "sVisits_perP") %>%  ggplot(aes(Scenario, SUM, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Visits Per Plant") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


### Mean Floral Abundance & Visits ####
plants %>% select(Scenario, Output, Mean) %>% filter(Output == "plantsf") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Floral Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plants$Output[plants$Output == "SVisitsP"] <- "sVisitsP"
plants %>% select(Scenario, Output, Mean) %>% filter(Output == "sVisitsP") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Pollinator Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


plants %>% select(Scenario, Output, Mean) %>% filter(Output == "sVisits_perP") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Pollinator Visits Per Plant") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


##### LACA Abundance & Visits #######

plants %>% select(Scenario, Output, LACA) %>% filter(Output == "plantsf") %>%  ggplot(aes(Scenario, LACA, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("LACA Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plants$Output[plants$Output == "SVisitsP"] <- "sVisitsP"
plants %>% select(Scenario, Output, LACA) %>% filter(Output == "sVisitsP") %>%  ggplot(aes(Scenario, LACA, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("LACA Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


plants %>% select(Scenario, Output, LACA) %>% filter(Output == "sVisits_perP") %>%  ggplot(aes(Scenario, LACA, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("LACA Pollinator Visits Per Plant") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

### Total Pollinator Abundance % Visits ######
pollinators %>% select(Scenario, Output, Sum) %>% filter(Output == "animalsf") %>%  ggplot(aes(Scenario, Sum, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


pollinators %>% select(Scenario, Output, Sum) %>% filter(Output == "sVisitsA") %>%  ggplot(aes(Scenario, Sum, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


pollinators %>% select(Scenario, Output, Sum) %>% filter(Output == "sVisits_perA") %>%  ggplot(aes(Scenario, Sum, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Visits Per Animal") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


### Mean Pollinator Abundance % Visits ######
pollinators %>% select(Scenario, Output, Mean) %>% filter(Output == "animalsf") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Pollinator Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


pollinators %>% select(Scenario, Output, Mean) %>% filter(Output == "sVisitsA") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Pollinator Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


pollinators %>% select(Scenario, Output, Mean) %>% filter(Output == "sVisits_perA") %>%  ggplot(aes(Scenario, Mean, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Pollinator Visits Per Animal") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


###### Pivot For Nesting and Functional Group ###


long_pollinators <- pollinators %>%
  pivot_longer(
    cols = 3:52, # Specify columns to pivot
    names_to = "ARTH",           # Name for the new column that will hold the original column names
    values_to = "Value"             # Name for the new column that will hold the values
  )


poll_full <- left_join(long_pollinators, func, by = "ARTH")


poll_full <- left_join(poll_full, nest, by = "ARTH")

poll_LACA <- left_join(poll_full, LACA, by = "ARTH")

##### Ground Nesting Bees ########

poll_full %>% select(Scenario, Output, Value, NEST, ARTH) %>% filter(Output == "animalsf") %>% filter(NEST == "Ground") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Ground Nesting Bee Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


poll_full %>% select(Scenario, Output, Value, NEST, ARTH) %>% filter(Output == "sVisitsA") %>% filter(NEST == "Ground") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Ground Nesting Bee Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

poll_full %>% select(Scenario, Output, Value, NEST, ARTH) %>% filter(Output == "sVisits_perA") %>% filter(NEST == "Ground") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Ground Nesting Bee Visits Per Animal") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## alternate approach that gets total of ground nesting by simulation instead of by species
nest %>% filter(NEST == "Ground") %>% distinct(ARTH) #generate species list of ground-nesters 

ground <- pollinators %>%
  dplyr::select("Scenario", "Output", "ANCA", "ANDRENA", "ANPL", 
                     "DIAL", "EUAC", "HALI", "LATI", "SAB", 
                     "SMANTH", "UNBEE1", "UNBEE3", "UNK_SM_BEE", 
                     "Halictid", "HATR_or_LATI") %>%   mutate(SUM_ground = rowSums(select(., ANCA:UNBEE1)))


#### By Functional Group of Pollinator ####
##### Ground Nesting Bees ########

poll_full %>% select(Scenario, Output, Value, Functional.Group, ARTH) %>% filter(Output == "animalsf") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) + facet_wrap(~Functional.Group) +
  geom_boxplot() +
  theme_classic() +
  ylab("Pollinator Abundance") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


poll_full %>% select(Scenario, Output, Value, Functional.Group, ARTH) %>% filter(Output == "sVisitsA") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) + facet_wrap(~Functional.Group) +
  geom_boxplot() +
  theme_classic() +
  ylab("Pollinator Visits") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

poll_full %>% select(Scenario, Output, Value, Functional.Group, ARTH) %>% filter(Output == "sVisits_perA") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) + facet_wrap(~Functional.Group) +
  geom_boxplot() +
  theme_classic() +
  ylab("Pollinator Visits per Poll") + theme(axis.title.y = element_text(size = 8, face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



######## Figure 1 #########

# total plant abundance 
a <- plants %>%
  dplyr::select(Scenario, Output, SUM) %>%
  filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%
  filter(Output == "plantsf") %>%
  ggplot(aes(Scenario, SUM, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Plant Abundance") + 
  theme(axis.title.y = element_text(size = 18, face = "bold"), 
        axis.text = element_text(size = 18)) + 
  scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))    # Remove legend

#total pollinator visits
b <- plants %>% dplyr::select(Scenario, Output, SUM) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>% filter(Output == "sVisitsP") %>%  ggplot(aes(Scenario, SUM, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Visits") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18))  +  scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))    # Remove legend
 #plants$Output[plants$Output == "SVisitsP"] <- "sVisitsP"
 
## LACA Visits 
c <- plants %>% dplyr::select(Scenario, Output, LACA) %>%  filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "sVisitsP") %>%  ggplot(aes(Scenario, LACA, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("LACA Visits") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +  scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))    # Remove legend

# total pollinator abundance 
d <- pollinators %>% dplyr::select(Scenario, Output, Sum) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>% filter(Output == "animalsf") %>%  ggplot(aes(Scenario, Sum, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total Pollinator Abundance") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +  scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))    # Remove legend

## Ground nester abundance 
#e <- poll_full %>% dplyr::select(Scenario, Output, Value, NEST, ARTH) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf") %>% filter(NEST == "Ground") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
 # geom_boxplot() +
  #theme_classic() +
  #ylab("Ground Nesting Bee Abundance") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +   scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  #theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  #theme(plot.margin = margin(10, 10, 10, 30)) + 
  #labs(x = "Goatgrass treatment") +  # Set axis title
  #theme(legend.position = "none", 
    #    axis.title.x = element_text(size = 20, face = "bold"))    # Remove legend

e <- ground %>% dplyr::select(Scenario, Output, SUM_ground) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf") %>%  ggplot(aes(Scenario, SUM_ground, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Ground Nesting Bee Abundance") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +   scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))   


#LACA abundance 
f <- plants %>%
  dplyr::select(Scenario, Output, LACA) %>%
  filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%
  filter(Output == "plantsf") %>%
  ggplot(aes(Scenario, LACA, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Total LACA Abundance") + 
  theme(axis.title.y = element_text(size = 18, face = "bold"), 
        axis.text = element_text(size = 18)) + 
  scale_x_discrete(labels = c("Goatgrass" = "Present", "Goatgrass_Removed" = "Removed")) +  # Re-label x-axis categories
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +  # Horizontal alignment of x-axis text
  theme(plot.margin = margin(10, 10, 10, 30)) + 
  labs(x = "Goatgrass treatment") +  # Set axis title
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 20, face = "bold"))  +
  scale_y_continuous(limits = c(1, 5), breaks = seq(1, 5, 1))

library(ggpubr)


ggarrange(a, b, d, c, f, e, common.legend = FALSE,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2) +  theme(plot.margin = margin(2,2,2,2, "cm"))

### Ground nesting bees by species ######

poll_full %>% filter(NEST == "Ground") %>% distinct(ARTH)
#1DIAL: LACA, LOHO, TRAL, NAJE, GITR, AGHE, CAPA, UNK, HOVI, HECO       
#2 UNK_SM_BEE: LIBI, DICA, LACA, GITR, VIDO, CAPA, NAJE
#3 LATI: HOVI, HECO, LACA, CAPA, UNK, GRCA      
#4 SAB: LACA, TRAL, GITR, DICA, DEVA, NAJE      
#5 EUAC: TRAL, DICA, CAPA, HOVI, HECO      
#6 ANCA: TRFU, TRAL, HOVI      
#7 ANDRENA: TRAL, LACA, GITR   
#8 HALI: HECO, LACA, HOVI      

#9 ANPL: TRAL      
#10 SMANTH: NAJE    
#11 UNBEE1: LACA    
#12 UNBEE3: LOHO    

poll_full %>% dplyr::select(Scenario, Output, Value, NEST, ARTH) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf") %>% filter(NEST == "Ground") %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~ARTH)
  ylab("Ground Nesting Bee Abundance") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(plot.margin = margin(10, 10, 10, 30)) + labs(x = NULL) + theme(legend.position = "none")


##### LACA generalist vs specialist response #######
## need to find a way to get sVisitsA by plant species 

# List of ARTH categories to filter by
LACA_list <- c("HETH", "DIAL", "SAB", "SYRPHID2", "SYRPHID1", "NOSP1", "BF1", "PANUR", 
                     "UNKFLY", "BF2", "SYRPHID3", "OSMIA", "RAB", "ADELA", "UNMOTH", "UNMOTH2", 
                     "UNMOTH3", "UNSFL", "ANDRENA", "UNBEE1", "FUZZ", "HALI", "LATI", "UNK_SM_BEE", 
                     "COTU", "UNMOTH1", "SPHEC", "BOM2", "Halictid", "REDMOTH2")

# Filter the dataframe using dplyr <- your_dataframe %>%
poll_LACA <- poll_LACA %>% filter(ARTH %in% LACA_list)

# View the filtered data
head(poll_LACA)

poll_LACA %>% dplyr::select(Scenario, Output, Value, ARTH, Specialization) %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf")  %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~Specialization)

# sVisitsA for everyone not just LACA 

#+
 # ylab("Ground Nesting Bee Abundance") + theme(axis.title.y = element_text(size = 18, face="bold"), axis.text=element_text(size=18)) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(plot.margin = margin(10, 10, 10, 30)) + labs(x = NULL) + theme(legend.position = "none")


###### LACA vs non-LACA pollinator response to restoration: #####
LACA_list <- c("HETH", "DIAL", "SAB", "SYRPHID2", "SYRPHID1", "NOSP1", "BF1", "PANUR", 
               "UNKFLY", "BF2", "SYRPHID3", "OSMIA", "RAB", "ADELA", "UNMOTH", "UNMOTH2", 
               "UNMOTH3", "UNSFL", "ANDRENA", "UNBEE1", "FUZZ", "HALI", "LATI", "UNK_SM_BEE", 
               "COTU", "UNMOTH1", "SPHEC", "BOM2", "Halictid", "REDMOTH2")

# Add a new column 'LACA_status' based on whether ARTH is in LACA_list
poll_full <- poll_full %>%
  mutate(LACA_pollinator = ifelse(ARTH %in% LACA_list, "YES", "NO"))

# View the first few rows of the updated dataframe
head(poll_LACA)

poll_full %>% dplyr::select(Scenario, Output, Value, ARTH, LACA_pollinator) %>% filter(LACA_pollinator == "YES") %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf")  %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~ARTH)


poll_full %>% dplyr::select(Scenario, Output, Value, ARTH, LACA_pollinator) %>% filter(LACA_pollinator == "NO") %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf")  %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~ARTH)

###### how pollinators only in restored plots respond #####
# Control Network 
# [1] "DIAL"       "SAB"        "BF2"        "SYRPHID1"   "NOSP1"      "UNKFLY"     "BF1"        "SYRPHID4"  
#[9] "SYRPHID6"   "SYRPHID5"   "SYRPHID2"   "ANDRENA"    "FUZZ"       "UNK_SM_BEE" "APME"       "Halictid"  
#[17] "UNSFL"      "ARISTOLIS"  "UNWASP1"    "SMANTH"   

#Restored Network 
#[1] "APME"         "BOM1"         "HETH"         "REDMOTH2"     "DIAL"         "SBS"          "SAB"         
#[8] "BF2"          "BOM2"         "SYRPHID2"     "SYRPHID1"     "NOSP1"        "UNSFL"        "BF1"         
#[15] "ANCA"         "EUAC"         "PANUR"        "UNKFLY"       "SYRPHID3"     "OSMIA"        "RAB"       
#[22] "SMANTH"       "ASAR"         "SYRPHID4"     "ADELA"        "ANPL"         "ANDRENA"      "UNMOTH"    
#[29] "UNMOTH2"      "UNMOTH3"      "HATR"         "LATI"         "MEGA"         "SYRPHID5"     "YHB"       
#[36] "SYRPHID6"     "ARISTOLIS"    "FUZZ"         "HALI"         "UNBEE1"       "UNBEE3"       "UNK_SM_BEE"
#[43] "COTU"         "UNMOTH1"      "SPHEC"        "UNWASP1"      "Halictid"     "HATR_or_LATI" "UNKLEP"    
#[50] "Peponapis"

# Control Network species
control_network <- c("DIAL", "SAB", "BF2", "SYRPHID1", "NOSP1", "UNKFLY", "BF1", 
                     "SYRPHID4", "SYRPHID6", "SYRPHID5", "SYRPHID2", "ANDRENA", 
                     "FUZZ", "UNK_SM_BEE", "APME", "Halictid", "UNSFL", "ARISTOLIS", 
                     "UNWASP1", "SMANTH")

# Restored Network species
restored_network <- c("APME", "BOM1", "HETH", "REDMOTH2", "DIAL", "SBS", "SAB", 
                      "BF2", "BOM2", "SYRPHID2", "SYRPHID1", "NOSP1", "UNSFL", 
                      "BF1", "ANCA", "EUAC", "PANUR", "UNKFLY", "SYRPHID3", "OSMIA", 
                      "RAB", "SMANTH", "ASAR", "SYRPHID4", "ADELA", "ANPL", "ANDRENA", 
                      "UNMOTH", "UNMOTH2", "UNMOTH3", "HATR", "LATI", "MEGA", "SYRPHID5", 
                      "YHB", "SYRPHID6", "ARISTOLIS", "FUZZ", "HALI", "UNBEE1", "UNBEE3", 
                      "UNK_SM_BEE", "COTU", "UNMOTH1", "SPHEC", "UNWASP1", "Halictid", 
                      "HATR_or_LATI", "UNKLEP", "Peponapis")

# Species that are in the Restored Network but not in the Control Network
species_in_restored_not_control <- setdiff(restored_network, control_network)

# View the result
species_in_restored_not_control


# Add a new column 'LACA_status' based on whether ARTH is in LACA_list
poll_full <- poll_full %>%
  mutate(Restored_Only = ifelse(ARTH %in% species_in_restored_not_control, "YES", "NO"))



poll_full %>% dplyr::select(Scenario, Output, Value, ARTH, Restored_Only)  %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf")  %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~Restored_Only)


poll_full %>% dplyr::select(Scenario, Output, Value, ARTH, Restored_Only)  %>% filter(Scenario %in% c("Goatgrass", "Goatgrass_Removed")) %>%  filter(Output == "animalsf")  %>%  ggplot(aes(Scenario, Value, colour = Scenario)) +
  geom_boxplot() +
  theme_classic() + facet_wrap(~ Restored_Only + ARTH, scales = "free") 




