####### Resilience Function #########
### for calculating resilience of simulatin outputs for goatgrass paper
### code by Becca Nelson
### created: 3-17-26 ###
## last updated 3-17-26 ###

compute_resilience_per_run_stacked <- function(df, baseline, disturbance, recovery) {
  
  species_cols <- setdiff(names(df), c("Scenario", "Output", "Mean", "SUM", "AETR", "Sum"))
  
  base <- df %>% filter(Scenario == baseline) %>% select(all_of(species_cols))
  dist <- df %>% filter(Scenario == disturbance) %>% select(all_of(species_cols))
  rec  <- df %>% filter(Scenario == recovery) %>% select(all_of(species_cols))
  

  if(!all(nrow(base) == nrow(dist), nrow(dist) == nrow(rec))) {
    stop("Number of rows for baseline, disturbance, recovery must match")
  }
  

  resilience_species <- 1 - abs(rec - base) / abs(dist - base)
  
  
  resilience_community <- rowMeans(resilience_species, na.rm = TRUE)
  
  return(list(
    species_df = resilience_species,
    community_vector = resilience_community
  ))
}

# Plants
plants_abund <- plants %>% filter(Output == "plantsf")
plant_res <- compute_resilience_per_run_stacked(
  df = plants_abund,
  baseline = "No_Goatgrass",
  disturbance = "Goatgrass",
  recovery = "Goatgrass_Removed"
)

plant_species_per_run <- plant_res$species_df
plant_community_per_run <- plant_res$community_vector

# Pollinators
animals_abund <- pollinators %>% filter(Output == "animalsf")
pollinator_res <- compute_resilience_per_run_stacked(
  df = animals_abund,
  baseline = "No_Goatgrass",
  disturbance = "Goatgrass",
  recovery = "Goatgrass_Removed"
)

pollinator_species_per_run <- pollinator_res$species_df
pollinator_community_per_run <- pollinator_res$community_vector
