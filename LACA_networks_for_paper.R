########################################################
###########################################################
######## Lasthenia Network Code #######
######## code by Rebecca Nelson for Nelson et al. ###############
#######  ########
###### created: 11-22-24 #########################
######### last updated: 5-1-25 ############
#################################################


rm(list = ls())

##### load required packages #########
require(tidyverse)
require(nlme)
require(lubridate)
require(bipartite)
require(AER)


##### upload data #####
laca_full <- read.csv("laca_full.csv")



######## Grass Cover Figure #########
a <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = log(LACA_abundance))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "LACA Abundance"            
  )

b <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Total_Pollinator_Richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Total Pollinator Richness"            
  )

c <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Total_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Total Pollinator Abundance"            
  )

d <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = NODfc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "NODFc"            
  )


e <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = ind_nestedness_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "LACA Nested Contribution"            
  )

f <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = betwenness_centrality_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Betweenness Centrality LACA"            
  )

g <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Shannon.diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Interaction Shannon Diversity"            
  )

library("GGally")
library(ggpubr)

# Example of arranging plots with more space between them
fig.grass <- ggarrange(a, b, c, d, e, f, g,  labels = c("A", "B", "C", "D", "E", "F", "G"),  label.y = 1.05) +
  theme(plot.margin = margin(2.5, 2.5, 2.5, 2.5, "cm")) 

# Save the plot
ggsave("fig_grass.png", fig.grass, width = 14, height = 16, dpi = 300)

####### Figure 1 ###########

a <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Site_Plant_Richness)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +
  theme(
    legend.title = element_text(colour = "black", size = 16),
    legend.text = element_text(colour = "black", size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(
    x = "Grass Cover",
    y = "Floral Richness"
  )


b <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = log(LACA_abundance))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +
  theme(
    legend.title = element_text(colour = "black", size = 16),
    legend.text = element_text(colour = "black", size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Goldfield Abundance"            
  )

c <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Total_Pollinator_Richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Total Pollinator Richness"            
  )

d <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = Shannon.diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Interaction Shannon Diversity"            
  )

e <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = LACA_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Goldfield Pollinator Visits"            
  )

f <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = LACA_Pollinator_Richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Goldfield Pollinator Richness"            
  )

g <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = ind_nestedness_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Goldfield Nested Contribution"            
  )

h <- laca_full %>%
  ggplot(aes(x = Grass_Cover, y = betwenness_centrality_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Grass Cover",                     
    y = "Goldfield Betweenness Centrality"            
  )

i <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = LACA_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Goldfield Abundance",                     
    y = "Goldfield Visits"            
  )

j <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = LACA_Pollinator_Richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Goldfield Abundance",                     
    y = "Goldfield Pollinator Richness"            
  )

k <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = ind_nestedness_laca )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Goldfield Abundance",                     
    y = "Goldfield Nestedness Contribution"            
  )

l <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = betwenness_centrality_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Goldfield Abundance",                     
    y = "Goldfield Betweeness Centrality"            
  )

m <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = niche.overlap.HL )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "Goldfield Abundance",                     
    y = "Pollinator Niche Overlap"            
  )




library("GGally")
library(ggpubr)

plots <- list(a, b, c, d, e, f, g, h, i, j, k, l, m)

plots <- lapply(plots, function(p) {
  p + theme(
    axis.title.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20) 
  )
})

fig.1 <- ggarrange(plotlist = plots,   labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
                   ncol = 4, nrow = 4, align = "hv"
) 

# Save the plot
ggsave("fig_1.png", fig.1, width = 14, height = 16, dpi = 300)

########## LACA figure #####
a <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = niche.overlap.HL )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "Pollinator Niche Overlap"            
  )

b <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = LACA_Pollinator_Richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "Pollinator Richness LACA"            
  )

c <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = LACA_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "Pollinator Abundance LACA"            
  )

d <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = NODfc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA_Abundance",                     
    y = "NODFc"            
  )


e <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = ind_nestedness_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "LACA Nested Contribution"            
  )

f <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = betwenness_centrality_laca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "Betweenness Centrality LACA"            
  )

g <- laca_full %>%
  ggplot(aes(x = log(LACA_abundance), y = Shannon.diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgoldenrod1") +
  theme_bw() +  # Use a clean theme
  theme(
    legend.title = element_text(colour = "black", size = 16),  # Increase legend title size
    legend.text = element_text(colour = "black", size = 14),   # Increase legend text size
    axis.title.x = element_text(size = 18),   # Increase x-axis title size
    axis.title.y = element_text(size = 18),   # Increase y-axis title size
    axis.text.x = element_text(size = 14),    # Increase x-axis number size
    axis.text.y = element_text(size = 14)     # Increase y-axis number size
  ) +
  labs(
    x = "LACA Abundance",                     
    y = "Interaction Shannon Diverstiy"            
  )



library("GGally")
library(ggpubr)

# Example of arranging plots with more space between them
fig.laca <- ggarrange(a, b, c, d, e, f, g, labels = c("A", "B", "C", "D", "E", "F", "G")) +
  theme(plot.margin = margin(0.2,0.2,2,0.2, "cm")) 

# Save the plot
ggsave("fig_laca.png", fig.grass, width = 14, height = 16, dpi = 300)


#ggsave("fig_grass.png", fig.grass, width = 10, height = 12, dpi = 300)
####### stats #########
require(lme4)
require(lmerTest)
require(AER)
require(MASS)


## What is the relationship between grass cover and laca abundance and total forb richness indpendenct of network

mod<- lm(log(LACA_abundance) ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod <- glm.nb(Site_Plant_Richness ~ Grass_Cover, 
              data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


### is there a relationship between grass cover and network properties?

mod<- glm.nb(Total_n ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 

mod <- glm.nb(LACA_n ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- glm.nb(Total_Pollinator_Richness ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 
 

mod<- glm.nb(LACA_Pollinator_Richness ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 



mod<- lm(NODfc ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 



mod<- lm(NODF ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(weighted.NODF ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(ind_nestedness_laca ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(betwenness_centrality_laca ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(closeness_centrality_laca ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(niche.overlap.HL ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)


mod<- lm(niche.overlap.LL ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)


mod<- lm(mean.number.of.shared.partners.HL ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)
  

mod<- lm(mean.number.of.shared.partners.LL ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)
 

mod<- lm(H2 ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)


mod<- lm(Shannon.diversity ~ Grass_Cover, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod)


### is there a relationship between site plant richness and network properties?
mod<- lm(NODfc ~ Site_Plant_Richness, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(ind_nestedness_laca ~ Site_Plant_Richness, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 

mod<- lm(niche.overlap.HL ~ Site_Plant_Richness, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(betwenness_centrality_laca ~ Site_Plant_Richness, data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 

### is there a relationship between laca abundance and network properties?
mod<- lm(NODfc ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 
 

mod<- lm(niche.overlap.HL ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(niche.overlap.LL ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(ind_nestedness_laca ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(betwenness_centrality_laca ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(closeness_centrality_laca ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 
  

mod<- lm(NODF ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(weighted.NODF ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(mean.number.of.shared.partners.HL ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(mean.number.of.shared.partners.LL ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(Shannon.diversity ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- lm(H2 ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


mod<- glm.nb(LACA_n ~ log(LACA_abundance), data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 



mod <- glm.nb(LACA_Pollinator_Richness ~ log(LACA_abundance), 
              data = laca_full)
plot(fitted(mod),resid(mod))
qqnorm(resid(mod)) 
summary(mod) 


