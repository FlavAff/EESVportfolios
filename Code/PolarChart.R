setwd("~/Documents/My Papers/Open/EESV portfolios/Code/")
rm(list = ls(all=TRUE)); #Remove all the objects in the memory

library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

datP <- read.csv("../Results/Provincial/SyncMetrics.csv")
datL <- read.csv("../Results/Local/SyncMetrics.csv")

# Named vector to map EESV names to specific colors
eesv_colors <- c(
  "AC" = "#F28E2B",     # A clear orange
  "IV" = "#4E79A7",     # A strong blue
  "Use" = "#E15759",    # A clear red
  "ES" = "#59A14F",     # A complementary green
  "Demand" = "#B07AA1"  # A distinct purple
)

provincial <- ggplot(datP, aes(x = EESV, y = Synchrony_Index, fill = EESV)) +
  
  # --- Create the bars ---
  # geom_col() is used for stat="identity" (values are provided)
  # width = 1 makes the wedges fill their angular space
  # color = "black" adds a thin outline to each wedge
  geom_col(width = 1, color = "black") +
  
  # --- Wrap it in a circle ---
  # This is the key function. theta = "x" (the default) wraps the x-axis (our categories).
  coord_polar() +
  
  # Add actual values
  geom_text(
      # Override 'y' to place text at 30% of the bar height (closer to center)
      aes(label = round(Synchrony_Index, digits = 2), y = Synchrony_Index * 0.5), 
      color = "white",
      size = 4,
      fontface = "bold" # Add bolding
    ) +
  
  scale_fill_manual(values = eesv_colors) +
  
  # --- Set the scale ---
  # Since these are ratios, we can set the y-axis (radius) from 0 to 1
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0), # Add grid lines
    labels = scales::percent # Format labels as percentages
  ) +
  
  # --- Clean up the theme ---
  theme_minimal() +
  labs(
    #title = "Relative Variance Reduction for EESVs",
    x = NULL, # Remove x-axis label
    y = NULL # Add y-axis (radius) label
  ) +
  theme(
    legend.position = "none", # Hide legend (labels are on the x-axis)
    axis.text.x = element_text(face = "bold.italic", size = 12),
    axis.text.y = element_blank(), # Hide y-axis text (we have grid lines)
    panel.grid.major.y = element_line(color = "grey90"), # Style grid lines
    panel.grid.major.x = element_blank() # Remove angular grid lines
  )


regional <- ggplot(datL, aes(x = EESV, y = Synchrony_Index, fill = EESV)) +
  
  # --- Create the bars ---
  # geom_col() is used for stat="identity" (values are provided)
  # width = 1 makes the wedges fill their angular space
  # color = "black" adds a thin outline to each wedge
  geom_col(width = 1, color = "black") +
  
  # --- Wrap it in a circle ---
  # This is the key function. theta = "x" (the default) wraps the x-axis (our categories).
  coord_polar() +
  
  #Wrap the species
  facet_wrap(~ Species, ncol = 3) +
  
  # Add actual values
  geom_text(
    # Override 'y' to place text at 30% of the bar height (closer to center)
    aes(label = round(Synchrony_Index, digits = 2), y = Synchrony_Index * 0.5), 
    color = "white",
    size = 4.5,
    fontface = "bold" # Add bolding
  ) +
  
  scale_fill_manual(values = eesv_colors) +
  
  # --- Set the scale ---
  # Since these are ratios, we can set the y-axis (radius) from 0 to 1
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0), # Add grid lines
    labels = scales::percent # Format labels as percentages
  ) +
  
  # --- Clean up the theme ---
  theme_minimal() +
  labs(
    #title = "Relative Variance Reduction for EESVs",
    x = NULL, # Remove x-axis label
    y = NULL # Add y-axis (radius) label
  ) +
  theme(
    legend.position = "none", # Hide legend (labels are on the x-axis)
    axis.text.x = element_text(face = "bold.italic", size = 12),
    axis.text.y = element_blank(), # Hide y-axis text (we have grid lines)
    panel.grid.major.y = element_line(color = "grey90"), # Style grid lines
    panel.grid.major.x = element_blank(), # Remove angular grid lines
    strip.text = element_text(face = "bold", size = 14),
    panel.border = element_blank(), 
    axis.line = element_blank() # This should remove that outer circle
    )

provincial
regional


ggsave("../Results/Figure4insert.png", plot = provincial, width = 6, height = 6, dpi = 300)
ggsave("../Results/Figure4outsert.png", plot = regional, width = 12, height = 9, dpi = 300)
