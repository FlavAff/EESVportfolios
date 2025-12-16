setwd("~/Documents/My Papers/Open/EESV portfolios/Code/")
rm(list = ls(all=TRUE)); #Remove all the objects in the memory

library(tidyverse)
library(ggplot2)
library(reshape2)
library(pracma)

#Get the provincial level EESVs
demand <- read_csv("../Data/DemandEESV.csv")
value <- read_csv("../Data/IVEESV.csv")
effort <- read_csv("../Data/ACEESV.csv")
catch <- read_csv("../Data/CatchEESV.csv") |> group_by(year, species) |> 
  summarise(catch = sum(catch, na.rm = TRUE), .groups = 'drop')
supply <- read_csv("../Data/SupplyEESV.csv") |> 
  filter(!(species == "Chum" & region == "Fraser") & !(species == "Coho" & region == "VIMI"))|> group_by(year, species) |> 
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop')


# ---- Peaks and Troughs Test ----
# Function to detect peaks and troughs
find_extremes <- function(series, years) {
  peak_indices <- findpeaks(series, nups = 1, ndowns = 1, sortstr = TRUE)[, 2]
  trough_indices <- findpeaks(-series, nups = 1, ndowns = 1, sortstr = TRUE)[, 2]
  
  # Ensure unique years
  peaks <- unique(years[peak_indices])
  troughs <- unique(years[trough_indices])
  
  list(peaks = peaks, troughs = troughs)
}

synchrony <- function(EESV,variable,scale,title){
  # Reshape data to wide format (Years as rows, Species as columns)
  df_wide <- EESV %>%
    select(year, scale, variable) %>%
    pivot_wider(names_from = scale, values_from = variable)
  # Get total number of years for denominator
  total_years <- nrow(df_wide)
  # Find peaks and troughs for each scale
  extremes <- lapply(df_wide[-1], find_extremes, years = df_wide$year)
  # Combine all peak/trough years to assess synchronization
  all_peaks <- unlist(lapply(extremes, function(x) x$peaks))
  all_troughs <- unlist(lapply(extremes, function(x) x$troughs))
  # Count peak/trough synchronization
  peak_sync <- as.data.frame(table(all_peaks))
  trough_sync <- as.data.frame(table(all_troughs))
  
  colnames(peak_sync) <- c("year", "count")
  colnames(trough_sync) <- c("year", "count")
  
  # Calculate peak percentage > 2
  if (nrow(peak_sync) > 0 && total_years > 0) {
    # Ensure count is numeric for comparison
    peak_sync$count <- as.numeric(peak_sync$count)
    # 1. Find number of years with > 2 peaks
    n_high_sync_years <- sum(peak_sync$count > 2)
    # 2. Calculate percentage
    percent_high_sync <- (n_high_sync_years / total_years) * 100
    # 3. Print the result to the console
    cat(sprintf(
      "Percentage of years with more than 2 peaks: %.2f%% (%d / %d years)\n",
      percent_high_sync, n_high_sync_years, total_years
    ))
  } else {
    cat("No peaks found or no years to analyze.\n")
  }
  # Calculate trough percentage > 2
  if (nrow(trough_sync) > 0 && total_years > 0) {
    # Ensure count is numeric for comparison
    trough_sync$count <- as.numeric(trough_sync$count)
    # 1. Find number of years with > 2 troughs
    n_low_sync_years <- sum(trough_sync$count > 2)
    # 2. Calculate percentage
    percent_low_sync <- (n_low_sync_years / total_years) * 100
    # 3. Print the result to the console
    cat(sprintf(
      "Percentage of years with more than 2 troughs: %.2f%% (%d / %d years)\n",
      percent_low_sync, n_low_sync_years, total_years
    ))
  } else {
    cat("No troughs found or no years to analyze.\n")
  }
  
  peak_sync$type <- "Peaks"
  trough_sync$type <- "Troughs"
  
  sync_data <- bind_rows(peak_sync, trough_sync) %>%
    mutate(year = as.numeric(as.character(year)))  # Convert factor to numeric
  
  # Plot the results
  ggplot(sync_data, aes(x = year, y = count, fill = type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ type) + 
    labs(title = title,
         x = "Year", 
         y = "Synchrony") +
    scale_fill_manual(values = c("Peaks" = "blue", "Troughs" = "red")) +
    theme_minimal()
}

png(file = "../Results/Provincial/ValueUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony(value, "landed.value", "species","Unweighted Instrumental Value Synchrony")
dev.off()
png(file = "../Results/Provincial/DemandUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony(demand, "price.per.kilo.wholesale", "species","Unweighted Demand Synchrony")
dev.off()
png(file = "../Results/Provincial/ACUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony(effort, "effort", "area","Unweighted Anthropogenic Contribution Synchrony")
dev.off()
png(file = "../Results/Provincial/UseUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony(catch, "catch", "species","Unweighted Use Synchrony")
dev.off()
png(file = "../Results/Provincial/SupplyUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony(supply, "abundance", "species","Unweighted Supply Synchrony")
dev.off()



# ---- Variance Reduction Test ----
var_red_ratio <- function(EESV,variable,scale) {
  # Reshape data to wide format (Years as rows, Species as columns)
  df_wide <- EESV %>%
    select(year, scale, variable) %>%
    pivot_wider(names_from = scale, values_from = variable)
  # Compute individual variances
  indiv_variances <- apply(df_wide[-1], 2, var, na.rm = TRUE)
  # Compute the variance of the mean time series (portfolio effect)
  mean_series <- rowMeans(df_wide[-1], na.rm = TRUE)
  portfolio_variance <- var(mean_series, na.rm = TRUE)
  # Variance reduction metric
  variance_reduction <- portfolio_variance / mean(indiv_variances) 
  cat("Variance Reduction Ratio:", variance_reduction, "\n")
  # Relative variance reduction metric
  variance_reduction_ratio <- 1 - (portfolio_variance / mean(indiv_variances))
  cat("Relative Variance Reduction:", variance_reduction_ratio, "\n")
}

var_red_ratio(value, "landed.value", "species")
var_red_ratio(demand, "price.per.kilo.wholesale", "species")
var_red_ratio(effort, "effort", "area")
var_red_ratio(catch, "catch", "species")
var_red_ratio(supply, "abundance", "species")




# ---- Check synchrony by species pair ----
pairwise_synchrony <- function(EESV, variable, scale) {
  # Reshape to wide format (Years as rows, Species as columns)
  df_wide <- EESV %>%
    select(year, scale, all_of(variable)) %>%
    pivot_wider(names_from = scale, values_from = all_of(variable))
  
  species_names <- colnames(df_wide)[-1]  # Exclude 'year'
  
  # Find peaks and troughs for each species
  extremes <- lapply(df_wide[-1], find_extremes, years = df_wide$year)
  
  # Convert list to a named vector for easier access
  names(extremes) <- species_names
  
  # Initialize synchrony matrices
  peak_sync_matrix <- matrix(0, nrow = length(species_names), ncol = length(species_names),
                             dimnames = list(species_names, species_names))
  trough_sync_matrix <- peak_sync_matrix  # Same size
  
  # Compute pairwise synchrony
  for (i in 1:length(species_names)) {
    for (j in i:length(species_names)) {
      if (i != j) {
        shared_peaks <- length(intersect(extremes[[i]]$peaks, extremes[[j]]$peaks))
        shared_troughs <- length(intersect(extremes[[i]]$troughs, extremes[[j]]$troughs))
        
        peak_sync_matrix[i, j] <- shared_peaks
        peak_sync_matrix[j, i] <- shared_peaks  # Symmetric
        
        trough_sync_matrix[i, j] <- shared_troughs
        trough_sync_matrix[j, i] <- shared_troughs
      }
    }
  }
  
  list(peak_sync = peak_sync_matrix, trough_sync = trough_sync_matrix)
}

# Example Usage:
sync_matricesV <- pairwise_synchrony(value, "landed.value", "species")
sync_matricesD <- pairwise_synchrony(demand, "price.per.kilo.wholesale", "species")
sync_matricesE <- pairwise_synchrony(effort, "effort", "area")
sync_matricesU <- pairwise_synchrony(catch, "catch", "species")
sync_matricesS <- pairwise_synchrony(supply, "abundance", "species")



# Print Synchrony Matrices
#print("Peak Synchrony Matrix:")
#print(sync_matrices$peak_sync)
#print("Trough Synchrony Matrix:")
#print(sync_matrices$trough_sync)

#Plot matrices
plot_sync_matrix <- function(sync_matrix, title, legname, scale) {
  melted_matrix <- melt(sync_matrix)
  colnames(melted_matrix) <- c("Species1", "Species2", "SyncCount")
  
  # Convert factor levels to preserve order
  melted_matrix$Species1 <- factor(melted_matrix$Species1, levels = colnames(sync_matrix))
  melted_matrix$Species2 <- factor(melted_matrix$Species2, levels = colnames(sync_matrix))
  
  # Keep only lower triangle (excluding diagonal)
  melted_matrix <- melted_matrix[as.numeric(melted_matrix$Species1) > as.numeric(melted_matrix$Species2), ]
  
  ggplot(melted_matrix, aes(x = Species1, y = Species2, fill = SyncCount)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(min(sync_matrix), max(sync_matrix))) +
    labs(title = title, x = scale, y = scale, fill = legname) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot Peak Synchrony
png(file = "../Results/Provincial/ValuePairwisePeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesV$peak_sync, "Unweighted Peak Synchrony \n Instrumental Value","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/DemandPairwisePeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesD$peak_sync, "Unweighted Peak Synchrony Demand","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/ACPairwisePeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesE$peak_sync, "Unweighted Peak Synchrony \n Anthropogenic Contribution","Sync Count","Area")
dev.off()
png(file = "../Results/Provincial/UsePairwisePeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesU$peak_sync, "Unweighted Peak Synchrony Use","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/SupplyPairwisePeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesS$peak_sync, "Unweighted Peak Synchrony Supply","Sync Count","Species")
dev.off()

# Plot Trough Synchrony
png(file = "../Results/Provincial/ValuePairwiseTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesV$trough_sync, "Unweighted Trough Synchrony \n Instrumental Value","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/DemandPairwiseTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesD$trough_sync, "Unweighted Trough Synchrony Demand","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/ACPairwiseTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesE$trough_sync, "Unweighted Trough Synchrony \n Anthropogenic contribution","Sync Count","Area")
dev.off()
png(file = "../Results/Provincial/UsePairwiseTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesU$trough_sync, "Unweighted Trough Synchrony Use","Sync Count","Species")
dev.off()
png(file = "../Results/Provincial/SupplyPairwiseTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(sync_matricesS$trough_sync, "Unweighted Trough Synchrony Supply","Sync Count","Species")
dev.off()


#Weight by natural variance when checking pairs to account for naturally more variable species
pairwise_weighted_synchrony <- function(EESV, variable, scale) {
  df_wide <- EESV %>%
    select(year, scale, all_of(variable)) %>%
    pivot_wider(names_from = scale, values_from = all_of(variable))
  
  species_names <- colnames(df_wide)[-1]
  extremes <- lapply(df_wide[-1], find_extremes, years = df_wide$year)
  names(extremes) <- species_names
  
  # Compute variance for each scale
  species_variance <- sapply(df_wide[-1], var, na.rm = TRUE)
  
  # Initialize weighted synchrony matrices
  peak_sync_matrix <- matrix(0, nrow = length(species_names), ncol = length(species_names),
                             dimnames = list(species_names, species_names))
  trough_sync_matrix <- peak_sync_matrix
  
  # Compute weighted synchrony
  for (i in 1:length(species_names)) {
    for (j in i:length(species_names)) {
      if (i != j) {
        shared_peaks <- length(intersect(extremes[[i]]$peaks, extremes[[j]]$peaks))
        shared_troughs <- length(intersect(extremes[[i]]$troughs, extremes[[j]]$troughs))
        
        # Weight synchronization by the geometric mean of species variances
        weight_factor <- sqrt(species_variance[i] * species_variance[j])
        peak_sync_matrix[i, j] <- shared_peaks / weight_factor
        peak_sync_matrix[j, i] <- shared_peaks / weight_factor
        
        trough_sync_matrix[i, j] <- shared_troughs / weight_factor
        trough_sync_matrix[j, i] <- shared_troughs / weight_factor
      }
    }
  }
  
  list(weighted_peak_sync = peak_sync_matrix, weighted_trough_sync = trough_sync_matrix)
}

# Example Usage:
weighted_sync_matricesV <- pairwise_weighted_synchrony(value, "landed.value","species")
weighted_sync_matricesD <- pairwise_weighted_synchrony(demand, "price.per.kilo.wholesale","species")
weighted_sync_matricesE <- pairwise_weighted_synchrony(effort, "effort","area")
weighted_sync_matricesU <- pairwise_weighted_synchrony(catch, "catch","species")
weighted_sync_matricesS <- pairwise_weighted_synchrony(supply, "abundance","species")

# Plot Weighted Synchrony
png(file = "../Results/Provincial/ValuePairwiseWeightedPeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesV$weighted_peak_sync, "Weighted Peak Synchrony \n Instrumental Value","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/DemandPairwiseWeightedPeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesD$weighted_peak_sync, "Weighted Peak Synchrony Demand","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/ACPairwiseWeightedPeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesE$weighted_peak_sync, "Weighted Peak Synchrony \n Anthropogenic contribution","Weighted Sync","Area")
dev.off()
png(file = "../Results/Provincial/UsePairwiseWeightedPeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesU$weighted_peak_sync, "Weighted Peak Synchrony Use","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/SupplyPairwiseWeightedPeaks.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesS$weighted_peak_sync, "Weighted Peak Synchrony Supply","Weighted Sync","Species")
dev.off()

png(file = "../Results/Provincial/ValuePairwiseWeightedTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesV$weighted_trough_sync, "Weighted Trough Synchrony \n Instrumental Value","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/DemandPairwiseWeightedTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesD$weighted_trough_sync, "Weighted Trough Synchrony Demand","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/ACPairwiseWeightedTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesE$weighted_trough_sync, "Weighted Trough Synchrony \n Anthropogenic Contribution","Weighted Sync","Area")
dev.off()
png(file = "../Results/Provincial/UsePairwiseWeightedTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesU$weighted_trough_sync, "Weighted Trough Synchrony Use","Weighted Sync","Species")
dev.off()
png(file = "../Results/Provincial/SupplyPairwiseWeightedTroughs.png", width = 1400, height = 1200, res = 300)
plot_sync_matrix(weighted_sync_matricesS$weighted_trough_sync, "Weighted Trough Synchrony Supply","Weighted Sync","Species")
dev.off()



synchrony_weighted <- function(EESV, variable, scale, title) {
  # Reshape data to wide format (Years as rows, Species as columns)
  df_wide <- EESV %>%
    select(year, scale, variable) %>%
    pivot_wider(names_from = scale, values_from = variable)
  
  # Compute variance per year across time series
  df_wide$variance <- apply(df_wide[-1], 1, var, na.rm = TRUE)
  df_wide$weight <- 1 / df_wide$variance  # Inverse variance as weight
  
  # Normalize weights between 0 and 1
  df_wide$weight <- (df_wide$weight - min(df_wide$weight, na.rm = TRUE)) / 
    (max(df_wide$weight, na.rm = TRUE) - min(df_wide$weight, na.rm = TRUE))
  
  # Find peaks and troughs for each scale
  extremes <- lapply(df_wide[-c(1, ncol(df_wide)-1, ncol(df_wide))], find_extremes, years = df_wide$year)
  
  # Count peak/trough synchronization
  all_peaks <- unlist(lapply(extremes, function(x) x$peaks))
  all_troughs <- unlist(lapply(extremes, function(x) x$troughs))
  
  peak_sync <- as.data.frame(table(all_peaks)) %>%
    mutate(year = as.numeric(as.character(all_peaks)))
  
  trough_sync <- as.data.frame(table(all_troughs)) %>%
    mutate(year = as.numeric(as.character(all_troughs)))
  
  # Merge with weight data and compute weighted synchrony
  peak_sync <- left_join(peak_sync, df_wide %>% select(year, weight), by = "year") %>%
    mutate(weighted_count = Freq * weight)
  
  trough_sync <- left_join(trough_sync, df_wide %>% select(year, weight), by = "year") %>%
    mutate(weighted_count = Freq * weight)
  
  # Format for plotting
  peak_sync$type <- "Peaks"
  trough_sync$type <- "Troughs"
  
  sync_data <- bind_rows(peak_sync, trough_sync) %>%
    select(year, weighted_count, type) %>%
    rename(count = weighted_count)
  
  # Plot the results
  ggplot(sync_data, aes(x = year, y = count, fill = type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ type) + 
    labs(title = title,
         x = "Year",  
         y = "Normalized Weighted Synchrony") +
    scale_fill_manual(values = c("Peaks" = "blue", "Troughs" = "red")) +
    theme_minimal()
}

png(file = "../Results/Provincial/ValueNormalisedWeightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony_weighted(value,"landed.value","species","Weighted Instrumental Value synchrony")
dev.off()
png(file = "../Results/Provincial/DemandNormalisedWeightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony_weighted(demand,"price.per.kilo.wholesale","species","Weighted Demand synchrony")
dev.off()
png(file = "../Results/Provincial/ACNormalisedWeightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony_weighted(effort,"effort","area","Weighted Anthropogenic Contribution synchrony")
dev.off()
png(file = "../Results/Provincial/UseNormalisedWeightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony_weighted(catch,"catch","species","Weighted Use synchrony")
dev.off()
png(file = "../Results/Provincial/SupplyNormalisedWeightedSynchrony.png", width = 1800, height = 1200, res = 300)
synchrony_weighted(supply,"abundance","species","Weighted Supply synchrony")
dev.off()
