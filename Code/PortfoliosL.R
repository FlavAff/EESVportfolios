setwd("~/Documents/My Papers/Open/EESV portfolios/Code/")
rm(list = ls(all=TRUE)); #Remove all the objects in the memory

library(tidyverse)
library(ggplot2)
library(reshape2)
library(pracma)

#Get the local level EESVs
SES <- read_csv("../Data/RegionalData.csv", show_col_types = FALSE) |> 
  mutate(total_catch = ifelse(is.na(abundance) & total_catch == 0, NA, total_catch)) |>
  mutate(harvest = ifelse(is.na(abundance) & harvest == 0, NA, harvest)) |>
  mutate(landed = ifelse(is.na(abundance) & landed == 0, NA, landed)) |>
  # Create a new column 'species_region' by combining 'species' and 'region'
  tidyr::unite("species_region", species, region, sep = "_", remove = FALSE)

source("PortfolioFuns.R")


#### TIME SERIES PLOTS ####
# --- Generate and Save Each Plot ---

# 1. Plot for Abundance (from supply data)
timeseries_plot(
  data = SES,
  y_var = "abundance",
  y_label = "Abundance",
  title = "Regional time series of ecological supply",
  filename = "../Results/Local/TimeSeries_Abundance.png"
)
# 2. Plot for Total Catch (from catch data)
timeseries_plot(
  data = SES,
  y_var = "total_catch",
  y_label = "Catch",
  title = "Regional time series of use",
  filename = "../Results/Local/TimeSeries_Catch.png"
)
# 3. Plot for Total Effort (from effort data)
timeseries_plot(
  data = SES,
  y_var = "total_effort",
  y_label = "Effort",
  title = "Regional time series of anthropogenic contribution",
  filename = "../Results/Local/TimeSeries_Effort.png"
)
# 4. Plot for Landed Value (from value data)
timeseries_plot(
  data = SES,
  y_var = "landed",
  y_label = "Landed Value",
  title = "Regional time series of instrumental value",
  filename = "../Results/Local/TimeSeries_Value.png"
)



#### PEAK & TROUGH SYNCHRONY PLOTS ####
for (spp in unique(SES$species)) {
  synchrony_per_spp("abundance","Supply",spp)
  synchrony_per_spp("total_catch","Use",spp)
  synchrony_per_spp("total_effort","AC",spp)
  synchrony_per_spp("landed","Value",spp)
}

#Spp X region overall synchrony (maybe makes no sense)
# png(file = "../Results/Local/ValueUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
# synchrony(SES, "landed", "species_region","Unweighted Instrumental Value Synchrony SppXReg")
# dev.off()
# png(file = "../Results/Local/ACUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
# synchrony(SES, "total_effort", "species_region","Unweighted Anthropogenic Contribution Synchrony SppXReg")
# dev.off()
# png(file = "../Results/Local/UseUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
# synchrony(SES, "total_catch", "species_region","Unweighted Use Synchrony SppXReg")
# dev.off()
# png(file = "../Results/Local/SupplyUnweightedSynchrony.png", width = 1800, height = 1200, res = 300)
# synchrony(SES, "abundance", "species_region","Unweighted Supply Synchrony SppXReg")
# dev.off()





####  VARIANCE REDUCTION TESTS ####
for (spp in unique(SES$species)) {
  dat <- SES |> filter(species == spp)
  
  if (spp == "Chum") {
    dat <- dat |> filter(region != "Fraser")
  }
  
  print(paste("Starting with species", spp))
  print(paste("Calcuating for supply"))
  var_red_ratio(dat, "abundance", "region")
  print(paste("Calcuating for use"))
  var_red_ratio(dat, "total_catch", "region")
  print(paste("Calcuating for AC"))
  var_red_ratio(dat, "total_effort", "region")
  print(paste("Calcuating for IV"))
  var_red_ratio(dat, "landed", "region")
}




####  SYNCHRONY PER LOCATION PAIRS ####
# Unweighted
for (spp in unique(SES$species)) {
  dat <- SES |> filter(species == spp)
  
  if (spp == "Chum") {
    dat <- dat |> filter(region != "Fraser")
  }
  
  sync_matricesS <- pairwise_synchrony(dat, "abundance", "region")
  p <- plot_sync_matrix(sync_matricesS$peak_sync, "Unweighted Peak Synchrony Supply","Sync Count","Region")
  ggsave(paste0("../Results/Local/SupplyPairwisePeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(sync_matricesS$trough_sync, "Unweighted Trough Synchrony Supply","Sync Count","Region")
  ggsave(paste0("../Results/Local/SupplyPairwiseTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  sync_matricesU <- pairwise_synchrony(dat, "total_catch", "region")
  p <- plot_sync_matrix(sync_matricesU$peak_sync, "Unweighted Peak Synchrony Use","Sync Count","Region")
  ggsave(paste0("../Results/Local/UsePairwisePeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(sync_matricesU$trough_sync, "Unweighted Trough Synchrony Use","Sync Count","Region")
  ggsave(paste0("../Results/Local/UsePairwiseTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  sync_matricesE <- pairwise_synchrony(dat, "total_effort", "region")
  p <- plot_sync_matrix(sync_matricesE$peak_sync, "Unweighted Peak Synchrony \n Anthropogenic Contribution","Sync Count","Region")
  ggsave(paste0("../Results/Local/ACPairwisePeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(sync_matricesE$trough_sync, "Unweighted Trough Synchrony \n Anthropogenic contribution","Sync Count","Region")
  ggsave(paste0("../Results/Local/ACPairwiseTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  sync_matricesV <- pairwise_synchrony(dat, "landed", "region")
  p <- plot_sync_matrix(sync_matricesV$peak_sync, "Unweighted Peak Synchrony \n Instrumental Value","Sync Count","Region")
  ggsave(paste0("../Results/Local/ValuePairwisePeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(sync_matricesV$trough_sync, "Unweighted Trough Synchrony \n Instrumental Value","Sync Count","Region")
  ggsave(paste0("../Results/Local/ValuePairwiseTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
}

# Weighted
for (spp in unique(SES$species)) {
  dat <- SES |> filter(species == spp)
  
  if (spp == "Chum") {
    dat <- dat |> filter(region != "Fraser")
  }
  
  weighted_sync_matricesS <- pairwise_weighted_synchrony(dat, "abundance","region")
  p <- plot_sync_matrix(weighted_sync_matricesS$weighted_peak_sync, "Weighted Peak Synchrony Supply","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/SupplyPairwiseWeightedPeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(weighted_sync_matricesS$weighted_trough_sync, "Weighted Trough Synchrony Supply","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/SupplyPairwiseWeightedTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  weighted_sync_matricesU <- pairwise_weighted_synchrony(dat, "total_catch","region")
  p <- plot_sync_matrix(weighted_sync_matricesU$weighted_peak_sync, "Weighted Peak Synchrony Use","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/UsePairwiseWeightedPeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(weighted_sync_matricesU$weighted_trough_sync, "Weighted Trough Synchrony Use","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/UsePairwiseWeightedTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  weighted_sync_matricesE <- pairwise_weighted_synchrony(dat, "total_effort","region")
  p <- plot_sync_matrix(weighted_sync_matricesE$weighted_peak_sync, "Weighted Peak Synchrony \n Anthropogenic contribution","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/ACPairwiseWeightedPeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(weighted_sync_matricesE$weighted_trough_sync, "Weighted Trough Synchrony \n Anthropogenic Contribution","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/ACPairwiseWeightedTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
  
  weighted_sync_matricesV <- pairwise_weighted_synchrony(dat, "landed","region")
  p <- plot_sync_matrix(weighted_sync_matricesV$weighted_peak_sync, "Weighted Peak Synchrony \n Instrumental Value","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/ValuePairwiseWeightedPeaks",spp,".png"), p, width = 8, height = 6, dpi = 300)
  p <- plot_sync_matrix(weighted_sync_matricesV$weighted_trough_sync, "Weighted Trough Synchrony \n Instrumental Value","Weighted Sync","Region")
  ggsave(paste0("../Results/Local/ValuePairwiseWeightedTroughs",spp,".png"), p, width = 8, height = 6, dpi = 300)
}
