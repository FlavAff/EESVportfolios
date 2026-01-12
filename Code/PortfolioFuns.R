timeseries_plot <- function(data, y_var, y_label, title, filename, log_transform = TRUE) {
  
  # Check that all required columns exist in the provided dataframe
  required_cols <- c("year", "species", "region", y_var)
  if (!all(required_cols %in% names(data))) {
    stop(paste("The input data is missing one or more required columns. It needs:", paste(required_cols, collapse=", ")))
  }
  
  # Ensure there are no zero or negative values if log transforming
  if (log_transform && any(data[[y_var]] <= 0, na.rm = TRUE)) {
    warning(paste("Some values in", y_var, "are zero or negative. They will be removed for log transformation."))
    data <- data |> filter(.data[[y_var]] > 0)
  }
  
  # Dynamically set the y-axis mapping and label based on the log_transform flag
  if (log_transform) {
    y_aes <- aes(y = log(.data[[y_var]]))
    y_axis_label <- paste(y_label, "(log)")
  } else {
    y_aes <- aes(y = .data[[y_var]])
    y_axis_label <- y_label
  }
  
  # Create the plot
  time_series_plot <- ggplot(data, aes(x = year, color = region)) +
    y_aes + # Apply the y-axis mapping
    geom_line(aes(group = region), size = 1, alpha = 0.8) + # Thicker lines with reduced opacity
    facet_wrap(~species, scales = "free") + # Facet by species with free scales
    scale_color_viridis_d(option = "plasma") + # Use a vibrant color palette
    labs(
      title = title,
      #subtitle = "Time series shown by region",
      x = "Year",
      y = y_axis_label,
      color = "Region"
    ) +
    theme_minimal(base_size = 18) + # Clean theme with a base font size
    theme(
      text = element_text(family = "Arial"), # Consistent font
      plot.title = element_text(size = 24, face = "bold"),
      strip.text = element_text(face = "bold", size = 18), # Bold facet labels
      strip.background = element_rect(fill = "grey95", linetype = "blank"), # Light background for facet labels
      axis.title.x = element_text(vjust = -0.5), # Adjust x-axis title position
      axis.title.y = element_text(vjust = 2), # Adjust y-axis title position
      legend.position = "bottom", # Move legend to the bottom
      panel.grid.major = element_line(colour = "grey90"), # Lighter grid lines
      panel.grid.minor = element_blank() # Remove minor grid lines
    ) +
    scale_x_continuous(breaks = seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), by = 5)) + # Custom x-axis breaks
    scale_y_continuous(labels = scales::comma) # Format y-axis labels with commas
  
  
  # Save the plot to the specified file
  ggsave(filename, plot = time_series_plot, width = 12, height = 9, dpi = 300)
  
  message(paste("Successfully saved plot to:", filename))
}

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

# Get per species plots for synchrony across regions
synchrony_per_spp <- function(var,var_name,spp) {
  message(paste("Processing synchrony for:", spp))
  
  # Filter data for the current species
  species_data <- SES |> 
    filter(species == spp)
  
  if (spp == "Chum") {
    SES <- SES |> filter(region != "Fraser")
  }
  
  #Unweighted synchrony graph
  # Create a dynamic title and filename for the plot
  plot_title <- paste(var_name,"Synchrony Across Regions for", spp)
  file_name <- paste0("../Results/Local/",var_name,"_Synchrony_", spp, ".png")
  # Call the synchrony function on the species-specific data
  p1 <- synchrony(
    EESV = species_data,
    variable = var,
    scale = "region",          # The key change: analyze across regions
    title = plot_title
  )
  ggsave(file_name, p1, width = 8, height = 6, dpi = 300)
  
  #Unweighted synchrony graph
  # Create a dynamic title and filename for the plot
  plot_title <- paste(var_name,"Weighted Synchrony Across Regions for", spp)
  file_name <- paste0("../Results/Local/",var_name,"_WeightedSynchrony_", spp, ".png")
  # Call the synchrony function on the species-specific data
  p2 <- synchrony_weighted(
    EESV = species_data,
    variable = var,
    scale = "region",          # The key change: analyze across regions
    title = plot_title
  )
  ggsave(file_name, p2, width = 8, height = 6, dpi = 300)
  
}


# --- Portfolio Metrics Function ---
# Calculates:
# 1. Component CVs (weighted) and Portfolio CV
# 2. Portfolio Effect (PE) = Average CV / Portfolio CV
# 3. Synchrony Index (phi) = Var(Sum) / (Sum(SD))^2
# Includes optional detrending to separate long-term trends from volatility.

calculate_portfolio_metrics <- function(data, variable, scale_col, detrend = TRUE) {
  
  # 1. Prepare Data: Pivot to wide format (Time x Component)
  # Note: Keeping NAs here is actually safer for the lm() step as long as we use na.exclude
  clean_data <- data |>
    select(year, all_of(scale_col), all_of(variable)) 
  
  # Pivot wider to get matrix of time series
  df_wide <- clean_data |>
    pivot_wider(names_from = all_of(scale_col), values_from = all_of(variable)) |>
    arrange(year)
  
  # Extract the time series matrix (excluding year column)
  mat <- as.matrix(df_wide |> select(-year))
  years <- df_wide$year
  n_components <- ncol(mat)
  
  # 2. Detrending
  component_stats <- data.frame(
    component = colnames(mat),
    mean = NA,
    sd = NA,
    variance = NA
  )
  
  residuals_mat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  
  for(i in 1:ncol(mat)) {
    y <- mat[,i]
    
    # Mean is always based on raw abundance/catch
    mu <- mean(y, na.rm = TRUE)
    
    # Check if we have enough data to even attempt a trend (at least 2 non-NA points)
    if(detrend && sum(!is.na(y)) > 2) {
      # Linear detrending
      # FIX: na.action = na.exclude ensures the residuals vector is the same length as y
      model <- lm(y ~ years, na.action = na.exclude)
      res <- residuals(model)
      sigma_sq <- var(res, na.rm = TRUE)
      residuals_mat[,i] <- res
    } else {
      sigma_sq <- var(y, na.rm = TRUE)
      # If detrend is FALSE or not enough data, use centered raw data
      residuals_mat[,i] <- y - mu 
    }
    
    component_stats$mean[i] <- mu
    component_stats$variance[i] <- sigma_sq
    component_stats$sd[i] <- sqrt(sigma_sq)
  }
  
  # 3. Calculate Portfolio (Aggregate) Statistics
  agg_mean <- sum(component_stats$mean, na.rm = TRUE)
  agg_series <- rowSums(mat, na.rm = TRUE)
  
  if(detrend && sum(!is.na(agg_series)) > 2) {
    # FIX: Applying na.exclude here as well for consistency
    agg_model <- lm(agg_series ~ years, na.action = na.exclude)
    agg_var <- var(residuals(agg_model), na.rm = TRUE)
  } else {
    agg_var <- var(agg_series, na.rm = TRUE)
  }
  agg_sd <- sqrt(agg_var)
  
  # 4. Calculate Final Metrics
  
  # A. Coefficient of Variation (CV)
  # Weighted Avg CV = Sum(SD_i) / Sum(Mean_i)
  weighted_avg_cv_components <- sum(component_stats$sd, na.rm = TRUE) / agg_mean
  cv_portfolio <- agg_sd / agg_mean
  
  # B. Portfolio Effect (PE)
  pe_ratio <- weighted_avg_cv_components / cv_portfolio
  
  # C. Synchrony Index (phi) - Loreau and de Mazancourt (2008)
  # phi = Var(Sum) / (Sum(SD))^2
  numerator_sync <- agg_var
  denominator_sync <- (sum(component_stats$sd, na.rm = TRUE))^2
  synchrony_phi <- numerator_sync / denominator_sync
  
  # Return results list
  return(list(
    CV_Portfolio = cv_portfolio,
    CV_Avg_Components = weighted_avg_cv_components,
    PE_Ratio = pe_ratio,
    Synchrony_Phi = synchrony_phi,
    N_Components = n_components,
    Detrended = detrend
  ))
}

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

#Weight by natural variance when checking pairs to account for naturally more variable regions
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
