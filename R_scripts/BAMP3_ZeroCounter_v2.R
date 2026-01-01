# === Setup Paths ===
input_folder <- "Worm_output/2 Filtered"
output_folder <- "Worm_output/3 Thrash rate"

fps <- 30 # Footage frames per second
expected_total_time <- 60 # Total video duration in seconds

# Create output folder if not exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# === PROCESS FILES ===
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# Initialize empty results dataframe
results <- data.frame(Solution = character(),
                      Strain = character(),
                      Replicate = character(),
                      TimeID = character(),
                      Thrash_Count = numeric(),
                      Total_time_s = numeric(),
                      Missing_time_s = numeric(),
                      Time_obs_s = numeric(),
                      Thrash_rate_per_min = numeric(),
                      stringsAsFactors = FALSE)

for (file in csv_files) {
  cat("Processing file:", basename(file), "\n")
  
  # Extract filename and remove suffix
  filename <- basename(file)
  filename_core <- sub("\\.csv.*$", "", filename)
  parts <- strsplit(filename_core, "_")[[1]]
  
  # Extract metadata
  Solution  <- if(length(parts) >= 1) parts[1] else NA
  Strain    <- if(length(parts) >= 2) parts[2] else NA
  Replicate <- if(length(parts) >= 3) parts[3] else NA
  TimeID    <- if(length(parts) >= 4) parts[4] else NA
  
  # Read CSV
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Identify Track columns (cols 3 onwards)
  track_cols <- 3:ncol(df)
  
  # === Compute Time Metrics ===
  # 1. Filter out completely empty rows (just in case the CSV has them)
  #    Check if a row has at least one non-NA value in the track columns
  has_data <- rowSums(!is.na(df[, track_cols, drop = FALSE])) > 0
  
  # 2. Observed Frames = number of rows that actually have data
  observed_frames <- sum(has_data)
  
  # 3. Calculate seconds
  time_obs_s <- observed_frames / fps
  missing_time_s <- expected_total_time - time_obs_s
  
  # Sanity check: ensure missing time isn't negative (if video > 60s)
  if (missing_time_s < 0) missing_time_s <- 0
  
  # === Compute Thrash Count ===
  total_thrash_count <- 0
  
  # We only process rows that have data
  df_clean <- df[has_data, ]
  
  for (idx in track_cols) {
    # Extract specific track
    track_vals <- df_clean[, idx]
    track_vals <- track_vals[!is.na(track_vals)]
    
    # Need at least 2 points
    if (length(track_vals) < 2) next
    
    # Calculate Signs
    signs <- sign(track_vals)
    
    # Compare current vs previous (Lag)
    curr_vals <- track_vals[-1]
    lag_vals  <- track_vals[-length(track_vals)]
    
    curr_sign <- signs[-1]
    lag_sign  <- signs[-length(signs)]
    
    # Conditions:
    # 1. Angle change > 10
    cond1 <- abs(lag_vals - curr_vals) > 10
    # 2. Sign crossed zero
    cond2 <- curr_sign != lag_sign
    
    total_thrash_count <- total_thrash_count + sum(cond1 & cond2)
  }
  
  # === Final Calculations ===
  # Avoid division by zero
  thrash_rate <- if (time_obs_s > 0) {
    floor(total_thrash_count / time_obs_s * 60)
  } else {
    0
  }
  
  # Append to results
  results <- rbind(results, data.frame(
    Solution = Solution,
    Strain = Strain,
    Replicate = Replicate,
    TimeID = TimeID,
    Thrash_Count = total_thrash_count,
    Total_time_s = expected_total_time,
    Missing_time_s = missing_time_s,
    Time_obs_s = time_obs_s,
    Thrash_rate_per_min = thrash_rate,
    stringsAsFactors = FALSE
  ))
  
  cat("  -> Obs Time:", round(time_obs_s, 2), "s | Missing:", round(missing_time_s, 2), "s | Thrashes:", total_thrash_count, "\n")
}

# Save results
output_file <- file.path(output_folder, "thrash_rates.csv")
write.csv(results, output_file, row.names = FALSE)

cat("\n\u2705 Summary saved to:", output_file, "\n")