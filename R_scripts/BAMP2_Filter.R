# === Set Your Input and Output Folder Paths ===
input_folder <- "Worm_output/1 Cleaned"
output_folder <- "Worm_output/2 Filtered"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
  cat("\u2705 Created output folder:", output_folder, "\n")
}

# === Parameters for Filtering ===
min_duration  <- 5.0        # Minimum duration in seconds
min_range     <- 60.0       # Minimum range of bending angle
average_angle <- 300        # Maximum allowed absolute average bending angle (was 70 in debug print, keeping 300 from var)
average_range <- 5.0        # Minimum average local peak-to-peak range
window_radius <- 5          # Peak detection window radius (in frames)

# === Helper Function: Find Local Peaks ===
find_local_peaks <- function(vec, radius) {
  n <- length(vec)
  maxima <- rep(FALSE, n)
  minima <- rep(FALSE, n)
  
  if (n < (2 * radius + 1)) {
    return(list(max = maxima, min = minima))
  }
  
  # Remove NAs for indexing logic or handle them inside
  # To keep strictly parallel to indices, we loop
  for (i in 1:n) {
    if (i <= radius || i > (n - radius)) next
    
    # Extract window
    window <- vec[(i - radius):(i + radius)]
    center <- vec[i]
    
    if (is.na(center) || any(is.na(window))) next
    
    # Exclude center from neighbors comparison
    neighbors <- window[-(radius + 1)]
    
    if (all(center >= neighbors)) maxima[i] <- TRUE
    if (all(center <= neighbors)) minima[i] <- TRUE
  }
  
  return(list(max = maxima, min = minima))
}

# === Helper Function: Calculate Peak Stats ===
# Wraps the peak logic to apply it to a vector of angles
calc_peak_avg <- function(vec, radius) {
  # Remove leading/trailing NAs for calculation, but index mapping can be tricky.
  # The original script filtered !is.na(BendingAngle) before processing.
  vec_clean <- vec[!is.na(vec)]
  
  if (length(vec_clean) == 0) return(NA)
  
  peaks <- find_local_peaks(vec_clean, radius)
  peak_indices <- which(peaks$max | peaks$min)
  
  if (length(peak_indices) < 2) return(NA) # Need at least 2 peaks for a range
  
  peak_values <- vec_clean[peak_indices]
  types <- ifelse(peaks$max[peak_indices], "max", "min")
  
  ranges <- c()
  i <- 1
  while (i < length(peak_values)) {
    if (types[i] != types[i+1]) {
      val <- abs(peak_values[i] - peak_values[i+1])
      ranges <- c(ranges, val)
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  
  if (length(ranges) > 0) return(mean(ranges)) else return(NA)
}

# === Main Processing Loop ===
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  cat("\U0001F4C4 Processing:", basename(file), "\n")
  
  # Read the file
  data <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Convert " " to NA (Base R approach)
  data[data == " "] <- NA
  
  if (ncol(data) < 3) {
    cat("  ⚠️ Skipped: fewer than 3 columns\n\n")
    next
  }
  
  # Ensure first columns are numeric
  names(data)[1:2] <- c("Frame", "Time") # Standardize names for easier access
  data$Frame <- as.numeric(data$Frame)
  data$Time  <- as.numeric(data$Time)
  
  # === 1. Reshape to Long Format (Manual Stacking) ===
  # Identify track columns
  track_cols <- 3:ncol(data)
  track_names <- colnames(data)[track_cols]
  
  # Create a list of data frames to stack (faster than generic reshape for large data)
  long_list <- vector("list", length(track_cols))
  
  for (i in seq_along(track_cols)) {
    col_idx <- track_cols[i]
    # Create temp df: Frame, Time, Track, Angle
    tmp <- data[, c(1, 2, col_idx)]
    colnames(tmp) <- c("Frame", "Time", "BendingAngle")
    tmp$Track <- track_names[i]
    long_list[[i]] <- tmp
  }
  
  long_data <- do.call(rbind, long_list)
  long_data$BendingAngle <- as.numeric(long_data$BendingAngle)
  
  # === 2. Compute Basic Stats per Track ===
  # We use aggregate to group by Track
  
  # Duration
  stats_dur <- aggregate(Time ~ Track, data = long_data, 
                         FUN = function(x) max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  names(stats_dur)[2] <- "duration"
  
  # Range
  stats_range <- aggregate(BendingAngle ~ Track, data = long_data, 
                           FUN = function(x) max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  names(stats_range)[2] <- "range_val"
  
  # Average Angle
  stats_avg <- aggregate(BendingAngle ~ Track, data = long_data, 
                         FUN = function(x) mean(x, na.rm=TRUE))
  names(stats_avg)[2] <- "avg_val"
  
  # === 3. Compute Peak Stats ===
  # Split angles by track to apply custom function
  angle_list <- split(long_data$BendingAngle, long_data$Track)
  peak_res <- sapply(angle_list, calc_peak_avg, radius = window_radius)
  
  stats_peak <- data.frame(Track = names(peak_res), avg_local_range = as.numeric(peak_res), stringsAsFactors = FALSE)
  
  # Merge all stats
  all_stats <- merge(stats_dur, stats_range, by = "Track")
  all_stats <- merge(all_stats, stats_avg, by = "Track")
  all_stats <- merge(all_stats, stats_peak, by = "Track")
  
  # === 4. Apply Filters ===
  # Identify valid tracks
  valid_df <- subset(all_stats, 
                     !is.na(duration) & 
                       !is.na(range_val) & 
                       !is.na(avg_val) & 
                       !is.na(avg_local_range) &
                       duration >= min_duration &
                       range_val >= min_range &
                       abs(avg_val) <= average_angle &
                       avg_local_range >= average_range)
  
  # Debug: Check for high average angle exclusions
  invalid_angle <- subset(all_stats, abs(avg_val) > average_angle)
  if (nrow(invalid_angle) > 0) {
    cat("  ⚠️ Tracks with average angle >", average_angle, "(excluded):\n")
    print(invalid_angle[, c("Track", "avg_val")])
  }
  
  valid_tracks <- valid_df$Track
  
  if (length(valid_tracks) == 0) {
    cat("  ⚠️ No valid tracks passed all criteria. Skipping save.\n\n")
    next
  }
  
  output_filename <- file.path(output_folder, sub("\\.csv$", "_filtered.csv", basename(file)))
  
  # Filter the long data to only valid tracks
  long_filtered <- subset(long_data, Track %in% valid_tracks)
  
  # === 5. Remove Overlapping Tracks on Same Frame ===
  # Logic: If multiple tracks claim the same Frame, keep the track with the longest duration.
  
  # Only care about rows with actual data
  long_filtered <- subset(long_filtered, !is.na(BendingAngle))
  
  # Add duration info to the long data for sorting
  long_filtered <- merge(long_filtered, valid_df[, c("Track", "duration")], by = "Track")
  
  # Sort by Frame (asc) and Duration (desc)
  # This puts the "longest duration" track first for every frame group
  long_filtered <- long_filtered[order(long_filtered$Frame, -long_filtered$duration), ]
  
  # Remove duplicates on Frame (keeps the first one, which is the longest duration)
  long_unique <- long_filtered[!duplicated(long_filtered$Frame), ]
  
  # Clean up columns for reshaping
  long_unique <- long_unique[, c("Frame", "Time", "Track", "BendingAngle")]
  
  # === 6. Reshape to Wide Format and Save ===
  # base R reshape() is robust for this
  wide_data <- reshape(long_unique, 
                       idvar = c("Frame", "Time"), 
                       timevar = "Track", 
                       direction = "wide")
  
  # reshape() names columns like "BendingAngle.TrackName"
  # Clean column names to remove the prefix
  colnames(wide_data) <- gsub("^BendingAngle\\.", "", colnames(wide_data))
  
  # Reorder columns: Frame, Time, then sorted tracks
  track_cols_final <- intersect(valid_tracks, colnames(wide_data))
  # Sort tracks numerically if possible (e.g., Track_1, Track_2), otherwise alpha
  track_cols_final <- sort(track_cols_final)
  
  wide_data <- wide_data[, c("Frame", "Time", track_cols_final)]
  
  # Sort rows by Frame (reshape sometimes shuffles order)
  wide_data <- wide_data[order(wide_data$Frame), ]
  
  # Write file
  write.csv(wide_data, output_filename, row.names = FALSE)
  cat("Saved filtered file with", length(valid_tracks), "tracks →", output_filename, "\n\n")
}