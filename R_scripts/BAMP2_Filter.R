# === Set Your Input and Output Folder Paths ===
input_folder <- "Worm_output/1 Cleaned"
output_folder <- "Worm_output/2 Filtered"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
  cat("\u2705 Created output folder:", output_folder, "\n")
}

# === Parameters for Filtering ===
min_duration  <- 5.0        
min_range     <- 60.0       
average_angle <- 300        
average_range <- 5.0        
window_radius <- 5          

# === Helper: Vectorized Peak Detection ===
# Uses 'embed' to create a sliding window matrix without loops
find_local_peaks_vectorized <- function(vec, radius) {
  n <- length(vec)
  # If vector is too short for one window, return all FALSE
  if (n < (2 * radius + 1)) {
    return(list(max = logical(n), min = logical(n)))
  }
  
  # Create a matrix where each row is a window of size (2*radius + 1)
  # embed() creates windows in reverse order, but min/max logic is agnostic to order
  w_size <- 2 * radius + 1
  m <- embed(vec, w_size)
  
  # The "center" of the window in embed's output is at column index: radius + 1
  # (Since embed reverses, the middle is still the middle)
  center_col <- radius + 1
  centers <- m[, center_col]
  
  # Calculate row-wise Max and Min efficiently
  # We split the matrix into a list of columns to use pmax/pmin
  m_cols <- split(m, col(m))
  row_maxs <- do.call(pmax, c(m_cols, list(na.rm = FALSE)))
  row_mins <- do.call(pmin, c(m_cols, list(na.rm = FALSE)))
  
  # Identify peaks (exclude NAs implicitly by comparison)
  # Note: embed shortens the result by (w_size - 1). We must pad FALSE at ends.
  is_max_inner <- (centers >= row_maxs) & !is.na(centers) & !is.na(row_maxs)
  is_min_inner <- (centers <= row_mins) & !is.na(centers) & !is.na(row_mins)
  
  # Pad the results to match original vector length
  pad <- rep(FALSE, radius)
  return(list(
    max = c(pad, is_max_inner, pad),
    min = c(pad, is_min_inner, pad)
  ))
}

# === Helper: Optimized Peak Stats ===
calc_peak_avg_opt <- function(vec, radius) {
  # Strip NAs first
  vec_clean <- vec[!is.na(vec)]
  if (length(vec_clean) == 0) return(NA)
  
  peaks <- find_local_peaks_vectorized(vec_clean, radius)
  peak_indices <- which(peaks$max | peaks$min)
  
  if (length(peak_indices) < 2) return(NA)
  
  peak_values <- vec_clean[peak_indices]
  types <- ifelse(peaks$max[peak_indices], "max", "min")
  
  # Pre-allocate to avoid growing vector
  ranges <- numeric(length(peak_values)) 
  count <- 0
  
  i <- 1
  n_peaks <- length(peak_values)
  
  # Optimized loop: Pre-check length to avoid error
  while (i < n_peaks) {
    if (types[i] != types[i+1]) {
      count <- count + 1
      ranges[count] <- abs(peak_values[i] - peak_values[i+1])
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  
  if (count > 0) return(mean(ranges[1:count])) else return(NA)
}

# === Main Processing Loop ===
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  cat("\U0001F4C4 Processing:", basename(file), "...")
  
  # Read file
  data <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  data[data == " "] <- NA
  
  if (ncol(data) < 3) {
    cat(" Skipped (columns < 3)\n")
    next
  }
  
  # Standardize core columns
  names(data)[1:2] <- c("Frame", "Time")
  data$Frame <- as.numeric(data$Frame)
  data$Time  <- as.numeric(data$Time)
  
  # === 1. Reshape to Long (Kept manual stacking, it's efficient enough) ===
  track_cols <- 3:ncol(data)
  track_names <- colnames(data)[track_cols]
  
  long_list <- vector("list", length(track_cols))
  for (i in seq_along(track_cols)) {
    tmp <- data[, c(1, 2, track_cols[i])]
    names(tmp) <- c("Frame", "Time", "BendingAngle")
    tmp$Track <- track_names[i]
    long_list[[i]] <- tmp
  }
  long_data <- do.call(rbind, long_list)
  long_data$BendingAngle <- as.numeric(long_data$BendingAngle)
  
  # Remove NAs early to speed up aggregation
  long_data <- long_data[!is.na(long_data$BendingAngle), ]
  
  if (nrow(long_data) == 0) { 
    cat(" Empty after NA removal.\n") 
    next 
  }
  
  # === 2. Compute Stats (Optimized with tapply) ===
  # tapply is generally faster than multiple aggregates
  
  # Split angles by track once
  angles_by_track <- split(long_data$BendingAngle, long_data$Track)
  times_by_track  <- split(long_data$Time, long_data$Track)
  
  # Vectorized calculation over the list
  durations <- vapply(times_by_track, function(x) max(x) - min(x), numeric(1))
  ranges    <- vapply(angles_by_track, function(x) max(x) - min(x), numeric(1))
  means     <- vapply(angles_by_track, mean, numeric(1))
  
  # Peak stats (using optimized function)
  peak_avgs <- vapply(angles_by_track, calc_peak_avg_opt, numeric(1), radius = window_radius)
  
  # Combine into a stats data frame
  stats_df <- data.frame(
    Track = names(durations),
    duration = durations,
    range_val = ranges,
    avg_val = means,
    avg_local_range = peak_avgs,
    stringsAsFactors = FALSE
  )
  
  # === 3. Apply Filters ===
  # Check criteria
  keep_mask <- !is.na(stats_df$duration) & 
    !is.na(stats_df$range_val) & 
    !is.na(stats_df$avg_val) & 
    !is.na(stats_df$avg_local_range) &
    stats_df$duration >= min_duration &
    stats_df$range_val >= min_range &
    abs(stats_df$avg_val) <= average_angle &
    stats_df$avg_local_range >= average_range
  
  valid_tracks <- stats_df$Track[keep_mask]
  
  if (length(valid_tracks) == 0) {
    cat(" No valid tracks found.\n")
    next
  }
  
  # Filter long data
  long_filtered <- subset(long_data, Track %in% valid_tracks)
  
  # === 4. Remove Overlaps (Keep Longest Duration) ===
  # Add duration info for sorting
  dur_lookup <- stats_df$duration[keep_mask]
  names(dur_lookup) <- valid_tracks
  long_filtered$duration <- dur_lookup[long_filtered$Track]
  
  # Sort: Frame asc, Duration desc
  long_filtered <- long_filtered[order(long_filtered$Frame, -long_filtered$duration), ]
  
  # Dedup by Frame (keeps first, which is now max duration)
  long_unique <- long_filtered[!duplicated(long_filtered$Frame), ]
  
  # === 5. Optimized Wide Reshape (Matrix Indexing) ===
  output_filename <- file.path(output_folder, sub("\\.csv$", "_filtered.csv", basename(file)))
  
  # Identify dimensions
  u_frames <- sort(unique(long_unique$Frame))
  u_tracks <- sort(valid_tracks) # Alphanumeric sort
  
  # Create empty matrix
  mat <- matrix(NA, nrow = length(u_frames), ncol = length(u_tracks))
  colnames(mat) <- u_tracks
  
  # Map Frame/Track to matrix coordinates
  # match() is very fast
  row_idx <- match(long_unique$Frame, u_frames)
  col_idx <- match(long_unique$Track, u_tracks)
  
  # Direct assignment
  mat[cbind(row_idx, col_idx)] <- long_unique$BendingAngle
  
  # Reconstruct Time column (map unique frames back to their time)
  # We use the time from the deduplicated data
  frame_time_map <- long_unique$Time[match(u_frames, long_unique$Frame)]
  
  # Combine into final data frame
  final_df <- data.frame(Frame = u_frames, Time = frame_time_map, mat, check.names = FALSE)
  
  write.csv(final_df, output_filename, row.names = FALSE)
  cat(" Saved with", length(valid_tracks), "tracks.\n")
}