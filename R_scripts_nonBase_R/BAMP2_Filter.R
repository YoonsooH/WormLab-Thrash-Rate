# === Load Libraries ===
library(dplyr)
library(tidyr)

# === Set Your Input and Output Folder Paths ===
input_folder <- "Worm_output/1 Cleaned"
output_folder <- "Worm_output/2 Filtered"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
  cat("\u2705 Created output folder:", output_folder, "\n")
}

# === Parameters for Filtering ===
min_duration <- 5.0          # Minimum duration in seconds
min_range <- 60.0            # Minimum range of bending angle
average_angle <- 300        # Maximum allowed absolute average bending angle
average_range <- 5.0         # Minimum average local peak-to-peak range
window_radius <- 5           # Peak detection window radius (in frames)

# === Helper Function: Find Local Peaks ===
find_local_peaks <- function(vec, radius) {
  n <- length(vec)
  maxima <- rep(FALSE, n)
  minima <- rep(FALSE, n)
  
  if (n < (2 * radius + 1)) {
    return(list(max = maxima, min = minima))  # Too short
  }
  
  for (i in 1:n) {
    if (i <= radius || i > (n - radius)) next
    window <- vec[(i - radius):(i + radius)]
    center <- vec[i]
    
    if (is.na(center) || any(is.na(window))) next
    neighbors <- window[-(radius + 1)]
    
    if (all(center >= neighbors)) maxima[i] <- TRUE
    if (all(center <= neighbors)) minima[i] <- TRUE
  }
  
  return(list(max = maxima, min = minima))
}

# === Main Processing Loop ===
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  cat("\U0001F4C4 Processing:", basename(file), "\n")
  
  # Read the file
  data <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  data[data == " "] <- NA
  if (ncol(data) < 3) {
    cat("  ⚠️ Skipped: fewer than 3 columns\n\n")
    next
  }
  
  # Convert to long format
  long_data <- data %>%
    pivot_longer(cols = 3:ncol(.), names_to = "Track", values_to = "BendingAngle") %>%
    mutate(
      BendingAngle = as.numeric(BendingAngle),
      `Time (s)` = as.numeric(`Time (s)`),
      Frame = as.numeric(Frame)
    )
  
  # Compute basic stats per track
  basic_stats <- long_data %>%
    group_by(Track) %>%
    summarize(
      duration = max(`Time (s)`, na.rm = TRUE) - min(`Time (s)`, na.rm = TRUE),
      range = max(BendingAngle, na.rm = TRUE) - min(BendingAngle, na.rm = TRUE),
      avg = mean(BendingAngle, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compute average local peak-to-peak range
  peak_stats <- long_data %>%
    filter(!is.na(BendingAngle)) %>%
    group_by(Track) %>%
    group_modify(~{
      vec <- .x$BendingAngle
      peaks <- find_local_peaks(vec, radius = window_radius)
      
      peak_indices <- which(peaks$max | peaks$min)
      peak_values <- vec[peak_indices]
      types <- ifelse(peaks$max[peak_indices], "max", "min")
      
      ranges <- c()
      i <- 1
      while (i < length(peak_values)) {
        if (types[i] != types[i + 1]) {
          range_val <- abs(peak_values[i] - peak_values[i + 1])
          ranges <- c(ranges, range_val)
          i <- i + 2
        } else {
          i <- i + 1
        }
      }
      
      avg_range <- ifelse(length(ranges) > 0, mean(ranges), NA)
      tibble(avg_local_range = avg_range)
    }) %>%
    ungroup()
  
  # Combine stats and apply filters
  all_stats <- basic_stats %>%
    left_join(peak_stats, by = "Track") %>%
    filter(
      !is.na(duration),
      !is.na(range),
      !is.na(avg),
      !is.na(avg_local_range),
      duration >= min_duration,
      range >= min_range,
      abs(avg) <= average_angle,
      avg_local_range >= average_range
    )
  
  # 🧪 Debugging: Print any invalid tracks that failed average angle check
  invalid_angle_tracks <- basic_stats %>%
    filter(abs(avg) > average_angle)
  
  if (nrow(invalid_angle_tracks) > 0) {
    cat("  ⚠️ Tracks with average angle > 70 (excluded):\n")
    print(invalid_angle_tracks %>% select(Track, avg))
  }
  
  valid_tracks <- all_stats$Track
  output_filename <- file.path(output_folder, sub("\\.csv$", "_filtered.csv", basename(file)))
  
  if (length(valid_tracks) == 0) {
    cat("  ⚠️ No valid tracks passed all criteria. Skipping save.\n\n")
    next
  }
  
  filtered_long <- long_data %>%
    filter(Track %in% valid_tracks)
  
  # === Remove Overlapping Tracks on the Same Frame ===
  overlap_df <- filtered_long %>%
    filter(!is.na(BendingAngle)) %>%
    group_by(Frame) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(overlap_df) > 0) {
    overlap_frames <- unique(overlap_df$Frame)
    
    frame_bounds <- filtered_long %>%
      filter(Frame %in% overlap_frames, !is.na(BendingAngle)) %>%
      group_by(Track) %>%
      summarize(
        start_frame = min(Frame),
        end_frame = max(Frame),
        duration = end_frame - start_frame,
        .groups = "drop"
      )
    
    keep_tracks <- filtered_long %>%
      filter(Frame %in% overlap_frames, !is.na(BendingAngle)) %>%
      left_join(frame_bounds, by = "Track") %>%
      group_by(Frame) %>%
      slice_max(order_by = duration, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(Frame, Track)
    
    filtered_long <- filtered_long %>%
      anti_join(overlap_df, by = c("Frame", "Track")) %>%
      bind_rows(
        filtered_long %>%
          inner_join(keep_tracks, by = c("Frame", "Track"))
      )
  }
  
  # === Reshape to Wide Format and Save ===
  filtered_wide <- filtered_long %>%
    pivot_wider(names_from = Track, values_from = BendingAngle)
  
  # Reorder columns: Frame, Time, then tracks
  filtered_wide <- filtered_wide[, c("Frame", "Time (s)", intersect(valid_tracks, colnames(filtered_wide)))]
  
  # Remove columns that are all NA
  filtered_wide <- filtered_wide[, c(TRUE, TRUE, colSums(!is.na(filtered_wide[, -(1:2)])) > 0)]
  
  # Write the filtered CSV
  write.csv(filtered_wide, output_filename, row.names = FALSE)
  cat("  ✅ Saved filtered file with", length(valid_tracks), "tracks →", output_filename, "\n\n")
}
