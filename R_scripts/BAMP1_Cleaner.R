# This script should be the first script to be run if working with mid-point bending angle data
# It will reformat the raw output from WormLab so it is easier to work with for subsequent steps.
# For example, WormLab also outputs (in the csv file) the name of the video that it used to generate these data.
# Those are meaningless in this context and will be removed because they do not make the data tidy.

# Set your input and output folder paths
input_folder <- "Worm_data"
output_folder <- "Worm_output/1 Cleaned"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
  cat("Created output folder:", output_folder, "\n")
}

# Define constants
N <- 3  # Number of rows to trim
new_header_1 <- "Frame"
new_header_2 <- "Time (s)"

# === FUNCTION: Pad CSV lines in memory ===
pad_commas <- function(line, target_commas) {
  actual_commas <- length(strsplit(line, ",")[[1]]) - 1
  missing_commas <- target_commas - actual_commas
  if (missing_commas > 0) {
    line <- paste0(line, paste(rep(",", missing_commas), collapse = ""))
  }
  return(line)
}

# === FUNCTION: Generate Track headers ===
generate_track_headers <- function(n) {
  paste0("Track", seq_len(n))
}

# Get all matching files
csv_files <- list.files(
  path = input_folder,
  pattern = "_Bending Angle - Mid-Point\\.csv$",
  full.names = TRUE
)

for (file in csv_files) {
  cat("Processing:", basename(file), "\n")
  
  # Read and clean lines
  lines <- readLines(file)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 1) {
    cat("No valid lines, skipping.\n")
    next
  }
  
  # Pad missing columns in lines
  comma_counts <- sapply(strsplit(lines, ","), length) - 1
  target_commas <- max(comma_counts)
  padded_lines <- sapply(lines, pad_commas, target_commas = target_commas)
  
  # Read as CSV
  data <- tryCatch({
    read.csv(text = padded_lines, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    cat("Failed to read:", e$message, "\n")
    next
  })
  
  if (is.null(data)) next
  
  # Trim first N rows
  if (nrow(data) > N) {
    data <- data[-(1:N), ]
  } else {
    data <- data[0, ]
  }
  
  # Rename column 1 and 2
  if (ncol(data) >= 2) {
    colnames(data)[1] <- new_header_1
    colnames(data)[2] <- new_header_2
  }
  
  # Normalize time column
  if (nrow(data) > 0 && ncol(data) >= 2) {
    first_value <- as.numeric(data[1, 2])
    if (!is.na(first_value)) {
      data[, 2] <- as.numeric(data[, 2]) - first_value
    } else {
      cat("Time column not numeric, skipping normalization.\n")
    }
  }
  
  # Rename remaining columns to Track1, Track2, ...
  if (ncol(data) > 2) {
    num_tracks <- ncol(data) - 2
    colnames(data)[3:ncol(data)] <- generate_track_headers(num_tracks)
  }
  
  # Save final output
  output_filename <- file.path(output_folder, sub("_Bending Angle - Mid-Point\\.csv$", "_cleaned.csv", basename(file)))
  # Replace NA and "" with " "
  data[is.na(data)] <- " "
  data[data == ""] <- " "
  write.csv(data, output_filename, row.names = FALSE)
  cat("Cleaned and renamed file saved to:", output_filename, "\n\n")
}
