# Counts 0-crossings (thrash count) and total missing time to calculate observed thrash rate
# It's called pseudoimpute because this script determines thrash rate w.r.t. only the observed time
# Assumes that the worm will behave similarly to what's been observed when it's not being observed

library(tidyverse)

input_folder <- "Worm_output/2 Filtered"
output_folder <- "Worm_output/3 Thrash rate"

fps = 30 # Footage frames per second

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
  filename_core <- sub("\\.csv.*$", "", filename)  # remove .csv and anything after
  parts <- strsplit(filename_core, "_")[[1]]
  
  # Extract metadata
  Solution <- parts[1]
  Strain <- parts[2]
  Replicate <- parts[3]
  TimeID <- parts[4]
  
  df <- read_csv(file)
  df2 <- df %>% 
    pivot_longer(cols = starts_with("Track"), names_to = "Track", values_to = "BendAng") %>% 
    filter(!is.na(BendAng)) %>% 
    mutate(sign = sign(BendAng)) %>% 
    mutate(sign_lag = lag(sign), BendAng_lag = lag(BendAng))
  
  # Compute missing time
  df3 <- pivot_longer(df, cols = 3:ncol(df), names_to = "track", values_to = "BA")
  md <- df3 %>% 
    group_by(Frame) %>% 
    summarise(n = n(), NAs = sum(is.na(BA))) %>% 
    filter(n == NAs)
  missingframes <- nrow(md) # this is total number of frames where there are missing data
  missingtime <- missingframes * 1/fps # total amount of seconds where there are missing data
  
  zerodf <- df2 %>% 
    filter(abs(BendAng_lag - BendAng) > 10) %>% 
    filter(sign != sign_lag)
  
  numzeroes <- nrow(zerodf)
  
  thrash_count = numzeroes
  
  time_obs_s = 60 - missingtime
  
  # Append to results
  results <- rbind(results, data.frame(Solution = Solution,
                                       Strain = Strain,
                                       Replicate = Replicate,
                                       TimeID = TimeID,
                                       Thrash_Count = thrash_count,
                                       Total_time_s = 60,
                                       Missing_time_s = missingtime,
                                       Time_obs_s = time_obs_s,
                                       Thrash_rate_per_min = floor(thrash_count / time_obs_s * 60), # Round DOWN
                                       stringsAsFactors = FALSE))
  
  cat("Successfully processed file.")
  
}

# Save results
output_file <- file.path(output_folder, "thrash_rates.csv")
write_csv(results, output_file)

cat("Summary saved to:", output_file, "\n")

# Possibility 1: Counting 0s in the data
# We cannot do this, as the data doesn't necessarily record 0 or values near 0.
# Possibility 2: Count peak using the peak algorithm (already in use)
# Problem: I don't understand what's going on there. Also pretty mid performance.
# Possibility 3: Count the number of 0-crossings to infer number of 0s in the data.
# The number of peaks is always going to be 1 + the number of crosses.