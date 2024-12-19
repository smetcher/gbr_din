library(raster)
library(dplyr)

# Function to combine depths 13 to 17 and output annual mean rasters
combine_depths_annual <- function(input_dir, output_dir) {
  # List all variables (subdirectories) in the input directory
  variables <- list.dirs(input_dir, full.names = FALSE, recursive = FALSE)
  
  for (variable in variables) {
    cat("Processing variable:", variable, "\n")
    
    variable_dir <- file.path(input_dir, variable)
    years <- list.dirs(variable_dir, full.names = FALSE, recursive = FALSE)
    
    # Prepare the output directory structure for the variable
    output_variable_dir <- file.path(output_dir, variable)
    if (!dir.exists(output_variable_dir)) {
      dir.create(output_variable_dir, recursive = TRUE)
    }
    
    for (year in years) {
      cat("  Year:", year, "\n")
      
      year_dir <- file.path(variable_dir, year)
      months <- list.dirs(year_dir, full.names = FALSE, recursive = FALSE)
      
      # Initialize a list to hold daily mean rasters for the year
      daily_means <- list()
      
      for (month in months) {
        cat("    Month:", month, "\n")
        
        month_dir <- file.path(year_dir, month)
        # List all raster files in the month directory
        raster_files <- list.files(month_dir, pattern = "\\.tiff$", full.names = TRUE)
        
        # Group raster files by day (based on the YYYYMMDD segment in filenames)
        daily_files <- split(
          raster_files,
          sub(".*_(\\d{8})_\\.tiff$", "\\1", basename(raster_files))
        )
        
        for (day in names(daily_files)) {
          # Get rasters for depths 13 to 17 for the day
          day_files <- daily_files[[day]]
          depth_files <- day_files[grepl("_1[3-7]_", day_files)]
          
          if (length(depth_files) == 5) {  # Expect exactly 5 depths
            # Read and stack all rasters for the day
            rasters <- lapply(depth_files, raster)
            mean_raster <- stack(rasters) %>% mean(na.rm = TRUE)
            
            # Add daily mean raster to the list
            daily_means[[day]] <- mean_raster
          } else {
            cat("      Skipped day:", day, "- Missing depths 13 to 17\n")
          }
        }
      }
      
      # If we have daily mean rasters for the year, calculate and save the annual mean
      if (length(daily_means) > 0) {
        annual_raster <- stack(daily_means) %>% mean(na.rm = TRUE)
        
        # Construct the output file path
        output_file <- file.path(output_variable_dir, paste0(variable, "_", year, "_annual_mean.tiff"))
        
        # Save the annual mean raster
        writeRaster(annual_raster, output_file, format = "GTiff", overwrite = TRUE)
        cat("      Saved annual mean for year:", year, "to", output_file, "\n")
      } else {
        cat("      Skipped year:", year, "- No valid daily rasters\n")
      }
      
      # Clear memory for the next year to reduce CPU & memory demands
      gc()
    }
  }
}

# Input/Output Directories
input_dir <- "env_data"           # Input folder containing data
output_dir <- "env_data_annual"  # Output folder to save combined depth annual rasters

# Run the function
combine_depths_annual(input_dir, output_dir)
