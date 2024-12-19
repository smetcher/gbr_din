library(raster)

# Function to combine binary maps across all years and variables into one map
combine_binary_maps_all_years <- function(input_dir, output_dir) {
  # List all variables (subdirectories) in the input directory
  variables <- list.dirs(input_dir, full.names = FALSE, recursive = FALSE)
  
  # Initialize a list to hold the rasters for all variables and years
  all_rasters <- list()
  
  for (variable in variables) {
    cat("Processing variable:", variable, "\n")
    
    # Directory for the current variable
    variable_dir <- file.path(input_dir, variable)
    
    # List all binary raster files for this variable
    raster_files <- list.files(variable_dir, pattern = "_binary\\.tiff$", full.names = TRUE)
    
    if (length(raster_files) == 0) {
      warning("  No binary rasters found for variable:", variable)
      next
    }
    
    # Read and store all rasters for this variable
    variable_rasters <- lapply(raster_files, raster)
    all_rasters <- append(all_rasters, variable_rasters)
  }
  
  if (length(all_rasters) > 0) {
    # Stack all rasters across all variables and years
    stacked_rasters <- stack(all_rasters)
    
    # Combine using binary logic: cell gets 1 if all rasters have 1; otherwise, 0
    combined_raster <- calc(stacked_rasters, function(x) {
      if (any(is.na(x))) {
        return(NA)  # Keep NA if any layer is NA
      } else if (all(x == 1)) {
        return(1)  # Set to 1 if all layers have a value of 1
      } else {
        return(0)  # Set to 0 otherwise
      }
    })
    
    # Ensure the output directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Construct the output file path
    output_file <- file.path(output_dir, "combined_binary_all_years.tiff")
    
    # Save the combined raster
    writeRaster(combined_raster, output_file, format = "GTiff", overwrite = TRUE)
    cat("Saved combined raster to:", output_file, "\n")
  } else {
    cat("No rasters found to combine across all variables and years.\n")
  }
}

# Input Directory and Output Directory
input_dir <- "env_data_binary_restrained"  # Directory with binary maps
output_dir <- "env_data_hsi"               # Output directory for combined map

# Run the function
combine_binary_maps_all_years(input_dir, output_dir)
