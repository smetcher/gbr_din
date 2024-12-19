library(raster)

# Function to constrain rasters across all years and assign binary values
constrain_binary_all_years <- function(input_dir, output_dir, thresholds) {
  # List all variables (subdirectories) in the input directory
  variables <- list.dirs(input_dir, full.names = FALSE, recursive = FALSE)
  
  for (variable in variables) {
    cat("Processing variable:", variable, "\n")
    
    # Get the thresholds for this variable
    min_val <- thresholds[[variable]]$min
    max_val <- thresholds[[variable]]$max
    
    variable_dir <- file.path(input_dir, variable)
    annual_files <- list.files(variable_dir, pattern = "_annual_mean\\.tiff$", full.names = TRUE)
    
    if (length(annual_files) == 0) {
      cat("    No annual mean files found for variable:", variable, "\n")
      next
    }
    
    # Process each annual file
    for (annual_file in annual_files) {
      # Extract the year from the filename
      year <- sub(".*_(\\d{4})_annual_mean\\.tiff$", "\\1", basename(annual_file))
      cat("  Year:", year, "\n")
      
      # Read the annual mean raster
      annual_raster <- raster(annual_file)
      
      # Apply thresholds and create binary raster
      binary_raster <- calc(annual_raster, function(x) {
        ifelse(is.na(x), NA, ifelse(x >= min_val & x <= max_val, 1, 0))
      })
      
      # Prepare the output directory structure
      output_variable_dir <- file.path(output_dir, variable)
      if (!dir.exists(output_variable_dir)) {
        dir.create(output_variable_dir, recursive = TRUE)
      }
      
      # Construct the output file path
      output_file <- file.path(output_variable_dir, paste0(variable, "_", year, "_binary.tiff"))
      
      # Save the binary raster
      writeRaster(binary_raster, output_file, format = "GTiff", overwrite = TRUE)
      cat("      Saved:", output_file, "\n")
    }
  }
}

# Define thresholds
thresholds <- list(
  Chl_a_sum = list(min = 0.22, max = Inf),
  temp = list(min = 20, max = 32),
  salt = list(min = 10, max = 38),
  Oxy_sat = list(min = 1.99, max = Inf)
)

# Input/Output Directories
input_dir <- "env_data_annual"              # Directory with annual mean files
output_dir <- "env_data_binary_restrained"      # Output directory for binary maps

# Run the function
constrain_binary_all_years(input_dir, output_dir, thresholds)