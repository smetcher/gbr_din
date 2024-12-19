library(raster)

# Function to resample rasters from 4km to 1km resolution, handling subdirectories and mirroring structure

resample_rasters_to_1km <- function(input_dir, output_dir) {
  # List all files and subdirectories within the input directory
  file_list <- list.files(input_dir, pattern = "\\.tiff$", full.names = TRUE, recursive = TRUE)
  
  for (raster_file in file_list) {
    cat("Processing file:", raster_file, "\n")
    
    # Load the raster file
    r <- raster(raster_file)
    
    # Disaggregate the raster to 1km resolution (our ereefs data is 4km)
    r_1km <- disaggregate(r, fact = 4)  # No method specified to preserve original values
    
    # Get the subdirectory structure relative to the input directory
    relative_path <- dirname(raster_file)
    relative_path <- gsub(input_dir, "", relative_path)  # Get the relative path
    
    # Create the corresponding subdirectory in the output directory
    output_subdir <- file.path(output_dir, relative_path)
    if (!dir.exists(output_subdir)) {
      dir.create(output_subdir, recursive = TRUE)
    }
    
    # Construct the full output file path (use the same filename as the original)
    output_file <- file.path(output_subdir, basename(raster_file))
    
    # Save the resampled raster
    writeRaster(r_1km, output_file, format = "GTiff", overwrite = TRUE)
    
    cat("Saved resampled raster to:", output_file, "\n")
  }
  
  cat("All files processed.\n")
}

# Directories
input_directory <- "env_data_hsi"  # Replace with your input directory
output_directory <- "env_data_hsi_1km"  # Directory to save the resampled maps

# Run the function
resample_rasters_to_1km(input_directory, output_directory)
