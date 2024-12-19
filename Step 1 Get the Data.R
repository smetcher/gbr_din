# R packages
library(RNetCDF)  
library(raster)   
library(stringr)  
library(readr)    

# Function to extract and save data
extract_and_save_data <- function(file_urls, variables, depths) {
  
  for (file_url in file_urls) {
    # Extract date from file URL
    date_str <- str_extract(file_url, "\\d{4}-\\d{2}")  # assuming the date format is YYYY-MM
    
    print(paste("Processing URL:", file_url)) # Debugging line
    
    # Open the NetCDF file
    ncfile <- open.nc(file_url)
    
    # Retrieve longitude and latitude
    lon <- var.get.nc(ncfile, "longitude")
    lat <- var.get.nc(ncfile, "latitude")
    
    # Retrieve time variable and convert to Date
    time_var <- var.get.nc(ncfile, "time")
    start_date <- as.Date("1990-01-01") + time_var  # Adjust base date as necessary
    
    # Determine the number of days in the file
    num_days <- length(time_var)
    
    for (day in 1:num_days) {
      time_st <- day  # index of the day
      
      for (depth in depths) {
        depth_st <- depth
        
        for (variable in variables) {
          # Extract data
          start_vector <- c(1, 1, depth_st, time_st)
          count_vector <- c(length(lon), length(lat), 1, 1)
          
          data <- var.get.nc(
            ncfile = ncfile,
            variable = variable,
            start = start_vector,
            count = count_vector
          )
          
          # Create raster
          raster_data <- data |>
            t() |>
            raster(
              xmn = min(lon), xmx = max(lon),
              ymn = min(lat), ymx = max(lat),
              crs = CRS("+init=epsg:4326")
            ) |>
            flip(direction = 'y')
          
          # Prepare directories
          output_dir <- file.path("env_data", variable, format(start_date[day], "%Y"), format(start_date[day], "%m"))
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }
          
          # Save raster
          date_suffix <- format(start_date[day], "%Y%m%d")
          filename <- paste(variable, depth_st, date_suffix, ".tiff", sep = "_")
          save_file <- file.path(output_dir, filename)
          
          writeRaster(
            x = raster_data,
            filename = save_file,
            format = "GTiff",
            overwrite = TRUE
          )
          
          cat(paste("Saved:", save_file, "\n"))
        }
      }
    }
    
    # Close the NetCDF file
    close.nc(ncfile)
  }
}

# Read file URLs from a text file
file_urls <- read_lines("file_urls.txt")

# Remove leading and trailing double quotes and commas
file_urls <- str_replace_all(file_urls, '^"|",$', '')

# Print to check
print(file_urls)

# Specify variables and depths
variables <- c("Oxygen, temp, salt, Chl_a_sum") 
depths <- c(13, 14, 15, 16, 17) #Depths, (13 = -8.8m, 14 = -5.55m, 15 = -3m, 16 = -1.50m, 17 = -0.50m)

# Call function to extract and save data
extract_and_save_data(file_urls, variables, depths)
