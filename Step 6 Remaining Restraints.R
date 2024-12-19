library(raster)
library(sf)
library(dplyr)

# Function to apply final constraints including NRM region exclusion
apply_all_constraints <- function(
    input_dir,
    bathymetry_file,
    shipping_shapefile,
    feature_shapefile,
    iucn_shapefile,
    nrm_shapefile,               # NEW: NRM Regions shapefile
    output_dir,
    min_depth = 9,
    max_depth = 200,
    iucn_codes = c("Ia", "Ib", "II", "III", "IV"),
    nrm_regions = c("Burdekin", "Burnett Mary", "Cape York", "Fitzroy", "Mackay Whitsunday", "Wet Tropics")  # NRM regions to include
) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load Shapefiles
  bathy <- raster(bathymetry_file)
  
  shipping_data <- st_read(shipping_shapefile) %>%
    filter(grepl("Cargo|Tanker|Military|Sailing", TYPE, ignore.case = TRUE))
  shipping_data$presence <- 1
  
  feature_data <- st_read(feature_shapefile) %>%
    filter(grepl("Reef|ISLAND", FEAT_NAME, ignore.case = TRUE))
  feature_data$presence <- 1
  
  iucn_data <- st_read(iucn_shapefile) %>%
    filter(iucn_code %in% iucn_codes)
  iucn_data$presence <- 1
  
  # NEW: Load NRM regions shapefile and filter to include only listed regions
  nrm_data <- st_read(nrm_shapefile) %>%
    filter(NRM_REGION %in% nrm_regions)
  nrm_data$presence <- 1
  
  # Get the list of raster files
  raster_files <- list.files(input_dir, pattern = "\\.tiff$", full.names = TRUE)
  
  # Apply Constraints
  for (raster_file in raster_files) {
    cat("Processing file:", raster_file, "\n")
    
    # Load raster
    r <- raster(raster_file)
    
    # Step 1: Depth constraints
    if (!compareCRS(r, bathy)) {
      bathy <- projectRaster(bathy, crs = crs(r))
    }
    bathy_aligned <- resample(bathy, r, method = "bilinear")
    constrained_raster <- overlay(r, bathy_aligned, fun = function(x, y) {
      ifelse(!is.na(y) & (y > max_depth | y < min_depth), 0, x)
    })
    
    # Step 2: Shipping constraints
    if (!st_crs(shipping_data) == crs(r)) {
      shipping_data <- st_transform(shipping_data, crs(r))
    }
    shipping_raster <- rasterize(shipping_data, r, field = "presence", background = NA)
    constrained_raster <- overlay(constrained_raster, shipping_raster, fun = function(x, y) {
      ifelse(!is.na(y) & y == 1, 0, x)
    })
    
    # Step 3: Reef/Island feature constraints
    if (!st_crs(feature_data) == crs(r)) {
      feature_data <- st_transform(feature_data, crs(r))
    }
    feature_raster <- rasterize(feature_data, r, field = "presence", background = NA)
    constrained_raster <- overlay(constrained_raster, feature_raster, fun = function(x, y) {
      ifelse(!is.na(y) & y == 1, 0, x)
    })
    
    # Step 4: IUCN protection constraints
    if (!st_crs(iucn_data) == crs(r)) {
      iucn_data <- st_transform(iucn_data, crs(r))
    }
    iucn_raster <- rasterize(iucn_data, r, field = "presence", background = NA)
    constrained_raster <- overlay(constrained_raster, iucn_raster, fun = function(x, y) {
      ifelse(!is.na(y) & y == 1, 0, x)
    })
    
    # Step 5: NRM region constraints (exclude sites outside specified NRM regions)
    if (!st_crs(nrm_data) == crs(r)) {
      nrm_data <- st_transform(nrm_data, crs(r))
    }
    nrm_raster <- rasterize(nrm_data, r, field = "presence", background = NA)
    constrained_raster <- overlay(constrained_raster, nrm_raster, fun = function(x, y) {
      ifelse(is.na(y) | y != 1, 0, x)
    })
    
    # Save the final constrained raster
    output_file <- file.path(output_dir, basename(raster_file))
    writeRaster(constrained_raster, output_file, format = "GTiff", overwrite = TRUE)
    cat("Saved constrained raster to:", output_file, "\n")
  }
  
  cat("All files processed.\n")
}

# Set directories, shapefile paths, and run the function
apply_all_constraints(
  input_dir = "env_data_hsi_1km",               
  bathymetry_file = "bathymetry/topo30.grd",
  shipping_shapefile = "shipping_data/cts_srr_07_2024_pt.shp",
  feature_shapefile = "gbr_feature/Great_Barrier_Reef_Features.shp",
  iucn_shapefile = "GBR_Zone/Great_Barrier_Reef_coast_marine_park_zoning.shp",
  nrm_shapefile = "Regions/Natural_Resource_Management_Regions.shp",   # NEW: NRM shapefile
  output_dir = "final_1km_longlat",
  min_depth = 9,
  max_depth = 200,
  iucn_codes = c("Ia", "Ib", "II", "III", "IV"),
  nrm_regions = c("Burdekin", "Burnett Mary", "Cape York", "Fitzroy", "Mackay Whitsunday", "Wet Tropics")
)
