library(raster)
library(sf)
library(dplyr)
library(tidyr)

# Function to calculate site data with equal-area projection, nitrogen reduction, and market value
calculate_site_data_equal_area <- function(
    raster_file, nrm_shapefile, 
    output_summary_csv, output_coordinates_csv, output_raster, 
    bivalves_per_m2, oyster_weight_kg, nitrogen_per_1000kg_oyster_kg, oyster_value_per_tonne
) {
  # Ensure the output directory exists
  output_folder <- dirname(output_summary_csv)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Load the raster map
  r <- raster(raster_file)
  
  # Load the NRM shapefile
  nrm_areas <- st_read(nrm_shapefile)
  
  # Filter to include only specified NRMs
  nrm_included <- c("Burdekin", "Burnett Mary", "Cape York", "Fitzroy", "Mackay Whitsunday", "Wet Tropics")
  nrm_areas <- nrm_areas %>% filter(NRM_REGION %in% nrm_included)
  
  # Reproject the NRM shapefile to match the CRS of the raster file
  nrm_areas_projected <- st_transform(nrm_areas, crs(r))
  
  # Mask the raster to exclude areas outside the NRM regions
  nrm_raster_mask <- rasterize(nrm_areas_projected, r, field = 1)
  masked_raster <- mask(r, nrm_raster_mask)
  
  # Reproject the masked raster to an equal-area projection (GDA94 / Australian Albers)
  equal_area_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +datum=WGS84 +units=m +no_defs"  # Australian Albers
  equal_area_raster <- projectRaster(masked_raster, crs = equal_area_crs, method = "ngb")
  
  # Calculate the area of each cell in the equal-area raster
  cell_area_m2 <- res(equal_area_raster)[1] * res(equal_area_raster)[2]
  
  # Convert raster cells to points (centroids)
  raster_points <- rasterToPoints(equal_area_raster, spatial = TRUE)
  raster_points_sf <- st_as_sf(raster_points)
  
  # Filter for cells with a value of 1 (suitable sites)
  raster_value_column <- names(raster_points_sf)[1]
  suitable_cells <- raster_points_sf %>%
    filter(!!sym(raster_value_column) == 1)
  
  # Perform a spatial join to keep only the points within the NRM regions
  joined_data <- st_join(suitable_cells, st_transform(nrm_areas, crs(equal_area_raster)), join = st_within)
  
  # Calculate bivalves, biomass (tonnes), nitrogen reduction, and market value based on cell area
  joined_data$bivalves_per_cell <- cell_area_m2 * bivalves_per_m2
  joined_data$biomass_per_cell_tonnes <- (joined_data$bivalves_per_cell * oyster_weight_kg) / 1000  # Convert to tonnes
  joined_data$nitrogen_reduction_per_cell_kg <- (joined_data$biomass_per_cell_tonnes * 1000 / 1000) * nitrogen_per_1000kg_oyster_kg
  
  # Calculate Reef Credit value (Reef Credit value = $100 per kg of nitrogen)
  joined_data$reef_credit_value <- joined_data$nitrogen_reduction_per_cell_kg * 100  # $100 per kg of nitrogen
  
  # Calculate market value for oyster biomass ($11,468.31 per tonne)
  joined_data$market_value <- joined_data$biomass_per_cell_tonnes * oyster_value_per_tonne
  
  # Summarize results for each NRM region
  nrm_summary <- joined_data %>%
    group_by(NRM_REGION) %>%
    summarize(
      total_sites = n(),
      total_area_km2_suitable = round((n() * cell_area_m2) / 1e6, 2),  # Total suitable area in km²
      total_bivalves = round(sum(bivalves_per_cell, na.rm = TRUE), 2),
      total_biomass_tonnes = round(sum(biomass_per_cell_tonnes, na.rm = TRUE), 2),  # Biomass in tonnes
      total_nitrogen_reduction_kg = round(sum(nitrogen_reduction_per_cell_kg, na.rm = TRUE), 2),
      total_reef_credit_value = round(sum(reef_credit_value, na.rm = TRUE), 2),
      total_market_value = round(sum(market_value, na.rm = TRUE), 2)  # Total market value of biomass
    ) %>%
    st_drop_geometry() %>%
    na.omit() %>%
    ungroup()
  
  # Ensure all specified NRM regions appear, even with zero results
  nrm_summary_full <- tibble(NRM_REGION = nrm_included) %>%
    full_join(nrm_summary, by = "NRM_REGION") %>%
    mutate(
      total_sites = replace_na(total_sites, 0),
      total_area_km2_suitable = replace_na(total_area_km2_suitable, 0),
      total_bivalves = replace_na(total_bivalves, 0),
      total_biomass_tonnes = replace_na(total_biomass_tonnes, 0),
      total_nitrogen_reduction_kg = replace_na(total_nitrogen_reduction_kg, 0),
      total_reef_credit_value = replace_na(total_reef_credit_value, 0),
      total_market_value = replace_na(total_market_value, 0)
    )
  
  # Save the summary to a CSV file
  write.csv(as.data.frame(nrm_summary_full), output_summary_csv, row.names = FALSE)
  
  # Reproject suitable points back to WGS 84 (long/lat)
  joined_data_wgs84 <- st_transform(joined_data, crs = 4326)
  
  # Save coordinates data in WGS 84
  coordinates_data <- joined_data_wgs84 %>%
    mutate(longitude = round(st_coordinates(.)[, 1], 6),  # Long/Lat rounded to 6 decimal places
           latitude = round(st_coordinates(.)[, 2], 6)) %>%
    select(NRM_REGION, longitude, latitude) %>%
    na.omit()
  write.csv(coordinates_data, output_coordinates_csv, row.names = FALSE)
  
  # Save the equal-area raster
  writeRaster(equal_area_raster, output_raster, overwrite = TRUE)
  
  return(list(summary_csv = output_summary_csv, coordinates_csv = output_coordinates_csv))
}

# Set files & Values, Run Function
calculate_site_data_equal_area(
  raster_file = "final_1km_longlat/combined_binary_all_years.tiff",
  nrm_shapefile = "Regions/Natural_Resource_Management_Regions.shp",
  output_summary_csv = "Results/output_summary.csv",
  output_coordinates_csv = "Results/output_coordinates.csv",
  output_raster = "Results/output_raster_equal_area.tif",
  bivalves_per_m2 = 131, # Gentry et al
  oyster_weight_kg = 0.0754, # Rennie et al
  nitrogen_per_1000kg_oyster_kg = 1.2, # Rennie et al
  oyster_value_per_tonne = 11468.31 # NSW Dept Primary Industries
)
