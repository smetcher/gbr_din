MARINE ENVIRONMENTAL DATA ANALYSIS

**Description**

This repository contains R scripts designed to automate the extraction,
processing, and analysis of marine environmental data. The workflow
transforms raw multi-dimensional NetCDF data into standardized GeoTIFF
rasters for use in GIS software or statistical modeling.

The primary goal is to flatten oceanographic data containing multiple
variables across various depths and time steps into daily, depth-specific
files.

**REPOSITORY STRUCTURE**

Step 1 Get the Data.R     : Primary script for data ingestion.
Step 2 Combine Depths.R   : Script for averaging depths and years.
Step 3 Binary Threshold.R : Script for reclassifying data to binary maps.
Step 4 Final Suitability.R: Script to combine all binary layers.
Step 5 Resample.R         : Script to increase spatial resolution.
Step 6 Final Constraints.R: Script to apply GIS and regional masks.
Step 7 Results and Value.R: Script for biomass and economic valuation.
file_urls.txt             : Configuration file for source data NetCDF URLs

**DATA EXTRACTION AND PREPROCESSING**

**SCRIPT: Step 1 Get the Data.R**

This script serves as the entry point. It automates data retrieval and
handles spatial coordinate mapping and depth-layer slicing.

KEY FUNCTIONALITY:
Automated URL Parsing: Reads sources from file_urls.txt.
Depth-Layer Slicing: Extracts specific vertical layers.
Directory Management: Organizes output by variable, year, and month.

DEPTH MAPPING:
The script currently targets the following depth indices:
Index 13: -8.80m
Index 14: -5.55m
Index 15: -3.00m
Index 16: -1.50m
Index 17: -0.50m

PREREQUISITES:
You will need R installed with the following packages:
RNetCDF, raster, stringr, readr

USAGE:

Populate file_urls.txt with NetCDF paths (one per line).

Ensure the variables vector includes your target parameters
(default: Oxygen, temp, salt, Chl_a_sum).

Run the script: source("Step 1 Get the Data.R")

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Time Origin: Assumes a NetCDF base date of 1990-01-01.


**SCRIPT: Step 2 Combine the mean Depths.R**

This script processes the daily rasters generated in Step 1 to create
integrated environmental layers.

KEY FUNCTIONALITY:
Depth Averaging: For every day, it calculates the mean value across the
water column using depths 13 through 17 (-8.8m to -0.5m).
Annual Mean Calculation: Aggregates all daily depth-averaged rasters into
a single mean raster for each calendar year.

PREREQUISITES:
R packages: raster, dplyr

USAGE:

Ensure the env_data folder exists and contains the Step 1 output.

Run: source("Step 2 Combine Depths.R")

Output will be saved in the env_data_annual directory.

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Time Origin: Assumes a NetCDF base date of 1990-01-01.
Error Handling: Step 2 skips days where all 5 depth layers are not present.

**SCRIPT: Step 3 Binary annual.R**

This script reclassifies the annual mean data into binary maps (1 or 0)
based on specific environmental thresholds.

KEY FUNCTIONALITY:
Binary Reclassification: Pixels within the threshold range are assigned
a value of 1 (suitable), while those outside are assigned 0 (unsuitable).
NA Preservation: Missing data (NA) values are maintained to ensure
consistent spatial masking.

CURRENT THRESHOLDS:
Chlorophyll a: 0.22 to Infinity
Temperature: 20 to 32 degrees
Salinity: 10 to 38
Oxygen Saturation: 1.99 to Infinity

PREREQUISITES:
R packages: raster

USAGE:

Ensure the env_data_annual folder contains files from Step 2.

Adjust the thresholds list in the script if necessary.

Run: source("Step 3 Binary Threshold.R")

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Time Origin: Assumes a NetCDF base date of 1990-01-01.
Error Handling: Step 2 skips days where all 5 depth layers are not present.

**SCRIPT: Step 4 Binary Combined.R**

This final script combines all binary layers across all years and all
environmental variables into a single cumulative suitability map.

KEY FUNCTIONALITY:
Multi-Layer Stacking: Compiles every binary TIFF generated in Step 3.
Boolean Logic: Employs an ALL-OR-NOTHING approach. A cell is assigned
a value of 1 only if it meets suitability requirements for ALL variables
across ALL years processed;
Rigorous Suitability: If even one variable in one year fails the
threshold at a specific pixel, that pixel is marked 0.

PREREQUISITES:
R packages: raster

USAGE:

Ensure the env_data_binary_restrained folder contains the Step 3 output.

Run: source("Step 4 Final Suitability.R")

The final result is saved as combined_binary_all_years.tiff in the
env_data_hsi directory.

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Time Origin: Assumes a NetCDF base date of 1990-01-01.
Error Handling: The workflow preserves NAs across all steps to prevent
land-mass or missing-data interference.

**SCRIPT: Step 5 Resolution to 1km.R**

This script increases the spatial resolution of the final suitability
maps to match higher resolution project requirements.

KEY FUNCTIONALITY:
Resolution Upscaling: Uses the disaggregate function to transform the
base 4km resolution (standard for eReefs data) into 1km resolution.
Recursive Processing: Automatically finds all TIFF files in the input
directory and its subdirectories.
Structure Mirroring: Recreates the exact input folder hierarchy in the
output folder.

PREREQUISITES:
R packages: raster

USAGE:

Set the input directory to the folder containing your final HSI maps.

Run: source("Step 5 Resample.R")

High-resolution files are saved in the env_data_hsi_1km directory.

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Resolution Change: Disaggregation factor of 4 (4km to 1km).
Time Origin: Assumes a NetCDF base date of 1990-01-01.

**SCRIPT: Step 6 Remaining Restraints.R**

This final stage applies logical spatial masks to the suitability maps
based on bathymetry, human activity, and conservation zones.

KEY FUNCTIONALITY:
Bathymetric Masking: Restricts results to depths between 9m and 200m.
Shipping Exclusion: Removes areas with Cargo, Tanker, Military, or
Sailing vessel traffic.
Geographic Features: Masks out Reefs and Islands.
Conservation Exclusion: Masks areas with IUCN protected status codes Ia,
Ib, II, III, and IV.
NRM Regional Filtering: Limits the final output to specific Natural
Resource Management regions (e.g., Burdekin, Wet Tropics, Fitzroy).

PREREQUISITES:
R packages: raster, sf, dplyr
External Data: Requires bathymetry .grd and various .shp shapefiles.

USAGE:

Ensure all required shapefiles are in the specified directories.

Run: source("Step 6 Final Constraints.R")

The final constrained suitability maps are saved to final_1km_longlat.

TECHNICAL NOTES
Coordinate System: WGS84 (EPSG:4326).
Output Format: GeoTIFF (.tiff).
Spatial Masking Logic: Step 6 uses overlay and rasterize to turn
unsuitable or excluded areas to 0.

**SCRIPT: Step 7 Equal Area Project and reduction calculations.R**

This final script performs the quantitative and economic analysis of the
suitable sites identified in the previous steps.

KEY FUNCTIONALITY:
Equal-Area Projection: Converts data to Australian Albers (GDA94) to
ensure precise area calculations (m2/km2) across the study region.
Biomass Estimation: Calculates total bivalve counts and biomass (tonnes)
based on stocking densities (131 per m2) and average weights.
Environmental Valuation: Estimates nitrogen reduction (kg) and assigns
a "Reef Credit" value at $100 per kg of nitrogen.
Market Valuation: Calculates total market value of oyster biomass based
on current industry pricing ($11,468.31 per tonne).
Regional Summaries: Generates statistics per NRM region, including total
sites, area, and total economic potential.

PREREQUISITES:
R packages: raster, sf, dplyr, tidyr

USAGE:

Ensure the final constrained raster from Step 6 is available.

Run: source("Step 7 Results and Value.R")

Check the /Results/ folder for output_summary.csv and coordinates.csv.

TECHNICAL NOTES
Initial CRS: WGS84 (EPSG:4326).
Equal-Area CRS: Australian Albers (+proj=aea).
Output Format: GeoTIFF (.tiff) and CSV (.csv).
Biomass Metrics: Based on Gentry et al. and Rennie et al. research.
