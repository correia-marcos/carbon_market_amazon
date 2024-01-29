# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Cut the Brazilian data, keeping only coverage in the Legal Amazon
# 
# We already downloaded the Cover and Land Use Maps of Brazil from MAPBIOMAS, 
# (https://brasil.mapbiomas.org/colecoes-mapbiomas/), 2000-2022. 
# We now want to select the part of the .tiff files that corresponds to the
# area of Legal Amazon, the base location of this project.
# 
# This script was implemented to:
#   1 - Open all .tiff files of the Cover and land Use and crop it
# 
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "dplyr", "sf", "sp", "raster", "readxl", "ggplot2")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import IBGE dataset of Legal Amazon
legal_amazon <- sf::st_read("Data/Amazon_info/Legal_Amazon(unique)/")

# Create  a list of years we have data on - coverage
years <- 2000:2022

# Loop through the years
for (year in years) {
  # Construct the file name
  file_name <- paste0("brasil_coverage_", year, ".tif")
  
  # Open the "tiff" file
  raster_data <- terra::rast(paste0("Data/Deforestation/", file_name))
  
  # Mask the raster using the legal_amazon shapefile
  raster_data_legal_amazon <- terra::crop(raster_data,
                                          terra::vect(legal_amazon))
  
  # Save the new cropped ".tiff" file
  writeRaster(raster_data_legal_amazon, paste0("Data/Deforestation/",
                                               "brasil_coverage_legal_amazon_",
                                               year,
                                               ".tif"),
              overwrite = TRUE)
}
