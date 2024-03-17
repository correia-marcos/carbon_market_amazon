# ============================================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# ============================================================================================
# @Goal: Cut the Brazilian data, keeping only coverage in the Legal Amazon
# 
# @Description: We already downloaded the Cover and Land Use Maps of Brazil from MAPBIOMAS,
# (https://brasil.mapbiomas.org/colecoes-mapbiomas/), 2000-2022. We now want to select the part
# of the .tiff files that corresponds to the area of Legal Amazon, the base location of this
# project.
# 
# @Summary: This program intends to
#   1 - Open all .tiff files of the Cover and land Use and crop it
#   2 - Save new cropped raster files
# 
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I-III: Import data, process and save
# ============================================================================================
# Import IBGE dataset of Legal Amazon
legal_amazon <- sf::st_read(here::here("Data", "Amazon_info", "Legal_Amazon(unique)"))

# Create  a list of years we have data on - coverage
years <- 2000:2022

crop_raster_files(list_years     = years,
                  crop_extension = legal_amazon)
