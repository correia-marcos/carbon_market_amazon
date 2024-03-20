# ============================================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# ============================================================================================
# Goal: Download necessary files from MAPBIOMAS
# 
# @Description: Simple download of raster files from MAPBIOMAS.
# See https://brasil.mapbiomas.org/colecoes-mapbiomas/
# 
# @Summary: This script intends to
#   1 - Download raster files from 2000 to 2022
# 
# @Date: fev 2024
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Process data
# ===========================================================================================
download_mapbiomas(2021, 2022)