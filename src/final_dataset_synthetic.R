# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Processing data for synthetic control estimation
# 
# @Description: In the last programs, we got the land use coverage data from each REDD+ project
# and each property that the project is inside (also for all conservation units in the Legal
# Amazon plus Goias state. After converting the data into a tidy data, panel format 
# in 'scr/preparation_synthetic_control.R' we now want to merge dataset and create treatment
# variables.
# 
# @Summary: This script intends to
#   1 - Merge datasets 
#   2 - create treatment datasets
# 
# @Date: fev 2024
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import datasets created on 'preparation_synthetic_control.R' and on 'create_main_shapefiles.R'
buffer_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "buffer_percentage.csv"))

conservation_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "conservation_percentage.csv"))

intersections_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "intersections_percentage.csv"))

redd_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "redd_valid_percentage.csv"))

property_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "properties_percentage.csv"))

property_project <-
  sf::st_read(here::here("results", "merge_property_data", "property_geometry.gpkg"))

projects <- 
  sf::st_read(here::here("results", "base_afolu_complete", "base_afolu_complete.gpkg"))


# ============================================================================================
# II: Process data
# ============================================================================================



# ============================================================================================
# III: Save processed data
# ============================================================================================