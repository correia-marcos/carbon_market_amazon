# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Estimate synthetic controls
# 
# @Description: In the last programs, we created ever information we need for
# the first synthetic control estimation! Now, we attempt to estimate it,
# using scpi library. Please check: 
# https://cran.r-project.org/web/packages/scpi/scpi.pdf
# 
# @summary: This program intends to
#   0 - Create short data processing (merging datasets and create new one) 
#   1 - Create function to apply synthetic controls
#   2 - Create figure of results
# 
# @Date: out 2023
# @author: Marcos


# Get all libraries and functions
source(here::here("src", "config_utils.R"))


# Import
final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

projects <- 
  sf::st_read(here::here("results", "base_afolu_complete", "base_afolu_complete.gpkg")) %>%
  filter(afolu == "REDD")


land_tenure_property      <- sf::st_read(here::here("results",
                                                    "merge_property_data",
                                                    "property_geometry.gpkg"))

