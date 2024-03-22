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

# ============================================================================================
# I: Import data
# ============================================================================================
final_buffer <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_buffer.csv"))

final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

final_redd <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

final_properties <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_properties.csv"))

final_intersections <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_intersections.csv"))

verra_832_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_832_conservation.csv"))

verra_1147_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_1147_conservation.csv"))

verra_1382_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_1382_conservation.csv"))

verra_2551_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_2551_conservation.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function from 'src/config_utils.R' to get filter data to create individual dataframes
create_treatment_dfs(final_redd_valid)

