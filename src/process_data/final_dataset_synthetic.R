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
source(here::here("src", "config", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import datasets created on 'preparation_synthetic_control.R' and on 'create_main_shapefiles.R'
buffer_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "buffer_percentage.csv"))

conservation_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "conservation_percentage.csv")) %>% 
  mutate(Total_natural_formation = Forest_pctg + Non_Forest_Natural_pctg, .after = year)

intersections_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "intersections_percentage.csv"))

redd_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "redd_percentage.csv"))

redd_valid_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "redd_valid_percentage.csv"))

property_percentage <- 
  readr::read_csv(here::here("results", "preparation_synthetic",
                             "properties_percentage.csv"))

projects <- 
  sf::st_read(here::here("results", "base_afolu_complete", "base_afolu_complete.gpkg")) %>%
  filter(afolu == "REDD")

conservation_units <- 
  sf::st_read(here::here("results", "conservation_units", "conservation_units.gpkg"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function from 'src/config/config_utils.R' to merge datasets
merged_buffer        <- merge_data_synthetic(land_coverage_df = buffer_percentage,
                                             information_df = projects,
                                             column_by = "id_registry")

merged_conservation  <- merge_data_synthetic(land_coverage_df = conservation_percentage,
                                             information_df = conservation_units,
                                             column_by = "id")

merged_intersections <- merge_data_synthetic(land_coverage_df = intersections_percentage,
                                             information_df = projects,
                                             column_by = "id_registry")

merged_properties    <- merge_data_synthetic(land_coverage_df = property_percentage,
                                             information_df = projects,
                                             column_by = "id_registry")

merged_redd          <- merge_data_synthetic(land_coverage_df = redd_percentage,
                                             information_df = projects,
                                             column_by = "id_registry")

merged_redd_valid    <- merge_data_synthetic(land_coverage_df = redd_valid_percentage,
                                             information_df = projects,
                                             column_by = "id_registry")

# Apply function from 'src/config/config_utils.R' to get a treatment columns
final_buffer        <- create_treatment_columns(df = merged_buffer,
                                                years_antecipation = 2)

final_intersections <- create_treatment_columns(df = merged_intersections,
                                                years_antecipation = 2)

final_properties    <- create_treatment_columns(df = merged_properties,
                                                years_antecipation = 2)

final_redd          <- create_treatment_columns(df = merged_redd,
                                                years_antecipation = 2)

final_redd_valid    <- create_treatment_columns(df = merged_redd_valid,
                                                years_antecipation = 2)

# Get the projects already treated of APD type
projects_apd_treated <- projects %>% 
  filter(redd_type == "APD") %>% 
  filter(!is.na(certification_date)) %>% 
  pull(id_registry)

print(projects_apd_treated)

# Apply function from 'src/config/config_utils.R' to create dataframes for each APD above
verra_1147 <- get_treatment_apd_cu(projects_id = projects_apd_treated[1],
                                   merged_project = merged_redd_valid,
                                   deforestation_criterion = 0.2, # *100 = percentage
                                   year = 2010)

verra_1382 <- get_treatment_apd_cu(projects_id = projects_apd_treated[2],
                                   merged_project = merged_redd_valid,
                                   deforestation_criterion = 0.2, # *100 = percentage
                                   year = 2010)

verra_2551 <- get_treatment_apd_cu(projects_id = projects_apd_treated[3],
                                   merged_project = merged_redd_valid,
                                   deforestation_criterion = 0.2, # *100 = percentage
                                   year = 2010)

verra_832 <- get_treatment_apd_cu(projects_id = projects_apd_treated[4],
                                  merged_project = merged_redd_valid,
                                  deforestation_criterion = 0.2, # *100 = percentage
                                  year = 2010)

# ============================================================================================
# III: Save processed data
# ============================================================================================
readr::write_csv(
  x            = final_buffer,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/final_buffer.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = final_intersections,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/final_intersections.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = final_properties,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/final_properties.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = final_redd,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/final_redd.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = final_redd_valid,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/final_redd_valid.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = verra_1147,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/apd_1147_conservation.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = verra_1382,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/apd_1382_conservation.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = verra_2551,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/apd_2551_conservation.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = verra_832,
  file         = paste0(here::here("results", "final_dataset_synthetic"),
                        "/apd_832_conservation.csv"),
  na           = "NA",
  col_names    = TRUE)
