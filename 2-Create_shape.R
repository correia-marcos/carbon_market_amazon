# =============================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Construct a single dataset of shapefiles 
# 
# We already create a dataset with virtually all carbon offsets in Brazil
# Afterwards, we took all KML files from Verra Registry 
# (https://registry.verra.org/), open then in QGIS software in order to
# repair issues (mostly merge layers and join shapes), and converter the files 
# into shapefile format. Now, we want to add all projects together in a single
# dataframe containing, also, the previous created information collected in an
# Excel file.
# 
# This script was implemented to:
#   1 - Create a dataframe all projects shapefiles 
#   2 - Merge this dataframe with the previous created csv file
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "dplyr", "sf", "raster", "stringr")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import data: csv previous created in the first script
data <- read.csv("Data/Projects_info/base_projects.csv", sep=';',
                 check.names = FALSE)

# List of Shapefiles projects
shp_files <- list.files("Data/Projects_info/Individual_shp/",
                        full.names = TRUE)

# Substituting white spaces per underline
colnames(data) <- gsub(" ", "_", colnames(data))

# Convert data informative columns into Date format
data <- data %>%
  mutate(
    project_start_crediting = as.Date(project_start_crediting,
                                      format = "%m/%d/%Y"),
    project_end_crediting = as.Date(project_end_crediting,
                                    format = "%m/%d/%Y"),
    certification_date = as.Date(certification_date,
                                   format = "%m/%d/%Y")
  )

# Remove white spaces from character columns
data <- data %>%
  mutate_if(is.character, str_trim)

# Create an empty data frame to store the data
sf_dfs <- data.frame()

# Loop through each KML file, read the data, and bind it to the data frame
for (project in shp_files) {
  # Read KML file
  shp_data <- sf::st_read(project) %>%
    dplyr::select(geometry) # We only need those columns

  # Remove Height of coordinates (can create problems)
  new_sf <- st_zm(shp_data, drop=T, what='ZM')
  
  # Extract the numeric code from the file name
  verra_code <- str_extract(project, "(?<=verra_)\\d+")

  # Add the numeric code as a column
  new_sf$id_registry <- as.character(verra_code)
  
  # Bind the data to the final data frame
  sf_dfs <- rbind(sf_dfs, new_sf)
}

# Merge shapefile dataset and the data from csv
data_agg <- left_join(sf_dfs, data, by = "id_registry")

# In order to save shapefile, we must reduce the size of column names
new_colnames <- c("id_registry", "registry", "prj_developer", "prj_name",
                  "prj_type", "credits_sl", "credits_rd", "buffer_tl",
                  "e_a_emissn", "prj_dscpt", "methodology", "state",
                  "prj_status", "doc_size", "gold_std", "vcs", "ccb-silver",
                  "ccb-gold", "ccb-bio", "ccb-cmmnty", "ccb-clima", "ccb-no",
                  "ccb", "ccb_dev", "fsc", "fsc_dev", "social_cb", "cdm",
                  "corsia", "sdg", "michael_gr", "area_ha", "afolu",
                  "redd_type", "pu_ind_lan", "vcs_val", "start", "end",
                  "perid_term", "crtfcation", "legal_issu",
                  "legal_more", "ccb_status", "ccb_type", "ccb_val", "ccb_std",
                  "prp_info", "aud_visit", "n_ver_ccb", "first_ver",
                  "second_ver", "third_ver", "fourth_ver", "fifth_ver",
                  "sixth__ver", "seven_ver", "geometry")

colnames(data_agg) <- new_colnames

# Save the last dataframe created
st_write(data_agg, "Results/base_afolu_complete",
         driver = "ESRI Shapefile",
         append = FALSE, encoding = "UTF-8")
