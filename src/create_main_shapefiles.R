# ============================================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# ============================================================================================
# @Goal: Construct a single dataset of shapefiles 
# 
# @Description: We create a dataset with virtually all carbon offsets in Brazil. Afterwards, we
# took all KML files from Verra Registry (https://registry.verra.org/), open then in QGIS
# software in order to repair issues (mostly merge layers and join shapes), and converted the
# files into shapefile format. Now, we want to add all projects together in a single dataframe
# containing, also, the previous created dataset from 'src/collect_issuance.csv'
# 
# @Summary: This program intends to
#   1 - Create a dataframe all projects shapefiles 
#   2 - Merge this dataframe with the previous created csv file
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import csv file previous created on 'src/collect_issuance.csv'
data <- readr::read_csv(here::here("results", "projects_base", "base_projects.csv"))

# List of Shapefiles projects
shp_files <- list.files(here::here("data", "Projects_info", "Individual_shp"),
                        full.names = TRUE)

# ============================================================================================
# II: Process data
# ============================================================================================
# Create an empty data frame to store the data
sf_dfs <- tibble::tibble()

# Loop through each KML file, read the data, and bind it to the data frame
for (project in shp_files) {
  # Read KML file
  shp_data <- sf::st_read(project) %>%
    dplyr::select(geometry) # We only need the geometry column

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

# Filter dataset for only registered projects
data_registered <- data_agg %>%
  filter(project_status %in% c("Registered", "On Hold"))

# ============================================================================================
# III: Save processed data
# ============================================================================================
# Save as GeoPackage the last dataframes created
st_write(
  obj    = data_agg,
  dsn    = paste0(here::here("results", "base_afolu_complete"),
                  "/base_afolu_complete.gpkg"),
  append = FALSE
  )

st_write(
  obj    = data_agg,
  dsn    = paste0(here::here("results", "base_afolu_registered"),
                  "/base_afolu_registered.gpkg"),
  append = FALSE
)
