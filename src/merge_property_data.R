# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Merge land tenure - Cadastro Ambiental Rural (CAR) - information with project info
# 
# @Description: In 'src/create_main_shapefiles', we generated a spatial dataset containing all
# AFOLU projects in Brazil. We want now to collect the CAR info of these projects and merge it
# in a new dataset. CAR is a public registry, mandatory for all rural properties in Brazil.
# by merging both datasets, we will have information of the properties that entered the carbon
# market.
# 
# @Summary: This script intends to
#   1 - Get all properties in the land tenure dataset that have REDD+ properties inside
#   2 - Merge the CAR information with the project information
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Create a list of the states to get CAR information
states <- list.files(here::here("Data", "Atlas_agriculture"))

# Import projects and the csv data
projects <- sf::st_read(here::here("results",
                                   "base_afolu_complete",
                                   "base_afolu_complete.gpkg"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Get only the REDD+ projects and change its CRS and make it valid 
projects_redd <- projects %>%
  filter(afolu == "REDD") %>%
  sf::st_make_valid()

# Applying functions from 'src/config_utils.R'
merge_tenure_geometry <- get_land_tenure_info(
  list_state      = states,
  sf_dataframe    = projects_redd,
  tenure_geometry = TRUE
  )

merge_within_geometry <- get_land_tenure_info(
  list_state      = states,
  sf_dataframe    = projects_redd,
  tenure_geometry = FALSE
  )

conservation_units <- get_conservation_units(
  list_state      = states,
  sf_dataframe    = projects_redd
  )

# Remove FID from datasets since it generates error when saving in GeoPackage format
merge_tenure_geometry <- merge_tenure_geometry %>%
  dplyr::select(!FID)

merge_within_geometry <- merge_within_geometry %>%
  dplyr::select(!FID)

conservation_units <- conservation_units %>%
  dplyr::select(!FID)

# ============================================================================================
# III: Save processed data
# ============================================================================================

# Saving file - we used Shapefile format instead of GeoPackage because of size of datasets
st_write(
  obj    = merge_tenure_geometry,
  dsn    = paste0(here::here("results", "merge_property_data"),
                  "/property_geometry.gpkg"),
  append = FALSE
  )

st_write(
  obj    = merge_within_geometry, 
  dsn    = paste0(here::here("results", "merge_property_data"),
                  "/intersection_geometry.gpkg"),
  append = FALSE,
)

st_write(
  obj    = conservation_units,
  dsn    = paste0(here::here("results", "conservation_units"), # no merge happened
                  "/conservation_units.gpkg"),
  append = FALSE,
)
