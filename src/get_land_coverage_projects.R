# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Collect  land coverage information on the REDD+ properties 
# 
# @Description: With outputs of 'merge_property_data.R' we created a sf dataframe that contains
# all properties that have REDD+ projects inside. With outputs of 'create_main_shapefiles.R' we
# also have a spatial dataset of all REDD+ projects available in Verra website. Now we use those
# datasets and extract land coverage information of raster files using all base of raster files.
# Raster consists of MAPBIOMAS Land Cover and User in Brazil, version 8. See:
# https://plataforma.brasil.mapbiomas.org
# 
# @Summary: This script intends to
#   1 - Open each raster file and extract its values based on dataframe geometries
#   2 - Create a panel datasets containing the land coverage values from 2000 to 2022
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Load the required datasets
projects                  <- sf::st_read(here::here("results",
                                                    "base_afolu_complete",
                                                    "base_afolu_complete.gpkg"))

land_tenure_intersections <- sf::st_read(here::here("results",
                                                    "merge_property_data",
                                                    "intersection_geometry.gpkg"))

land_tenure_property      <- sf::st_read(here::here("results",
                                                    "merge_property_data",
                                                    "property_geometry.gpkg"))

conservation_units        <- sf::st_read(here::here("results",
                                                    "conservation_units",
                                                    "conservation_units.gpkg"))

# Make a list of raster files and read them
raster_files <- list.files(path       = here::here("data", "Land_coverage"),
                           pattern    = "^brasil_coverage_legal_amazon_[0-9]{4}\\.tif$",
                           full.names = TRUE)

# ============================================================================================
# II: Process data
# ============================================================================================

# Run this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Change projects data frame column name
projects <- projects %>% 
  rename(id = id_registry)

# Filter for REDD+ projects make 
projects_redd <- projects %>%
  filter(project_type == "REDD+")

# Make spatial data valid
projects_redd_extracted <- projects_redd %>% 
  sf::st_make_valid() %>% 
  sf::st_collection_extract(type = "POLYGON")

land_tenure_intersections <- land_tenure_intersections %>%
  sf::st_make_valid() %>% 
  sf::st_collection_extract(type = "POLYGON")

land_tenure_property <- land_tenure_property %>%
  sf::st_make_valid() %>% 
  sf::st_collection_extract(type = "POLYGON")

conservation_units <- conservation_units %>% 
  sf::st_make_valid() %>% 
  sf::st_collection_extract(type = "POLYGON")

# Compiling function so it runs faster
compiled_coverage_fun <- compiler::cmpfun(get_coverage_properties)

# Applying function
land_coverage_redd          <- compiled_coverage_fun(list_of_rast_files = raster_files,
                                                     sf_dataframe = projects_redd)

land_coverage_redd_extract  <- compiled_coverage_fun(list_of_rast_files = raster_files,
                                                     sf_dataframe = projects_redd_extracted)

land_coverage_car           <- compiled_coverage_fun(list_of_rast_files = raster_files,
                                                     sf_dataframe = land_tenure_property)

land_coverage_intersections <- compiled_coverage_fun(list_of_rast_files = raster_files,
                                                     sf_dataframe = land_tenure_intersections)

land_coverage_conservation  <- compiled_coverage_fun(list_of_rast_files = raster_files,
                                                     sf_dataframe = conservation_units)


# ============================================================================================
# III: Save processed data
# ============================================================================================
# Save files
readr::write_csv(
  x            = land_coverage_car,
  file         = paste0(here::here("results", "land_coverage"), "/full_property.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")

readr::write_csv(
  x            = land_coverage_redd_extract,
  file         = paste0(here::here("results", "land_coverage"), "/redd_projects_divided.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")

readr::write_csv(
  x            = land_coverage_redd,
  file         = paste0(here::here("results", "land_coverage"), "/redd_projects.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")

readr::write_csv(
  x            = land_coverage_intersections,
  file         = paste0(here::here("results", "land_coverage"), "/intersections.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")

readr::write_csv(
  x            = land_coverage_conservation,
  file         = paste0(here::here("results", "land_coverage"), "/conservation_units.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")
