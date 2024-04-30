# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Processing data for synthetic control estimation
# 
# @Description: In the last programs, we got the land use coverage data from each REDD+ project
# and each property that the project is inside (also for all conservation units in the Legal
# Amazon plus Goias state. We now want to converted the data from last last program into a tidy
# data, panel format for synthetic control estimation.
# 
# @Summary: This script intends to
#   1 - Change column positions 
#   2 - Aggregate values
#   3 - Create new columns for aggregated classification
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import datasets created on 'get_land_coverage_projects.R' and on 'create_main_shapefiles.R'
coverage_buffer <- 
  readr::read_csv(here::here("results", "land_coverage", "buffer_projects.csv")) %>%
  dplyr::rename(Not_Observed = "0",
                id_registry = id) # correct name of column in other datasets)

coverage_intersections <-
  readr::read_csv(here::here("results", "land_coverage", "intersections.csv")) %>%
  dplyr::rename(Not_Observed = "0")

coverage_conservation <-
  readr::read_csv(here::here("results", "land_coverage", "conservation_units.csv")) %>%
  dplyr::rename(Not_Observed = "0")

coverage_property <-
  readr::read_csv(here::here("results", "land_coverage", "full_property.csv")) %>%
  dplyr::rename(Not_Observed = "0")

coverage_redd <-
  readr::read_csv(here::here("results", "land_coverage", "redd_projects.csv")) %>%
  dplyr::rename(Not_Observed = "0",
                id_registry = id) # correct name of column in other datasets

coverage_redd_divided <-
  readr::read_csv(here::here("results", "land_coverage", "redd_projects_divided.csv")) %>%
  dplyr::rename(Not_Observed = "0",
                id_registry = id) # correct name of column in other datasets

property_project <-
  sf::st_read(here::here("results", "merge_property_data", "property_geometry.gpkg"))

legend <-
  readr::read_delim(here::here("data", "Legend", "Codigos-da-legenda-colecao-8.csv"),
                    delim = ";") %>% 
  # Removing unnecessary white spaces
  mutate(across(where(is.character), trimws))
# ============================================================================================
# II: Process data
# ============================================================================================
# Apply function from 'src/config_utils.R' to change columns names in all coverage datasets
coverage_buffer <- convert_columns_panel(
  legend_df            = legend,
  column_id            = 'Class_ID',
  column_description   = 'Description',
  land_coverage_df     = coverage_buffer)

coverage_conservation <- convert_columns_panel(
  legend_df            = legend,
  column_id            = 'Class_ID',
  column_description   = 'Description',
  land_coverage_df     = coverage_conservation)

coverage_intersections <- convert_columns_panel(
    legend_df          = legend,
    column_id          = 'Class_ID',
    column_description = 'Description',
    land_coverage_df   = coverage_intersections)

coverage_property <- convert_columns_panel(
    legend_df          = legend,
    column_id          = 'Class_ID',
    column_description = 'Description',
    land_coverage_df   = coverage_property)

coverage_redd <- convert_columns_panel(
  legend_df            = legend,
  column_id            = 'Class_ID',
  column_description   = 'Description',
  land_coverage_df     = coverage_redd)

coverage_redd_divided <- convert_columns_panel(
  legend_df            = legend,
  column_id            = 'Class_ID',
  column_description   = 'Description',
  land_coverage_df     = coverage_redd_divided)


# Apply function from 'src/config_utils.R' to summarize values
coverage_intersections <- agg_coverage_values(land_coverage_df    = coverage_intersections,
                                              property_project_df = property_project)
coverage_redd         <- agg_coverage_values(land_coverage_df     = coverage_redd,
                                             property_project_df  = property_project)
coverage_redd_divided <- agg_coverage_values(land_coverage_df     = coverage_redd_divided,
                                             property_project_df  = property_project)
coverage_property     <- agg_coverage_values(land_coverage_df     = coverage_property,
                                             property_project_df  = property_project)
coverage_buffer       <- agg_coverage_values(land_coverage_df     = coverage_buffer,
                                             property_project_df  = property_project)

# Apply function from 'src/config_utils.R' to get general classes of land use
buffer             <- get_coverage_classes(land_coverage_df = coverage_buffer,
                                           legend_df = legend)
conservation       <- get_coverage_classes(land_coverage_df = coverage_conservation,
                                           legend_df = legend)
intersections      <- get_coverage_classes(land_coverage_df = coverage_intersections,
                                           legend_df = legend)
properties         <- get_coverage_classes(land_coverage_df = coverage_property,
                                           legend_df = legend)
redd               <- get_coverage_classes(land_coverage_df = coverage_redd,
                                           legend_df = legend)
redd_valid         <- get_coverage_classes(land_coverage_df = coverage_redd_divided,
                                           legend_df = legend)

# Apply function from 'src/config_utils.R' to get general classes of land use
buffer_percentage        <- calculate_land_percentages(buffer)
conversation_percentage  <- calculate_land_percentages(conservation)
intersections_percentage <- calculate_land_percentages(intersections)
properties_percentage    <- calculate_land_percentages(properties)
redd_percentage          <- calculate_land_percentages(redd)
redd_valid_percentage    <- calculate_land_percentages(redd_valid)

# ============================================================================================
# III: Save processed data
# ============================================================================================
# Saving results
readr::write_csv(
  x            = coverage_intersections,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/intersections_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = coverage_buffer,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/buffer_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = coverage_conservation,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/conservation_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = coverage_property,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/property_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = coverage_redd,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = coverage_redd_divided,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_valid_subclasses.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = buffer,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/buffer_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = buffer_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/buffer_percentage.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = conservation,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/conservation_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = conversation_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/conservation_percentage.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = intersections,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/intersections_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = intersections_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/intersections_percentage.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = redd,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = redd_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_percentage.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = redd_valid,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_valid_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = redd_valid_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/redd_valid_percentage.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = properties,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/properties_raw.csv"),
  na           = "0",
  col_names    = TRUE)

readr::write_csv(
  x            = properties_percentage,
  file         = paste0(here::here("results", "preparation_synthetic"),
                        "/properties_percentage.csv"),
  na           = "0",
  col_names    = TRUE)
