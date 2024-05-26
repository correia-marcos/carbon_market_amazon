# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Create tables of content for the dissertation text
# 
# @Description: This program uses the results from 'src/process_data'. Before running this code,
# make sure that the results have already been generated. We wish to collect some important
# information already generated to prepare tables for the text of the dissertation.
# 
# @summary: This program intends to
#   1 - Create table to show all important info of projects
# 
# 
# @Date: out 2023
# @author: Marcos


# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# Get specific heavy packages for tables
groundhog.library(pkg  = c("xtable", "units", "knitr"),
                  date = "2024-03-03")

# ============================================================================================
# I: Import data
# ============================================================================================
final_redd           <- readr::read_csv(here::here("results", 
                                                   "final_dataset_synthetic", 
                                                   "final_redd_valid.csv"))

projects             <- sf::st_read(here::here("results", 
                                               "base_afolu_complete", 
                                               "base_afolu_complete.gpkg"))

land_tenure_property <- sf::st_read(here::here("results",
                                               "merge_property_data",
                                               "intersection_geometry.gpkg"))
# ============================================================================================
# II: Process data
# ============================================================================================
# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Group by desc_class and list all unique desc_subclass for each desc_class
subclass_by_class <- land_tenure_property %>%
  sf::st_drop_geometry() %>%
  group_by(desc_class) %>%
  summarise(subclasses = list(unique(desc_subclass)), .groups = 'drop')

public_subclass  <- subclass_by_class$subclasses[3][[1]]
private_subclass <- subclass_by_class$subclasses[2][[1]]

# Make projects valid
projects <- projects %>% 
  st_make_valid()

# Add columns, remove ARR projects and arrange dataset
projects <- projects %>% 
  filter(project_type == "REDD+") %>% 
  mutate(treatment = ifelse(project_status %in% c("Registered", "On Hold"), 1, 0),
         .after = project_name) %>% 
  mutate(polygon_area = sf::st_area(geom), .after = treatment) %>%
  mutate(polygon_area = set_units(polygon_area, hectares)) %>%
  mutate(polygon_area = as.numeric(drop_units(polygon_area))) %>% 
  arrange(desc(treatment))

# Divided by treated
projects_treated <- projects %>%
  filter(treatment == 1)

# Create new dataframe by grouping all public area intersection information for each project
public_areas <- land_tenure_property %>% 
  filter(desc_class == "Áreas Públicas") %>%
  mutate(area_hectares = st_area(geom)) %>%
  mutate(area_hectares = set_units(area_hectares, hectares)) %>%
  mutate(area_hectares = as.numeric(drop_units(area_hectares))) %>% 
  group_by(id_registry) %>%
  summarise(total_public_area_ha = sum(area_hectares, na.rm = TRUE)) %>%
  sf::st_drop_geometry()


# Join dataframes
projects <- projects %>%
  left_join(public_areas, by = "id_registry") %>%
  mutate(percentage_public_area = 100 * (total_public_area_ha / polygon_area),
         .after = polygon_area)  

# Selecting columns for table
projects_table <- projects %>%
  sf::st_drop_geometry() %>%
  dplyr::select(id_registry, project_name, state, methodology_version, treatment,
         start, end, certification_date, redd_type, polygon_area, percentage_public_area, 
         area_hectares)

# Making sure dates are in the correct format
projects_table$start <- format(projects_table$start, "%Y-%m-%d")
projects_table$end <- format(projects_table$end, "%Y-%m-%d")
projects_table$certification_date <- format(projects_table$certification_date, "%Y-%m-%d")

# Create Latex table with knitr package
latex_table <- kable(projects_table, format = "latex", 
                     caption = "Summary of Project Information", 
                     align = c('l', 'l', 'l', 'l', 'c', 'l', 'l', 'l', 'r', 'r', 'l', 'r'),
                     booktabs = TRUE)

print(latex_table)
# ============================================================================================
# III: Save processed data
# ============================================================================================
write.csv(latex_table,
          here::here("results", "tables", "all_projects_latex.csv"), 
          row.names = FALSE)

write.csv(projects_table, 
          here::here("results", "tables", "projects_table.csv"), 
          row.names = FALSE)