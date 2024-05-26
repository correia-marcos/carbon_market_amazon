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

# ============================================================================================
# II: Process data
# ============================================================================================
# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

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

# Get id of treated projects
treated_projects <- projects %>%
  sf::st_drop_geometry() %>%
  filter(treatment == 1)

donor <- projects %>% 
  sf::st_drop_geometry() %>%
  filter(treatment == 0)

# Convert to vector 
treated_ids <- as.vector(treated_projects$id_registry)
  
# Create vector of area information
vector_before <- c(Variable = "polygon area", 
                   `Before Treatment - Registered` = mean(treated_projects$polygon_area))

vector_after <- c(Variable = "polygon area", 
                  `After Treatment - Registered` = mean(treated_projects$polygon_area))

vector_donor <- c(Variable = "polygon area", 
                  `Potential Donor Pool - Under Development` = mean(donor$polygon_area))

# Calculate average for each group
averages_before_treatment <- final_redd %>%
  filter(id_registry %in% treated_ids) %>%
  filter(treatment == 0) %>%
  summarize(across(c(Total_natural_formation,
                     Forest_pctg,
                     Non_Forest_Natural_pctg, 
                     Farming_pctg, 
                     Non_vegetated_area_pctg, 
                     Water_pctg, 
                     Not_Observed_pctg), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable",
               values_to = "Before Treatment - Registered") %>%
  rbind(., vector_before)
  


averages_after_treatment <- final_redd %>%
  filter(id_registry %in% treated_ids) %>%
  filter(treatment == 1) %>%
  summarize(across(c(Total_natural_formation, 
                     Forest_pctg, 
                     Non_Forest_Natural_pctg, 
                     Farming_pctg, 
                     Non_vegetated_area_pctg, 
                     Water_pctg, 
                     Not_Observed_pctg), 
                   \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "Variable",
               values_to = "After Treatment - Registered") %>% 
  rbind(., vector_after)

averages_potential_donor_pool <- final_redd %>%
  filter(!id_registry %in% treated_ids) %>% 
  summarize(across(c(Total_natural_formation, 
                     Forest_pctg,
                     Non_Forest_Natural_pctg, 
                     Farming_pctg,
                     Non_vegetated_area_pctg, 
                     Water_pctg, 
                     Not_Observed_pctg),
                   \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "Variable",
               values_to = "Potential Donor Pool - Under Development") %>% 
  rbind(., vector_donor)

# Combine results
averages_table <- full_join(averages_before_treatment, 
                            averages_after_treatment, 
                            by = "Variable") %>%
  full_join(., 
            averages_potential_donor_pool, 
            by = "Variable")

# Round values
averages_table <- averages_table %>%
  mutate(`Before Treatment - Registered` = 
           as.numeric(`Before Treatment - Registered`)) %>% 
  mutate(`After Treatment - Registered` = 
           as.numeric(`After Treatment - Registered`)) %>% 
  mutate(`Potential Donor Pool - Under Development` = 
           as.numeric(`Potential Donor Pool - Under Development`)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))


# Generate latex table
latex_table <- xtable(averages_table)

latex_table
# ============================================================================================
# III: Save processed data
# ============================================================================================
write.csv(latex_table,
          here::here("results", "tables", "synthetic_mean.csv"), 
          row.names = FALSE)
