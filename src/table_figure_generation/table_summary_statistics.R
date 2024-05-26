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
#   1 - Create table of descriptive statistics 
# 
# 
# @Date: out 2023
# @author: Marcos


# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# Get specific heavy packages for tables
groundhog.library(pkg  = c("xtable", "units"),
                  date = "2024-03-03")

# ============================================================================================
# I: Import data
# ============================================================================================
# Import required data
final_redd_valid     <- readr::read_csv(here::here("results",
                                               "final_dataset_synthetic",
                                               "final_redd_valid.csv"))

land_tenure_property <- sf::st_read(here::here("results",
                                               "merge_property_data",
                                               "property_geometry.gpkg"))

projects             <- sf::st_read(here::here("results", 
                                               "base_afolu_complete", 
                                               "base_afolu_complete.gpkg")) %>% 
  filter(project_type == "REDD+")

# ============================================================================================
# II: Process data
# ============================================================================================
# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Filter land_tenure_property to remove unwanted rows
private_land_tenure <- land_tenure_property %>%
  filter((desc_class %in% c("Áreas Privadas")))

# Make polygons valid
projects <- projects %>%
  mutate(geom = st_make_valid(geom))

# Calculate number of properties per project
property_count <- private_land_tenure %>%
  sf::st_drop_geometry() %>% 
  group_by(id_registry) %>%
  summarise(property_count = n_distinct(id))

# Create new columns for project dataframe
projects <- projects %>%
  mutate(
    certification_time = as.numeric(difftime(certification_date, start, 
                                             units = "days") / 365.25),
         polygon_area = sf::st_area(geom),
    .after = project_type) %>%
  mutate(polygon_area = set_units(polygon_area, m^2)) %>%
  mutate(polygon_area = set_units(polygon_area, hectares)) %>%
  mutate(polygon_area = as.numeric(drop_units(polygon_area)))

# Generate first descriptive statistics
desc_stats <- final_redd_valid %>%
  dplyr::select(Total_natural_formation,
                Forest_pctg,
                Non_Forest_Natural_pctg,
                Farming_pctg,
                Non_vegetated_area_pctg,
                Water_pctg,
                Not_Observed_pctg) %>%
  summarise(across(c(Total_natural_formation,
                     Forest_pctg,
                     Non_Forest_Natural_pctg, 
                     Farming_pctg,
                     Non_vegetated_area_pctg,
                     Water_pctg,
                     Not_Observed_pctg),
                   list(Mean = ~ mean(., na.rm = TRUE),
                        Median = ~ median(., na.rm = TRUE),
                        `St. Dev` = ~ sd(., na.rm = TRUE),
                        Min = ~ min(., na.rm = TRUE),
                        Max = ~ max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"), 
               names_pattern = "(.*)_(.*)") %>% 
  pivot_wider(names_from = Statistic, values_from = value) %>%
  mutate(Variable = recode(Variable,
                           Total_natural_formation = "Total natural formation",
                           Forest_pctg = "Forest formation",
                           Non_Forest_Natural_pctg = "Non forest natural formation",
                           Farming_pctg = "Farming",
                           Non_vegetated_area_pctg = "Non vegetated area",
                           Water_pctg = "Water",
                           Not_Observed_pctg = "Not observed pixels"),
         Unit = "percentage") %>% 
  dplyr::select(Variable, Unit, Mean, Median, `St. Dev`, Min, Max)


# Calculate descriptive statistics for other variables
additional_stats <- projects %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(polygon_area,
                area_hectares,
                certification_time,
                estimated_annual_emission_reductions,
                document_size_pages) %>%
  summarise(across(everything(),
                   list(Mean = ~ mean(., na.rm = TRUE),
                        Median = ~ median(., na.rm = TRUE),
                        `St. Dev` = ~ sd(., na.rm = TRUE),
                        Min = ~ min(., na.rm = TRUE),
                        Max = ~ max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), 
               names_to = c("Variable", "Statistic"), 
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = Statistic, values_from = value) %>%
  mutate(Variable = recode(
    Variable,
    estimated_annual_emission_reductions = "Estimated annual emission reductions",
    area_hectares = "Project reported area",
    document_size_pages = "Document size (pages)",
    certification_time = "Certification time span",
    polygon_area = "Polygon area")) %>%
  mutate(Unit = case_when(
    Variable == "Estimated annual emission reductions" ~ "credits",
    Variable == "Project reported area" ~ "hectares",
    Variable == "Document size (pages)" ~ "count",
    Variable == "Polygon area" ~ "hectares",
    TRUE ~ "years"
  ))


# Calculate descriptive statistics for the last variable
property_stats <- data.frame(Variable = "Number of private properties per project",
                             Unit = "count",
                             Mean = mean(property_count$property_count),
                             Median = median(property_count$property_count),
                             standard = sd(property_count$property_count),
                             Min = min(property_count$property_count),
                             Max = max(property_count$property_count)) %>% 
  rename(`St. Dev` = standard)

# Join dataframes
final_desc_stats <- bind_rows(desc_stats, additional_stats, property_stats)

# Convert the dataframe to LaTeX format
latex_table <- xtable(final_desc_stats, caption = "Summary Statistics of REDD+ projects")

print(latex_table, include.rownames = FALSE, hline.after = NULL, floating = FALSE)


# ============================================================================================
# III: Save processed data
# ============================================================================================
write.csv(latex_table,
          here::here("results", "tables", "final_desc_stats.csv"), 
          row.names = FALSE)
