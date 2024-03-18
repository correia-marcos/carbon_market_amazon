# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Processing data for synthetic control estimation
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
#   4 - Merge datasets
# 
# @Date: out 2023
# @author: Marcos


# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import datasets created on 'get_land_coverage_projects.R' and 'create_main_shapefiles.R'
coverage_property      <- readr::read_csv(here::here("results",
                                                "land_coverage",
                                                "full_property.csv"))

coverage_intersections <- readr::read_csv(here::here("results",
                                                     "land_coverage",
                                                     "intersections.csv"))

coverage_redd          <- readr::read_csv(here::here("results",
                                            "land_coverage",
                                            "redd_projects.csv"))

coverage_redd_divided  <- readr::read_csv(here::here("results",
                                                "land_coverage",
                                                "redd_projects_divided.csv"))

coverage_conservation  <- readr::read_csv(here::here("results",
                                                    "land_coverage",
                                                    "conservation_units.csv"))

projects               <- sf::st_read(here::here("results",
                                   "base_afolu_complete",
                                   "base_afolu_complete.gpkg"))

merge_property_proj    <- sf::st_read(here::here("results",
                                              "merge_property_data",
                                              "property_geometry.gpkg"))

# ============================================================================================
# II: Process data
# ============================================================================================

# Define vectors with column position for coverage dataframes
order_car <- c("id", "year",
               "3", "4", "5", "6", # Forest class
               "11", "12", "29", # Non forest natural class
               "15", "41", "9", "39", "21", "20", "35", "62", # Farming
               "23", "24", "30", "25", # Non vegetated area
               "33", # Water
               "0") # Not observed  

order_redd <- c("id", "year",
                "3", "4", "5", "6", # Forest class
                "11", "12", "29", "32", # Non forest natural class
                "15", "41", "9", "39", "21", "20", "35", "62", # Farming
                "23", "24", "30", "25", # Non vegetated area
                "33", # Water
                "0") # Not observed
# Why order_redd has 32 and order_car don't? Because project 4068 don't have property
# information (only Hydrography information) and is the only project that contains the class 32
# of vegetation

# Change column positions and arrange it
coverage_property <- coverage_property %>% 
  dplyr::select(all_of(order_car)) %>%
  arrange(id, year)


# Add project info to coverage data
coverage_car$id_rgst <- data_car$id_rgst[match(coverage_car$id,
                                               data_car$id)]
coverage_redd$prj_typ <- projects$prj_typ[match(coverage_redd$id,
                                                projects$id_rgst)]
# Convert the column to numeric
coverage_car$id_rgst <- as.numeric(coverage_car$id_rgst)

# Group the values by "year" / "id_rgst" and sum the values in numerical columns
coverage_car_agg <- coverage_car %>%
  group_by(year, id_rgst) %>%
  summarize_at(
    .vars = vars(-id),
    .funs = sum,
    na.rm = TRUE) %>%
  arrange(id_rgst, year) %>%
  ungroup() # to remove any issues later (this don't change the code)

# Filter values from coverage data that actually match REDD projects (not ARR)
coverage_redd <- filter(coverage_redd, prj_typ == 'REDD+')

# Now, Remove the project type column and change NA values to zero
coverage_redd <- dplyr::select(coverage_redd, -prj_typ) %>%
  mutate_all(~if_else(is.na(.), 0, .))

# Create new dataframe based on the values of coverage data, summing columns
coverage_car_reduced <- coverage_car_agg %>%
  mutate(
    Forest = rowSums(coverage_car_agg[, 3:6]),
    Other_natural_formation = rowSums(coverage_car_agg[, 7:9]),
    Farming = rowSums(coverage_car_agg[, 10:17]),
    Non_vegetated_area = rowSums(coverage_car_agg[, 18:21])
    ) %>% 
  rename(Water = X33,
         Non_observed = X0) %>%
  # Select only the necessary columns
  dplyr::select(id_rgst, year, Forest, Other_natural_formation,
                Farming, Non_vegetated_area, Water, Non_observed)

# Create new dataframe based on the values of coverage data, summing columns
coverage_redd_reduced <- coverage_redd %>%
  mutate(
    Forest = rowSums(coverage_redd[, 3:6]),
    Other_natural_formation = rowSums(coverage_redd[, 7:10]),
    Farming = rowSums(coverage_redd[, 11:18]),
    Non_vegetated_area = rowSums(coverage_redd[, 19:22])
  ) %>% 
  rename(Water = X33,
         Non_observed = X0,
         id_rgst = id) %>%
  # Select only the necessary columns
  dplyr::select(id_rgst, year, Forest, Other_natural_formation,
                Farming, Non_vegetated_area, Water, Non_observed)


# Creating a version of those dataframes in percentage
coverage_car_pctg <- coverage_car_reduced
coverage_car_pctg <- coverage_car_pctg %>% 
  # Calculate percentage of other columns
  mutate(
    Forest_pctg = Forest / (rowSums(.[, 3:8])),
    Other_natural_formation_pctg= Other_natural_formation / (rowSums(.[, 3:8])),
    Farming_pctg =  Farming / (rowSums(.[, 3:8])),
    Non_vegetated_area_pctg =  Non_vegetated_area / (rowSums(.[, 3:8])),
    Water_pctg =  Water / (rowSums(.[, 3:8])),
    Non_observed_pctg =  Non_observed / (rowSums(.[, 3:8])),
  ) %>% 
  dplyr::select(id_rgst, year, Forest_pctg, Other_natural_formation_pctg,
                Farming_pctg, Non_vegetated_area_pctg, Water_pctg,
                Non_observed_pctg)


# Creating a version of those dataframes in percentage
coverage_redd_pctg <- coverage_redd_reduced
coverage_redd_pctg <- coverage_redd_pctg %>% 
  # Calculate percentage of other columns
  mutate(
    Forest_pctg = Forest / (rowSums(.[, 3:8])),
    Other_natural_formation_pctg =  Other_natural_formation / (rowSums(.[, 3:8])),
    Farming_pctg =  Farming / (rowSums(.[, 3:8])),
    Non_vegetated_area_pctg =  Non_vegetated_area / (rowSums(.[, 3:8])),
    Water_pctg =  Water / (rowSums(.[, 3:8])),
    Non_observed_pctg =  Non_observed / (rowSums(.[, 3:8])),
  ) %>% 
  dplyr::select(id_rgst, year, Forest_pctg, Other_natural_formation_pctg,
                Farming_pctg, Non_vegetated_area_pctg, Water_pctg,
                Non_observed_pctg)

# Converting id_rgst column from projects to double
projects <- projects %>%
  mutate(id_rgst = as.double(id_rgst))

# Creating final datasets
final_car_raw <- coverage_car_reduced %>%
  left_join(projects, by = "id_rgst") %>%
  dplyr::select(-geometry)

final_car_pctg <- coverage_car_pctg %>%
  left_join(projects, by = "id_rgst") %>%
  dplyr::select(-geometry)

final_redd_raw <- coverage_redd_reduced %>% 
  left_join(projects, by = "id_rgst") %>%
  dplyr::select(-geometry)

final_redd_pctg <- coverage_redd_pctg %>% 
  left_join(projects, by = "id_rgst") %>%
  dplyr::select(-geometry)



# ============================================================================================
# III: Save processed data
# ============================================================================================

# Saving results
write.csv2(final_car_raw, file = "Results/Final_base/final_car_raw.csv",
           row.names = FALSE)
write.csv2(final_car_pctg, file = "Results/Final_base/final_car_pctg.csv",
           row.names = FALSE)
write.csv2(final_redd_raw, file = "Results/Final_base/final_redd_raw.csv",
           row.names = FALSE)
write.csv2(final_redd_pctg, file = "Results/Final_base/final_redd_pctg.csv",
           row.names = FALSE)
