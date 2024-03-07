# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Processing data for synthetic control estimation
# 
# In the last programs, we created get the land use coverage data from each
# REDD+ project and each property that the project is inside. We now want to
# converted the data from last last program into a tidy data, panel format for
# synthetic control estimation.
# 
# This program is made to:
#   1 - Change column positions 
#   2 - Aggregating values
#   3 - Create new columns for aggregated classification 
#   4 - Merge datasets
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "haven", "foreign", "dplyr", "sf", "sp", "scpi")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import the required datasets
coverage_car <- read.csv2("Results/Final_base/car_coverage.csv")
coverage_redd <- read.csv2("Results/Final_base/projects_coverage.csv")
projects <- sf::st_read("Results/base_afolu_complete")
data_car <- sf::st_read("Results/CAR_redd_atlas")

# Define vectors with column position for coverage dataframes
order_car <- c("id", "year",
               "X3", "X4", "X5", "X6", # Forest class
               "X11", "X12", "X29", # Non forest natural class
               "X15", "X41", "X9", "X39", "X21", "X20", "X35", "X62", # Farming
               "X23", "X24", "X30", "X25", # Non vegetated area
               "X33", # Water
               "X0") # Not observed  

order_redd <- c("id", "year",
                "X3", "X4", "X5", "X6", # Forest class
                "X11", "X12", "X29", "X32", # Non forest natural class
                "X15", "X41", "X9", "X39", "X21", "X20", "X35", "X62", # Farming
                "X23", "X24", "X30", "X25", # Non vegetated area
                "X33", # Water
                "X0") # Not observed
# Why order_redd has X32 and order_car don't? Because project 4068 don't have
# property information (only Hydrography information) and is the only project
# that contains the class 32 of vegetation

# Remove column "X" from coverage data and change column positions
coverage_car <- coverage_car[, -which(names(coverage_car) == "X")] %>% 
  dplyr::select(all_of(order_car)) %>%
  arrange(id, year)

coverage_redd <- coverage_redd[, -which(names(coverage_redd) == "X")] %>% 
  dplyr::select(all_of(order_redd)) %>% 
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
    Other_natural_formation_pctg =  Other_natural_formation / (rowSums(.[, 3:8])),
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
  left_join(projects, by = "id_rgst")
final_car_pctg <- coverage_car_pctg %>%
  left_join(projects, by = "id_rgst")
final_redd_raw <- coverage_redd_reduced %>% 
  left_join(projects, by = "id_rgst")
final_redd_pctg <- coverage_redd_pctg %>% 
  left_join(projects, by = "id_rgst")

# Saving results
write.csv2(coverage_car_reduced, file = "Results/Final_base/final_car_raw.csv",
           row.names = FALSE)
write.csv2(coverage_car_pctg, file = "Results/Final_base/final_car_pctg.csv",
           row.names = FALSE)
write.csv2(coverage_redd_reduced, file = "Results/Final_base/final_redd_raw.csv",
           row.names = FALSE)
write.csv2(coverage_redd_pctg, file = "Results/Final_base/final_redd_pctg.csv",
           row.names = FALSE)
