# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Collect  land coverage information on the REDD+ properties 
# 
# With the file 4-Merge_car.R we created a sf dataframe that contains all
# properties that have REDD+ projects inside. Now we use this dataframe
# and extract land coverage information of properties using all base of raster
# files. Raster consists of MAPBIOMAS Land Cover and User in Brazil, version 8.
# See: https://plataforma.brasil.mapbiomas.org
# 
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "dplyr", "sf", "sp", "raster", "terra", "exactextractr",
          "tidyverse", "furrr")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Run this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Load the required datasets
projects <- sf::st_read("Results/base_afolu_complete")
data_car <- st_read("Results/CAR_redd_atlas/")

# Change projects data frame column name
colnames(projects)[1] <- 'id'

# Filter for REDD+ projects
projects_redd <- projects %>%
  filter(prj_typ == "REDD+")

# Specify the directory path and regular expression pattern for matching files
directory_path <- "Data/Deforestation/"
pattern <- "^brasil_coverage_legal_amazon_[0-9]{4}\\.tif$"

# List raster files matching the pattern in the directory and read them
raster_files <- list.files(path = directory_path,
                           pattern = pattern,
                           full.names = TRUE)
raster_list = lapply(raster_files, raster)

# Function --------------------------------------------------------------------
# @written_on: 12/01/2024
# @written_by: Marcos Paulo
# @Input     : List of raster files and dataframe of land tenure spatial data
# @Output    : Dataframe with +- 40 columns with coverage land use, id and year
# @purpose   : Get the coverage land use of properties in IMAFLORA dataset
# @desc      : The function works in following way:
# - for each raster file variable (one for each year) - i
#   - for each property in the land tenure dataset - j
#       - Do processing to get all the coverage usage of the properties in a 
#       - new dataset, saving the ID of the property and the year of land use
get_coverage_properties <- function(list_of_rast_files, data_land_tenure) {
  # Get time of execution
  start <- Sys.time()
  # Create an empty data frame to store the data
  coverage_car <-  data.frame()
  
  # Init the first loop
  for (i in 1:length(list_of_rast_files)) {
    cat(paste0( "Working on the following raster:", 
                "\n",
                filename(raster_list[[i]]),
                "\n"))
    
    # Get the year of the raster
    raster_name <- filename(list_of_rast_files[[i]])
    year_string <- substr(raster_name,
                          nchar(raster_name) - 7,
                          nchar(raster_name) - 4)
    year_num <- as.numeric(year_string)
    
    # Init the second loop 
    for (j in 1:nrow(data_land_tenure)) {
      # Take the j line of the land data
      property <- data_land_tenure[j,]
      
      # Extract from raster only the important spatial area
      extraction <- exact_extract(list_of_rast_files[[i]], property)
      
      # Create a data frame, sum the total area of each legend class, invert
      # lines with columns, add ID and YEAR columns to it 
      raster_property <- extraction[[1]] %>%
        group_by(value) %>% 
        summarise(class_id = sum(coverage_fraction)) %>%
        tidyr::pivot_wider(names_from = value, values_from = class_id) %>% 
        mutate(id = property$id) %>% 
        mutate(year = year_num)
      
      # Add values to dataframe
      coverage_car <- bind_rows(coverage_car, raster_property)
    }
  }
  
  # Show time of code execution
  print( Sys.time() - start )
  
  # Return the final dataframe
  return(coverage_car)
}

# Applying function
land_coverage_car <- get_coverage_properties(list_of_rast_files = raster_list,
                                             data_land_tenure = data_car)

land_coverage_redd <- get_coverage_properties(list_of_rast_files = raster_list,
                                              data_land_tenure = projects_redd)

# Save files
write.csv2(land_coverage_car,
           file = 'Results/Final_base/car_coverage.csv')
write.csv2(land_coverage_redd,
           file = 'Results/Final_base/projects_coverage.csv')

# =====================
# erros
# =====================


