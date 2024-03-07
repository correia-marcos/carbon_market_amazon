# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Merge the Cadastro Ambiental Rural (CAR) information with project info
# 
# In the second script, we generated a spatial dataset containing all AFOLU
# projects in Brazil. We want now to collect the CAR info of these projects
# and merge it in a new dataset. CAR is a public registry, mandatory for all
# rural properties in Brazil. Merge both datasets, we will have information
# of the properties that entered the carbon market.
# 
# This script was implemented to:
#   1 - Get all properties in the CAR dataset that have REDD+ properties inside
#   2 - Merge the CAR information with the project information
# 
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "dplyr", "sf", "sp", "lwgeom")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Get a list of the states to get CAR information
states <- list.files("Data/ATLAS/")

# Import projects and the csv data
projects <- sf::st_read("Results/base_afolu_complete")

# Get only the REDD+ projects and change its CRS and make it valid 
projects_redd <- projects[projects$afolu == "REDD", ] %>% 
  sf::st_make_valid()

# Create an empty data frame to store the data
projects_car <-  data.frame()

# Loop over the states where we have CAR information
for (state in states){
  # Read the CAR file for each state and make it as SF object
  car <- sf::st_read(paste0("Data/ATLAS/", state)) %>% 
    st_as_sf(wkt = "geom")
  
  # Add the correct CRS - https://atlasagropecuario.imaflora.org/downloads 
  st_crs(car) <- 4674 # Using this CRS based on their methodology
  
  # Make the SF polygons valid and convert CRS
  car <- sf::st_make_valid(car) %>%
    st_transform(crs = st_crs(projects_redd))  
  
  # Check if a project is within a property on state CAR 
  intersections <- st_join(projects_redd, car) %>% 
    subset(!is.na(id))  # Remove lines with NA values in "id"
    
  # Replace the geometry in the merged result with the geometry from 'car'
  intersections$geometry <- st_geometry(car[match(intersections$id, car$id), ])

  # Filter lines in the CAR dataset
  # car_reduced <- car[match(intersections$id, car$id), ]
  
  # Add merged dataset to new dataframe
  projects_car <- rbind(projects_car, intersections)
}
# Before saving it, make sure to check if the df only contains polygons
projects_car_new <- st_cast(projects_car, "POLYGON") # need to resolve issue!

# Saving file
st_write(projects_car, "Results/CAR_redd_atlas", driver = "ESRI Shapefile",
         append = FALSE, encoding = "UTF-8")