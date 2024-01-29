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
pkgs <- c("ggplot2", "dplyr", "sf", "sp")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Get a list of the states to get CAR information
states <- list.files("Data/CAR")

# Import projects and the csv data
projects <- sf::st_read("Results/base_afolu_complete")

# Drop unnecessary columns
remove_columns <- c("prj_dsc", "gld_std", "sdg", "prd_trm",
                    "legl_mr", "ccb_typ", "ccb_val", "ccb_std",
                    "prp_inf", "aud_vst", "frst_vr", "scnd_vr", "thrd_vr",
                    "frth_vr", "ffth_vr", "sxth__v", "sevn_vr")

projects_dropped <- projects[, !(names(projects) %in% remove_columns)]
# Get only the REDD+ projects and change its CRS 
projects_redd <- projects[projects$afolu == "REDD", ]

# Make shapefiles valid
projects_redd <- sf::st_make_valid(projects_redd)

# Create an empty data frame to store the data
projects_car <-  data.frame()

# Loop over the states where we have CAR information
for (state in states){
  # Read the CAR file for each state
  car <- sf::st_read(paste0("Data/CAR/", state))

  # Make shapefiles valid and convert CRS to the same as in REDD+ projects
  car <- sf::st_make_valid(car)
  car <- st_transform(car, crs = st_crs(projects_redd))  
  
  # Check if a project is within a property on state CAR 
  intersections <- st_join(projects_redd, car)

  # Remove lines with NA values in "gid"
  intersections <- subset(intersections, !is.na(gid))
  
  projects_car <- rbind(projects_car, intersections)
}

# Remove columns that have way too large numbers to be saved in shapefile
drop_columns <- c("ag_area_lo", "aru_area_l", "carpo_area", "carpr_area",
                  "ml_area_lo", "table_sour", "nd_b_area_", "nd_i_area_",
                  "ql_area_lo", "ti_h_area_", "ti_n_area_", "tlpc_area_",
                  "tlpl_area_", "trans_area", "ucpi_area_", "ucus_area_",
                  "urb_area_l", "name", "prj_dsc", "gld_std", "sdg", "prd_trm",
                  "legl_mr", "ccb_typ", "ccb_val", "ccb_std",
                  "prp_inf", "aud_vst", "frst_vr", "scnd_vr", "thrd_vr",
                  "frth_vr", "ffth_vr", "sxth__v", "sevn_vr")
projects_car_dropped <- projects_car[, !(names(projects_car) %in%
                                           drop_columns)]

st_write(projects_car, "Results/CAR_redd", driver = "ESRI Shapefile",
         append = FALSE, encoding = "UTF-8")




hehe <- sf::st_read("Data/ATLAS/AC")
