# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Collect 
# 
# We already create a dataset with virtually all 
# 
# This script was implemented to:
#   1 - Create Figures for presentation
# 
# 
# @Date: out 2023
# @author: Marcos

# Clear environment
rm(list = ls())

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("ggplot2", "dplyr", "sf", "sp", "raster", "terra")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Use this line to avoid geometries issues - allow for self-intersection
sf_use_s2(FALSE)

# Load dataset of merged projects with CAR
data_car <- st_read("Results/CAR_redd")
data_projects <- sf::st_read("Results/base_afolu_complete")

# Load tiff file
tiff_file <- "Data/Deforestation/brasil_coverage_legal_amazon_2000.tif"
raster_data <- raster(tiff_file)

# Função para calcular a porcentagem de cada tipo
calculate_percentage <- function(project_geometry, raster_data) {
  # Recortar o raster usando a geometria do projeto
  raster_cropped <- crop(raster_data, extent(project_geometry))
  
  # Calcular a área total do projeto
  area_total <- st_area(project_geometry)
  
  # Calcular a área de cada classe de cobertura
  area_forest <- sum(values(raster_cropped) == 1)
  area_non_forest_natural <- sum(values(raster_cropped) == 2)
  area_agriculture <- sum(values(raster_cropped) == 3)
  area_others_coverage <- sum(values(raster_cropped) %in% c(4, 5))
  
  # Calcular a porcentagem de cada classe de cobertura
  perc_forest <- 100 * (area_forest / area_total)
  perc_non_forest_natural <- 100 * (area_non_forest_natural / area_total)
  perc_agriculture <- 100 * (area_agriculture / area_total)
  perc_others_coverage <- 100 * (area_others_coverage / area_total)
  
  return(c(perc_forest, perc_non_forest_natural, perc_agriculture, perc_others_coverage))
}


# Inicializar as colunas
data_final$Forest <- NA
data_final$Non_forest_natural <- NA
data_final$Agriculture <- NA
data_final$Others_coverage <- NA

# Loop sobre as geometrias dos projetos
for (i in 1:nrow(data_final)) {
  # Obter a geometria do projeto
  project_geometry <- st_geometry(data_final[i, ])
  
  # Calcular as porcentagens
  percentages <- calculate_percentage(project_geometry, raster_data)
  
  # Atribuir os resultados às colunas
  data_final[i, c("Forest", "Non_forest_natural", "Agriculture", "Others_coverage")] <- percentages
}



plot(raster_data)

ext <- st_bbox(project_geometry)
raster_cropped <- crop(raster_data, ext)


raster_cropped <- ratify(raster_cropped)



# 1. Cortar o arquivo tiff para cada geometria em data_final usando terra
raster_data_cropped <- vector("list", length = nrow(data_final))

for (i in 1:nrow(data_final)) {
  project_geometry <- st_geometry(data_final[i, ])
  
  # Cortar o raster usando a geometria do projeto
  raster_data_cropped[[i]] <- crop(raster_data, project_geometry)
}



# Calculate the percentage of each land cover type
land_cover_percentages <- 
  cellStats(legal_amazon_cropped, stat = "sum") / ncell(legal_amazon_cropped) * 100
