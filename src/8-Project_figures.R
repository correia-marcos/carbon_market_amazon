# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# Goal: We created this program the deforestation analysis of this
# paper - We intersect deforestation data with 
# 
# We already create a dataset with virtually all 
# 
# This program is made to:
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
pkgs <- c("ggplot2", "foreign", "dplyr", "sf", "sp", "raster", "readxl",
          "RColorBrewer", "lubridate")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import IBGE dataset of Legal Amazon and REDD+ plus ARR projects
legal_amazon <- sf::st_read("Data/Legal_Amazon/")
projects_arbache <- sf::st_read("Data/base_arbache")
projects <- sf::st_read("Data/base_afolu_complete")

# Import data: excel previous created by author
data <- readxl::read_excel("Data/base_full.xlsx")

# Substituting white spaces per underline
colnames(data) <- gsub(" ", "_", colnames(data))

# Saving column names
names <- colnames(data)
names <- append(names, "geometry")

# Drop the column "Name" as it is not informative
projects <- subset(projects, select = -Name)

# Change the column names in Projects dataframe
colnames(projects) <- names

# Sub-setting REDD+ and ARR projects from the dataset
projects_redd <- projects[projects$AFOLU == "REDD", ]

# Add a new column called "Status_Indicator"
projects_redd <- projects_redd %>%
  mutate(Status_Indicator = ifelse(Project_Status %in% c("Registered",
                                                         "On Hold"),
                                   "Registered", "In Development"))

# Convert into factor
projects_redd$Status_Indicator <- as.factor(projects_redd$Status_Indicator)

projects_redd$Status_Indicator <- factor(projects_redd$Status_Indicator,
                                         levels = c("In Development",
                                                    "Registered"))
# =============================================================================
# =============================================================================
# Amazon Figure
# =============================================================================
# =============================================================================

# Turn off axis elements in ggplot for better visual comparison
newTheme <- list(theme(line = element_blank(),
                       panel.background = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.title.y = element_blank(),
                       plot.title = element_text(face="bold", size=14)
                       )
                 )

# Plot the Projects in Legal Amazon
ggplot() +
  geom_sf(data = legal_amazon, fill = "white") +
  geom_sf(data = projects_redd, aes(fill = Status_Indicator,
                                    color = Status_Indicator)) +
  scale_fill_manual(values = c("In Development" = "navy",
                               "Registered" = "darkgreen"),
                    name = "Project Status") +
  scale_color_manual(values = c("In Development" = "navy",
                                "Registered" = "darkgreen"),
                     guide = FALSE) + 
  newTheme


ggsave(path = "Fig/", filename = "legal_amazon_v2.tiff", device='tiff',
       dpi=700)


# =============================================================================
# =============================================================================
# Deforestation in Legal amazon
# =============================================================================
# =============================================================================

annual_deforest <- read.table("Data/Deforestation/agg_legal_amazon.csv",
                              sep = ";", header = TRUE)

# Turn Area variable into numerical value
annual_deforest$area <- as.numeric(gsub(",", ".",
                                        gsub("\\.", "", 
                                             annual_deforest$area.km.)))

ggplot(data = annual_deforest, aes(x = year, y = area,
                                   group=1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(x = "Year",
       y = "Deforested Area (km^2)") +
  theme_minimal()

ggsave(path = "Fig/", filename = "Increase in deforestation - Legal Amazon.tiff",
       device='tiff', dpi=700)

# =============================================================================
# =============================================================================
# Junk code
# =============================================================================
# =============================================================================

verra_1094 <- st_read("Data/kml/verra_1094.kml")
verra_1094_shp <- st_read("Data/Individual_shp/verra_1094")
verra_738 <- st_read("Data/kml/verra_738.kml")
verra_738_shp <- st_read("Data/Individual_shp/verra_738")
verra_832 <- st_read("Data/kml/verra_832.kml")
verra_832_shp <- st_read("Data/Individual_shp/verra_832")
verra_1663 <- st_read("Data/kml/verra_1663.kml")
verra_1663_shp <- st_read("Data/Individual_shp/verra_1663")

projects_738 <- projects[2, "geometry"]
projects_832 <- projects[3, "geometry"]


extent(verra_738)
extent(projects_738)

plot(verra_1663)
plot(verra_1663_shp$geometry)

plot(verra_1094)
plot(verra_1094_shp$geometry)
plot(verra_738$geometry)
plot(verra_738_shp$geometry)

plot(projects_738)
plot(verra_832$geometry)
plot(verra_832_shp$geometry)
plot(projects_832)

# Change names
colnames(projects) <- names 

# Converter as colunas de data para o formato datetime
projects <- projects %>%
  mutate(
    Start_period = mdy(),
    End_period = mdy(projects$Project_Expiration)
  )