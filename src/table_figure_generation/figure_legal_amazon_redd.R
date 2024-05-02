# ============================================================================= 
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Produce figure of REDD+ projects in Legal Amazon
#
# @Description: We use data from projects from verra registry to plot all REDD+ projects in the
# legal Amazon. We also take information of the boundary of Legal Amazon from IBGE (Brazilian
# Institute of Geography and Statistics) and data of land use coverage from MAPBIOMAS.
# Please check:
# https://brasil.mapbiomas.org/colecoes-mapbiomas/
# https://registry.verra.org
# https://www.ibge.gov.br/geociencias/cartas-e-mapas/mapas-regionais/15819-amazonia-legal.html
# 
# @summary: This program intends to:
#    1 - Create a plot with land use coverage and all RED+ projects
#
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# Get specific heavy packages for plotting
groundhog.library(pkg  = c("extrafont", "Cairo"),
                  date = "2024-03-03")

# Run function from 'src/config/config_utils.R' to check if fonts have already been imported
check_and_import_fonts()

# ============================================================================================
# I: Import data
# ============================================================================================

# Import required data
legal_amazon        <- sf::st_read(here::here("data", "Amazon_info", "Legal_amazon"))
legal_amazon_border <- sf::st_read(here::here("data", "Amazon_info", "Legal_amazon(unique)"))
projects            <- sf::st_read(here::here("results",
                                              "base_afolu_complete",
                                              "base_afolu_complete.gpkg"))
land_coverage       <- terra::rast(here::here("data",
                                              "Land_coverage",
                                              "brasil_coverage_legal_amazon_2022.tif"))
legend              <- readr::read_delim(here::here("data",
                                                    "Legend",
                                                    "Codigos-da-legenda-colecao-8.csv"),
                                         delim = ";") %>%
  mutate(across(where(is.character), trimws)) # Removing unnecessary white spaces

# ============================================================================================
# II: Process data
# ============================================================================================
# Define a mapping of colors for raster values
color_mapping <- setNames(c("darkgreen", "olivedrab2", "sandybrown", "gray", "blue"), 1:5)

# Filter only REDD+ projects from projects dataset and then create new column 'status'
projects_redd <- projects %>%
  filter(project_type == "REDD+") %>% 
  mutate(color = ifelse(project_status %in% c("Registered", "On Hold"),
                         "cyan", 
                         "red4"),
         .after = project_type) 

# Convert the legal amazon dataset to the same CRS as projects and land_coverage
legal_amazon <- st_transform(legal_amazon, crs = crs(projects_redd))

# Mask the land_coverage_km raster with the legal_amazon limits
land_coverage_masked <- mask(land_coverage, legal_amazon)

# Apply function from 'src/config_utils.R' to reclassify the raster based on broader categories
land_coverage_classified <- reclassifyLandCoverage(land_coverage = land_coverage_masked,
                                                   legend_df     = legend)

# aggregate raster into pixels of 2.5 square km due to memory size limitations
land_coverage_km         <- terra::aggregate(land_coverage_classified,
                                             fact = 50,
                                             fun = median,
                                             na.rm = TRUE)

# Convert the aggregated reclassified raster to a dataframe for plotting
land_coverage_df         <- as.data.frame(land_coverage_km, xy = TRUE)
unique_values            <- unique(land_coverage_df$brasil_coverage_2022)

# Create new dataframe without zero value in a column
land_coverage_reduced    <- subset(land_coverage_df, brasil_coverage_2022 != 0)

# Add column to the above dataframe
land_coverage_reduced <- land_coverage_reduced %>%
  mutate(color = color_mapping[as.character(brasil_coverage_2022)])

# Remove values that are not integer (happened do to misbehaving - 307 out of 2270133)
land_coverage_reduced <- land_coverage_reduced %>%
  filter(brasil_coverage_2022 %in% c(1, 2, 3, 4, 5))

# -----------------------------------------
# Plotting
# -----------------------------------------
plot <- ggplot() +
  geom_raster(data = land_coverage_reduced, aes(x = x, y = y, fill = color), alpha = 0.7) +
  geom_sf(data = legal_amazon, fill = NA, color = "white", ) +
  geom_sf(data = projects_redd, aes(fill = color), color = NA, size = 2, shape = 21) +
  scale_fill_identity(guide = 'legend',
                      labels = c("darkgreen" = "Forest",
                                 "olivedrab2" = "Non Forest Natural Formation", 
                                 "sandybrown" = "Farming", 
                                 "gray" = "Non vegetated area",
                                 "blue" = "Water",
                                 "cyan" = "REDD+ treated", 
                                 "red4" = "REDD+ donors (not yet treated)"),
                      breaks = c("darkgreen",
                                 "olivedrab2",
                                 "sandybrown",
                                 "gray",
                                 "blue",
                                 "cyan",
                                 "red4")) +
  scale_shape_manual(values = c(16, 16, 16, 16, 16, 17, 18)) +
  coord_sf() +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)) +
  guides(
    fill = guide_legend(override.aes = list(shape = c(NA, NA, NA, NA, NA, 17, 18))))

plot
# ============================================================================================
# III: Save processed data
# ============================================================================================
ggsave(here::here("results", "figures", "legal_amazon_redds.pdf"), plot,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)
