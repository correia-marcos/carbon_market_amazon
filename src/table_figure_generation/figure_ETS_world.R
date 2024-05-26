# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Produce figure of worldwide ETS available
# 
# @Description: We use data from the World Bank Carbon Pricing Dashboard to create make a plot
# with the all ETS available worldwide. We also take geometries from countries, cities, and
# regions so that we can plot a world view of ETS. We downloaded data in April 2024. Please
# check:
# https://carbonpricingdashboard.worldbank.org/
# https://open.alberta.ca/opendata/gda-4d939041-851b-4848-bd30-44dbf129e16c
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# https://open.canada.ca/data/en/dataset/a883eb14-0c0e-45c4-b8c4-b54c4a819edb
# https://data.humdata.org/dataset/cod-ab-chn?
# https://data.humdata.org/dataset/cod-ab-jpn?
# https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-russian-federation?
# https://data.dtu.dk/articles/dataset/Shapefile_of_European_countries/23686383
# 
# @summary: This program intends to
#   1 - Create sf dataset of ETS mechanisms Worldwide 
#   2 - Create figures with sf data
# 
# @Date: April 2024
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# Get specific heavy packages for plotting
groundhog.library(pkg  = c("rnaturalearth","rnaturalearthdata"),
                  date = "2024-03-03")

# Run function from 'src/config/config_utils.R' to check if fonts have already been imported
check_and_import_fonts()

# ============================================================================================
# I: Import data
# ============================================================================================
# Import data from World Bank, selecting only important columns
carbon_initiatives <- read_excel(here::here("data", "World_bank_dashboard", "data-latest.xlsx"),
                                 skip = 1)

# Import data about the all countries, states and places
world              <- ne_countries(scale = "medium", returnclass = "sf")
usa                <- sf::st_read(here::here("data", "ETS", "US"))
canada             <- sf::st_read(here::here("data", "ETS", "Canada"))
china              <- sf::st_read(here::here("data", "ETS", "China"))
japan              <- sf::st_read(here::here("data", "ETS", "Japan"))
russia             <- sf::st_read(here::here("data", "ETS", "Russia"))

# Define US states that represent RGGI ETS and euro countries the represent EU ETS
rggi               <- c("Connecticut", "Delaware", "Maine", "Maryland", "Massachusetts",
                        "New Hampshire", "New Jersey", "New York", "Pennsylvania",
                        "Rhode Island", "Vermont")

euro_countries     <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
                        "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
                        "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg","Malta", "Netherlands", "Poland", "Portugal", "Romania",
                        "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland", "Liechtenstein",
                        "Norway")

# ============================================================================================
# II: Process data
# ============================================================================================

# Convert values of "*" into NA, select columns filter lines containing NA values and others
ets_initiatives <- carbon_initiatives %>%
  dplyr::select(1:7) %>%
  mutate_all(~na_if(., "*")) %>%
  filter(!if_all(everything(), is.na)) %>% # filter Na values
  filter(!str_detect(Status, "Abolished")) %>%  # filter lines with the term "abolished" 
  filter(str_detect(Type, "ETS")) %>% 
  expand_ets(., vector_states = rggi, ets_name = "RGGI") %>%  # Apply function config_utils.R
  expand_ets(., vector_states = euro_countries, ets_name = "EU")

# Filter columns on world dataset
world <- world %>% 
  dplyr::select(1:10) %>% 
  rename(name = admin)

# Convert column name, select column and change value
usa <- usa %>%
  mutate(name = case_when(
    NAME == "Georgia" ~ "Georgia State",
    TRUE ~ NAME)) %>% 
  dplyr::select(name)

canada <- canada %>% 
  rename(name = PRENAME) %>% 
  dplyr::select(name)

china <- china %>% 
  rename(name = ADM1_EN) %>%
  dplyr::select(name)

russia <- russia %>% 
  rename(name = shapeName) %>%
  dplyr::select(name)

japan <- japan %>%
  mutate(across(where(is.character), ~ str_squish(.))) %>%  # Remove unnecessary spaces
  rename(name = ADM1_EN) %>%
  dplyr::select(name)


# Define crs of world data so other dataframe can be converted to it
crs_target <- crs(world)

# Merge datasets to take sf information for the ETS data and convert CRS
usa_ets     <- usa %>% 
  inner_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>% 
  st_transform(., crs_target)

canada_ets  <- canada %>% 
  inner_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>% 
  st_transform(., crs_target)

china_ets   <- china %>% 
  inner_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>% 
  st_transform(., crs_target)

japan_ets <- japan %>% 
  inner_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>% 
  st_transform(., crs_target)

russia_ets <- russia %>% 
  inner_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>% 
  st_transform(., crs_target)


# Merge all state/city datasets previous created, add new column
state_ets <- rbind(usa_ets, canada_ets, china_ets, japan_ets, russia_ets) %>%
  mutate(status_small = case_when(
    str_detect(Status, "Under development") ~ "In development",
    str_detect(Status, "Under consideration") ~ "Under consideration",
    str_detect(Status, "Implemented") ~ "Implemented",
    TRUE ~ "No implementation"), # case when no one of the above applies
    .after = Status)

# Convert geometry from multipolygon to points
state_ets_points <- state_ets %>%
  sf::st_make_valid() %>% # make geometries valid to avoid duplicate vertex
  mutate(geometry = st_centroid(geometry))

# Create the sf dataframe with country ETS information and EU ETS condition column
country_ets <- world %>%
  left_join(ets_initiatives, by = c("name" = "Jurisdiction covered")) %>%
  mutate(status_small = case_when(
    str_detect(Status, "Under development") ~ "In development",
    str_detect(Status, "Under consideration") ~ "Under consideration",
    str_detect(Status, "Implemented") ~ "Implemented",
    TRUE ~ "No implementation"),  # case when no one of the above applies
    .after = Status) %>% 
  mutate(color = ifelse(`Instrument name` == "EU ETS", "EU-ETS",
                        status_small), # Add a new column to differentiate EU from non EU
         .after = Type) %>% 
  mutate(colors = ifelse(is.na(color), "No implementation", color),
         .after = color)

# Define order of levels
levels_status <- c("EU-ETS", "Implemented", "In development", "Under consideration", 
                   "No implementation")

# Convert column into factor
country_ets <- country_ets %>% 
  mutate(colors = factor(colors, levels = levels_status))

# -----------------------------------------
# Plotting
# -----------------------------------------
plot <- ggplot() +
  geom_sf(data = country_ets, aes(fill = colors), size = 1) +
  geom_sf(data = state_ets_points, aes(color = status_small), size = 1.7, shape = 19) +
  scale_fill_manual(values = c("Under consideration" = "peru",
                               "In development" = "darkolivegreen4",
                               "Implemented" = "darkgreen",
                               "EU-ETS" = "darkblue",
                               "No implementation" = "lightgrey"),
                    name = "Status National / Regional ETS:", drop = FALSE) +
  scale_color_manual(values = c("Under consideration" = "lightsteelblue",
                                "In development" = "gold",
                                "Implemented" = "black"),
                     name = "Status subnational ETS:", drop = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.title = element_text(size = 23),
        legend.text = element_text(size = 23),
        text = element_text(family = "Times New Roman")) +
  guides(
    fill = guide_legend(order = 1, title = "Status National/Regional ETS:",
                        override.aes = list(size = 1)),
    color = guide_legend(order = 2, title = "Status subnational ETS:",
                         override.aes = list(size = 3)))

plot
# ============================================================================================
# III: Save processed data
# ============================================================================================
ggsave(here::here("results", "figures", "ets_status_map.pdf"), plot,
       device = cairo_pdf, width = 16, height = 9, dpi = 300)
