# ============================================================================= 
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Produce a figure of the distribution of carbon projects by type in Brazil
#
# @Description: We use data from verra registry to plot all projects distribute by type using
# bar plot. Please check: 
# https://registry.verra.org
# https://www.ibge.gov.br/geociencias/todos-os-produtos-geociencias.html
#
# @summary: This program intends to: 
#    1 - Create a bar plot of all carbon projects in Brazil distribute by type 
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

# Import csv file previous created on 'src/process_data/collect_issuance.csv'
data <- readr::read_csv(here::here("results", "projects_base", "base_projects.csv"))



# Import data: excel previous created by author and shapefile of Brazil (IBGE)
brazil <- as(sf::st_read("Data/Brazil_territory"),
             "Spatial")
brazil_df <- sf::st_as_sf(brazil) # turn Brazil shapefile into a dataframe

# Substituting white spaces per underline
colnames(data) <- gsub(" ", "_", colnames(data))


# =============================================================================
# =============================================================================
# Figure of Projects in Brazil
# =============================================================================
# =============================================================================

# Divide projects by their state, then summarize number of projects by state
count_state <- data %>%
  tidyr::separate_rows(State, sep = "/", convert = TRUE) %>% 
  group_by(State) %>%
  summarise(Number_of_projects = n()) %>%
  filter(State != "BR") %>%  # Remove BR as they have no direct state
  rbind(c("DF", 0)) %>% # Add DF 'state' for the plot
  mutate(Number_of_projects = as.numeric(Number_of_projects))

# Merge Dataframes
brazil_projects <- merge(x = brazil_df, y = count_state,
                         by.x = "SIGLA_UF", by.y = "State")


# Turn off axis elements in ggplot for better visual comparison
newTheme <- list(theme(line = element_blank(),
                       panel.background = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.title.y = element_blank(),
                       plot.title = element_text(face="bold", size=14)
                       ))

# Get a classic palette "YlGnBu", with 9 colors - for better colors in plot
coul <- brewer.pal(9, "RdPu")

# Extend the palette by the amount of different values in the dataset
coul <- colorRampPalette(coul)(n_distinct(brazil_projects$Number_of_projects))

# Plot Brazil map with color by amount of projects in the State
ggplot(brazil_projects) +
  geom_sf(aes(fill=factor(Number_of_projects))) +
  scale_fill_manual(values = coul, name = "Number of projects") +
  guides(fill=guide_legend(ncol=2)) +
  newTheme

ggsave(path = "Fig/", filename = "Brazil_projs.tiff", device='tiff', dpi=700)

# =============================================================================
# =============================================================================
# Figure types of Projects in Brazil
# =============================================================================
# =============================================================================


# Plotting the data
ggplot(data, aes(x = Project_Type, fill = Project_Type)) +
  geom_bar(color = "black") +
  theme(line = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ylab("Number of Projects") +
  scale_fill_manual("Project type",
                    values = c("dodgerblue","darkmagenta","darkolivegreen3",
                               "darkorange", "darkgreen",
                               "lightsteelblue1", "gray16"))
 
ggsave(path = "Fig/", filename = "Type_projs.tiff", device='tiff', dpi=700)
