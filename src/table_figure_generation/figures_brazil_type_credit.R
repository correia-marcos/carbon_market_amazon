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
#    1 - Create a spatial plot showing the distribution of projects by state
#    2 - Create a bar plot of carbon projects in Brazil distribute by type and number
#    2 - Create a bar plot of carbon projects in Brazil distribute by type and credit volume
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# Run function from 'src/config/config_utils.R' to check if fonts have already been imported
check_and_import_fonts()

# ============================================================================================
# I: Import data
# ============================================================================================

# Import required data
data     <- readr::read_csv(here::here("results", "projects_base", "base_projects.csv"))
credit   <- readxl::read_excel(here::here("data", "Projects_info", "credit_year", "full.xlsx"))
brazil   <- sf::st_read("Data/Brazil_territory")

# ============================================================================================
# II: Process data
# ============================================================================================
# Remove all CDM projects from the dataset and change the value of a specific project_type
data <- data %>% 
  filter(registry != "CDM") %>%
  mutate(project_type = ifelse(project_type == "Energy Production or Conservation//REDD",
                               "REDD+", 
                               project_type))
  

# Transform the credit dataframe into a longer format
credit_long <- credit %>%
  pivot_longer(cols = c("issuance", "retirement"), names_to = "type", values_to = "value") %>% 
  mutate(value = value / 1e6)


# Divide projects by their state, then summarize number of projects by state
count_state <- data %>%
  tidyr::separate_rows(state, sep = "/", convert = TRUE) %>% 
  group_by(state) %>%
  summarise(Number_of_projects = n()) %>%
  filter(state != "BR") %>%  # Remove BR as they have no direct state
  rbind(c("DF", 0)) %>% # Add DF 'state' for the plot
  mutate(Number_of_projects = as.numeric(Number_of_projects))

# Group by project type e summarize registered credits
credits_type <- data %>%
  group_by(project_type) %>%
  summarise(total_credits = sum(saleable_credits_registered)) %>%
  mutate(total_credits = total_credits / 1e6)

# Merge Dataframes
brazil_projects <- merge(x = brazil, y = count_state, by.x = "SIGLA_UF", by.y = "state")

# --------------------------------------
# Plotting - Brazil map by number of projects
# --------------------------------------

# Get a classic palette with 5 colors - for better colors in plot
colors <- brewer.pal(9, "Blues")

# Extend the palette by the amount of different values in the dataset
colors <- colorRampPalette(colors)(n_distinct(brazil_projects$Number_of_projects))

# Plot Brazil map with color by amount of projects in the State
plot_brazil <- ggplot(brazil_projects) +
  geom_sf(aes(fill=factor(Number_of_projects))) +
  scale_fill_manual(values = colors, name = "Number of projects") +
  guides(fill=guide_legend(ncol=2)) +
  theme(text = element_text(family = "Times New Roman"),
        line = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(face="bold", size=14),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 20))


plot_brazil

# --------------------------------------
# Plotting - types of Projects in Brazil
# --------------------------------------

# Plotting bar plot distributed by type 
plot_types <- ggplot(data, aes(x = project_type, fill = project_type)) +
  geom_bar(color = "black") +
  theme(text = element_text(family = "Times New Roman"),
        line = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  labs(x = "Project Type", y = "Number of projects") +
  scale_fill_manual("Project type",
                    values = c("peru","darkred","darkolivegreen3",
                               "lightsteelblue1", "darkgreen",
                               "darkblue", "gray16"))
 
plot_types


# --------------------------------------
# Plotting - credits by types of Projects
# --------------------------------------
plot_issues <- ggplot(credits_type, aes(x = project_type,
                                        y = total_credits,
                                        fill = project_type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = label_number(suffix = "M"),
                     breaks = seq(0, 70, length.out = 8)) +
  theme(text = element_text(family = "Times New Roman"),
        line = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_fill_manual("Project type",
                    values = c("peru","darkred","darkolivegreen3",
                               "lightsteelblue1", "darkgreen",
                               "darkblue", "gray16")) +
  labs(x = "Project Type", y = "Number of Saleable Credits Registered (in millions)",
       fill = "Project Type")


plot_issues

# --------------------------------------
# Plotting - all credits by year
# --------------------------------------
plot_credit_br <- ggplot(credit_long, aes(x = as.numeric(year), y = value, fill = type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("issuance" = "darkblue", "retirement" = "darkgreen")) +
  scale_y_continuous(labels = label_number(suffix = "M"),
                     breaks = seq(0, 45, length.out = 8)) +
  scale_x_continuous(breaks = unique(credit_long$year)) +
  labs(x = "Year",
       y = "Credit Amount (in millions)",
       fill = "Credit") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Times New Roman"),
        panel.grid.minor = element_blank())

plot_credit_br

# ============================================================================================
# III: Save processed data
# ============================================================================================
ggsave(here::here("results", "figures", "projects_in_brazil.pdf"), plot_brazil,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "figures", "projects_type.pdf"), plot_types,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "figures", "projects_credits.pdf"), plot_issues,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "figures", "brazil_credits.pdf"), plot_credit_br,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)
