# ===========================================
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





final_redd <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv")) %>% 
  rename(area_ha = area_hectares)

final_properties <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_properties.csv")) %>% 
  rename(area_ha = area_hectares)

final_intersections <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic",
                             "final_intersections.csv")) %>%
  rename(area_ha = area_hectares)

verra_832_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_832_conservation.csv"))

verra_1147_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_1147_conservation.csv"))

verra_1382_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_1382_conservation.csv"))

verra_2551_conservation <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "apd_2551_conservation.csv"))

conservation_units <- 
  sf::st_read(here::here("data", "Conservation_units")) %>%
  st_set_geometry(NULL)

projects <- 
  sf::st_read(here::here("results", "base_afolu_complete", "base_afolu_complete.gpkg")) %>%
  filter(afolu == "REDD")

legend_df <- 
  readr::read_csv2(here::here("data", "Legend", "Codigos-da-legenda-colecao-8.csv"))
# Create new column based on the sum of Forest and Non Forest Natural Formation

# =============================================================================================
# =============================================================================================
# Get verra projects
# =============================================================================================
# =============================================================================================
unique(projects$project_status)
unique(projects$redd_type)

tratados_aud <- projects %>% 
  filter(project_status == "Registered" | project_status == "On Hold") %>%
  filter(redd_type == "AUD")
  
tratados_apd <- projects %>% 
  filter(project_status == "Registered" | project_status == "On Hold") %>%
  filter(redd_type == "APD")

# =============================================================================================
# =============================================================================================
# PLACEBO ESTIMATION
# =============================================================================================
# =============================================================================================
# Get a list of treated APD projects
verra_apd_names <- c("verra_1147_conservation", "verra_1382_conservation",
                     "verra_2551_conservation", "verra_832_conservation")

# Apply function from 'src/config_utils.R' to get merge information about conservation units
merge_filter_clean(conservation_df = conservation_units,
                   verra_df_names = verra_apd_names)

# Create treatment df with placebos
placebo_df <- rbind(verra_1147_conservation, verra_1382_conservation,
                    verra_2551_conservation, verra_832_conservation) %>% 
  unique() %>%
  mutate(
    treatment = ifelse(year >= year(start), 1, 0), .after = year)

average_area <- 25739

# Passo 1: Separar linhas com os id_registry específicos
specific_rows <- placebo_df %>%
  filter(id_registry %in% c("1147", "1382", "832", "2551"))

# Passo 2 e 3: Filtrar as 20 unidades não especificadas mais próximas de 25739 em area_ha
other_units_df <- placebo_df %>%
  # Primeiro, filtra excluindo os id_registry específicos
  filter(!id_registry %in% c("1147", "1382", "832", "2551")) %>%
  # Agrupa por id_registry
  group_by(id_registry) %>%
  # Filtra grupos onde todos os valores de treatment são iguais a 1 e
  # onde existe variabilidade na coluna Total_natural_formation
  filter(!all(treatment == 1) & n_distinct(Total_natural_formation) > 1) %>%
  # Desagrupa para evitar agrupamento indesejado em operações futuras
  ungroup()

closest_ids <- other_units_df %>% 
  mutate(difference = abs(area_ha - 25739)) %>% 
  group_by(id_registry) %>%
  summarise(min_difference = min(difference)) %>%
  ungroup() %>%
  arrange(min_difference) %>% 
  head(20) %>%
  pull(id_registry)

# Filtra as linhas que correspondem aos 20 id_registry selecionados
closest_rows_df <- other_units_df %>%
  filter(id_registry %in% closest_ids)

# Passo 4: Combinar os dois subconjuntos
placebo_df_filtered <- bind_rows(specific_rows, closest_rows_df)


# Make placebo synthetic control
df <- scpi::scdataMulti(placebo_df_filtered, id.var = "id_registry",
                        outcome.var = "Total_natural_formation",
                        treatment.var = "treatment",
                        time.var = "year", constant = TRUE)





# Instale e carregue o pdftools
install.packages("pdftools")
library(pdftools)

# Gera uma lista dos caminhos dos arquivos PDF que você acabou de salvar
pdf_files <- list.files("results/synthetic_controls", full.names = TRUE, pattern = "\\.pdf$")

# Combine os PDFs em um único arquivo
pdf_combine(pdf_files, output = "results/synthetic_controls/combined_plots.pdf")




# Unit time treatment effect (\tau_{ik}) preparation fo AUD projects
df_aud <- scpi::scdataMulti(data_aud, id.var = "id_rgst",
                            outcome.var = "Forest_pctg",
                            treatment.var = "treatment",
                            time.var = "year", constant = TRUE)

# Prediction of Synthetic Control
result_aud <- scpi::scest(df_aud, w.constr = list("name" = "simplex"))

# Plotting
plots <- scplotMulti(result_aud, e.out = TRUE, ncols = 6)
plots

# Get the Prediction Intervals for Synthetic Control Methods
respi <- scpi(df_aud, w.constr = list("name" = "simplex"), cores = 3, sims = 10,
              rho = "type-1", e.method = "gaussian", rho.max = 0.5)

# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE)

# =============================================================================
# get the Average treatment effect on the treated (\tau_{.k})

df_aud_att <- scdataMulti(data_aud, id.var = "id_rgst",
                          outcome.var = "Total_natural_formation",
                          treatment.var = "treatment",
                          time.var = "year",
                          constant = TRUE,
                          effect = "time")

# Prediction of Synthetic Control for ATT
result_apd_att <- scest(df_aud_att, w.constr = list("name" = "simplex"))

# Plot result
scplotMulti(result_apd_att)

# Get the Prediction Intervals for Synthetic Control for ATT
respi <- scpi(df_aud_att, w.constr = list("name" = "simplex"), cores = 3,
              sims = 200, e.method = "gaussian")

# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE)

# =============================================================================
# =============================================================================
# Synthetic control estimation for APD (Avoided planned deforestation) REDD+
# Treatment: REDD+ APD projects year of operation 
# Donors: REDD+ APD projects not yet registered
# =============================================================================
# =============================================================================
# Defining the Final dataset: data
data <- bind_rows(redd_treated_registered, redd_control)

# Create new column based on the sum of Forest and Non Forest Natural Formation
data <- data %>% 
  mutate(Total_natural_formation = Forest_pctg + Other_natural_formation_pctg,
         .after = treatment)

# Filter APD projects
data_apd <- data %>% 
  filter(rdd_typ == "APD" | rdd_typ == "AUD/APD")

# Identify donors with no variation
donors_no_var <- data_apd %>%
  group_by(id_rgst) %>%
  filter(diff(range(Total_natural_formation)) <= 0.00001)%>%
  distinct(id_rgst) %>%
  ungroup() %>% 
  pull(id_rgst)

# Remove donors with no variation from the dataset
data_apd <- data_apd[!data_apd$id_rgst %in% donors_no_var, ]

# =============================================================================
# Unit time treatment effect (\tau_{ik}) preparation fo AUD projects
df_apd <- scdataMulti(data_apd, id.var = "id_rgst",
                      outcome.var = "Total_natural_formation",
                      treatment.var = "treatment",
                      time.var = "year", constant = TRUE,
                      cointegrated.data = TRUE)

# Prediction of Synthetic Control for UTTE
result_apd <- scest(df_apd, w.constr = list("name" = "simplex"))

# Plotting
plots <- scplotMulti(result_apd, e.out = TRUE, ncols = 4, joint = TRUE)
plots

# Get the Prediction Intervals for Synthetic Control Methods
respi <- scpi(df_apd, w.constr = list("name" = "simplex"), cores = 3,
              sims = 200, rho = "type-1", e.method = "all")

# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE, verbose = TRUE)

# =============================================================================
# get the Average treatment effect on the treated (\tau_{.k})

df_apd_att <- scdataMulti(data_apd, id.var = "id_rgst",
                          outcome.var = "Total_natural_formation",
                          treatment.var = "treatment",
                          time.var = "year",
                          constant = TRUE,
                          effect = "time")

# Prediction of Synthetic Control for ATT
result_apd_att <- scest(df_apd_att, w.constr = list("name" = "simplex"))

# Plot result
scplotMulti(result_apd_att)

# Get the Prediction Intervals for Synthetic Control for ATT
respi <- scpi(df_apd_att, w.constr = list("name" = "simplex"), cores = 3,
              sims = 200, e.method = "gaussian")

# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE)


# =============================================================================
# =============================================================================
# Synthetic control estimation for AUD (avoided unplanned deforestation) REDD+
# Treatment: REDD+ AUD projects year of operation 
# Donors: REDD+ AUD projects not yet registered
# =============================================================================
# =============================================================================
# Defining the Final dataset: data
data <- bind_rows(redd_treated_no_antecipation, redd_control)

# Create new column based on the sum of Forest and Non Forest Natural Formation
data <- data %>% 
  mutate(Total_natural_formation = Forest_pctg + Other_natural_formation_pctg,
         .after = treatment)

# Filter AUD projects
data_aud <- data %>%
  filter(rdd_typ == "AUD") %>% 
  filter(!id_rgst == 1094) # Too little pre intervention time

# Identify donors with no/low variation - reduction donor pool may be important
donors_no_var <- data_aud %>%
  group_by(id_rgst) %>%
  filter(diff(range(Total_natural_formation)) <= 0.0001) %>% # 0.0005 all time
  distinct(id_rgst) %>%
  ungroup() %>% 
  pull(id_rgst)

# Remove donors with no variation from the dataset
data_aud <- data_aud[!data_aud$id_rgst %in% donors_no_var, ]

# Unit time treatment effect (\tau_{ik}) preparation fo AUD projects
df_aud <- scdataMulti(data_aud, id.var = "id_rgst",
                      outcome.var = "Total_natural_formation",
                      treatment.var = "treatment",
                      time.var = "year", constant = TRUE)

# Prediction of Synthetic Control
result_aud <- scest(df_aud, w.constr = list("name" = "simplex"))

# Plotting
plots <- scplotMulti(result_aud, e.out = TRUE, ncols = 6)
plots

# Get the Prediction Intervals for Synthetic Control Methods
respi <- scpi(df_aud, w.constr = list("name" = "simplex"), cores = 3, sims = 10,
              rho = "type-1", e.method = "gaussian", rho.max = 0.5)

# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE)





# ==============================================================================

# Create dataset for the synthetic control
verra_1686 <- filter(final_dataset_panel, ID == "1686")

# Get the year of the beginning of the project
treatment_year <- unique(as.integer(format(verra_1686$Project_begin, "%Y")))

# Add all donors
verra_1686 <- rbind(verra_1686, final_dataset_panel %>%
                      filter(Treatment == 0 & REDD_type == "AUD"))

# Create the outcome column: protected_land
verra_1686 <- mutate(verra_1686, protected_land = natural_lands + forest)

# Subset data for the pre-treatment period
verra_1686_pre <- verra_1686[verra_1686$year < treatment_year, ]

# Identify donors with no variation in the variable protected_land in the pre-treatment period
donors_no_variation <- unique(
  verra_1686_pre[verra_1686_pre$protected_land == max(verra_1686_pre$protected_land), "ID"])

# 3. Remove donors with no variation from the dataset
verra_1686 <- verra_1686[!verra_1686$ID %in% donors_no_variation, ]

####################################
### Set options for data preparation
id.var      <- "ID"                              
time.var    <- "year"                                 
period.pre  <- seq(from = 2008, to = treatment_year, by = 1)    
period.post <- seq(from = treatment_year+1, to = 2021, by = 1)                           
unit.tr     <- "1686"                         
unit.co     <- setdiff(unique(verra_1686$ID), unit.tr) 
outcome.var <- "protected_land"                                  
cov.adj     <- NULL                                   
features    <- NULL                                   
constant    <- FALSE                                  
report.missing <- FALSE                              
cointegrated.data <- TRUE                             


####################################
### Data preparation

df  <-   scdata(df = verra_1686, id.var = id.var, time.var = time.var,
                outcome.var = outcome.var, period.pre = period.pre,
                period.post = period.post, unit.tr = unit.tr,
                unit.co = unit.co, cov.adj = cov.adj, features = features,
                constant = constant, cointegrated.data = cointegrated.data)


####################################
## Set options for inference
u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
e.alpha  <- 0.05                         # Confidence level (out-of-sample uncertainty)
rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
rho.max  <- 1                            # Maximum value attainable by rho
sims     <- 200                          # Number of simulations
V        <- NULL                         # Weighting matrix (if NULL it is the identity matrix)
u.order  <- 1                            # Degree of polynomial in B and C when modelling u
u.lags   <- 0                            # Lags of B to be used when modelling u
u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u
u.missp  <- T                            # If TRUE then the model is treated as misspecified
e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
e.order  <- 1                            # Lags of B to be used when modelling e
e.method <- "gaussian"                   # Estimation method for out-of-sample uncertainty
cores    <- 3                            # Number of cores to be used by scpi
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# Results
set.seed(8894)
result  <- scpi(data = df,u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                u.missp = u.missp, sims = sims, e.order = e.order,
                e.lags = e.lags, e.method = e.method, cores = cores,
                w.constr = w.constr, u.alpha = u.alpha, e.alpha = e.alpha,
                rho = rho, rho.max = rho.max) 

# Plot the result



# =============================================================================
# =============================================================================
# Figures
# =============================================================================
# =============================================================================
# Store data on treated unit, synthetic unit, and prediction bars
y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
y.act <- rbind(result$data$Y.pre, result$data$Y.post)

sc.l  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
sc.r  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]

# Store other specifics
period.pre  <- result$data$specs$period.pre
period.post <- result$data$specs$period.post
T0          <- period.pre[length(period.pre)] # intercept
plot.range  <- c(period.pre, period.post)

# Actual data
dat    <- data.frame(t     = c(period.pre, period.post),
                     Y.act = c(y.act),
                     sname = "Treated")

# Fill with NAs Y.fit and confidence bounds where missing
Y.fit.na  <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.l.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.r.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))

names <- strsplit(rownames(y.fit), "\\.")
not.missing.plot <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
names <- strsplit(rownames(sc.l), "\\.")
not.missing.ci   <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))

Y.fit.na[not.missing.plot, 1] <- y.fit
sc.l.na[not.missing.ci, 1]    <- sc.l
sc.r.na[not.missing.ci, 1]    <- sc.r


# Synthetic unit data
dat.sc <- data.frame(t        = c(period.pre, period.post),
                     Y.sc     = Y.fit.na,
                     lb       = c(sc.l.na), ub = c(sc.r.na),
                     sname    = "SC Unit")

# Set ticks, event label and merge
x.ticks <- c(seq(plot.range[1], plot.range[length(plot.range)], length.out = 5), T0)
x.ticks <- round(unique(x.ticks))


event.lab <- paste("\n", "REDD+", sep = "")
event.lab.height <- 1

dat.plot    <- subset(dat,    t %in% plot.range)
dat.sc.plot <- subset(dat.sc, t %in% plot.range)

plotdf <- dplyr::left_join(dat.plot, dat.sc.plot, by = 't')


## Plot specs
plot <- ggplot() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(x = "Year", y = "% of Forest and Natural Formation") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"))

## Add Series to plot
plot <- plot + 
  geom_line( data = plotdf, aes(x = t, y = Y.act, colour = sname.x), linetype = 'solid') +
  geom_point(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), shape = 1) +
  geom_line( data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), linetype = 'dashed') +
  geom_point(data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), shape = 19) +
  geom_vline(xintercept = T0, linetype = "dashed") +
  geom_text(aes(x = T0, label = event.lab, y = event.lab.height), angle = 90, size = 4) +
  scale_x_continuous(breaks = x.ticks) + 
  scale_color_manual(name = "", values = c("mediumblue", "grey46"),
                     labels = c("Synthetic Control", "Treated"),
                     guide = guide_legend(override.aes = list(
                       linetype = c('dashed','solid'), shape = c(19, 1))))

## Add confidence bars and plot
plot + geom_errorbar(data = plotdf,
                     aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                     width = 0.5, linetype = 1)








# Saving plot

scplot(result = result, fig.path = "Fig/",
       fig.name = "verra_1686", fig.format = "png", plot.range = (2000:2022),
       label.xy = list(x.lab = "Year",
                       y.lab = "Percentage of Forest and Non-Forest Natural Formation"),
       x.ticks = NULL, e.out = T, event.label = list(lab = "REDD+", height = 10))


# ==============================================================================

# Change column names
colnames(coverage_car) <- c("Forest Formation", "Wetland", "Pasture",
                            "id", "year", "Floodable Forest", "Grassland",
                            "Urban Area", "River, Lake and Ocean",
                            "") 

# Substituting white spaces per underline
colnames(data) <- gsub(" ", "_", colnames(data))

# Filtering only REDD projects
data <- data %>% 
  filter(Project_Type == "REDD+")

# Correct the Date columns in data
data$Project_Start_Crediting <- as.Date(data$Project_Start_Crediting,
                                        format = "%m/%d/%Y")

# Drop the geometry column from the "projects" dataframe
projects_no_geom <- st_drop_geometry(projects)

# Join "coverage" with "projects" based on the "gid" column and add "ID" column
coverage_prjcts <- left_join(coverage,
                             dplyr::select(projects_no_geom, gid, ID, UF,
                                           Status, CrdtPSD, CrdtPED),
                             by = "gid")

# Remove rows with NA in the "ID" column
coverage_prjcts <- na.omit(coverage_prjcts)

# Convert the "ID" column in the dataframe "coverage_prjcts" to character
coverage_prjcts$ID <- as.character(coverage_prjcts$ID)

# Join the resulting dataset with "data" based on the "ID" and "Registry_ID"
final_dataset <- left_join(coverage_prjcts,
                           dplyr::select(data,
                                         AFOLU,
                                         ID_registry,
                                         REDD_Type,
                                         Project_Start_Crediting),
                           by = c("ID" = "ID_registry"))

# Remove white spaces
final_dataset$ID <- trimws(final_dataset$ID)

# Filter only REDD projects
final_dataset <- final_dataset %>% 
  filter(AFOLU == "REDD")

# Summarise the dataset
columns_to_average <- c("forest", "natural_lands", "agriculture", "pasture")

# Group by and calculate the mean
final_dataset_panel <- final_dataset %>%
  group_by(ID, year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.character), first),
            across(where(~ inherits(., "Date")), first))


# Order the dataframe by ID and year
final_dataset_panel <- final_dataset_panel %>%
  arrange(ID, year)

# Change the column names
names <- c('ID', 'year', 'gid', 'forest', 'natural_lands', 'agriculture',
           'pasture', 'mosaic', 'UF', 'Status', 'Start_period', 'End_period',
           'AFOLU', 'REDD_type', "Project_begin")

colnames(final_dataset_panel) <- names

# Convert date information to date class
final_dataset_panel$Start_period <- as.Date(final_dataset_panel$Start_period,
                                            format = "%m/%d/%Y")
final_dataset_panel$End_period <- as.Date(final_dataset_panel$End_period,
                                          format = "%m/%d/%Y")

# Create the Treatment column
final_dataset_panel <- final_dataset_panel %>%
  mutate(Treatment = ifelse(Status == "Registered", 1, 0))

# Save dataset
write.csv(final_dataset_panel, file = "Data/final_forest.csv",
          row.names = FALSE)

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
pkgs <- c("ggplot2", "scpi", "dplyr")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import the required datasets
final_data <- read.csv("Data/final_forest.csv")

registered <- final_data %>% 
  filter(Treatment == 1 & REDD_type == "AUD")

not_registered <- final_data %>% 
  filter(Treatment == 0 & REDD_type == "AUD")

ids <- unique(registered$ID)
ids_development <- unique(not_registered$ID)

mean(registered$forest)

mean_not <- mean(not_registered$forest)

max(registered$forest)
max(not_registered$forest)

min(registered$forest)
min(not_registered$forest)

# =============================================================================
# =============================================================================
# Synthetic control estimation
# =============================================================================
# =============================================================================
# Create dataset for the synthetic control
final_data <- data
verra_1953 <- final_data %>% 
  filter(id_rgst == "1953")

# Get the year of the beginning of the project
treatment_year <- unique(year(verra_1953$start))

# Add all donors
verra_1953 <- rbind(verra_1953, final_data %>%
                      filter(treatment == 0 & rdd_typ == "AUD"))

# Create the outcome column: protected_land
verra_1953 <- mutate(verra_1953, protected_land = natural_lands + forest)

# Subset data for the pre-treatment period
verra_1953_pre <- verra_1953[verra_1953$year < treatment_year, ]

# Identify donors with no variation in the variable protected_land in the pre-treatment period
donors_no_variation <- unique(
  verra_1953_pre[verra_1953_pre$protected_land == max(verra_1953_pre$protected_land), "ID"])

# 3. Remove donors with no variation from the dataset
verra_1953 <- verra_1953[!verra_1953$ID %in% donors_no_variation, ]

####################################
### Set options for data preparation
id.var      <- "id_rgst"                              
time.var    <- "year"                                 
period.pre  <- seq(from = 2000, to = 2009, by = 1)    
period.post <- seq(from = 2009+1, to = 2021, by = 1)                           
unit.tr     <- "1118"                         
unit.co     <- setdiff(unique(data_aud$id_rgst), unit.tr) 
outcome.var <- "Total_natural_formation"                                  
cov.adj     <- NULL                                   
features    <- NULL                                   
constant    <- FALSE                                  
report.missing <- FALSE                              
cointegrated.data <- TRUE                             


####################################
### Data preparation

df  <-   scdata(df = data_aud, id.var = id.var, time.var = time.var,
                outcome.var = outcome.var, period.pre = period.pre,
                period.post = period.post, unit.tr = unit.tr,
                unit.co = unit.co, cov.adj = cov.adj, features = features,
                constant = constant, cointegrated.data = cointegrated.data)


####################################
## Set options for inference
u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
e.alpha  <- 0.05                         # Confidence level (out-of-sample uncertainty)
rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
rho.max  <- 1                            # Maximum value attainable by rho
sims     <- 200                          # Number of simulations
V        <- NULL                         # Weighting matrix (if NULL it is the identity matrix)
u.order  <- 1                            # Degree of polynomial in B and C when modelling u
u.lags   <- 0                            # Lags of B to be used when modelling u
u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u
u.missp  <- T                            # If TRUE then the model is treated as misspecified
e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
e.order  <- 1                            # Lags of B to be used when modelling e
e.method <- "gaussian"                   # Estimation method for out-of-sample uncertainty
cores    <- 3                            # Number of cores to be used by scpi
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# Results
set.seed(8894)
result  <- scpi(data = df,u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                u.missp = u.missp, sims = sims, e.order = e.order,
                e.lags = e.lags, e.method = e.method, cores = cores,
                w.constr = w.constr, u.alpha = u.alpha, e.alpha = e.alpha,
                rho = rho, rho.max = rho.max) 




# =============================================================================
# =============================================================================
# Figures
# =============================================================================
# =============================================================================
# Store data on treated unit, synthetic unit, and prediction bars
y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
y.act <- rbind(result$data$Y.pre, result$data$Y.post)

sc.l  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
sc.r  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]

# Store other specifics
period.pre  <- result$data$specs$period.pre
period.post <- result$data$specs$period.post
T0          <- period.pre[length(period.pre)] # intercept
plot.range  <- c(period.pre, period.post)

# Actual data
dat    <- data.frame(t     = c(period.pre, period.post),
                     Y.act = c(y.act),
                     sname = "Treated")

# Fill with NAs Y.fit and confidence bounds where missing
Y.fit.na  <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.l.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.r.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))

names <- strsplit(rownames(y.fit), "\\.")
not.missing.plot <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
names <- strsplit(rownames(sc.l), "\\.")
not.missing.ci   <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))

Y.fit.na[not.missing.plot, 1] <- y.fit
sc.l.na[not.missing.ci, 1]    <- sc.l
sc.r.na[not.missing.ci, 1]    <- sc.r


# Synthetic unit data
dat.sc <- data.frame(t        = c(period.pre, period.post),
                     Y.sc     = Y.fit.na,
                     lb       = c(sc.l.na), ub = c(sc.r.na),
                     sname    = "SC Unit")

# Set ticks, event label and merge
x.ticks <- c(seq(plot.range[1], plot.range[length(plot.range)], length.out = 5), T0)
x.ticks <- round(unique(x.ticks))


event.lab <- paste("\n", "REDD+", sep = "")
event.lab.height <- 1

dat.plot    <- subset(dat,    t %in% plot.range)
dat.sc.plot <- subset(dat.sc, t %in% plot.range)

plotdf <- dplyr::left_join(dat.plot, dat.sc.plot, by = 't')


## Plot specs
plot <- ggplot() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(x = "Year", y = "% of Forest and Natural Formation") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"))

## Add Series to plot
plot <- plot + 
  geom_line( data = plotdf, aes(x = t, y = Y.act, colour = sname.x), linetype = 'solid') +
  geom_point(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), shape = 1) +
  geom_line( data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), linetype = 'dashed') +
  geom_point(data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), shape = 19) +
  geom_vline(xintercept = T0, linetype = "dashed") +
  geom_text(aes(x = T0, label = event.lab, y = event.lab.height), angle = 90, size = 4) +
  scale_x_continuous(breaks = x.ticks) + 
  scale_color_manual(name = "", values = c("mediumblue", "grey46"),
                     labels = c("Synthetic Control", "Treated"),
                     guide = guide_legend(override.aes = list(
                       linetype = c('dashed','solid'), shape = c(19, 1))))

## Add confidence bars and plot
plot + geom_errorbar(data = plotdf,
                     aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                     width = 0.5, linetype = 1)






# =============================================================================
# Synthetic control estimation
# =============================================================================
# =============================================================================
# Create dataset for the synthetic control
verra_1113 <- final_data %>% 
  filter(ID == "1113")

# Get the year of the beginning of the project
treatment_year <- 2011

# Add all donors
verra_1113 <- rbind(verra_1113, final_data %>%
                      filter(Treatment == 0 & REDD_type == "AUD"))

# Create the outcome column: protected_land
verra_1113 <- mutate(verra_1113, protected_land = natural_lands + forest)

# Subset data for the pre-treatment period
verra_1113_pre <- verra_1113[verra_1113$year < treatment_year, ]

# Identify donors with no variation in the variable protected_land in the pre-treatment period
donors_no_variation <- unique(
  verra_1113_pre[verra_1113_pre$protected_land == max(verra_1113_pre$protected_land), "ID"])

# 3. Remove donors with no variation from the dataset
verra_1113 <- verra_1113[!verra_1113$ID %in% donors_no_variation, ]

####################################
### Set options for data preparation
id.var      <- "ID"                              
time.var    <- "year"                                 
period.pre  <- seq(from = 2008, to = treatment_year, by = 1)    
period.post <- seq(from = treatment_year+1, to = 2021, by = 1)                           
unit.tr     <- "1113"                         
unit.co     <- setdiff(unique(verra_1113$ID), unit.tr) 
outcome.var <- "protected_land"                                  
cov.adj     <- NULL                                   
features    <- NULL                                   
constant    <- FALSE                                  
report.missing <- FALSE                              
cointegrated.data <- TRUE                             


####################################
### Data preparation

df  <-   scdata(df = verra_1113, id.var = id.var, time.var = time.var,
                outcome.var = outcome.var, period.pre = period.pre,
                period.post = period.post, unit.tr = unit.tr,
                unit.co = unit.co, cov.adj = cov.adj, features = features,
                constant = constant, cointegrated.data = cointegrated.data)


####################################
## Set options for inference
u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
e.alpha  <- 0.05                         # Confidence level (out-of-sample uncertainty)
rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
rho.max  <- 1                            # Maximum value attainable by rho
sims     <- 200                          # Number of simulations
V        <- NULL                         # Weighting matrix (if NULL it is the identity matrix)
u.order  <- 1                            # Degree of polynomial in B and C when modelling u
u.lags   <- 0                            # Lags of B to be used when modelling u
u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u
u.missp  <- T                            # If TRUE then the model is treated as misspecified
e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
e.order  <- 1                            # Lags of B to be used when modelling e
e.method <- "gaussian"                   # Estimation method for out-of-sample uncertainty
cores    <- 3                            # Number of cores to be used by scpi
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# Results
set.seed(8894)
result  <- scpi(data = df,u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                u.missp = u.missp, sims = sims, e.order = e.order,
                e.lags = e.lags, e.method = e.method, cores = cores,
                w.constr = w.constr, u.alpha = u.alpha, e.alpha = e.alpha,
                rho = rho, rho.max = rho.max) 




# =============================================================================
# =============================================================================
# Figures
# =============================================================================
# =============================================================================
# Store data on treated unit, synthetic unit, and prediction bars
y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
y.act <- rbind(result$data$Y.pre, result$data$Y.post)

sc.l  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
sc.r  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]

# Store other specifics
period.pre  <- result$data$specs$period.pre
period.post <- result$data$specs$period.post
T0          <- period.pre[length(period.pre)] # intercept
plot.range  <- c(period.pre, period.post)

# Actual data
dat    <- data.frame(t     = c(period.pre, period.post),
                     Y.act = c(y.act),
                     sname = "Treated")

# Fill with NAs Y.fit and confidence bounds where missing
Y.fit.na  <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.l.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.r.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))

names <- strsplit(rownames(y.fit), "\\.")
not.missing.plot <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
names <- strsplit(rownames(sc.l), "\\.")
not.missing.ci   <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))

Y.fit.na[not.missing.plot, 1] <- y.fit
sc.l.na[not.missing.ci, 1]    <- sc.l
sc.r.na[not.missing.ci, 1]    <- sc.r


# Synthetic unit data
dat.sc <- data.frame(t        = c(period.pre, period.post),
                     Y.sc     = Y.fit.na,
                     lb       = c(sc.l.na), ub = c(sc.r.na),
                     sname    = "SC Unit")

# Set ticks, event label and merge
x.ticks <- c(seq(plot.range[1], plot.range[length(plot.range)], length.out = 5), T0)
x.ticks <- round(unique(x.ticks))


event.lab <- paste("\n", "REDD+", sep = "")
event.lab.height <- 1

dat.plot    <- subset(dat,    t %in% plot.range)
dat.sc.plot <- subset(dat.sc, t %in% plot.range)

plotdf <- dplyr::left_join(dat.plot, dat.sc.plot, by = 't')


## Plot specs
plot <- ggplot() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(x = "Year", y = "% of Forest and Natural Formation") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"))

## Add Series to plot
plot <- plot + 
  geom_line( data = plotdf, aes(x = t, y = Y.act, colour = sname.x), linetype = 'solid') +
  geom_point(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), shape = 1) +
  geom_line( data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), linetype = 'dashed') +
  geom_point(data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), shape = 19) +
  geom_vline(xintercept = T0, linetype = "dashed") +
  geom_text(aes(x = T0, label = event.lab, y = event.lab.height), angle = 90, size = 4) +
  scale_x_continuous(breaks = x.ticks) + 
  scale_color_manual(name = "", values = c("mediumblue", "grey46"),
                     labels = c("Synthetic Control", "Treated"),
                     guide = guide_legend(override.aes = list(
                       linetype = c('dashed','solid'), shape = c(19, 1))))

## Add confidence bars and plot
plot + geom_errorbar(data = plotdf,
                     aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                     width = 0.5, linetype = 1)


















# =============================================================================
# Synthetic control estimation
# =============================================================================
# =============================================================================
# Create dataset for the synthetic control
verra_1115 <- final_data %>% 
  filter(ID == "1115")

# Get the year of the beginning of the project
treatment_year <- 2011

# Add all donors
verra_1115 <- rbind(verra_1115, final_data %>%
                      filter(Treatment == 0 & REDD_type == "AUD"))

# Create the outcome column: protected_land
verra_1115 <- mutate(verra_1115, protected_land = natural_lands + forest)

# Subset data for the pre-treatment period
verra_1115_pre <- verra_1115[verra_1115$year < treatment_year, ]

# Identify donors with no variation in the variable protected_land in the pre-treatment period
donors_no_variation <- unique(
  verra_1115_pre[verra_1115_pre$protected_land == max(verra_1115_pre$protected_land), "ID"])

# 3. Remove donors with no variation from the dataset
verra_1115 <- verra_1115[!verra_1115$ID %in% donors_no_variation, ]

####################################
### Set options for data preparation
id.var      <- "ID"                              
time.var    <- "year"                                 
period.pre  <- seq(from = 2008, to = treatment_year, by = 1)    
period.post <- seq(from = treatment_year+1, to = 2021, by = 1)                           
unit.tr     <- "1115"                         
unit.co     <- setdiff(unique(verra_1115$ID), unit.tr) 
outcome.var <- "protected_land"                                  
cov.adj     <- NULL                                   
features    <- NULL                                   
constant    <- FALSE                                  
report.missing <- FALSE                              
cointegrated.data <- TRUE                             


####################################
### Data preparation

df  <-   scdata(df = verra_1115, id.var = id.var, time.var = time.var,
                outcome.var = outcome.var, period.pre = period.pre,
                period.post = period.post, unit.tr = unit.tr,
                unit.co = unit.co, cov.adj = cov.adj, features = features,
                constant = constant, cointegrated.data = cointegrated.data)


####################################
## Set options for inference
u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
e.alpha  <- 0.05                         # Confidence level (out-of-sample uncertainty)
rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
rho.max  <- 1                            # Maximum value attainable by rho
sims     <- 200                          # Number of simulations
V        <- NULL                         # Weighting matrix (if NULL it is the identity matrix)
u.order  <- 1                            # Degree of polynomial in B and C when modelling u
u.lags   <- 0                            # Lags of B to be used when modelling u
u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u
u.missp  <- T                            # If TRUE then the model is treated as misspecified
e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
e.order  <- 1                            # Lags of B to be used when modelling e
e.method <- "gaussian"                   # Estimation method for out-of-sample uncertainty
cores    <- 3                            # Number of cores to be used by scpi
w.constr <- list(name = "simplex")       # Simplex-type constraint set

# Results
set.seed(8894)
result  <- scpi(data = df,u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                u.missp = u.missp, sims = sims, e.order = e.order,
                e.lags = e.lags, e.method = e.method, cores = cores,
                w.constr = w.constr, u.alpha = u.alpha, e.alpha = e.alpha,
                rho = rho, rho.max = rho.max) 




# =============================================================================
# =============================================================================
# Figures
# =============================================================================
# =============================================================================
# Store data on treated unit, synthetic unit, and prediction bars
y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
y.act <- rbind(result$data$Y.pre, result$data$Y.post)

sc.l  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
sc.r  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]

# Store other specifics
period.pre  <- result$data$specs$period.pre
period.post <- result$data$specs$period.post
T0          <- period.pre[length(period.pre)] # intercept
plot.range  <- c(period.pre, period.post)

# Actual data
dat    <- data.frame(t     = c(period.pre, period.post),
                     Y.act = c(y.act),
                     sname = "Treated")

# Fill with NAs Y.fit and confidence bounds where missing
Y.fit.na  <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.l.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
sc.r.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))

names <- strsplit(rownames(y.fit), "\\.")
not.missing.plot <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
names <- strsplit(rownames(sc.l), "\\.")
not.missing.ci   <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))

Y.fit.na[not.missing.plot, 1] <- y.fit
sc.l.na[not.missing.ci, 1]    <- sc.l
sc.r.na[not.missing.ci, 1]    <- sc.r


# Synthetic unit data
dat.sc <- data.frame(t        = c(period.pre, period.post),
                     Y.sc     = Y.fit.na,
                     lb       = c(sc.l.na), ub = c(sc.r.na),
                     sname    = "SC Unit")

# Set ticks, event label and merge
x.ticks <- c(seq(plot.range[1], plot.range[length(plot.range)], length.out = 5), T0)
x.ticks <- round(unique(x.ticks))


event.lab <- paste("\n", "REDD+", sep = "")
event.lab.height <- 1

dat.plot    <- subset(dat,    t %in% plot.range)
dat.sc.plot <- subset(dat.sc, t %in% plot.range)

plotdf <- dplyr::left_join(dat.plot, dat.sc.plot, by = 't')


## Plot specs
plot <- ggplot() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(x = "Year", y = "% of Forest and Natural Formation") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"))

## Add Series to plot
plot <- plot + 
  geom_line( data = plotdf, aes(x = t, y = Y.act, colour = sname.x), linetype = 'solid') +
  geom_point(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), shape = 1) +
  geom_line( data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), linetype = 'dashed') +
  geom_point(data = plotdf, aes(x = t, y = Y.sc,  colour = sname.y), shape = 19) +
  geom_vline(xintercept = T0, linetype = "dashed") +
  geom_text(aes(x = T0, label = event.lab, y = event.lab.height), angle = 90, size = 4) +
  scale_x_continuous(breaks = x.ticks) + 
  scale_color_manual(name = "", values = c("mediumblue", "grey46"),
                     labels = c("Synthetic Control", "Treated"),
                     guide = guide_legend(override.aes = list(
                       linetype = c('dashed','solid'), shape = c(19, 1))))

## Add confidence bars and plot
plot + geom_errorbar(data = plotdf,
                     aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                     width = 0.5, linetype = 1)



BC_combined <- cbind(synthetic_1094$data$B, synthetic_1094$data$C)
block_A = synthetic_1094$data$A

modelo <- lm(synthetic_1094$data$A ~ synthetic_1094$data$B)
model <- lm(block_A %>% %>% %>% %>% ~ BC_combined)

# Sumário do modelo para verificar a significância dos preditores
summary(model)
summary(modelo)
# Calcular os VIFs para o modelo ajustado
vifs <- car::vif(model)