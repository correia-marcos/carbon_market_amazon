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
pkgs <- c("ggplot2", "haven", "foreign", "dplyr", "sf", "sp",
          "raster", "viridis", "cartography", "readxl", "RColorBrewer",
          "ggplot2", "scpi")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import the required datasets
coverage <- read.csv("Data/base_final.csv", 
                     colClasses=c("NULL", NA, NA, NA, NA, NA, NA, NA))
data <- readxl::read_excel("Data/base_full.xlsx")
projects <- sf::st_read("Data/Final_base")
legal_amazon <- sf::st_read("Data/Legal_Amazon/")

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
# =============================================================================
# Synthetic control estimation
# =============================================================================
# =============================================================================
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
       fig.name = "verra_1686", fig.format = "png", plot.range = (2008:2021),
       label.xy = list(x.lab = "Year",
                       y.lab = "Percentage of Forest and Non-Forest Natural Formation"),
       x.ticks = NULL, e.out = T, event.label = list(lab = "REDD+", height = 10))
