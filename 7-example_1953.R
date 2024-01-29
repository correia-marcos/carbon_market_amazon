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
verra_1953 <- final_data %>% 
  filter(ID == "1953")

# Get the year of the beginning of the project
treatment_year <- 2016

# Add all donors
verra_1953 <- rbind(verra_1953, final_data %>%
                      filter(Treatment == 0 & REDD_type == "AUD"))

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
id.var      <- "ID"                              
time.var    <- "year"                                 
period.pre  <- seq(from = 2008, to = treatment_year, by = 1)    
period.post <- seq(from = treatment_year+1, to = 2021, by = 1)                           
unit.tr     <- "1953"                         
unit.co     <- setdiff(unique(verra_1953$ID), unit.tr) 
outcome.var <- "protected_land"                                  
cov.adj     <- NULL                                   
features    <- NULL                                   
constant    <- FALSE                                  
report.missing <- FALSE                              
cointegrated.data <- TRUE                             


####################################
### Data preparation

df  <-   scdata(df = verra_1953, id.var = id.var, time.var = time.var,
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