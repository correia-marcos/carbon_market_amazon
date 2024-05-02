# =============================================================================
# Dissertation: Can carbon market save the Amazon: Evidence from Brazil
# =============================================================================
# @Goal: Estimate synthetic controls
# 
# @Description: In the last programs, we created ever information we need for
# the first synthetic control estimation! Now, we attempt to estimate it,
# using scpi library. Please check: 
# https://cran.r-project.org/web/packages/scpi/scpi.pdf
# 
# @summary: This program intends to
#   0 - Create short data processing (merging datasets and create new one) 
#   1 - Create function to apply synthetic controls
#   2 - Create figure of results
# 
# @Date: out 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv")) %>% 
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

# ============================================================================================
# II: Process data
# ============================================================================================
# Set seed
set.seed(56)

# Get a list of treated APD projects
verra_apd_names <- c("verra_1147_conservation", "verra_1382_conservation",
                     "verra_2551_conservation", "verra_832_conservation")

# Apply function from 'src/config/config_utils.R' to get merge info on conservation units
merge_filter_clean(conservation_df = conservation_units,
                   verra_df_names = verra_apd_names)

# Apply function from 'src/config/config_utils.R' to create data for individual dataframes
create_treatment_dfs(final_redd_valid, additional = TRUE)


# ------------
# Estimation process
# ------------

# Parameters for estimate_unit_synthetic function
tolerance              <- 1 # some donors didn't have any variance in the outcome, so we remove
tolerance_more         <- .005
outcome_var            <- "Total_natural_formation"
covariates_adjust      <- NULL
covariates_adjust_more <- list(c("constant", "trend"))

# Apply function from 'src/config/config_utils.R' to estimate synthetic controls and plot them
synthetic_1094    <- estimate_unit_synthetic(treatment_df = verra_1094,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1094         <- generate_plots_synthetic(synthetic_1094,
                                              treatment_df = verra_1094,
                                              lower = 0.95,
                                              upper = 1.02,
                                              redd_height = 0.96,
                                              certification_height = 0.967)

synthetic_1112    <- estimate_unit_synthetic(treatment_df = verra_1112,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1112         <- generate_plots_synthetic(result = synthetic_1112,
                                              treatment_df = verra_1112,
                                              lower = 0.96,
                                              upper = 1.02,
                                              redd_height = 0.972,
                                              certification_height = 0.972)


synthetic_1113    <- estimate_unit_synthetic(treatment_df = verra_1113,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1113         <- generate_plots_synthetic(result = synthetic_1113,
                                              treatment_df = verra_1113,
                                              lower = 0.95,
                                              upper = 1.0,
                                              redd_height = 0.96,
                                              certification_height = 0.96)


synthetic_1115    <- estimate_unit_synthetic(treatment_df = verra_1115,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
plot_1115         <- generate_plots_synthetic(result = synthetic_1115,
                                              treatment_df = verra_1115,
                                              lower = 0.97, 
                                              upper = 1.06,
                                              redd_height = 1.035,
                                              certification_height = 1.035)


synthetic_1118    <- estimate_unit_synthetic(treatment_df = verra_1118,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1118         <- generate_plots_synthetic(result = synthetic_1118,
                                              treatment_df = verra_1118,
                                              lower = 0.95,
                                              upper = 1.0,
                                              redd_height = 0.96,
                                              certification_height = 0.96)


# in this synthetic, the tolerance level makes it so that there is only one donors - change it
synthetic_1147    <- estimate_unit_synthetic(treatment_df = verra_1147,
                                             acceptable_variation = 0.005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
plot_1147         <- generate_plots_synthetic(result = synthetic_1147,
                                              treatment_df = verra_1147,
                                              lower = 0.98,
                                              upper = 1.0,
                                              redd_height = 0.985,
                                              certification_height = 0.985)


# synthetic_1147_cu <- estimate_unit_synthetic(treatment_df = verra_1147_conservation,
#                                              acceptable_variation = 5,
#                                              outcome = outcome_var,
#                                              cov_adj = covariates_adjust,
#                                              conservation_donors = TRUE)
# 
# plot_1147_cu      <- generate_plots_synthetic(result = synthetic_1147_cu,
#                                               treatment_df = verra_1147_conservation,
#                                               lower = 0.1,
#                                               upper = 2,
#                                               redd_height = 1.65,
#                                               certification_height = 0.865)


synthetic_1329    <- estimate_unit_synthetic(treatment_df = verra_1329,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1329         <- generate_plots_synthetic(result = synthetic_1329,
                                              treatment_df = verra_1329,
                                              lower = 0.95, 
                                              upper = 1.0,
                                              redd_height = 0.962,
                                              certification_height = 0.962)


synthetic_1382    <- estimate_unit_synthetic(treatment_df = verra_1382,
                                             acceptable_variation = 0.01,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1382         <- generate_plots_synthetic(result = synthetic_1382,
                                              treatment_df = verra_1382,
                                              lower = 0.985,
                                              upper = 1.015,
                                              redd_height = 0.99,
                                              certification_height = 0.99)


# synthetic_1382_cu <- estimate_unit_synthetic(treatment_df = verra_1382_conservation,
#                                              acceptable_variation = 1,
#                                              outcome = outcome_var,
#                                              cov_adj = covariates_adjust,
#                                              conservation_donors = TRUE)
# plot_1382_cu      <- generate_plots_synthetic(result = synthetic_1382_cu,
#                                               treatment_df = verra_1382_conservation,
#                                               lower = 0.6,
#                                               upper = 1.55,
#                                               redd_height = 1.2,
#                                               certification_height = 0.85)


synthetic_1503    <- estimate_unit_synthetic(treatment_df = verra_1503,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1503         <- generate_plots_synthetic(result = synthetic_1503,
                                              treatment_df = verra_1503,
                                              lower = .85,
                                              upper = 1.,
                                              redd_height = .91,
                                              certification_height = .91)


synthetic_1571    <- estimate_unit_synthetic(treatment_df = verra_1571,
                                             acceptable_variation = 0.005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1571         <- generate_plots_synthetic(result = synthetic_1571,
                                              treatment_df = verra_1571,
                                              lower = .995,
                                              upper = 1.0,
                                              redd_height = .99685,
                                              certification_height = .99685)


synthetic_1654    <- estimate_unit_synthetic(treatment_df = verra_1654,
                                             acceptable_variation = 0.005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1654         <- generate_plots_synthetic(result = synthetic_1654,
                                              treatment_df = verra_1654,
                                              lower = .98,
                                              upper = 1.0,
                                              redd_height = .9875,
                                              certification_height = .9875)


synthetic_1686    <- estimate_unit_synthetic(treatment_df = verra_1686,
                                             acceptable_variation = .005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1686         <- generate_plots_synthetic(result = synthetic_1686,
                                              treatment_df = verra_1686,
                                              lower = .99,
                                              upper = 1.015,
                                              redd_height = 1.00825,
                                              certification_height = 1.00825)


synthetic_1811    <- estimate_unit_synthetic(treatment_df = verra_1811,
                                             acceptable_variation = 1,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1811         <- generate_plots_synthetic(result = synthetic_1811,
                                              treatment_df = verra_1811,
                                              lower = .87,
                                              upper = 1.0,
                                              redd_height = .965,
                                              certification_height = .965)


synthetic_1953    <- estimate_unit_synthetic(treatment_df = verra_1953,
                                             acceptable_variation = .005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_1953         <- generate_plots_synthetic(result = synthetic_1953,
                                              treatment_df = verra_1953,
                                              lower = .98,
                                              upper = 1.0,
                                              redd_height = .98265,
                                              certification_height = .98365)


synthetic_2252    <- estimate_unit_synthetic(treatment_df = verra_2252,
                                             acceptable_variation = 0.01,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_2252         <- generate_plots_synthetic(result = synthetic_2252,
                                              treatment_df = verra_2252,
                                              lower = .985,
                                              upper = 1.0,
                                              redd_height = .99235,
                                              certification_height = .990235)


synthetic_2373    <- estimate_unit_synthetic(treatment_df = verra_2373,
                                             acceptable_variation = 0.05,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_2373         <- generate_plots_synthetic(result = synthetic_2373,
                                              treatment_df = verra_2373,
                                              lower = .99,
                                              upper = 1.01,
                                              redd_height = .993015,
                                              certification_height = .993215)


synthetic_2508    <- estimate_unit_synthetic(treatment_df = verra_2508,
                                             acceptable_variation = 0.05,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_2508         <- generate_plots_synthetic(result = synthetic_2508,
                                              treatment_df = verra_2508,
                                              lower = .95,
                                              upper = 1.02,
                                              redd_height = .965,
                                              certification_height = 1.0068)


# synthetic_2551    <- estimate_unit_synthetic(treatment_df = verra_2551,
#                                              acceptable_variation = tolerance_more,
#                                              outcome = outcome_var,
#                                              cov_adj = covariates_adjust)
# plot_2551         <- generate_plots_synthetic(result = synthetic_2551,
#                                               treatment_df = verra_2551,
#                                               lower = .95,
#                                               upper = 1.02,
#                                               redd_height = .965,
#                                               certification_height = 1.007)

# synthetic_2551_cu <- estimate_unit_synthetic(treatment_df = verra_2551_conservation,
#                                              acceptable_variation = 12,
#                                              outcome = outcome_var,
#                                              cov_adj = covariates_adjust,
#                                              conservation_donors = TRUE)
# plot_2551_cu      <- generate_plots_synthetic(result = synthetic_2551_cu,
#                                               treatment_df = verra_2551_conservation,
#                                               lower = .95,
#                                               upper = 1.02,
#                                               redd_height = .965,
#                                               certification_height = 1.007)

synthetic_2566    <- estimate_unit_synthetic(treatment_df = verra_2566,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_2566         <- generate_plots_synthetic(result = synthetic_2566,
                                              treatment_df = verra_2566,
                                              lower = .90,
                                              upper = 1.0,
                                              redd_height = .965,
                                              certification_height = 0.965)


synthetic_2558    <- estimate_unit_synthetic(treatment_df = verra_2558,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_2558         <- generate_plots_synthetic(result = synthetic_2558,
                                              treatment_df = verra_2558,
                                              lower = .95,
                                              upper = 1.0,
                                              redd_height = .958,
                                              certification_height = 0.9599)


synthetic_832     <- estimate_unit_synthetic(treatment_df = verra_832,
                                             acceptable_variation = 0.05,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_832         <- generate_plots_synthetic(result = synthetic_832,
                                              treatment_df = verra_832,
                                              lower = .95,
                                              upper = 1.0,
                                              redd_height = .96,
                                              certification_height = .96)


# synthetic_832_cu     <- estimate_unit_synthetic(treatment_df = verra_832_conservation,
#                                                 acceptable_variation = 5,
#                                                 outcome = outcome_var,
#                                                 cov_adj = covariates_adjust,
#                                                 conservation_donors = TRUE)
# plot_832_cu         <- generate_plots_synthetic(result = synthetic_832_cu,
#                                                 treatment_df = verra_832_conservation,
#                                                 lower = .01,
#                                                 upper = 1.5,
#                                                 redd_height = .3,
#                                                 certification_height = .3)


synthetic_875     <- estimate_unit_synthetic(treatment_df = verra_875,
                                             acceptable_variation = .005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_875         <- generate_plots_synthetic(result = synthetic_875,
                                             treatment_df = verra_875,
                                             lower = .98,
                                             upper = 1.0,
                                             redd_height = .9856,
                                             certification_height = .9856)

synthetic_963     <- estimate_unit_synthetic(treatment_df = verra_963,
                                             acceptable_variation = tolerance,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_963         <- generate_plots_synthetic(result = synthetic_963,
                                             treatment_df = verra_963,
                                             lower = .90,
                                             upper = 1.0,
                                             redd_height = .9256,
                                             certification_height = .9256)


synthetic_977     <- estimate_unit_synthetic(treatment_df = verra_977,
                                             acceptable_variation = .005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_977         <- generate_plots_synthetic(result = synthetic_977,
                                             treatment_df = verra_977,
                                             lower = .98,
                                             upper = 1.0,
                                             redd_height = .98456,
                                             certification_height = .98456)


synthetic_981     <- estimate_unit_synthetic(treatment_df = verra_981,
                                             acceptable_variation = .005,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust)
plot_981         <- generate_plots_synthetic(result = synthetic_981,
                                             treatment_df = verra_981,
                                             lower = .97,
                                             upper = 1.0,
                                             redd_height = .97456,
                                             certification_height = .97556)

# ------------------------------
# Aggregating all plots
# ------------------------------
# Create list with all plots
plot_names <- ls(pattern = "^plot_")

# Make a list of all plots in plot_names
plots <- lapply(plot_names, get)

# Dividing the list of plots into subsets of 12 (or 6 if you prefer)
plot_subgroups <- split_list(plots, 6) # or 6


# ============================================================================================
# III: Save processed data
# ============================================================================================
# Loop to save each subgroup as a PDF, adjusting the layout
for (i in seq_along(plot_subgroups)) {
  combined_plot <- reduce(plot_subgroups[[i]], `+`) + 
    plot_layout(ncol = 2) + # Adjust to 2 columns if using subgroups of 6
    theme(plot.title = element_text(size = 16)) # Adjusts the size of the plot title
  
  # Salve in pdf
  ggsave(paste0("results/synthetic_controls/group_", i, ".pdf"),
         combined_plot, width = 30, height = 20, dpi = 300)}


# Salve each plots as PDF
for(plot_name in plot_names) {
  plot_path <- paste0(here::here("results", "synthetic_controls"), "/", plot_name, ".pdf")
  print(plot_path)
  ggsave(plot_path, plot = get(plot_name), device = "pdf", width = 10, height = 8, dpi = 300)}
