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

# Get specific heavy packages for tables
groundhog.library(pkg  = c("xtable", "knitr"),
                  date = "2024-03-03")

# ============================================================================================
# I: Import data
# ============================================================================================
final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv")) %>% 
  rename(area_ha = area_hectares)

# ============================================================================================
# II: Process data
# ============================================================================================
# Set seed
set.seed(56)

# List of treated and donors
treated <- final_redd_valid %>% 
  filter(project_status %in% c("Registered", "On Hold")) %>%
  distinct(id_registry) %>%
  pull(id_registry)

donors <- final_redd_valid %>% 
  filter(!project_status %in% c("Registered", "On Hold")) %>%
  distinct(id_registry) %>%
  pull(id_registry)

# Apply function from 'src/config/config_utils.R' to create data for individual dataframes
create_treatment_dfs(final_redd_valid, additional = TRUE)

# ------------
# Estimation process
# ------------

# Parameters for estimate_unit_synthetic function
tolerance              <- 1 # some donors didn't have any variance in the outcome, so we remove
tolerance_more         <- 0.05
outcome_var            <- "Total_natural_formation"
covariates_adjust      <- NULL
covariates_adjust_more <- list(c("constant", "trend"))

# Apply function from 'src/config/config_utils.R' to estimate synthetic controls and plot them
synthetic_1094    <- estimate_unit_synthetic(treatment_df = verra_1094,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1094      <- synthetic_1094$est.results$w
plot_1094         <- generate_plots_synthetic(result = synthetic_1094,
                                              treatment_df = verra_1094,
                                              lower = 0.9885,
                                              upper = 1.016,
                                              redd_height = 0.992985,
                                              certification_height = 0.993985)

synthetic_1112    <- estimate_unit_synthetic(treatment_df = verra_1112,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1112      <- synthetic_1112$est.results$w
plot_1112         <- generate_plots_synthetic(result = synthetic_1112,
                                              treatment_df = verra_1112,
                                              lower = 0.96,
                                              upper = 1.0,
                                              redd_height = 0.972,
                                              certification_height = 0.972)


synthetic_1113    <- estimate_unit_synthetic(treatment_df = verra_1113,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1113      <- synthetic_1113$est.results$w
plot_1113         <- generate_plots_synthetic(result = synthetic_1113,
                                              treatment_df = verra_1113,
                                              lower = 0.94,
                                              upper = 1.0,
                                              redd_height = 0.96,
                                              certification_height = 0.9596)


synthetic_1115    <- estimate_unit_synthetic(treatment_df = verra_1115,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1115      <- synthetic_1115$est.results$w
plot_1115         <- generate_plots_synthetic(result = synthetic_1115,
                                              treatment_df = verra_1115,
                                              lower = 0.97, 
                                              upper = 1.0,
                                              redd_height = .9859,
                                              certification_height = .97859)


synthetic_1118    <- estimate_unit_synthetic(treatment_df = verra_1118,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1118      <- synthetic_1118$est.results$w
plot_1118         <- generate_plots_synthetic(result = synthetic_1118,
                                              treatment_df = verra_1118,
                                              lower = 0.95,
                                              upper = 1.0,
                                              redd_height = 0.96,
                                              certification_height = 0.96)


# in this synthetic, the tolerance level makes it so that there is only one donors - change it
synthetic_1147    <- estimate_unit_synthetic(treatment_df = verra_1147,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1147      <- synthetic_1147$est.results$w
plot_1147         <- generate_plots_synthetic(result = synthetic_1147,
                                              treatment_df = verra_1147,
                                              lower = 0.98,
                                              upper = 1.0,
                                              redd_height = 0.985,
                                              certification_height = 0.985)

synthetic_1329    <- estimate_unit_synthetic(treatment_df = verra_1329,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1329      <- synthetic_1329$est.results$w
plot_1329         <- generate_plots_synthetic(result = synthetic_1329,
                                              treatment_df = verra_1329,
                                              lower = 0.95, 
                                              upper = 1.0,
                                              redd_height = 0.962,
                                              certification_height = 0.962)


synthetic_1382    <- estimate_unit_synthetic(treatment_df = verra_1382,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1382      <- synthetic_1382$est.results$w
plot_1382         <- generate_plots_synthetic(result = synthetic_1382,
                                              treatment_df = verra_1382,
                                              lower = 0.985,
                                              upper = 1.02,
                                              redd_height = 0.99,
                                              certification_height = 0.99055)


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
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1503      <- synthetic_1503$est.results$w
plot_1503         <- generate_plots_synthetic(result = synthetic_1503,
                                              treatment_df = verra_1503,
                                              lower = .85,
                                              upper = 1.,
                                              redd_height = .91,
                                              certification_height = .91)


synthetic_1571    <- estimate_unit_synthetic(treatment_df = verra_1571,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1571      <- synthetic_1571$est.results$w
plot_1571         <- generate_plots_synthetic(result = synthetic_1571,
                                              treatment_df = verra_1571,
                                              lower = .958,
                                              upper = 1.025,
                                              redd_height = .99685,
                                              certification_height = .99685)


synthetic_1654    <- estimate_unit_synthetic(treatment_df = verra_1654,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1654      <- synthetic_1654$est.results$w
plot_1654         <- generate_plots_synthetic(result = synthetic_1654,
                                              treatment_df = verra_1654,
                                              lower = .975,
                                              upper = 1.0,
                                              redd_height = .9875,
                                              certification_height = .98675)


synthetic_1686    <- estimate_unit_synthetic(treatment_df = verra_1686,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1686      <- synthetic_1686$est.results$w
plot_1686         <- generate_plots_synthetic(result = synthetic_1686,
                                              treatment_df = verra_1686,
                                              lower = .98,
                                              upper = 1.02,
                                              redd_height = 1.00825,
                                              certification_height = 1.009825)


synthetic_1811    <- estimate_unit_synthetic(treatment_df = verra_1811,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1811      <- synthetic_1811$est.results$w
plot_1811         <- generate_plots_synthetic(result = synthetic_1811,
                                              treatment_df = verra_1811,
                                              lower = .87,
                                              upper = .95,
                                              redd_height = .8795,
                                              certification_height = .87095)


synthetic_1953    <- estimate_unit_synthetic(treatment_df = verra_1953,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_1953      <- synthetic_1953$est.results$w
plot_1953         <- generate_plots_synthetic(result = synthetic_1953,
                                              treatment_df = verra_1953,
                                              lower = .97,
                                              upper = 1.0,
                                              redd_height = .98265,
                                              certification_height = .979365)


synthetic_2252    <- estimate_unit_synthetic(treatment_df = verra_2252,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2252      <- synthetic_2252$est.results$w
plot_2252         <- generate_plots_synthetic(result = synthetic_2252,
                                              treatment_df = verra_2252,
                                              lower = .985,
                                              upper = 1.025,
                                              redd_height = .99235,
                                              certification_height = .9929235)


synthetic_2373    <- estimate_unit_synthetic(treatment_df = verra_2373,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2373      <- synthetic_2373$est.results$w
plot_2373         <- generate_plots_synthetic(result = synthetic_2373,
                                              treatment_df = verra_2373,
                                              lower = .985,
                                              upper = 1.015,
                                              redd_height = .993015,
                                              certification_height = .9901215)


synthetic_2508    <- estimate_unit_synthetic(treatment_df = verra_2508,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2508      <- synthetic_2508$est.results$w
plot_2508         <- generate_plots_synthetic(result = synthetic_2508,
                                              treatment_df = verra_2508,
                                              lower = .98,
                                              upper = 1.02,
                                              redd_height = .99,
                                              certification_height = .99)


synthetic_2551    <- estimate_unit_synthetic(treatment_df = verra_2551,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2551      <- synthetic_2551$est.results$w
plot_2551         <- generate_plots_synthetic(result = synthetic_2551,
                                              treatment_df = verra_2551,
                                              lower = .98,
                                              upper = 1.0,
                                              redd_height = .985,
                                              certification_height = .985)


synthetic_2566    <- estimate_unit_synthetic(treatment_df = verra_2566,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2566      <- synthetic_2566$est.results$w
plot_2566         <- generate_plots_synthetic(result = synthetic_2566,
                                              treatment_df = verra_2566,
                                              lower = .88,
                                              upper = .98,
                                              redd_height = .965,
                                              certification_height = 0.96485)


synthetic_2558    <- estimate_unit_synthetic(treatment_df = verra_2558,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_2558      <- synthetic_2558$est.results$w
plot_2558         <- generate_plots_synthetic(result = synthetic_2558,
                                              treatment_df = verra_2558,
                                              lower = .95,
                                              upper = 1.0,
                                              redd_height = .958,
                                              certification_height = 0.9599)


synthetic_832     <- estimate_unit_synthetic(treatment_df = verra_832,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_832      <- synthetic_832$est.results$w
plot_832         <- generate_plots_synthetic(result = synthetic_832,
                                              treatment_df = verra_832,
                                              lower = .95,
                                              upper = 1.065,
                                              redd_height = .9604,
                                              certification_height = .96809)


synthetic_875     <- estimate_unit_synthetic(treatment_df = verra_875,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_875      <- synthetic_875$est.results$w
plot_875         <- generate_plots_synthetic(result = synthetic_875,
                                             treatment_df = verra_875,
                                             lower = .97,
                                             upper = 1.02,
                                             redd_height = .9856,
                                             certification_height = .9846)

synthetic_963     <- estimate_unit_synthetic(treatment_df = verra_963,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_963      <- synthetic_963$est.results$w
plot_963         <- generate_plots_synthetic(result = synthetic_963,
                                             treatment_df = verra_963,
                                             lower = .90,
                                             upper = 1.0,
                                             redd_height = .9256,
                                             certification_height = .9256)


synthetic_977     <- estimate_unit_synthetic(treatment_df = verra_977,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_977      <- synthetic_977$est.results$w
plot_977         <- generate_plots_synthetic(result = synthetic_977,
                                             treatment_df = verra_977,
                                             lower = .98,
                                             upper = 1.01,
                                             redd_height = .98456,
                                             certification_height = .98656)


synthetic_981     <- estimate_unit_synthetic(treatment_df = verra_981,
                                             acceptable_variation = tolerance_more,
                                             outcome = outcome_var,
                                             cov_adj = covariates_adjust_more)
weights_981      <- synthetic_981$est.results$w
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

# Dividing the list of plots into subsets of some number (6 if you prefer)
plot_subgroups <- split_list(plots, 9) # or 6


# ------------------------------
# Aggregating all weights
# ------------------------------
# List all variable names matching the pattern weights_{number}
weight_vars <- ls(pattern = "^weights_\\d{3,4}$")

# Gather all variables into a list
weight_list <- mget(weight_vars, envir = .GlobalEnv)

# Initialize a matrix for the transformed data
result_matrix <- matrix(0, nrow = 50, ncol = 25, 
                        dimnames = list(donors, treated))

# Populate the matrix with corresponding values
for (var_name in names(weight_list)) {
  # Extract the number from the variable name
  num <- gsub("weights_", "", var_name)
  # Check if the number is in the treated columns
  if (num %in% treated) {
    # Get the weight vector
    weights <- weight_list[[var_name]]
    # For each weight, find the corresponding row and update the matrix
    for (index in names(weights)) {
      # Extract the suffix after the dot
      suffix <- sub("^[^.]*\\.", "", index)
      if (suffix %in% rownames(result_matrix)) {
        result_matrix[suffix, num] <- weights[index]
      }
    }
  }
}
# Round the entire array to 5 decimal digits
result_matrix <- round(result_matrix, 5)

# Set any value that is 0.00000 after rounding to absolute zero
result_matrix[result_matrix == 0.00000] <- "0"

# Create xtable object from the result matrix
latex_table <- xtable(result_matrix)
# ============================================================================================
# III: Save processed data
# ============================================================================================
# Loop to save each subgroup as a PDF, adjusting the layout
for (i in seq_along(plot_subgroups)) {
  combined_plot <- reduce(plot_subgroups[[i]], `+`) + 
    plot_layout(ncol = 3) + # Adjust to 2 columns if using subgroups of 6
    theme(plot.title = element_text(size = 16),
          legend.text = element_text(size = 22))
  
  # Salve in pdf
  ggsave(paste0("results/synthetic_controls/group_", i, ".pdf"),
         combined_plot, width = 30, height = 20, dpi = 300)}


# Salve each plots as PDF
for(plot_name in plot_names) {
  plot_path <- paste0(here::here("results", "synthetic_controls"), "/", plot_name, ".pdf")
  print(plot_path)
  ggsave(plot_path, plot = get(plot_name), device = "pdf", width = 10, height = 8, dpi = 300)}


# Save table
latex_content <- print(latex_table, 
                       type = "latex", 
                       include.rownames = TRUE, 
                       include.colnames = TRUE, 
                       floating = FALSE, 
                       print.results = FALSE)
writeLines(latex_content, 
           here::here("results", "tables", "synthetic_weights.tex"))

readr::write_csv(
  x            = latex_table,
  file         = paste0(here::here("results", "synthetic_controls"),
                        "/synthetic_weights_redd.csv"),
  na           = "NA",
  col_names    = TRUE)
