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
final_buffer <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_buffer.csv"))

final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

final_redd <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

final_properties <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_properties.csv"))

final_intersections <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_intersections.csv"))

issuances <- 
  readr::read_csv(here::here("results", "projects_base", "projects_issuance.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================
# Set seed
set.seed(56)

# Set this variable for one of the imported datasets and merge info with issuances df
dataset <- final_buffer

dataset <- dataset %>% 
  left_join(issuances, by = c("id_registry" = "ID", "year" = "year"))

# Add new column in third position
# Identifying the original columns from 'final_redd_valid' and the new columns from 'issuances'
original_cols <- names(final_redd_valid)
new_cols <- setdiff(names(dataset), original_cols)

# Reordering the columns
dataset <- dataset %>%
  dplyr::select(all_of(original_cols[1:2]), # Keeps the first two original columns in place
                all_of(new_cols), # Adds new columns from 'issuances' in the third position
                all_of(original_cols[-c(1:2)])) # Adds the rest of the original columns afterwards

# Apply function from 'src/config/config_utils.R' to get filter data
dataset_aud_valid <- filter_type_variation(dataset, type = "AUD", tolerance = 0.05,
                                        additional = TRUE)
dataset_apd_valid <- filter_type_variation(dataset, type = "APD", tolerance = 0.05,
                                        additional = TRUE)


###############################################################################
# MULTIPLE TREATED UNITS
###############################################################################
features <- list(c("Total_natural_formation"))
covs_adj <- list(c("constant","trend"))

df_aud <- scpi::scdataMulti(dataset_aud_valid, id.var = "id_registry",
                            outcome.var = "Total_natural_formation",
                            treatment.var = "treatment",
                            cointegrated.data = TRUE,
                            cov.adj = covs_adj,
                            features = features,
                            time.var = "year")

df_apd <- scpi::scdataMulti(dataset_apd_valid, id.var = "id_registry",
                            outcome.var = "Total_natural_formation",
                            treatment.var = "treatment",
                            cointegrated.data = TRUE,
                            cov.adj = covs_adj,
                            features = features,
                            time.var = "year")

# Prediction of Synthetic Control
result_aud <- scpi::scest(df_aud, w.constr = list("name" = "simplex"))
result_apd <- scpi::scest(df_apd, w.constr = list("name" = "simplex"))

# Check predictions
scplotMulti(result_aud)
scplotMulti(result_apd)

# Plotting
plots_aud <- scplotMulti(result_aud, type = "series", joint = FALSE, ncols = 4)
plots_apd <- scplotMulti(result_apd, type = "series", joint = FALSE)

# Show plots
plots_aud
plots_apd


######################################################
# average unit treatment effect (\tau_{i.})
######################################################
# features <- list(c("Total_natural_formation", "retired_credits"))
covs_adj <- list(c("constant","trend"))

df_aud_unit_time <- scpi::scdataMulti(dataset_aud_valid, id.var = "id_registry",
                                      outcome.var = "Total_natural_formation",
                                      treatment.var = "treatment",
                                      cointegrated.data = TRUE,
                                      cov.adj = covs_adj,
                                      features = features, effect = "unit-time",
                                      time.var = "year", constant = TRUE)

df_apd_unit_time <- scpi::scdataMulti(dataset_apd_valid, id.var = "id_registry",
                                      outcome.var = "Total_natural_formation",
                                      treatment.var = "treatment",
                                      cointegrated.data = TRUE,
                                      cov.adj = covs_adj,
                                      features = features, effect = "unit-time",
                                      time.var = "year", constant = TRUE)

# Prediction of Synthetic Control - anticipation = 0
result_aud_unit_time <- scpi::scest(df_aud_unit_time, w.constr = list("name" = "simplex"))
result_apd_unit_time <- scpi::scest(df_apd_unit_time, w.constr = list("name" = "simplex"))

# Show results
scplotMulti(result_aud_unit_time)
scplotMulti(result_apd_unit_time)

# Estimate staggered synthetic controls with Prediction Intervals
synthetic_aud_unit_time <- scpi::scpi(df_aud_unit_time,
                                      w.constr = list("name" = "simplex"),
                                      cores = 3, sims = 50, e.method = "gaussian")

synthetic_apd_unit_time <- scpi::scpi(df_apd_unit_time,
                                      w.constr = list("name" = "simplex"),
                                      cores = 3, sims = 50, e.method = "gaussian")


# plot treatment
scplotMulti(synthetic_aud_unit_time, type = "series", joint = TRUE,
            save.data = "results/synthetic_controls/synthetic_aud_unit_time.pdf")
scplotMulti(synthetic_apd_unit_time, type = "series", joint = TRUE,
            save.data = "results/synthetic_controls/synthetic_apd_unit_time.pdf")

######################################################
# average treatment effect on the treated (\tau_{.k})
######################################################
features <- list(c("Total_natural_formation"))
covs_adj <- list(c("constant","trend"))

# get the Average treatment effect on the treated (\tau_{.k})
df_aud_att <- scpi::scdataMulti(dataset_aud_valid, id.var = "id_registry",
                                outcome.var = "Total_natural_formation",
                                treatment.var = "treatment",
                                cointegrated.data = TRUE,
                                cov.adj = covs_adj,
                                features = features, effect = "time",
                                time.var = "year")

df_apd_att <- scpi::scdataMulti(dataset_apd_valid, id.var = "id_registry",
                                outcome.var = "Total_natural_formation",
                                treatment.var = "treatment",
                                cointegrated.data = TRUE,
                                cov.adj = covs_adj,
                                features = features, effect = "time",
                                time.var = "year")

# Prediction of Synthetic Control
result_aud_att <- scest(df_aud_att, w.constr = list("name" = "simplex"))
result_apd_att <- scest(df_apd_att, w.constr = list("name" = "simplex"))

# Show results
scplotMulti(result_aud_att)
scplotMulti(result_apd_att)


# Estimate staggered synthetic controls with Prediction Intervals
synthetic_aud_att <- scpi::scpi(df_aud_att,
                                w.constr = list("name" = "simplex"),
                                cores = 3, sims = 50, e.method = "gaussian")

synthetic_apd_att <- scpi::scpi(df_apd_att,
                                w.constr = list("name" = "simplex"),
                                cores = 3, sims = 50, e.method = "gaussian")

# Plot treatment
plot_att_aud <- scplotMulti(synthetic_aud_att, type = "series", joint = TRUE)
plot_att_apd <- scplotMulti(synthetic_apd_att, type = "series", joint = TRUE)


efecct <- synthetic_apd_att$data$Y.post.agg - synthetic_apd_att$est.results$Y.post.fit
print(efecct)
# ============================
# Plotting
# ============================
plot <- ggplot(synthetic_aud_att$data) + xlab("Date") + ylab("Outcome") +
  geom_line(aes(x=Time, y=Y, colour=Type)) + 
  geom_point(aes(x=Time, y=Y, colour=Type), size=1.5) + 
  geom_vline(aes(xintercept=Tdate)) +
  facet_wrap(~ID, ncol = 2) + theme(legend.position="bottom") +
  scale_color_manual(name = "", values = c("black", "blue"),
                     labels = c("Treated", "Synthetic Control"))

plot.w1 <- plot + geom_errorbar(data = synthetic_aud_att,
                                aes(x = Time, ymin = lb.gaussian, ymax = ub.gaussian),
                                colour = "blue",
                                width = 0.5, linetype = 1)
