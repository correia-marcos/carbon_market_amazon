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

# Run function from 'src/config/config_utils.R' to check if fonts have already been imported
check_and_import_fonts()
# ============================================================================================
# I: Import data
# ============================================================================================
final_redd_valid <- 
  readr::read_csv(here::here("results", "final_dataset_synthetic", "final_redd_valid.csv"))

# ============================================================================================
# II: Process data
# ============================================================================================

# Set seed
set.seed(56)

# Apply function from 'src/config/config_utils.R' to get filter data
dataset_aud_valid <- filter_type_variation(final_redd_valid, type = "AUD", tolerance = 0.005,
                                           additional = FALSE)
dataset_apd_valid <- filter_type_variation(final_redd_valid, type = "APD", tolerance = 0.005,
                                           additional = TRUE)

# Get the id of all projects and all treated projects in both datasets
projects_aud <- unique(dataset_aud_valid$id_registry)
projects_apd <- unique(dataset_apd_valid$id_registry)
treated_aud <- unique((dataset_aud_valid %>% filter(!is.na(certification_date)))$id_registry)
treated_apd <- unique((dataset_apd_valid %>% filter(!is.na(certification_date)))$id_registry)

###############################################################################
# MULTIPLE TREATED UNITS
###############################################################################
features <- list(c("Total_natural_formation"))
covs_adj <- list(c("constant", "trend"))

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
                                cores = 3, sims = 10, e.method = "gaussian")

synthetic_apd_att <- scpi::scpi(df_apd_att,
                                w.constr = list("name" = "simplex"),
                                cores = 3, sims = 200, e.method = "gaussian")

# Save the ATT effect as dataframe
att_aud <- as.data.frame(
  synthetic_aud_att[["data"]][["Y.post.agg"]] - 
  synthetic_aud_att[["est.results"]][["Y.post.fit"]])

# Save the ATT effect as dataframe
att_apd <- as.data.frame(
  synthetic_apd_att[["data"]][["Y.post.agg"]] - 
  synthetic_apd_att[["est.results"]][["Y.post.fit"]])


# Plot treatment
plot_att_aud_series <- scplotMulti(synthetic_aud_att,
                                   type = "series",
                                   e.out = TRUE,
                                   joint = TRUE,
                                   col.treated = "black",
                                   col.synth = "darkred")
plot_att_aud_treat <- scplotMulti(synthetic_aud_att,
                                   type = "treatment",
                                   e.out = TRUE,
                                   joint = TRUE,
                                   col.treated = "black",
                                   col.synth = "darkred")

plot_att_apd_series <- scplotMulti(synthetic_apd_att,
                                   type = "series",
                                   e.out = TRUE,
                                   joint = TRUE,
                                   col.treated = "black",
                                   col.synth = "darkred")
plot_att_apd_treat <- scplotMulti(synthetic_apd_att,
                                   type = "treatment",
                                   e.out = TRUE,
                                   joint = TRUE,
                                   col.treated = "black",
                                   col.synth = "darkred")

# ============================
# Plotting
# ============================
final_aud_plot1 <- plot_att_aud_series[[1]]
final_aud_plot2 <- plot_att_aud_treat[[1]]
final_apd_plot1 <- plot_att_apd_series[[1]]
final_apd_plot2 <- plot_att_apd_treat[[1]]

final_aud_plot1 <- final_aud_plot1 +
  labs(title = NULL,
       y = "Natural formation (in percentage)",
       x = "Date (in years)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", size = 20),
        axis.text = element_text(color = "black", size = 20),
        text = element_text(family = "Times New Roman"),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm"),
        strip.text.x = element_blank()) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = Inf, y = Inf, label = "", hjust = 1.1, vjust = 1.1)

final_aud_plot2 <- final_aud_plot2 +
  labs(title = NULL,
       y = "Effect (in percentage)",
       x = "Date (in years)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", size = 20),
        axis.text = element_text(color = "black", size = 20),
        text = element_text(family = "Times New Roman"),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm"),
        strip.text.x = element_blank()) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = Inf, y = Inf, label = "", hjust = 1.1, vjust = 1.1)
  

final_apd_plot1 <- final_apd_plot1 +
  labs(title = NULL,
       y = "Natural formation (in percentage)",
       x = "Date (in years)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", size = 20),
        axis.text = element_text(color = "black", size = 20),
        text = element_text(family = "Times New Roman"),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm"),
        strip.text.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = Inf, y = Inf, label = "", hjust = 1.1, vjust = 1.1)

final_apd_plot2 <- final_apd_plot2 +
  labs(title = NULL,
       y = "Effect (in percentage)",
       x = "Date (in years)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "black", size = 20),
        axis.text = element_text(color = "black", size = 20),
        text = element_text(family = "Times New Roman"),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm"),
        strip.text.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("text", x = Inf, y = Inf, label = "", hjust = 1.1, vjust = 1.1)


# Combine plots
combined_plot_aud <- final_aud_plot1 | final_aud_plot2 +
  plot_annotation(title = "ATT for AUD REDD+ Projects", 
                  caption = "Subtitles: (a) Actual and synthetic time series, 
                  (b) ATT treatment effect") +
  plot_layout(guides = 'collect') +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.caption = element_text(size = 14)
  )

# ============================================================================================
# III: Save processed data
# ============================================================================================
ggsave(here::here("results", "synthetic_controls", "aud_att_series.pdf"), final_aud_plot1,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "synthetic_controls", "aud_att_treat.pdf"), final_aud_plot2,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "synthetic_controls", "apd_att_series.pdf"), final_aud_plot1,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

ggsave(here::here("results", "synthetic_controls", "apd_att_treat.pdf"), final_apd_plot2,
       device = cairo_pdf, width = 16, height = 9, units = "in", dpi = 600)

readr::write_csv(
  x            = att_aud,
  file         = paste0(here::here("results", "synthetic_controls"),
                        "/ATT_aud_values.csv"),
  na           = "NA",
  col_names    = TRUE)

readr::write_csv(
  x            = att_apd,
  file         = paste0(here::here("results", "synthetic_controls"),
                        "/ATT_apd_values.csv"),
  na           = "NA",
  col_names    = TRUE)
