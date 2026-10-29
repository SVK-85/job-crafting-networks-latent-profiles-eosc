#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                  ┏┓┏┳┓┏┓┏┓  ┓┏┓                              #
#                                  ┗┓ ┃ ┣ ┃┃  ┃┏┛                              #
#                                  ┗┛ ┻ ┗┛┣┛  ┻┗━                              #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                             Exploratory Latent profiles                      #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.1 Select scores, impute missing values, and estimate latent profiles
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.1.1 Set the random seed for reproducibility
set.seed(20261021)

# 12.1.2 Select the seven job crafting dimension scores
jc13_lpa <- jc11_scores %>%
dplyr::select(JCQ_TC, 
              JCQ_CC, 
              JCQ_RC, 
              JCS_ISJR,
              JCS_DHJD, 
              JCS_ISOJR, 
              JCS_ICJD) %>%

# 12.1.3 Impute missing scores using a single imputation procedure
tidyLPA::single_imputation(method = "imputeData") %>%

# 12.1.4 Estimate one- to seven-profile solutions using mclust
tidyLPA::estimate_profiles(n_profiles = 1:7,
                           models = 1,
                           package = "mclust")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.2 Inspect all solutions and save the fitted models
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.2.1 Extract fit indices and sort by number of profiles
jc13_fit <- tidyLPA::get_fit(jc13_lpa) %>% dplyr::arrange(Classes)

# 12.2.2 Display selected fit and classification indices
print(dplyr::select(jc13_fit, 
                    Model, 
                    Classes, 
                    AIC, 
                    BIC, 
                    SABIC,
                    Entropy, 
                    prob_min, 
                    n_min, 
                    BLRT_p), width = Inf)
# 12.2.3 Save all fitted profile models
saveRDS(jc13_lpa, file.path(output_dir_12, "1.Step12_tidyLPA_Models.rds"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.3 Identify the provisional minimum-BIC solution
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.3.1 Select the solution with the lowest finite BIC
jc13_best_fit <- jc13_fit %>% dplyr::filter(is.finite(BIC)) %>%
dplyr::slice_min(BIC, n = 1, with_ties = FALSE)

# 12.3.2 Display the fit indices of the selected solution
print(jc13_best_fit, width = Inf)

# 12.3.3 Extract the optimal number of latent profiles
jc13_best_k <- jc13_best_fit$Classes[1]

# 12.3.4 Retrieve the corresponding fitted profile model
# Model order follows 1:7, matching list positions to profile counts.
jc13_best_model <- jc13_lpa[[jc13_best_k]]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.4 Compare the BIC and AHP recommendations
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.4.1 Compare profile solutions using information criteria and AHP
jc13_comparison <- tidyLPA::compare_solutions(jc13_lpa, statistics = c(
                                              "AIC", "AWE", "BIC", "CLC", "KIC"))
# 12.4.2 Display the model comparison results
print(jc13_comparison)

# 12.4.3 Extract the solution recommended by the AHP procedure
jc13_ahp_fit <- jc13_comparison$fits[jc13_comparison$AHP, ]

# 12.4.4 Display the fit indices of the AHP-selected solution
print(jc13_ahp_fit, width = Inf)

# 12.4.5 Compare BIC and AHP recommendations without changing model selection
jc13_selection_comparison <- dplyr::bind_rows(BIC = jc13_best_fit,
                                              AHP = jc13_ahp_fit, 
                                              .id = "selection_criterion")

# 12.4.6 Display the recommendations from both selection criteria
print(jc13_selection_comparison, width = Inf)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.5 Select fitted models and define a common standardization reference
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.5.1 Retrieve the fitted two- to seven-profile solutions
jc13_model2 <- jc13_lpa[["model_1_class_2"]]
jc13_model3 <- jc13_lpa[["model_1_class_3"]]
jc13_model4 <- jc13_lpa[["model_1_class_4"]]
jc13_model5 <- jc13_lpa[["model_1_class_5"]]
jc13_model6 <- jc13_lpa[["model_1_class_6"]]
jc13_model7 <- jc13_lpa[["model_1_class_7"]]

# 12.5.2 Define the seven job crafting dimensions in plotting order
jc13_vars <- c("JCQ_TC", 
               "JCQ_CC", 
               "JCQ_RC", 
               "JCS_ISJR",
               "JCS_DHJD", 
               "JCS_ISOJR", 
               "JCS_ICJD")

# 12.5.3 Extract the completed dataset from the five-profile solution
jc13_model5_data <- tidyLPA::get_data(jc13_model5)

# 12.5.4 Compute reference means and SDs for score standardization
jc13_reference <- as.data.frame(psych::describe(jc13_model5_data[jc13_vars]))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.6 Calculate class sizes and percentages across profile solutions
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.6.1 Count participants assigned to each class in the two-profile model
jc13_sizes2 <- tidyLPA::get_data(jc13_model2) %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 2L)

# 12.6.2 Count participants assigned to each class in the three-profile model
jc13_sizes3 <- tidyLPA::get_data(jc13_model3) %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 3L)

# 12.6.3 Count participants assigned to each class in the four-profile model
jc13_sizes4 <- tidyLPA::get_data(jc13_model4) %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 4L)

# 12.6.4 Count participants assigned to each class in the five-profile model
jc13_sizes5 <- jc13_model5_data %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 5L)

# 12.6.5 Count participants assigned to each class in the six-profile model
jc13_sizes6 <- tidyLPA::get_data(jc13_model6) %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 6L)

# 12.6.6 Count participants assigned to each class in the seven-profile model
jc13_sizes7 <- tidyLPA::get_data(jc13_model7) %>%
               dplyr::count(Class, name = "N") %>%
               dplyr::mutate(Classes = 7L)

# 12.6.7 Combine class counts and calculate within-solution proportions
jc13_sizes <- dplyr::bind_rows(jc13_sizes2, jc13_sizes3, jc13_sizes4,
                               jc13_sizes5, jc13_sizes6, jc13_sizes7) %>%
              dplyr::group_by(Classes) %>%
              dplyr::mutate(Proportion = as.numeric(proportions(N))) %>%
              dplyr::ungroup() %>%
              dplyr::select(Classes, Class, N, Proportion)

# 12.6.8 Define percentage labels with one decimal place
jc13_percent_label <- scales::label_percent(accuracy = 0.1)

# 12.6.9 Convert class proportions into formatted percentage labels
jc13_sizes$Percentage <- jc13_percent_label(jc13_sizes$Proportion)

# 12.6.10 Display class counts, proportions, and percentages
print(jc13_sizes, n = Inf)

# 12.6.11 Export class distributions for all six profile solutions
readr::write_csv(jc13_sizes, file.path(output_dir_12,
"2.Step12_2_to_7_Profile_Counts.csv"))


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.7 Extract and standardize the model-estimated class means
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.7.1 Extract class-specific means from the one- to seven-profile models
jc13_means <- tidyLPA::get_estimates(jc13_lpa) %>%
dplyr::filter(Model == 1, Classes %in% 2:7, Category == "Means")

# 12.7.2 Standardize estimated class means using the common reference sample
jc13_means$Mean_z <- as.numeric(scale(t(jc13_means$Estimate),
center = jc13_reference[jc13_means$Parameter, "mean"],
scale = jc13_reference[jc13_means$Parameter, "sd"]))

# 12.7.3 Export raw and standardized class means for all profile solutions
readr::write_csv(dplyr::select(jc13_means,
                               Model,
                               Classes,
                               Class,
                               Parameter,
                               Mean_raw = Estimate, Mean_z),
file.path(output_dir_12,
"3.Step12_2_to_7_Profile_Means_Z.csv"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.8 Arrange profile plots and save standardized bar figures
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.8.1 Extract class-specific means for plotting across all solutions
jc13_plot_means <- tidyLPA::get_estimates(jc13_lpa) %>%
dplyr::filter(Model == 1, Classes %in% 1:7, Category == "Means")

# 12.8.2 Standardize plotting means using the common reference sample
jc13_plot_means$Mean_z <- as.numeric(scale(
t(jc13_plot_means$Estimate),
center = jc13_reference[jc13_plot_means$Parameter, "mean"],
scale = jc13_reference[jc13_plot_means$Parameter, "sd"]))

# 12.8.3 Define the common y-axis limits for all profile figures
jc13_z_limits <- c(-2, 2)

# 12.8.4 Define short labels for the seven job crafting dimensions
jc13_profile_labels <- c(JCQ_TC = "JCQ-TC", 
                         JCQ_CC = "JCQ-CC",
                         JCQ_RC = "JCQ-RC", 
                         JCS_ISJR = "JCS-ISJR",
                         JCS_DHJD = "JCS-DHJD",
                         JCS_ISOJR = "JCS-ISOJR",
                         JCS_ICJD = "JCS-ICJD")

# 12.8.5 Define fill colours for each job crafting dimension
jc13_profile_colors <- c(JCQ_TC = "#A6CEE3", 
                         JCQ_CC = "#6EAED6",
                         JCQ_RC = "#347EB4", 
                         JCS_ISJR = "#FFE08A",
                         JCS_DHJD = "#FFC04D", 
                         JCS_ISOJR = "#F49A35",
                         JCS_ICJD = "#DE741F")

# 12.8.6 Define fill patterns for each job crafting dimension
jc13_profile_patterns <- c(JCQ_TC = "stripe", 
                           JCQ_CC = "stripe",
                           JCQ_RC = "stripe", 
                           JCS_ISJR = "stripe",
                           JCS_DHJD = "crosshatch", 
                           JCS_ISOJR = "circle",
                           JCS_ICJD = "wave")

# 12.8.7 Define pattern angles for each job crafting dimension
jc13_profile_angles <- c(JCQ_TC = 45, 
                         JCQ_CC = 135, 
                         JCQ_RC = 90,
                         JCS_ISJR = 0, 
                         JCS_DHJD = 45, 
                         JCS_ISOJR = 0,
                         JCS_ICJD = 0)

# 12.8.8 Define a plotting function for standardized profile bar charts
jc13_profile_plot <- function(k) {
ggplot2::ggplot(dplyr::filter(jc13_plot_means, Classes == k),
ggplot2::aes(x = factor(Class, levels = seq_len(k)), y = Mean_z,
fill = factor(Parameter, levels = jc13_vars),
pattern = factor(Parameter, levels = jc13_vars),
pattern_angle = factor(Parameter, levels = jc13_vars))) +
ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.45) +
ggpattern::geom_col_pattern(position = ggplot2::position_dodge(width = 0.84),
                            width = 0.78, colour = "grey30", 
                            linewidth = 0.2,
                            pattern_fill = "grey20", 
                            pattern_colour = "grey20",
                            pattern_alpha = 0.55,
                            pattern_density = 0.18, 
                            pattern_spacing = 0.012,
                            pattern_size = 0.12) +
ggplot2::scale_x_discrete(labels = paste("Profile", seq_len(k)),
                          expand = ggplot2::expansion(add = 0.55)) +
ggplot2::scale_y_continuous(breaks = -2:2,
labels = c("-2", "-1", "0", "+1", "+2"),
expand = ggplot2::expansion(mult = 0)) +
ggplot2::scale_fill_manual(values = jc13_profile_colors,
                           breaks = jc13_vars, 
                           labels = jc13_profile_labels,
name = NULL, drop = FALSE) +
ggpattern::scale_pattern_manual(values = jc13_profile_patterns,
                                breaks = jc13_vars, 
                                guide = "none") +
ggpattern::scale_pattern_angle_manual(values = jc13_profile_angles,
                                      breaks = jc13_vars, 
                                      guide = "none") +
ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(
pattern = unname(jc13_profile_patterns[jc13_vars]),
pattern_angle = unname(jc13_profile_angles[jc13_vars])))) +
ggplot2::coord_cartesian(ylim = jc13_z_limits) +
ggplot2::labs(x = NULL, y = NULL, title = NULL, subtitle = NULL,
              caption = NULL) +
ggplot2::theme_minimal(base_size = 16, base_family = "sans") +
ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
panel.grid.major.y = ggplot2::element_line(colour = "grey90",
                                           linewidth = 0.3),
panel.grid.minor = ggplot2::element_blank(),
axis.line.y = ggplot2::element_line(colour = "grey45", linewidth = 0.3),
axis.text.x = ggplot2::element_text(size = 16, colour = "grey20"),
axis.text.y = ggplot2::element_text(size = 14, colour = "grey20"),
legend.position = "right",
legend.title = ggplot2::element_blank(),
legend.text = ggplot2::element_text(size = 14),
legend.key.size = grid::unit(5, "mm"),
legend.box.spacing = grid::unit(6, "mm"),
plot.margin = ggplot2::margin(6, 8, 6, 8, unit = "mm"))
}

# 12.8.9 Generate, print, and save profile plots for the two- to six-profile solutions
for (k in 2:6) {
jc13_plot <- jc13_profile_plot(k)
print(jc13_plot)
ggplot2::ggsave(file.path(output_dir_12, paste0(k + 2, ".Step12_", k,
"_Profiles_Bars.pdf")),
jc13_plot, width = 320, height = 180, units = "mm",
device = grDevices::cairo_pdf, bg = "white")
}

# 12.8.10 Print the plot for the BIC-selected solution
if (jc13_best_k >= 2) {
jc13_plot <- jc13_profile_plot(jc13_best_k)
print(jc13_plot)
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.9 Examine work engagement and meaningful work across five profiles
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.9.1 Define the two distal outcome scores
jc13_outcome_names <- c("UWES3_total", "WAMI_total")

# 12.9.2 Extract the existing work engagement and meaningful work scores
jc13_outcome_scores <- jc11_scores[jc13_outcome_names]

# 12.9.3 Initialize the outcome dataset
jc13_outcomes <- jc13_outcome_scores

# 12.9.4 Add modal class assignments from the five-profile solution
jc13_outcomes$Class <- factor(jc13_model5_data$Class, levels = 1:5)

# 12.9.5 Count participants assigned to each profile
jc13_outcome_sizes <- jc13_outcomes %>%
dplyr::count(Class, name = "N_profile")

# 12.9.6 Reshape outcomes and standardize scores across participants
jc13_outcomes_long <- jc13_outcomes %>%
tidyr::pivot_longer(cols = dplyr::all_of(jc13_outcome_names),
                    names_to = "Outcome", 
                    values_to = "Score") %>%
dplyr::group_by(Outcome) %>%
dplyr::mutate(Score_z = as.numeric(scale(Score))) %>%
dplyr::ungroup() %>%
dplyr::mutate(Outcome = factor(Outcome, levels = jc13_outcome_names))

# 12.9.7 Calculate the overall reference mean and SD for each outcome
jc13_outcome_reference <- jc13_outcomes_long %>%
dplyr::group_by(Outcome) %>%
dplyr::summarise(Reference_mean = mean(Score, na.rm = TRUE),
                 Reference_sd = sd(Score, na.rm = TRUE),
                 .groups = "drop")

# 12.9.8 Summarize raw and standardized outcome scores within profiles
jc13_outcome_summary <- jc13_outcomes_long %>%
dplyr::group_by(Class, Outcome) %>%
dplyr::summarise(N_valid = psych::describe(Score, fast = TRUE)$n,
                 Mean_raw = mean(Score, na.rm = TRUE),
                 SD_raw = sd(Score, na.rm = TRUE),
                 Mean_z = mean(Score_z, na.rm = TRUE),
                 SD_z = sd(Score_z, na.rm = TRUE),
                 .groups = "drop") %>%
dplyr::left_join(jc13_outcome_sizes, by = "Class") %>%
dplyr::select(Class, Outcome, N_profile, N_valid,
              Mean_raw, SD_raw, Mean_z, SD_z)

# 12.9.9 Display descriptive outcome statistics across five profiles
print(jc13_outcome_summary, n = Inf, width = Inf)

# 12.9.10 Export outcome descriptives for the five-profile solution
readr::write_csv(jc13_outcome_summary, file.path(output_dir_12,
"10.Step12_5_Profiles_Outcome_Descriptives.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.10 Plot standardized work engagement and meaningful work by profile
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.10.1 Define short labels for the two distal outcomes
jc13_outcome_labels <- c(UWES3_total = "UWES-3", WAMI_total = "WAMI")

# 12.10.2 Define fill colours for work engagement and meaningful work
jc13_outcome_colors <- c(UWES3_total = "#A8D5BA", WAMI_total = "#E9A3A3")

# 12.10.3 Define distinct fill patterns for the two outcomes
jc13_outcome_patterns <- c(UWES3_total = "stripe", WAMI_total = "circle")

# 12.10.4 Define pattern angles for the two outcomes
jc13_outcome_angles <- c(UWES3_total = 45, WAMI_total = 0)

# 12.10.5 Create grouped bar charts of standardized outcome means
jc13_outcome_plot <- ggplot2::ggplot(jc13_outcome_summary,
                     ggplot2::aes(x = Class, y = Mean_z, 
                                  fill = Outcome,
                                  pattern = Outcome, 
                                  pattern_angle = Outcome)) +
ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.45) +
ggpattern::geom_col_pattern(position = ggplot2::position_dodge(width = 0.84),
                            width = 0.78, colour = "grey30", 
                            linewidth = 0.2, pattern_fill = "grey20", 
                            pattern_colour = "grey20", pattern_alpha = 0.55, 
                            pattern_density = 0.18, pattern_spacing = 0.012,
                            pattern_size = 0.12) +
ggplot2::scale_x_discrete(labels = paste("Profile", 1:5), drop = FALSE, 
                          expand = ggplot2::expansion(add = 0.55)) +
ggplot2::scale_y_continuous(breaks = -2:2,
labels = c("-2", "-1", "0", "+1", "+2"),
expand = ggplot2::expansion(mult = 0)) +
ggplot2::scale_fill_manual(values = jc13_outcome_colors, 
                           breaks = jc13_outcome_names, 
                           labels = jc13_outcome_labels, name = NULL,
                           drop = FALSE) +
ggpattern::scale_pattern_manual(values = jc13_outcome_patterns, 
                               breaks = jc13_outcome_names, 
                               guide = "none") +
ggpattern::scale_pattern_angle_manual(values = jc13_outcome_angles,
                                      breaks = jc13_outcome_names, 
                                      guide = "none") +
ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(
pattern = unname(jc13_outcome_patterns[jc13_outcome_names]),
pattern_angle = unname(jc13_outcome_angles[jc13_outcome_names])))) +
ggplot2::coord_cartesian(ylim = c(-2, 2)) +
ggplot2::labs(x = NULL, y = NULL, title = NULL, subtitle = NULL,
caption = NULL) +
ggplot2::theme_minimal(base_size = 16, base_family = "sans") +
ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
panel.grid.minor = ggplot2::element_blank(),
axis.line.y = ggplot2::element_line(colour = "grey45", linewidth = 0.3),
axis.text.x = ggplot2::element_text(size = 16, colour = "grey20"),
axis.text.y = ggplot2::element_text(size = 14, colour = "grey20"),
legend.position = "right", legend.title = ggplot2::element_blank(),
legend.text = ggplot2::element_text(size = 14),
legend.key.size = grid::unit(5, "mm"),
legend.box.spacing = grid::unit(6, "mm"),
plot.margin = ggplot2::margin(6, 8, 6, 8, unit = "mm"))

# 12.10.6 Display the standardized outcome profile chart
print(jc13_outcome_plot)

# 12.10.7 Export the outcome profile chart as a vector PDF
ggplot2::ggsave(file.path(output_dir_12,
"11.Step12_5_Profiles_Engagement_WAMI_Z.pdf"), jc13_outcome_plot,
width = 320, height = 180, units = "mm",
device = grDevices::cairo_pdf, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.11 Export the full dataset with five-profile assignments
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.11.1 Verify sample size and completeness of profile assignments
stopifnot(nrow(data_scored) == nrow(jc13_model5_data),
          !anyNA(jc13_model5_data$Class))

# 12.11.2 Retain the full dataset with the calculated scale scores
jc13_classified_data <- data_scored

# 12.11.3 Append the modal five-profile assignments for each participant
jc13_classified_data$LPA_Profile_5 <- as.integer(jc13_model5_data$Class)

# 12.11.4 Export the complete dataset with five-profile assignments
readr::write_csv(jc13_classified_data, file.path(output_dir_12,
"12.Step12_Dataset_With_5_Profile_Assignments.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.13 Create separate participant datasets for each latent profile
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.13.1 Extract participants assigned to Profile 1
jc13_profile1_data <- jc13_classified_data %>%
dplyr::filter(LPA_Profile_5 == 1L)

# 12.13.2 Extract participants assigned to Profile 2
jc13_profile2_data <- jc13_classified_data %>%
dplyr::filter(LPA_Profile_5 == 2L)

# 12.13.3 Extract participants assigned to Profile 3
jc13_profile3_data <- jc13_classified_data %>%
dplyr::filter(LPA_Profile_5 == 3L)

# 12.13.4 Extract participants assigned to Profile 4
jc13_profile4_data <- jc13_classified_data %>%
dplyr::filter(LPA_Profile_5 == 4L)

# 12.13.5 Extract participants assigned to Profile 5
jc13_profile5_data <- jc13_classified_data %>%
dplyr::filter(LPA_Profile_5 == 5L)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.14 Retain the five participant subsets for the reporting archive
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Store all five profile-specific datasets in a named list
jc13_profile_datasets <- list(Profile_1 = jc13_profile1_data,
                              Profile_2 = jc13_profile2_data,
                              Profile_3 = jc13_profile3_data,
                              Profile_4 = jc13_profile4_data,
                              Profile_5 = jc13_profile5_data)


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.15 Prepare the five-profile model for BCH outcome comparisons
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.15.1 Retrieve the fitted five-profile mclust model
jc13_bch_mclust <- jc13_lpa[["model_1_class_5"]]$model

# 12.15.2 Extract the observed job crafting indicators used in the LPA
jc13_bch_indicators <- jc13_bch_mclust$data

# 12.15.3 Extract estimated means, covariance matrices, and class proportions
jc13_bch_parameters <- list(means = t(jc13_bch_mclust$parameters$mean),
covs = jc13_bch_mclust$parameters$variance$sigma,
P.Z = jc13_bch_mclust$parameters$pro)

# 12.15.4 Calculate posterior class probabilities using LCPA
jc13_bch_posterior <- LCPA::get.P.Z.Xn.LPA(response = jc13_bch_indicators,
                                           means = jc13_bch_parameters$means,
                                           covs = jc13_bch_parameters$covs,
                                           P.Z = jc13_bch_parameters$P.Z)

# 12.15.5 Compare modal class assignments from mclust and LCPA
jc13_bch_assignments <- table(mclust = jc13_bch_mclust$classification,
LCPA = max.col(jc13_bch_posterior, ties.method = "first"))

# 12.15.6 Display agreement between the two classification procedures
print(jc13_bch_assignments)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.16 Select and standardize distal outcomes for BCH analysis
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.16.1 Select the existing UWES-3 and WAMI total scores
jc13_bch_outcomes <- jc13_outcome_scores[jc13_outcome_names]

# 12.16.2 Verify matching sample sizes between indicators and outcomes
stopifnot(nrow(jc13_bch_indicators) == nrow(jc13_bch_outcomes))

# 12.16.3 Standardize both distal outcomes using sample means and SDs
jc13_bch_outcomes_z <- as.data.frame(scale(jc13_bch_outcomes))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.17 Estimate BCH-adjusted distal outcome comparisons
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.17.1 Estimate BCH-adjusted comparisons for UWES-3 and WAMI
jc13_bch_fit <- LCPA::LCPA(response = jc13_bch_indicators,
                            L = 5,
                            type.analysis = "ZY",
                            type.model = "LPA",
                            dependent.variables = jc13_bch_outcomes_z,
                            family = "gaussian",
                            control.model = list(
                            params = jc13_bch_parameters,
                            constraint = "E0",
                            is.sort = FALSE),
                            method.3step = "BCH",
                            CEP.error = TRUE,
                            method.regression = "Analytic",
                            method.SE = "Analytic",
                            vis = TRUE)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.18 Inspect BCH results and extract distal outcome comparisons
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.18.1 Display the summary of the fitted BCH model
print(summary(jc13_bch_fit))

# 12.18.2 Extract BCH results for work engagement (UWES-3)
jc13_bch_uwes <- jc13_bch_fit$dependent.variables$t1$UWES3_total

# 12.18.3 Extract BCH results for meaningful work (WAMI)
jc13_bch_wami <- jc13_bch_fit$dependent.variables$t1$WAMI_total

# 12.18.4 Display the omnibus test for work engagement
print(jc13_bch_uwes$omnibus)

# 12.18.5 Display the omnibus test for meaningful work
print(jc13_bch_wami$omnibus)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.19 Extract BCH-adjusted outcome means and standard errors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.19.1 Assemble BCH-adjusted standardized means and standard errors
jc13_bch_means <- data.frame(Profile = 1:5,
                             UWES3_mean_z = as.numeric(jc13_bch_uwes$estimate),
                             UWES3_SE_z = as.numeric(jc13_bch_uwes$se),
                             WAMI_mean_z = as.numeric(jc13_bch_wami$estimate),
                             WAMI_SE_z = as.numeric(jc13_bch_wami$se))

# 12.19.2 Display BCH-adjusted means and standard errors across profiles
print(jc13_bch_means)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.20 Adjust omnibus p-values and archive the BCH analysis
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.20.1 Assemble BCH-adjusted omnibus Wald tests for both outcomes
jc13_bch_omnibus <- data.frame(
Outcome = jc13_outcome_names,
N_valid = c(jc13_bch_uwes$observations, jc13_bch_wami$observations),
N_missing = c(jc13_bch_uwes$omitted, jc13_bch_wami$omitted),
Wald = c(unname(jc13_bch_uwes$omnibus$statistic),
         unname(jc13_bch_wami$omnibus$statistic)),
df = c(unname(jc13_bch_uwes$omnibus$parameter),
       unname(jc13_bch_wami$omnibus$parameter)),
p_value = c(unname(jc13_bch_uwes$omnibus$p.value),
            unname(jc13_bch_wami$omnibus$p.value)))

# 12.20.2 Apply Holm correction across the two omnibus tests
jc13_bch_omnibus$p_Holm <- stats::p.adjust(
jc13_bch_omnibus$p_value, method = "holm")

# 12.20.3 Display the omnibus tests and adjusted p-values
print(jc13_bch_omnibus)

# 12.20.4 Archive the BCH model, diagnostics, and outcome results
saveRDS(list(model = jc13_bch_fit,
             outcome_reference = jc13_outcome_reference,
             assignment_check = jc13_bch_assignments,
             means_z = jc13_bch_means,
             omnibus = jc13_bch_omnibus),
file.path(output_dir_12, "13.Step12_LCPA_BCH_Analysis.rds"))


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.21 BCH pairwise comparisons and a final outcome table
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.1 Retrieve the existing BCH results
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.1.1 Restore archived BCH results if objects are unavailable
if (!exists("jc13_bch_fit") || !exists("jc13_outcome_reference") ||
!exists("jc13_bch_omnibus")) {
jc13_bch_archive <- readRDS(file.path(output_dir_12,
"13.Step12_LCPA_BCH_Analysis.rds"))
# 12.21.1.2 Recover the fitted BCH model
jc13_bch_fit <- jc13_bch_archive$model
# 12.21.1.3 Recover the outcome standardization reference
jc13_outcome_reference <- jc13_bch_archive$outcome_reference
# 12.21.1.4 Recover the adjusted omnibus test results
jc13_bch_omnibus <- jc13_bch_archive$omnibus
}
# 12.21.1.5 Define the two distal outcomes in reporting order
jc13_outcome_names <- c("UWES3_total", "WAMI_total")
# 12.21.1.6 Verify the presence of both outcomes in the BCH results
stopifnot(setequal(names(jc13_bch_fit$dependent.variables$t1),
jc13_outcome_names))
# 12.21.1.7 Extract BCH results for work engagement
jc13_bch_uwes <- jc13_bch_fit$dependent.variables$t1$UWES3_total
# 12.21.1.8 Extract BCH results for meaningful work
jc13_bch_wami <- jc13_bch_fit$dependent.variables$t1$WAMI_total

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.2 Import the BCH means and their full covariance matrices
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.2.1 Construct the estimated marginal means object for UWES-3
jc13_bch_uwes_emm <- emmeans::emmobj(bhat = jc13_bch_uwes$estimate,
                                     V = jc13_bch_uwes$vcov,
                                     levels = list(Profile = LETTERS[1:5]),
                                     df = Inf,
                                     estName = "mean_z")
# 12.21.2.2 Construct the estimated marginal means object for WAMI
jc13_bch_wami_emm <- emmeans::emmobj(bhat = jc13_bch_wami$estimate,
                                     V = jc13_bch_wami$vcov,
                                     levels = list(Profile = LETTERS[1:5]),
                                     df = Inf,
                                     estName = "mean_z")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.3 Compute the ten pairwise contrasts for each outcome
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.3.1 Estimate pairwise BCH contrasts for work engagement
jc13_bch_uwes_pairs <- emmeans::contrast(jc13_bch_uwes_emm,
                                         method = "pairwise")
# 12.21.3.2 Estimate pairwise BCH contrasts for meaningful work
jc13_bch_wami_pairs <- emmeans::contrast(jc13_bch_wami_emm,
                                         method = "pairwise")
# 12.21.3.3 Combine UWES-3 contrasts and retain their inferential statistics
jc13_bch_pairwise <- dplyr::bind_rows(UWES3_total = as.data.frame(summary(
jc13_bch_uwes_pairs, infer = c(TRUE, TRUE), level = 0.95, adjust = "none")),
# 12.21.3.4 Append WAMI contrasts and identify the compared profiles
WAMI_total = as.data.frame(summary(jc13_bch_wami_pairs, infer = c(TRUE, TRUE), 
level = 0.95, adjust = "none")), .id = "Outcome") %>% tidyr::separate(contrast, 
into = c("Profile_1", "Profile_2"),
sep = " - ", remove = FALSE) %>% dplyr::group_by(Outcome) %>% 
# 12.21.3.5 Apply Holm correction within each outcome
dplyr::mutate(p_Holm = stats::p.adjust(p.value, method = "holm")) %>%
# 12.21.3.6 Rename statistics and flag significant adjusted contrasts
dplyr::ungroup() %>% dplyr::rename(difference_z = estimate, SE_difference_z = SE,
CI95_lower_z_pointwise = asymp.LCL, CI95_upper_z_pointwise = asymp.UCL, Wald_z = z.ratio,
p_value = p.value) %>% dplyr::mutate(significant_Holm = p_Holm < 0.05)
# 12.21.3.7 Display all pairwise comparisons for both outcomes
print(jc13_bch_pairwise, n = 20, width = Inf)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.4 Identify the other profiles with significantly different means
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.4.1 Retain profile comparisons significant after Holm correction
jc13_bch_significant_pairs <- jc13_bch_pairwise %>% 
# 12.21.4.2 Extract the outcome and both compared profiles
dplyr::filter(significant_Holm) %>% dplyr::select(Outcome, Profile_1, Profile_2)
# 12.21.4.3 Register significant comparisons in both profile directions
jc13_bch_table_letters <- dplyr::bind_rows(jc13_bch_significant_pairs %>%
# 12.21.4.4 Identify the first profile and its comparison partner
dplyr::rename(Profile = Profile_1, Other = Profile_2), 
jc13_bch_significant_pairs %>%
# 12.21.4.5 Reverse the comparison to include the second profile
dplyr::rename(Profile = Profile_2, Other = Profile_1)) %>%
# 12.21.4.6 Sort significant comparisons by outcome and profile
dplyr::arrange(Outcome, Profile, Other) %>%
# 12.21.4.7 Group comparison partners within each profile and outcome
dplyr::group_by(Outcome, Profile) %>%
# 12.21.4.8 Concatenate profiles with significantly different means
dplyr::summarise(differs_from = paste(Other, collapse = ","), .groups = "drop")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.5 Retain numeric means and express them in original score units
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.5.1 Extract BCH-adjusted UWES-3 means and confidence intervals
jc13_bch_table_means <- dplyr::bind_rows(UWES3_total = as.data.frame(summary(
#
jc13_bch_uwes_emm, infer = c(TRUE, FALSE), level = 0.95, adjust = "none")),
# 12.21.5.2 Append BCH-adjusted WAMI means and confidence intervals
WAMI_total = as.data.frame(summary(jc13_bch_wami_emm, infer = c(TRUE, FALSE), 
#
level = 0.95, adjust = "none")), .id = "Outcome") %>%
# 12.21.5.3 Attach significant pairwise comparison labels
dplyr::left_join(jc13_bch_table_letters, by = c("Outcome", "Profile")) %>%
# 12.21.5.4 Convert profile labels to numbers and replace missing labels
dplyr::mutate(Profile_number = match(Profile, LETTERS[1:5]),
differs_from = tidyr::replace_na(differs_from, "")) %>%
# 12.21.5.5 Rename standardized standard errors and confidence limits
dplyr::rename(SE_mean_z = SE,CI95_lower_z_pointwise = asymp.LCL, 
CI95_upper_z_pointwise = asymp.UCL) %>%
# 12.21.5.6 Attach the original outcome means and standard deviations
dplyr::left_join(jc13_outcome_reference, by = "Outcome") %>%
# 12.21.5.7 Back-transform BCH estimates into original score units
dplyr::mutate(Mean_raw = Reference_mean + Reference_sd * mean_z,
SE_mean_raw = Reference_sd * SE_mean_z, CI95_lower_raw_pointwise = Reference_mean + 
Reference_sd * CI95_lower_z_pointwise, CI95_upper_raw_pointwise = Reference_mean + 
Reference_sd * CI95_upper_z_pointwise)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.6 Prepare the final table with five profile columns and overall tests
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.21.6.1 Rename omnibus p-values and format Wald test statistics
jc13_bch_table_overall <- jc13_bch_omnibus %>%
dplyr::rename(omnibus_p_value = p_value, omnibus_p_Holm = p_Holm) %>%

dplyr::mutate(Stars = dplyr::case_when(omnibus_p_Holm < 0.001 ~ "***",
                                       omnibus_p_Holm < 0.01 ~ "**",
                                       omnibus_p_Holm < 0.05 ~ "*",
                                       TRUE ~ ""),
                                       Overall_test = paste0(sprintf("%.2f", Wald), Stars)) %>%
                                       dplyr::select(-Stars)
# 12.21.6.2 Format profile means with significant comparison labels
jc13_bch_final_table <- jc13_bch_table_means %>% dplyr::mutate(Profile_column = paste0("Profile_", 
                                                               Profile_number, "_", Profile),
Cell = dplyr::if_else(differs_from == "", sprintf("%.2f", Mean_raw), 
                      sprintf("%.2f [%s]", Mean_raw, differs_from))) %>%
# 12.21.6.3 Retain outcome names, profile columns, and formatted means
dplyr::select(Outcome, Profile_column, Cell) %>%
# 12.21.6.4 Arrange the five profiles in separate table columns
tidyr::pivot_wider(names_from = Profile_column, values_from = Cell) %>%
# 12.21.6.5 Append omnibus Wald tests and Holm-adjusted p-values
dplyr::left_join(jc13_bch_table_overall, by = "Outcome") %>%
# 12.21.6.6 Select and order the final reporting columns
dplyr::select(Outcome, N_valid, N_missing, Profile_1_A, Profile_2_B, Profile_3_C,
              Profile_4_D, Profile_5_E, Overall_test, Wald, df, omnibus_p_value, 
              omnibus_p_Holm)
# 12.21.6.7 Display the final BCH outcome comparison table
print(jc13_bch_final_table, width = Inf)
# 12.21.6.8 Export the BCH comparison table for publication
readr::write_csv(jc13_bch_final_table, file.path(output_dir_12,
"14.Step12_LCPA_BCH_Final_Table.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.22 Compare age, gender, and education across the five profiles
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.1 Select profile assignments and demographic variables
jc13_demo_data <- jc13_classified_data %>%
dplyr::select(LPA_Profile_5, Age, Gender, Educational_level)
# 12.22.2 Verify sample size consistency with the BCH analysis
stopifnot(nrow(jc13_demo_data) == nrow(jc13_bch_fit$arguments$response))
# 12.22.3 Calculate age descriptives within each latent profile
jc13_demo_age_descriptives <- jc13_demo_data %>%
dplyr::group_by(LPA_Profile_5) %>%
# 12.22.4 Summarize valid observations, mean age, and standard deviation
dplyr::summarise(N_profile = dplyr::n(),
N_valid = psych::describe(Age, fast = TRUE)$n,
Mean_years = mean(Age, na.rm = TRUE),
SD_years = sd(Age, na.rm = TRUE),
.groups = "drop")
# 12.22.5 Define original gender and education response categories
jc13_demo_original_labels <- data.frame(Variable = c(rep("Gender", 3), 
rep("Educational_level", 6)), Original_code = c(0:2, 0:5),
Category_label = c("Male", "Female", "Does not identify with the available options",
"Incomplete primary", "Primary", "Lower secondary",
"Upper secondary or advanced vocational", "University degree",
"Postgraduate"))
# 12.22.6 Reshape categorical demographic variables into long format
jc13_demo_frequencies <- jc13_demo_data %>%
tidyr::pivot_longer(cols = c(Gender, Educational_level),
# 12.22.7 Count observed demographic categories within each profile
names_to = "Variable", values_to = "Original_code") %>%
# 12.22.8 Group demographic frequencies by profile and variable
dplyr::count(LPA_Profile_5, Variable, Original_code, name = "N") %>%
# 12.22.9 Calculate category proportions within profiles
dplyr::group_by(LPA_Profile_5, Variable) %>%
# 12.22.10 Convert category frequencies into within-profile proportions
dplyr::mutate(Proportion = as.numeric(proportions(N))) %>%
# 12.22.11 Restore original demographic category labels
dplyr::ungroup() %>% dplyr::left_join(
jc13_demo_original_labels, by = c("Variable", "Original_code"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.12 Define demographic variables for corrected inference
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.12.1 Standardize age and recode gender and education
jc13_demo_analysis <- jc13_demo_data %>%
dplyr::transmute(Age = as.numeric(scale(Age)),
                 Gender = dplyr::if_else(Gender %in% c(0, 1), Gender, NA_real_),
                 Education = dplyr::case_when(Educational_level %in% 0:3 ~ 0,
                 Educational_level == 4 ~ 1,
                 Educational_level == 5 ~ 2,
                 TRUE ~ NA_real_))
# 12.22.12.2 Define labels for the analyzed demographic categories
jc13_demo_labels <- data.frame(Variable = c("Gender", "Gender", rep("Education", 3)),
Category = c("0", "1", "0", "1", "2"),
Category_label = c("Male", "Female", "Non-university education",
                    "University degree", "Postgraduate"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.13 Estimate corrected demographic auxiliary comparisons
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.13.1 Estimate BCH-adjusted comparisons of mean age
jc13_demo_age_fit <- LCPA::LCPA(response = jc13_bch_fit$arguments$response,
                                L = 5,
                                type.analysis = "ZY",
                                type.model = "LPA",
                                dependent.variables = jc13_demo_analysis["Age"],
                                family = "gaussian",
                                control.model = list(params = jc13_bch_fit$params, 
                                                     constraint = "E0", 
                                                     is.sort = FALSE),
                                method.3step = "BCH",
                                CEP.error = TRUE,
                                method.regression = "Analytic",
                                method.SE = "Analytic",
                                vis = TRUE)
# 12.22.13.2 Estimate corrected ML comparisons for categorical variables
jc13_demo_categorical_fit <- LCPA::LCPA(response = jc13_bch_fit$arguments$response,
                                        L = 5,
                                        type.analysis = "ZY",
                                        type.model = "LPA",
                  dependent.variables = jc13_demo_analysis[c("Gender", "Education")],
                                        family = "categorical",
                                        control.model = list(
                                        params = jc13_bch_fit$params, 
                                        constraint = "E0", 
                                        is.sort = FALSE),
                                        method.3step = "ML",
                                        CEP.error = TRUE,
                                        method.regression = "Analytic",
                                        method.SE = "Analytic",
                                        vis = TRUE)
# 12.22.13.3 Display both demographic model summaries
print(summary(jc13_demo_age_fit))
print(summary(jc13_demo_categorical_fit))
# 12.22.13.4 Extract age, gender, and education results
jc13_demo_age <- jc13_demo_age_fit$dependent.variables$t1$Age
jc13_demo_gender <- jc13_demo_categorical_fit$dependent.variables$t1$Gender
jc13_demo_education <- jc13_demo_categorical_fit$dependent.variables$t1$Education
# 12.22.13.5 Collect the fitted demographic models for archiving
saveRDS(list(Age_BCH = jc13_demo_age_fit, 
              Categorical_ML = jc13_demo_categorical_fit),
# 12.22.13.6 Specify the model archive destination
file.path(output_dir_12, "15.Step12_Demographics_Models.rds"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.14 Retain omnibus tests and estimation diagnostics
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.14.1 Initialize the demographic omnibus results table
jc13_demo_omnibus <- data.frame(Variable = c("Age", "Gender", "Education"),
# 12.22.14.2 Identify the estimation method for each variable
Method = c("BCH", "Corrected ML three-step", "Corrected ML three-step"),
# 12.22.14.3 Record the valid sample sizes
N_valid = c(jc13_demo_age$observations, 
            jc13_demo_gender$observations,
            jc13_demo_education$observations),
# 12.22.14.4 Record omitted observations
N_omitted = c(jc13_demo_age$omitted, 
              jc13_demo_gender$omitted,
              jc13_demo_education$omitted),
# 12.22.14.5 Extract the omnibus Wald statistics
Wald = c(unname(jc13_demo_age$omnibus$statistic),
         unname(jc13_demo_gender$omnibus$statistic),
         unname(jc13_demo_education$omnibus$statistic)),
# 12.22.14.6 Extract the Wald test degrees of freedom
df = c(unname(jc13_demo_age$omnibus$parameter),
       unname(jc13_demo_gender$omnibus$parameter),
       unname(jc13_demo_education$omnibus$parameter)),
# 12.22.14.7 Extract the unadjusted omnibus p-values
p_value = c(jc13_demo_age$omnibus$p.value,
             jc13_demo_gender$omnibus$p.value,
             jc13_demo_education$omnibus$p.value),
# 12.22.14.8 Record the convergence diagnostics
Converged = c(jc13_demo_age$converged, 
              jc13_demo_gender$converged,
              jc13_demo_education$converged),
# 12.22.14.9 Record parameter admissibility
Admissible = c(jc13_demo_age$admissible, 
               jc13_demo_gender$admissible,
               jc13_demo_education$admissible),
# 12.22.14.10 Record class shifts and apply Holm correction
Class_shift = c(jc13_demo_age$class.shift, 
                jc13_demo_gender$class.shift,
                jc13_demo_education$class.shift)) %>%
dplyr::mutate(p_Holm = stats::p.adjust(p_value, method = "holm"))
# 12.22.14.11 Display the omnibus results and diagnostics
print(jc13_demo_omnibus)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.15 Express corrected age means and contrasts in years
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.15.1 Describe age in the full participant sample
jc13_demo_age_reference <- psych::describe(jc13_demo_data$Age, fast = TRUE)
# 12.22.15.2 Construct standardized BCH age marginal means
jc13_demo_age_emm_z <- emmeans::emmobj(bhat = jc13_demo_age$estimate,
                                       V = jc13_demo_age$vcov,
                                       levels = list(Profile = LETTERS[1:5]),
                                       df = Inf)
# 12.22.15.3 Back-transform age estimates into years
jc13_demo_age_emm <- emmeans::contrast(jc13_demo_age_emm_z,
                                       method = "identity",
                                       scale = jc13_demo_age_reference$sd,
                                       offset = jc13_demo_age_reference$mean,
                                       name = "Profile")
# 12.22.15.4 Extract age means and pointwise confidence intervals
jc13_demo_age_means <- as.data.frame(summary(jc13_demo_age_emm, 
                                             infer = c(TRUE, FALSE), 
                                             level = 0.95, 
                                             adjust = "none")) %>%
dplyr::rename(Mean_years = estimate, 
             SE_years = SE, 
             CI95_lower_years_pointwise = asymp.LCL,
              CI95_upper_years_pointwise = asymp.UCL)
# 12.22.15.5 Compute pairwise profile contrasts for age
jc13_demo_age_pairs <- as.data.frame(summary(emmeans::contrast(jc13_demo_age_emm, method = "pairwise"),
# 12.22.15.6 Request unadjusted pointwise confidence intervals
infer = c(TRUE, TRUE), level = 0.95, adjust = "none")) %>% 
# 12.22.15.7 Rename age difference and uncertainty statistics
dplyr::rename(Difference_years = estimate, SE_difference_years = SE, CI95_lower_years_pointwise = asymp.LCL,
# 12.22.15.8 Complete labels and adjust pairwise p-values
CI95_upper_years_pointwise = asymp.UCL, Wald_z = z.ratio, p_value = p.value) %>%
dplyr::mutate(p_Holm = stats::p.adjust(p_value, method = "holm"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.16 Retain categorical probabilities and pairwise tests
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 12.22.16.1 Collect categorical results for gender and education
jc13_demo_categorical <- list(Gender = jc13_demo_gender, 
                              Education = jc13_demo_education)
# 12.22.16.2 Initialize the estimated probability results
jc13_demo_probability_list <- list()
# 12.22.16.3 Initialize the pairwise comparison results
jc13_demo_pairwise_list <- list()
# 12.22.16.4 Process each categorical demographic variable
for (variable in c("Gender", "Education")) {
# 12.22.16.5 Retrieve the current variable results
jc13_demo_result <- jc13_demo_categorical[[variable]]
# 12.22.16.6 Determine the number of response categories
jc13_demo_ncat <- ncol(jc13_demo_result$estimate)
# 12.22.16.7 Collect class-specific category probabilities and SEs
jc13_demo_probability_list[[variable]] <- data.frame(Profile = rep(LETTERS[1:5], 
                                                     each = jc13_demo_ncat),
                                                     Category = rep(colnames(
                                                     jc13_demo_result$estimate), 
                                                     times = 5),
                                                     Probability = as.vector(t(
                                                     jc13_demo_result$estimate)),
                                                     SE_probability = as.vector(t(
                                                     jc13_demo_result$se)))
jc13_demo_keep <- rep(seq_len(jc13_demo_ncat) < jc13_demo_ncat, times = 5)
# 12.22.16.8 Exclude the reference category from contrast estimation
jc13_demo_probability_emm <- emmeans::emmobj(
bhat = as.vector(t(jc13_demo_result$estimate))[jc13_demo_keep],
V = jc13_demo_result$vcov[jc13_demo_keep, jc13_demo_keep],
levels = list(Category = colnames(jc13_demo_result$estimate)[-jc13_demo_ncat],
Profile = LETTERS[1:5]), df = Inf)
# 12.22.16.9 Form pairwise profile contrasts by category
jc13_demo_distribution_contrasts <- emmeans::contrast(jc13_demo_probability_emm,
                                                        method = "pairwise",
                                                        by = "Category")
# 12.22.16.10 Calculate joint tests of category differences
jc13_demo_pairwise_list[[variable]] <- as.data.frame(emmeans::test(
jc13_demo_distribution_contrasts, joint = TRUE, by = "contrast"))
}
jc13_demo_probabilities <- dplyr::bind_rows(jc13_demo_probability_list, 
.id = "Variable") %>%
# 12.22.16.11 Match probability estimates to category labels
dplyr::left_join(jc13_demo_labels, by = c("Variable", "Category"))
# 12.22.16.12 Combine categorical pairwise comparison tables
jc13_demo_categorical_pairs <- dplyr::bind_rows(
# 12.22.16.13 Group pairwise results by demographic variable
jc13_demo_pairwise_list, .id = "Variable") %>%
dplyr::group_by(Variable) %>%
# 12.22.16.14 Apply Holm correction within each variable
dplyr::mutate(p_Holm = stats::p.adjust(p.value, method = "holm")) %>%
# 12.22.16.15 Remove grouping from the corrected results
dplyr::ungroup() %>%
# 12.22.16.16 Select the pairwise reporting statistics
dplyr::select(Variable, contrast, df = df1, Wald_chisq = Chisq,
p_value = p.value, p_Holm)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.17 Prepare the demographic reporting table for Quarto
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.22.17.1 Define percentage labels with one decimal place
jc13_demo_percent <- scales::label_percent(accuracy = 0.1)
# 12.22.17.2 Begin the combined demographic reporting table
jc13_demo_final_table <- dplyr::bind_rows(
jc13_demo_age_means %>%
# 12.22.17.3 Label the age estimate in years
dplyr::transmute(Variable = "Age", 
# 12.22.17.4 Format the age mean for each profile
Category_label = "Mean age (years)", Profile, Cell = sprintf("%.2f", Mean_years)),
jc13_demo_probabilities %>%
# 12.22.17.5 Select the category probabilities for reporting
dplyr::transmute(Variable, Category_label, Profile,
Cell = jc13_demo_percent(Probability))) %>%
# 12.22.17.6 Arrange profiles into separate columns
tidyr::pivot_wider(names_from = Profile, values_from = Cell) %>%
# 12.22.17.7 Attach the omnibus test results
dplyr::left_join(jc13_demo_omnibus, by = "Variable")
# 12.22.17.8 Display the demographic reporting table
print(jc13_demo_final_table)
# 12.22.17.9 Export the final demographic table for Quarto
readr::write_csv(jc13_demo_final_table, file.path(output_dir_12,
"16.Step12_Demographics_Final_Table.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 12.23 Archive numerical reporting details and participant subsets
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 12.23.1 Collect outcome estimates and inferential results
jc13_reporting <- list(Outcomes = list(reference = jc13_outcome_reference, 
descriptives = jc13_outcome_summary, pairwise = jc13_bch_pairwise,
means_and_letters = jc13_bch_table_means, final_table = jc13_bch_final_table,
note = NULL),
# 12.23.2 Collect demographic results and diagnostics
Demographics = list(age_descriptives = jc13_demo_age_descriptives,
original_frequencies = jc13_demo_frequencies, original_labels = jc13_demo_original_labels,
analysis_labels = jc13_demo_labels, omnibus = jc13_demo_omnibus,
age_reference = jc13_demo_age_reference, age_means = jc13_demo_age_means,
age_pairwise = jc13_demo_age_pairs, probabilities = jc13_demo_probabilities,
categorical_pairwise = jc13_demo_categorical_pairs, final_table = jc13_demo_final_table,
note = NULL), Profile_datasets = jc13_profile_datasets)
# 12.23.3 Save the complete reporting archive as an RDS file
saveRDS(jc13_reporting, file.path(output_dir_12,
"17.Step12_Reporting_Objects.rds"))
