#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                ┏┓┏┳┓┏┓┏┓  ┓┏┓                                #
#                                ┗┓ ┃ ┣ ┃┃  ┃ ┫                                #
#                                ┗┛ ┻ ┗┛┣┛  ┻┗┛                                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#       Observed Job Crafting Network, Meaningful Work and Work Engagement     #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.1 Select the observed scores and complete-case sample
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 13.1.1 Select the seven crafting dimensions and two total outcome scores
dims <- c("JCQ_TC", "JCQ_CC", "JCQ_RC", "JCS_ISJR", "JCS_DHJD",
          "JCS_ISOJR", "JCS_ICJD", "WAMI_total", "UWES3_total")
varnames_net <- c("TC", "CC", "RC", "ISJR", "DHJD", "ISOJR", "ICJD",
                  "WAMI", "UWES-3")
instrument <- c(rep("JCQ", 3), rep("JCS", 4), "WAMI", "UWES")
color_vector_pr <- stats::setNames(c(rep("#5B8FEA", 3), rep("#F28E2B", 4),
                                     "#59A14F", "#B07AA1"), dims)
aggregated_data_all <- as.data.frame(jc11_scores[, dims, drop = FALSE])

# 13.1.2 Retain the same complete cases for transformation and both networks

complete_rows <- stats::complete.cases(aggregated_data_all)
aggregated_data <- aggregated_data_all[complete_rows, , drop = FALSE]
case_ids <- data.frame(source_row = seq_len(nrow(aggregated_data_all)),
ID = data_scored$ID, included = complete_rows)

# 13.1.3 Describe and archive the analysis sample
sample_flow <- as.data.frame(table(status = factor(complete_rows,
levels = c(FALSE, TRUE), labels = c("Excluded", "Included"))))
names(sample_flow)[2] <- "N"
sample_flow$proportion <- as.numeric(proportions(sample_flow$N))
sample_flow$N_total <- nrow(aggregated_data_all)
print(sample_flow, row.names = FALSE)
readr::write_csv(sample_flow, file.path(output_dir_13,
"1.Step13_Sample_Flow.csv"), na = "NA")
readr::write_csv(cbind(case_ids, aggregated_data_all), file.path(output_dir_13,
"2.Step13_All_Observed_Scores.csv"), na = "NA")
readr::write_csv(cbind(case_ids[complete_rows, ], aggregated_data),
file.path(output_dir_13, "3.Step13_Complete_Case_Raw_Scores.csv"), na = "NA")

# 13.1.4 Archive the node order, labels, instruments, and colours
node_registry <- data.frame(score = dims, label = varnames_net,
instrument = instrument, color = unname(color_vector_pr))
readr::write_csv(node_registry, file.path(output_dir_13,
"4.Step13_Node_Registry.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.2 Apply the nonparanormal transformation
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 13.2.1 Transform the nine complete observed scores
aggregated_data_npn <- as.data.frame(huge::huge.npn(as.matrix(aggregated_data), 
                                     npn.func = "shrinkage", verbose = FALSE))
colnames(aggregated_data_npn) <- dims
readr::write_csv(cbind(case_ids[complete_rows, ], aggregated_data_npn),
file.path(output_dir_13, "5.Step13_Complete_Case_NPN_Scores.csv"), na = "NA")

# 13.2.2 Record the specifications used in this step
analysis_settings <- data.frame(parameter = c("N_input", 
                                               "N_network", 
                                               "nodes", 
                                               "missing", 
                                               "NPN_method",
                                               "network", 
                                               "EBIC_gamma", 
                                               "threshold", 
                                               "MGM_lambdaSel", 
                                               "MGM_folds",
                                               "MGM_rule", 
                                               "bootstrap_NPN", 
                                               "seed_base"),
value = c(nrow(aggregated_data_all), nrow(aggregated_data), length(dims),
"complete cases on nine observed scores", "shrinkage", "EBICglasso",
0.5, TRUE, "CV", 10, "AND", "reestimated within each resample", 20261025L))
readr::write_csv(analysis_settings, file.path(output_dir_13,
"6.Step13_Analysis_Settings.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.3 Describe the raw and transformed scores
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 13.3.1 Calculate descriptives on the same complete-case sample
desc_info <- as.data.frame(psych::describe(aggregated_data, na.rm = TRUE, type = 2))
desc_info_npn <- as.data.frame(psych::describe(aggregated_data_npn, na.rm = TRUE, type = 2))
print(desc_info)
print(desc_info_npn)

# 13.3.2 Combine the raw and NPN descriptive statistics
desc_columns <- c("n", "mean", "sd", "min", "max", "skew", "kurtosis")
descriptives <- rbind(data.frame(score = dims, 
                                 scale = "Raw", 
                                 desc_info[, desc_columns],
                                 row.names = NULL),
data.frame(score = dims, 
           scale = "NPN", 
           desc_info_npn[, desc_columns],
           row.names = NULL))

# 13.3.3 Flag skewness and excess kurtosis within +/-1
descriptives$within_abs1_skew_kurtosis <- with(descriptives,
abs(skew) <= 1 & abs(kurtosis) <= 1)
print(descriptives, row.names = FALSE)
readr::write_csv(descriptives, file.path(output_dir_13,
"8.Step13_Raw_and_NPN_Descriptives.csv"), na = "NA")
# Note: This is a descriptive flag, not a test of normality.

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.4 Compare descriptives and original-score reliability
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
reliability <- jc11_report[match(dims, jc11_report$score),
c("score", "n_items", "N_valid", "cronbach_alpha_raw",
"mcdonald_omega_total", "omega_factors")]
names(reliability)[names(reliability) == "N_valid"] <- "N_reliability"
reliability$reliability_sample <- "scale-specific complete cases, Step 10"

# 13.4.2 Combine the descriptive and reliability results without rounding
table6_df <- tidyr::pivot_wider(descriptives, 
                                id_cols = score, 
                                names_from = scale,
                                values_from = c(n, mean, sd, min, max, skew, kurtosis,
                                within_abs1_skew_kurtosis), names_glue = "{.value}_{scale}")
table6_df <- dplyr::left_join(table6_df, reliability, by = "score")
print(table6_df, width = Inf)
readr::write_csv(table6_df, file.path(output_dir_13,
"9.Step13_Descriptives_and_Original_Reliability.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.5 Plot the original score distributions
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.5.1 Arrange the observed scores in long format
plot_data <- tidyr::pivot_longer(aggregated_data,
cols = dplyr::everything(), names_to = "score", values_to = "value")
plot_data$score <- factor(plot_data$score, levels = rev(dims))

# 13.5.2 Plot the full observed-score range without an embedded title
plot1 <- ggplot2::ggplot(plot_data,
ggplot2::aes(x = value, y = score, fill = score)) +
ggridges::geom_density_ridges(alpha = 0.7, scale = 0.8, color = NA) +
ggplot2::scale_fill_manual(values = color_vector_pr, guide = "none") +
ggplot2::scale_y_discrete(labels = stats::setNames(varnames_net, dims)) +
ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.03)) +
ggridges::theme_ridges(font_size = 12) +
ggplot2::theme(legend.position = "none") +
ggplot2::labs(x = "Observed sum score", y = NULL, title = NULL)

# 13.5.3 Export the original distributions as PDF and PNG
ggplot2::ggsave(file.path(output_dir_13,
"10.Step13_Raw_Score_Distributions.pdf"), plot1,
width = 8, height = 7, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_13,
"11.Step13_Raw_Score_Distributions.png"), plot1,
width = 8, height = 7, units = "in", dpi = 300, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.6 Plot the NPN-transformed score distributions
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.6.1 Arrange the transformed scores in the same plotting order
plot_data_npn <- tidyr::pivot_longer(aggregated_data_npn,
cols = dplyr::everything(), names_to = "score", values_to = "value")
plot_data_npn$score <- factor(plot_data_npn$score, levels = rev(dims))

# 13.6.2 Plot the transformed score distributions
plot2 <- ggplot2::ggplot(plot_data_npn,
ggplot2::aes(x = value, y = score, fill = score)) +
ggridges::geom_density_ridges(alpha = 0.7, scale = 0.8, color = NA) +
ggplot2::scale_fill_manual(values = color_vector_pr, guide = "none") +
ggplot2::scale_y_discrete(labels = stats::setNames(varnames_net, dims)) +
ggridges::theme_ridges(font_size = 12) +
ggplot2::theme(legend.position = "none") +
ggplot2::labs(x = "NPN-transformed score", y = NULL, title = NULL)

# 13.6.3 Export the transformed distributions as PDF and PNG
ggplot2::ggsave(file.path(output_dir_13,
"12.Step13_NPN_Score_Distributions.pdf"), plot2,
width = 8, height = 7, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_13,
"13.Step13_NPN_Score_Distributions.png"), plot2,
width = 8, height = 7, units = "in", dpi = 300, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.7 Compare raw and transformed Pearson correlations
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.7.1 Calculate correlations and pointwise confidence intervals
correlation_data <- list(Raw = aggregated_data, NPN = aggregated_data_npn)
pairs <- utils::combn(seq_along(dims), 2)
correlation_results <- list()
for (version in names(correlation_data)) {
values <- correlation_data[[version]]
for (pair in seq_len(ncol(pairs))) {
first <- pairs[1, pair]
second <- pairs[2, pair]
test <- stats::cor.test(values[[first]], values[[second]],
                       method = "pearson", conf.level = 0.95)
correlation_results[[length(correlation_results) + 1L]] <- data.frame(
transformation = version, node1 = dims[first], node2 = dims[second],
N = nrow(values), r = unname(test$estimate), t = unname(test$statistic),
df = unname(test$parameter), p_value = test$p.value,
CI95_lower = test$conf.int[1], CI95_upper = test$conf.int[2], row.names = NULL)
}
}
correlations <- dplyr::bind_rows(correlation_results)
readr::write_csv(correlations, file.path(output_dir_13,
"14.Step13_Pearson_Correlations_Raw_and_NPN.csv"), na = "NA")
# Note: These conventional Pearson intervals do not adjust for fitting NPN.

# 13.7.2 Combine raw upper-triangle and NPN lower-triangle correlations
raw_cor <- stats::cor(aggregated_data)
npn_cor <- stats::cor(aggregated_data_npn)
table_combined <- raw_cor
table_combined[lower.tri(table_combined)] <- npn_cor[lower.tri(npn_cor)]
diag(table_combined) <- NA_real_
readr::write_csv(data.frame(variable = dims, raw_cor, check.names = FALSE),
file.path(output_dir_13, "15.Step13_Pearson_Raw_Matrix.csv"), na = "NA")
readr::write_csv(data.frame(variable = dims, npn_cor, check.names = FALSE),
file.path(output_dir_13, "16.Step13_Pearson_NPN_Matrix.csv"), na = "NA")
readr::write_csv(data.frame(variable = dims, table_combined,
check.names = FALSE), file.path(output_dir_13,
"17.Step13_Pearson_Combined_Matrix.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.8 Estimate the observed-score network using EBICglasso
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 13.8.1 Estimate the nine-node network and archive the fitted model
set.seed(20261025L + 19L)
network1 <- bootnet::estimateNetwork(data = aggregated_data,
                                     default = "EBICglasso",
                                     corMethod = "npn",
                                     missing = "listwise",
                                     tuning = 0.5,
                                     threshold = TRUE,
                                     labels = dims)
saveRDS(network1, file.path(output_dir_13,
"18.Step13_EBICglasso_Model.rds"))

# 13.8.2 Export all weights and unique candidate edges
weights <- network1$graph
dimnames(weights) <- list(dims, dims)
edge_index <- which(upper.tri(weights), arr.ind = TRUE)
edges <- data.frame(node1 = dims[edge_index[, 1]],
node2 = dims[edge_index[, 2]], partial_correlation = weights[edge_index])
edges$nonzero <- edges$partial_correlation != 0
readr::write_csv(data.frame(variable = dims, weights, check.names = FALSE),
file.path(output_dir_13, "19.Step13_EBICglasso_Weights.csv"), na = "NA")
readr::write_csv(edges, file.path(output_dir_13,
"20.Step13_EBICglasso_Edges.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.9 Estimate supplementary MGM node predictability
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 13.9.1 Estimate the Gaussian pairwise MGM on the transformed scores
set.seed(20261025L + 190L)
fit_mgm <- mgm::mgm(data = as.matrix(aggregated_data_npn),
                    type = rep("g", length(dims)),
                    level = rep(1, length(dims)),
                    k = 2,
                    lambdaSel = "CV",
                    lambdaFolds = 10,
                    ruleReg = "AND",
                    scale = TRUE,
                    saveModels = TRUE)
saveRDS(fit_mgm, file.path(output_dir_13, "21.Step13_MGM_Model.rds"))

# 13.9.2 Calculate and archive the native in-sample prediction errors
pred1 <- stats::predict(object = fit_mgm,
                        data = as.matrix(aggregated_data_npn),
                        errorCon = c("RMSE", "R2"),
                        errordecimals = Inf)
saveRDS(pred1, file.path(output_dir_13, "22.Step13_MGM_Predictions.rds"))
predictability <- data.frame(variable = dims, label = varnames_net,
as.data.frame(pred1$errors)[, c("RMSE", "R2"), drop = FALSE],
source = "Separate Gaussian MGM; in-sample prediction", row.names = NULL)

# 13.9.3 Keep the raw R-squared values and bound only the displayed rings
predictability$R2_for_ring <- pmin(1, pmax(0, predictability$R2))
predictability$ring_clamped <-
predictability$R2_for_ring != predictability$R2
print(predictability)
readr::write_csv(predictability, file.path(output_dir_13,
"23.Step13_MGM_Node_Predictability.csv"), na = "NA")

# 13.9.4 Calculate the mean node predictability
mean_predictability <- data.frame(N = nrow(aggregated_data),
nodes = length(dims), mean_R2 = mean(predictability$R2),
source = "Separate Gaussian MGM; in-sample prediction")
print(mean_predictability)
readr::write_csv(mean_predictability, file.path(output_dir_13,
"24.Step13_MGM_Mean_Predictability.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.10 Plot the network and overlay MGM predictability
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.10.1 Calculate and archive the common spring layout
set.seed(20261025L + 20L)
network_layout <- qgraph::qgraph(weights, layout = "spring",
                                 DoNotPlot = TRUE)$layout
layout_table <- data.frame(variable = dims, label = varnames_net,
x = network_layout[, 1], y = network_layout[, 2])
readr::write_csv(layout_table, file.path(output_dir_13,
"25.Step13_Network_Layout.csv"), na = "NA")

# 13.10.2 Plot EBICglasso edges and supplementary MGM R-squared rings
grDevices::pdf(file = file.path(output_dir_13,
"26.Step13_Network_MGM_Predictability.pdf"), width = 8, height = 8,
useDingbats = FALSE, bg = "white")
graph1 <- qgraph::qgraph(weights,
                         layout = network_layout,
                         labels = varnames_net,
                         color = unname(color_vector_pr[dims]),
                         theme = "colorblind",
                         edge.labels = FALSE,
                         vsize = 8,
                         label.cex = 0.9,
                         pie = predictability$R2_for_ring)
grDevices::dev.off()

# 13.10.3 Export the same network as a 300-dpi PNG
grDevices::png(file.path(output_dir_13,
"27.Step13_Network_MGM_Predictability.png"), width = 8, height = 8,
units = "in", res = 300, bg = "white",
type = if (capabilities("cairo")) "cairo" else getOption("bitmapType"))
plot(graph1)
grDevices::dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.11 Calculate strength, betweenness, and closeness
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.11.1 Calculate and archive the native centrality indices
cent_results <- qgraph::centrality_auto(weights, weighted = TRUE,
                                        signed = TRUE)
print(cent_results)
saveRDS(cent_results, file.path(output_dir_13,
"28.Step13_Centrality_Auto.rds"))
centrality_table <- data.frame(variable = dims, label = varnames_net,
cent_results$node.centrality, row.names = NULL, check.names = FALSE)
if (!"Strength" %in% names(centrality_table)) {
centrality_table$Strength <- rowSums(abs(weights))
}
readr::write_csv(centrality_table, file.path(output_dir_13,
"29.Step13_Centrality_Native.csv"), na = "NA")
readr::write_csv(data.frame(variable = dims,
cent_results$ShortestPathLengths, check.names = FALSE),
file.path(output_dir_13, "30.Step13_Shortest_Path_Lengths.csv"), na = "NA")
# Note: Shortest-path measures disregard edge signs. Retain the native
#       treatment of disconnected components and undefined values.

# 13.11.2 Standardize each centrality index without replacing undefined values
measures <- c("Strength", "Betweenness", "Closeness")
centrality_values <- as.matrix(centrality_table[measures])
centrality_values[!is.finite(centrality_values)] <- NA_real_
centrality_z <- scale(centrality_values)
centrality_z[!is.finite(centrality_z)] <- NA_real_
centrality_long <- data.frame(variable = rep(dims, length(measures)),
label = rep(varnames_net, length(measures)),
measure = rep(measures, each = length(dims)),
value = as.vector(as.matrix(centrality_table[measures])),
z = as.vector(centrality_z))
readr::write_csv(centrality_long, file.path(output_dir_13,
"31.Step13_Centrality_Raw_and_Z.csv"), na = "NA")

# 13.11.3 Plot the three standardized centrality indices
centrality_plot <- ggplot2::ggplot(centrality_long,
ggplot2::aes(x = z, y = factor(label, levels = rev(varnames_net)), group = 1)) +
ggplot2::geom_path(linewidth = 0.45, na.rm = TRUE) +
ggplot2::geom_point(size = 2, na.rm = TRUE) +
ggplot2::facet_wrap(ggplot2::vars(measure), nrow = 1) +
ggplot2::labs(x = "Centrality (z score)", y = NULL) +
ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(output_dir_13,
"32.Step13_Centrality.pdf"), centrality_plot,
width = 10, height = 5, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_13,
"33.Step13_Centrality.png"), centrality_plot,
width = 10, height = 5, units = "in", dpi = 300, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 13.12 Calculate and plot one-step expected influence
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 13.12.1 Calculate raw and standardized expected influence in node order
EI <- data.frame(variable = dims, label = varnames_net,
ExpectedInfluence = rowSums(weights))
EI$ExpectedInfluence_z <- as.numeric(scale(replace(EI$ExpectedInfluence,
!is.finite(EI$ExpectedInfluence), NA_real_)))
EI$ExpectedInfluence_z[!is.finite(EI$ExpectedInfluence_z)] <- NA_real_
print(EI)
readr::write_csv(EI, file.path(output_dir_13,
"34.Step13_Expected_Influence.csv"), na = "NA")

# 13.12.2 Plot standardized expected influence without fixed axis clipping
EI_plot <- ggplot2::ggplot(EI,
ggplot2::aes(x = factor(label, levels = varnames_net),
y = ExpectedInfluence_z, group = 1)) +
ggplot2::geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.3) +
ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
ggplot2::geom_point(shape = 21, fill = "white", size = 3, na.rm = TRUE) +
ggplot2::labs(x = NULL, y = "Expected influence (z score)") +
ggplot2::theme_minimal(base_size = 12) +
ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(file.path(output_dir_13,
"35.Step13_Expected_Influence.pdf"), EI_plot,
width = 9, height = 4.5, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_13,
"36.Step13_Expected_Influence.png"), EI_plot,
width = 9, height = 4.5, units = "in", dpi = 300, bg = "white")

# Note: Liu, Lafferty, & Wasserman (2009), JMLR, 10, 2295-2328.
#       https://www.jmlr.org/papers/v10/liu09a.html
#       Epskamp & Fried (2018), Psychological Methods, 23, 617-634.
#       doi: 10.1037/met0000167
#       Haslbeck & Waldorp (2018), Behavior Research Methods, 50, 853-861.
#       doi: 10.3758/s13428-017-0910-x
#       Robinaugh, Millner, & McNally (2016), J Abnorm Psychol, 125, 747-757.
#       doi: 10.1037/abn0000181
