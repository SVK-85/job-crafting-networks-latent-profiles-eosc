#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                ┏┓┏┳┓┏┓┏┓  ┓┏━                                #
#                                ┗┓ ┃ ┣ ┃┃  ┃┗┓                                #
#                                ┗┛ ┻ ┗┛┣┛  ┻┗┛                                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#         Bridge Nodes Between Job Crafting and WAMI/UWES Correlates           #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.1 Define the two communities and the common graphic style
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.1.1 Create the results directory for this step
output_dir_15 <- file.path(results_root_dir, "Step15_Bridge_Nodes")
dir.create(output_dir_15, recursive = TRUE, showWarnings = FALSE)

# 15.1.2 Assign the seven job crafting scores and the two correlates
bridge_communities <- stats::setNames(
ifelse(instrument %in% c("JCQ", "JCS"), "Job Crafting", "Correlates"), dims)

# 15.1.3 Use one light colour for each community in every Step 15 figure
bridge_colors <- c("Job Crafting" = "#F2D39B",
                   "Correlates" = "#8FDDE8")
bridge_border_color <- "#E31A1C"

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.2 Calculate expected influence and bridge expected influence
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.2.1 Calculate the raw bridge indices on the Step 13 network
bridge_raw <- networktools::bridge(network = weights,
                                   communities = unname(bridge_communities[dims]),
                                   useCommunities = "all",
                                   directed = FALSE,
                                   nodes = dims,
                                   normalize = FALSE)

# 15.2.2 Normalize bridge indices for the different community sizes
bridge_normalized <- networktools::bridge(network = weights,
                                          communities = unname(bridge_communities[dims]),
                                          useCommunities = "all",
                                          directed = FALSE,
                                          nodes = dims,
                                          normalize = TRUE)

# 15.2.3 Reuse the expected influence calculated in Step 13
expected_influence <- EI$ExpectedInfluence[
match(dims, EI$variable)]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.3 Identify and export the bridge nodes
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.3.1 Combine expected influence and the native bridge indices
bridge_results <- data.frame(variable = dims,
                             label = varnames_net,
                             instrument = instrument,
                             community = unname(bridge_communities[dims]),
                             expected_influence = unname(expected_influence),
                             bridge_strength_raw = unname(bridge_raw[["Bridge Strength"]][dims]),
                             bridge_strength_normalized = unname(
                             bridge_normalized[["Bridge Strength"]][dims]),
                             bridge_ei_1_raw = unname(
                             bridge_raw[["Bridge Expected Influence (1-step)"]][dims]),
                             bridge_ei_1_normalized = unname(
                             bridge_normalized[["Bridge Expected Influence (1-step)"]][dims]),
                             row.names = NULL)

# 15.3.2 Rank signed bridge expected influence within each community
bridge_results$rank_within_community <- ave(bridge_results$bridge_ei_1_raw,
bridge_results$community, FUN = function(x) rank(-x, ties.method = "min"))

# 15.3.3 Retain the highest value and any exact ties in each community
maximum_bridge_ei <- ave(bridge_results$bridge_ei_1_raw,
                         bridge_results$community, FUN = max)
bridge_results$is_bridge_node <- 
abs(bridge_results$bridge_ei_1_raw - maximum_bridge_ei) <=
sqrt(.Machine$double.eps)
bridge_results$selection_rule <- ifelse(
bridge_results$is_bridge_node,
"Highest signed one-step Bridge EI within community",
"Not the within-community maximum")
bridge_node_results <- bridge_results[
bridge_results$is_bridge_node, , drop = FALSE]

# 15.3.4 Export all bridge indices and the selected bridge nodes
print(bridge_results[order(bridge_results$community,
                           bridge_results$rank_within_community), ],
row.names = FALSE)
readr::write_csv(bridge_results, file.path(output_dir_15,
"1.Step15_Bridge_Centrality.csv"), na = "NA")
readr::write_csv(bridge_node_results, file.path(output_dir_15,
"2.Step15_Bridge_Nodes.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.4 Plot expected influence and bridge expected influence together
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.4.1 Combine EI and normalized Bridge EI in long format
ei_bei_results <- rbind(data.frame(variable = dims,
                                   label = varnames_net,
                                   community = unname(bridge_communities[dims]),
                                   measure = "Expected Influence (1-step)",
                                    value = unname(expected_influence)),
data.frame(variable = dims,
label = varnames_net,
           community = unname(bridge_communities[dims]),
           measure = "Bridge Expected Influence (1-step, normalized)",
           value = unname(
bridge_normalized[["Bridge Expected Influence (1-step)"]][dims])))

# 15.4.2 Standardize each index separately for the common display
ei_bei_results$measure <- factor(ei_bei_results$measure,
               levels = c("Expected Influence (1-step)",
       "Bridge Expected Influence (1-step, normalized)"))
ei_bei_results$z <- ave(ei_bei_results$value,
                        ei_bei_results$measure,
FUN = function(x) as.numeric(scale(x)))
readr::write_csv(ei_bei_results, file.path(output_dir_15,
"3.Step15_EI_and_Bridge_EI.csv"), na = "NA")

# 15.4.3 Plot centrality and bridge centrality in two aligned panels
ei_bei_plot <- ggplot2::ggplot(ei_bei_results,
ggplot2::aes(x = z,
y = factor(label, levels = rev(varnames_net)), group = 1)) +
ggplot2::geom_vline(xintercept = 0, colour = "grey70",
                    linetype = 2, linewidth = 0.35) +
ggplot2::geom_path(colour = "#303030", linewidth = 0.5) +
ggplot2::geom_point(ggplot2::aes(fill = community),
                    shape = 21, size = 3.2,
                    colour = "#333333", stroke = 0.65) +
ggplot2::facet_wrap(ggplot2::vars(measure), nrow = 1) +
ggplot2::scale_fill_manual(values = bridge_colors,
                           breaks = names(bridge_colors),
                           name = NULL) +
ggplot2::labs(x = "Centrality (z score)", y = NULL) +
ggplot2::theme_minimal(base_size = 12) +
ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               legend.position = "bottom")

# 15.4.4 Export the combined EI and Bridge EI figure
ggplot2::ggsave(file.path(output_dir_15,
"4.Step15_EI_Bridge_EI_Centrality.pdf"), ei_bei_plot,
width = 10, height = 5.5, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_15,
"5.Step15_EI_Bridge_EI_Centrality.png"), ei_bei_plot,
width = 10, height = 5.5, units = "in", dpi = 300, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.5 Plot the network in the same style as Step 13
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.5.1 Reuse the Step 13 MGM predictability rings in node order
node_predictability <- predictability$R2_for_ring[
match(dims, predictability$variable)]

# 15.5.2 Define the two network groups and the bridge-node borders
bridge_groups <- list(
"Job Crafting" = which(bridge_communities[dims] == "Job Crafting"),
"Correlates" = which(bridge_communities[dims] == "Correlates"))
bridge_border_colors <- ifelse(
bridge_results$is_bridge_node,
bridge_border_color, "#333333")
bridge_border_widths <- ifelse(
bridge_results$is_bridge_node, 2.4, 1)

# 15.5.3 Keep the Step 13 layout, edges, node size, and predictability rings
bridge_network <- qgraph::qgraph(weights,
                                 layout = network_layout,      
                                 labels = varnames_net,
                                 groups = bridge_groups,
                                 color = unname(bridge_colors[names(bridge_groups)]),
                                 theme = "colorblind",
                                 edge.labels = FALSE,
                                 vsize = 8,
                                 label.cex = 0.9,
                                 borders = TRUE,
                                 border.color = bridge_border_colors,
                                 border.width = bridge_border_widths,
                                 pie = node_predictability,
                                 legend = FALSE,
                                 mar = c(3, 3, 3, 10),
                                 DoNotPlot = TRUE)

# 15.5.4 Add one legend for the communities and red bridge-node border
plot_bridge_network <- function(network) {
plot(network)
graphics::legend(x = 1.45, y = 0, legend = c(names(bridge_colors), "Bridge node"),
pch = 21, pt.bg = c(unname(bridge_colors), "white"),
col = c("#333333", "#333333", bridge_border_color),
pt.lwd = c(1, 1, 2.4), pt.cex = 1.45, cex = 0.8, bty = "n", xjust = 0.5,
yjust = 0.5, xpd = NA)
}

# 15.5.5 Export the network with the explanatory legend
grDevices::pdf(file.path(output_dir_15,
"6.Step15_Bridge_Network_Predictability.pdf"),
width = 8, height = 8, useDingbats = FALSE, bg = "white")
plot_bridge_network(bridge_network)
grDevices::dev.off()

grDevices::png(file.path(output_dir_15,
"7.Step15_Bridge_Network_Predictability.png"),
width = 8, height = 8, units = "in", res = 300, bg = "white",
type = if (capabilities("cairo")) "cairo" else getOption("bitmapType"))
plot_bridge_network(bridge_network)
grDevices::dev.off()

# 15.5.6 Save the native results and the plotted network object
saveRDS(list(raw = bridge_raw,
             normalized = bridge_normalized,
             bridge_table = bridge_results,
             bridge_nodes = bridge_node_results,
             ei_bei = ei_bei_results,
             colors = bridge_colors,
             bridge_border_color = bridge_border_color,
             bridge_border_colors = bridge_border_colors,
             bridge_border_widths = bridge_border_widths,
             predictability = node_predictability,
             network = bridge_network),
file.path(output_dir_15, "8.Step15_Bridge_Objects.rds"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 15.6 Bootstrap accuracy and stability of bridge expected influence
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 15.6.1 Define fresh bootstrap settings
bridge_boot_statistic <- "bridgeExpectedInfluence"
bridge_boot_membership <- unname(bridge_communities[dims])
bridge_boots <- as.integer(getOption("jobcrafting.bridge.nBoots", 2000L))
bridge_available_cores <- parallel::detectCores(logical = FALSE)
if (is.na(bridge_available_cores)) bridge_available_cores <- 1L
bridge_boot_cores <- as.integer(getOption("jobcrafting.bridge.nCores",
max(1L, min(7L, bridge_available_cores - 1L))))
bridge_boot_args <- list(normalize = TRUE, directed = FALSE)

# 15.6.2 Run a fresh case-dropping bootstrap and calculate the CS coefficient
set.seed(20261028L + 1601L)
bridge_boot_case <- bootnet::bootnet(network1, nCores = bridge_boot_cores,
nBoots = bridge_boots, type = "case", statistics = bridge_boot_statistic,
communities = bridge_boot_membership, useCommunities = "all",
bridgeArgs = bridge_boot_args, verbose = FALSE)
saveRDS(bridge_boot_case, file.path(output_dir_15,
"10.Step15_Bootstrap_Case_Bridge_EI.rds"))

bridge_cs_value <- bootnet::corStability(bridge_boot_case, cor = 0.7,
                                         statistics = bridge_boot_statistic, 
                                         verbose = FALSE)
bridge_cs <- data.frame(statistic = bridge_boot_statistic, 
                        normalized = TRUE, 
                        correlation = 0.7,
                        probability = 0.95, 
                        CS = unname(bridge_cs_value[bridge_boot_statistic]))
readr::write_csv(bridge_cs, file.path(output_dir_15,
"11.Step15_Bridge_EI_CS.csv"), na = "NA")

bridge_case_plot <- plot(bridge_boot_case,
                         statistics = bridge_boot_statistic,
                         panels = FALSE)
ggplot2::ggsave(file.path(output_dir_15, "12.Step15_Bridge_EI_Case_Stability.pdf"), 
bridge_case_plot, width = 9, height = 6, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_15, "13.Step15_Bridge_EI_Case_Stability.png"), 
bridge_case_plot, width = 9, height = 6, units = "in", dpi = 300, bg = "white")

# 15.6.3 Run a fresh nonparametric bootstrap and export percentile intervals
set.seed(20261028L + 1602L)
bridge_boot_nonparam <- bootnet::bootnet(network1, 
                                        nCores = bridge_boot_cores,
                                        nBoots = bridge_boots, 
                                        type = "nonparametric", 
                                        statistics = bridge_boot_statistic,
                                        communities = bridge_boot_membership, 
                                        useCommunities = "all",
                                        bridgeArgs = bridge_boot_args, 
                                        verbose = FALSE)
saveRDS(bridge_boot_nonparam, file.path(output_dir_15,
"14.Step15_Bootstrap_Nonparametric_Bridge_EI.rds"))

bridge_boot_summary <- as.data.frame(summary(bridge_boot_nonparam,
                                             statistics = bridge_boot_statistic))
bridge_boot_summary$label <- unname(varnames_net[match(bridge_boot_summary$id, dims)])
bridge_boot_summary$community <- unname(bridge_communities[bridge_boot_summary$id])
readr::write_csv(bridge_boot_summary, file.path(output_dir_15,
"15.Step15_Bridge_EI_Bootstrap_Intervals.csv"), na = "NA")

bridge_interval_plot <- plot(bridge_boot_nonparam, 
                             statistics = bridge_boot_statistic, 
                             plot = "area",
                             CIstyle = "quantiles",
                             labels = TRUE,
                             order = "sample",
                             panels = FALSE)
ggplot2::ggsave(file.path(output_dir_15,
"16.Step15_Bridge_EI_Bootstrap_Intervals.pdf"), bridge_interval_plot,
width = 9, height = 7, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_15,
"17.Step15_Bridge_EI_Bootstrap_Intervals.png"), bridge_interval_plot,
width = 9, height = 7, units = "in", dpi = 300, bg = "white")

# 15.6.4 Test all pairwise differences in normalized Bridge EI
bridge_ei_ids <- as.character(bridge_boot_nonparam$sampleTable$id[
bridge_boot_nonparam$sampleTable$type == bridge_boot_statistic])
bridge_difference_rows <- lapply(
seq_len(length(bridge_ei_ids) - 1L),
function(index) bootnet::differenceTest(bridge_boot_nonparam,
                                        x = bridge_ei_ids[index],
                                        y = bridge_ei_ids[(index + 1L):length(bridge_ei_ids)],
                                        measure = bridge_boot_statistic, 
                                        alpha = 0.05, 
                                        verbose = FALSE))
bridge_difference_tests <- do.call(rbind, bridge_difference_rows)
bridge_sample_values <- stats::setNames(
bridge_boot_nonparam$sampleTable$value[
bridge_boot_nonparam$sampleTable$type == bridge_boot_statistic],
bridge_boot_nonparam$sampleTable$id[
bridge_boot_nonparam$sampleTable$type == bridge_boot_statistic])
bridge_difference_tests$sample_difference_y_minus_x <- unname(
bridge_sample_values[bridge_difference_tests$id2] -
bridge_sample_values[bridge_difference_tests$id1])
readr::write_csv(bridge_difference_tests, file.path(output_dir_15,
"18.Step15_Bridge_EI_Difference_Tests.csv"), na = "NA")

bridge_difference_plot <- plot(bridge_boot_nonparam,
                               statistics = bridge_boot_statistic,
                               plot = "difference",
                               order = "sample",
                               labels = TRUE,
                               panels = FALSE)
ggplot2::ggsave(file.path(output_dir_15,
"19.Step15_Bridge_EI_Difference_Tests.pdf"), bridge_difference_plot,
width = 9, height = 8, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_15,
"20.Step15_Bridge_EI_Difference_Tests.png"), bridge_difference_plot,
width = 9, height = 8, units = "in", dpi = 300, bg = "white")
