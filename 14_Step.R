#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                ┏┓┏┳┓┏┓┏┓  ┓┏┓                                #
#                                ┗┓ ┃ ┣ ┃┃  ┃┃┃                                #
#                                ┗┛ ┻ ┗┛┣┛  ┻┗╋                                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#           Observed score network: bootstrap accuracy and stability           #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.0 Reuse the fitted network and define bootstrap settings
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 14.0.1 Check the fitted network and observed expected influence
ggm14_required_objects <- c("network1", "EI", "output_dir_14")
ggm14_missing_objects <- ggm14_required_objects[
!vapply(ggm14_required_objects, exists, logical(1), inherits = TRUE)]
if (length(ggm14_missing_objects) > 0L) {
stop("Run Step 13 in the same R session first. Missing: ",
     paste(ggm14_missing_objects, collapse = ", "))
}
stopifnot(dir.exists(output_dir_14))

# 14.0.2 Retain the original seeds and 2,000 bootstrap replications
ggm_seed <- 20261025L
ggm_boots <- getOption("jobcrafting.ggm.nBoots", 2000L)
ggm_available_cores <- parallel::detectCores(logical = FALSE)
if (is.na(ggm_available_cores)) ggm_available_cores <- 1L
ggm_cores <- getOption("jobcrafting.ggm.nCores",
                       max(1L, min(7L, ggm_available_cores - 1L)))
stopifnot(length(ggm_boots) == 1L, is.finite(ggm_boots), ggm_boots >= 2,
          ggm_boots == as.integer(ggm_boots),
          length(ggm_cores) == 1L, is.finite(ggm_cores), ggm_cores >= 1,
          ggm_cores == as.integer(ggm_cores))
ggm_boots <- as.integer(ggm_boots)
ggm_cores <- as.integer(ggm_cores)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.1 Bootstrap case-dropping stability of expected influence
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Why: Repeat NPN and EBICglasso within each resampled raw-score dataset.
#      Keep the native 5%-75% dropping grid and the original 2,000 replicates.
#      A constant sample EI vector has no defined correlation stability.

ggm_boot_case <- NULL
ggm_cs <- data.frame(statistic = "expectedInfluence", correlation = 0.7,
                      probability = 0.95, CS = NA_real_,
                      status = "Undefined: constant sample EI")
if (stats::sd(EI$ExpectedInfluence) > sqrt(.Machine$double.eps)) {
set.seed(ggm_seed + 231L)

ggm_boot_case <- bootnet::bootnet(network1, 
                                  nCores = ggm_cores, 
                                  nBoots = ggm_boots,
                                  type = "case", 
                                  statistics = "expectedInfluence")
saveRDS(ggm_boot_case, file.path(output_dir_14,
"1.Step14_Bootstrap_Case_EI.rds"))
ggm_cs_value <- bootnet::corStability(ggm_boot_case, 
                                      cor = 0.7, 
                                      statistics = "expectedInfluence")
ggm_cs$CS <- unname(ggm_cs_value["expectedInfluence"])
ggm_cs$status <- "Estimated on the configured case-dropping grid"

# 14.1.1 Export the native stability summary and figure
ggm_case_summary <- as.data.frame(summary(ggm_boot_case, statistics = "expectedInfluence"))
readr::write_csv(ggm_case_summary, file.path(output_dir_14,
"2.Step14_Case_EI_Stability_Summary.csv"), na = "NA")
ggm_case_plot <- plot(ggm_boot_case, statistics = "expectedInfluence", panels = FALSE)
 ggplot2::ggsave(file.path(output_dir_14,"3.Step14_Case_EI_Stability.pdf"), plot = ggm_case_plot,
width = 9, height = 6, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
ggplot2::ggsave(file.path(output_dir_14,
"4.Step14_Case_EI_Stability.png"), plot = ggm_case_plot,
width = 9, height = 6, units = "in", dpi = 300, bg = "white")
} else {
warning("EI is constant: case-dropping correlation stability is undefined.")
}
readr::write_csv(ggm_cs, file.path(output_dir_14,
"5.Step14_EI_Correlation_Stability.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.2 Bootstrap edges and EI together without repeating identical fits
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
set.seed(ggm_seed + 232L)
ggm_boot_nonparam <- bootnet::bootnet(network1, nCores = ggm_cores, nBoots = ggm_boots,
type = "nonparametric", statistics = c("edge", "expectedInfluence"))
saveRDS(ggm_boot_nonparam, file.path(output_dir_14,
"6.Step14_Bootstrap_Nonparametric.rds"))

# 14.2.1 Export the native summary and original-sample statistic identifiers
ggm_boot_summary <- as.data.frame(summary(
  ggm_boot_nonparam, statistics = c("edge", "expectedInfluence")))
readr::write_csv(ggm_boot_summary, file.path(output_dir_14,
"7.Step14_Nonparametric_Summary.csv"), na = "NA")
readr::write_csv(as.data.frame(ggm_boot_nonparam$sampleTable),
file.path(output_dir_14,
"8.Step14_Bootstrap_Sample_Statistics.csv"), na = "NA")

# 14.2.2 Record completed draws and nonfinite stored statistics
ggm_boot_diagnostics <- do.call(rbind, lapply(
list(Case_EI = ggm_boot_case, Nonparametric = ggm_boot_nonparam),
function(object) {
if (is.null(object)) {return(data.frame(type = "case", requested = ggm_boots,
completed = 0L, nonfinite_values = NA_integer_,
status = "Skipped: constant sample EI"))
}
data.frame(type = object$type, 
           requested = ggm_boots,
           completed = length(unique(object$bootTable$name)),
           nonfinite_values = sum(!is.finite(object$bootTable$value)),
           status = "Completed; inspect any native bootstrap warnings")
  }))
rownames(ggm_boot_diagnostics) <- NULL
readr::write_csv(ggm_boot_diagnostics, file.path(output_dir_14,
"9.Step14_Bootstrap_Diagnostics.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.3 Retain native pointwise difference tests with exact statistic IDs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Why: Bounds concern id2 minus id1, not overlap of two marginal intervals.
#      Tests are two-sided at alpha .05 and unadjusted for multiple comparisons.
#      Follow the original exploratory difference-test procedure explicitly.

ggm_difference_tests <- function(object, measure, ids) {
empty <- data.frame(id1 = character(), 
                    id2 = character(),
                    measure = character(), 
                    lower = numeric(),
                    upper = numeric(), 
                    significant = logical(),
                    sample_difference_y_minus_x = numeric())
if (length(ids) < 2L) return(empty)
rows <- lapply(seq_len(length(ids) - 1L), function(index) {
bootnet::differenceTest(object, 
                        x = ids[index], 
                        y = ids[(index + 1L):length(ids)],
                        measure = measure, 
                        alpha = 0.05, 
                        verbose = FALSE)
})
results <- do.call(rbind, rows)
original <- object$sampleTable[object$sampleTable$type == measure, ]
values <- stats::setNames(original$value, original$id)
results$sample_difference_y_minus_x <-
unname(values[results$id2] - values[results$id1])
rownames(results) <- NULL
results
}

# 14.3.1 Draw the same native difference tests that are exported to CSV
ggm_difference_plot <- function(tests, 
                                sample, 
                                show_values = FALSE,
                                diagonal_colors = NULL) {
sample <- sample[order(sample$value), c("id", "value")]
ids <- as.character(sample$id)
cells <- rbind(data.frame(id1 = tests$id1, 
                          id2 = tests$id2,
                          significant = tests$significant),
data.frame(id1 = tests$id2, 
            id2 = tests$id1, 
            significant = tests$significant),
data.frame(id1 = ids, 
          id2 = ids, 
          significant = NA))
cells$status <- ifelse(cells$id1 == cells$id2, "Diagonal",
ifelse(cells$significant, "Different", "Not different"))
cells$id1 <- factor(cells$id1, levels = ids)
cells$id2 <- factor(cells$id2, levels = ids)
fill_values <- c("Diagonal" = "white", 
                 "Different" = "black",
                 "Not different" = "grey80")
if (!is.null(diagonal_colors)) {
stopifnot(all(ids %in% names(diagonal_colors)),
!anyNA(diagonal_colors[ids]),
all(nzchar(diagonal_colors[ids])))
diagonal_cells <- cells$id1 == cells$id2
cells$status[diagonal_cells] <-
as.character(cells$id1[diagonal_cells])
fill_values <- c(fill_values, diagonal_colors[ids])
  }
figure <- ggplot2::ggplot(cells,
ggplot2::aes(x = id1, y = id2, fill = status)) +
ggplot2::geom_tile(color = "white", linewidth = 0.2) +
ggplot2::scale_fill_manual(values = fill_values, guide = "none") +
ggplot2::coord_fixed() +
ggplot2::labs(x = NULL, y = NULL) +
ggplot2::theme_minimal(base_size = 10) +
ggplot2::theme(panel.grid = ggplot2::element_blank(),
axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
if (show_values) {
diagonal <- data.frame(id1 = factor(ids, levels = ids),
                       id2 = factor(ids, levels = ids),
                       value = formatC(sample$value, digits = 2,
                                            format = "f"))
figure <- figure + ggplot2::geom_text(data = diagonal,
                   ggplot2::aes(x = id1, y = id2, label = value),
                   inherit.aes = FALSE, size = 3)
}
figure
}

# 14.3.2 Compare every pair of expected-influence values
ggm_ei_ids <- as.character(ggm_boot_nonparam$sampleTable$id[
ggm_boot_nonparam$sampleTable$type == "expectedInfluence"])
ggm_ei_differences <- ggm_difference_tests(
ggm_boot_nonparam, "expectedInfluence", ggm_ei_ids)
readr::write_csv(ggm_ei_differences, file.path(output_dir_14,

  "10.Step14_EI_Difference_Tests.csv"), na = "NA")
ggm_ei_difference_plot <- ggm_difference_plot(ggm_ei_differences,
ggm_boot_nonparam$sampleTable[
ggm_boot_nonparam$sampleTable$type == "expectedInfluence", ],
show_values = TRUE)

ggplot2::ggsave(file.path(output_dir_14,
"11.Step14_EI_Differences.pdf"), plot = ggm_ei_difference_plot,
width = 9, height = 7, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")

ggplot2::ggsave(file.path(output_dir_14,
"12.Step14_EI_Differences.png"), plot = ggm_ei_difference_plot,
width = 9, height = 7, units = "in", dpi = 300, bg = "white")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.4 Compare nonzero edges using the same nonparametric replicates
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Why: Match onlyNonZero = TRUE in the original exploratory edge comparison.
#      Native IDs avoid ambiguity in node labels and undirected edge order.

ggm_edge_ids <- as.character(ggm_boot_nonparam$sampleTable$id[
ggm_boot_nonparam$sampleTable$type == "edge" &
ggm_boot_nonparam$sampleTable$value != 0])
ggm_edge_differences <- ggm_difference_tests(
ggm_boot_nonparam, "edge", ggm_edge_ids)
readr::write_csv(ggm_edge_differences, file.path(output_dir_14,
"13.Step14_Edge_Difference_Tests.csv"), na = "NA")
if (length(ggm_edge_ids) >= 2L) {
# 14.4.1 Match diagonal tiles to the network edge colors
# Why: Reuse the colorblind theme of the network in Section 13.10.
#      Only diagonal tiles show edge colors; other tiles retain test results.
ggm_edge_color_graph <- qgraph::qgraph(
ggm_boot_nonparam$sample$graph, theme = "colorblind", DoNotPlot = TRUE)
ggm_edge_color_from <- ggm_boot_nonparam$sample$labels[
ggm_edge_color_graph$Edgelist$from]
ggm_edge_color_to <- ggm_boot_nonparam$sample$labels[
ggm_edge_color_graph$Edgelist$to]
ggm_edge_diagonal_colors <- stats::setNames(
rep(ggm_edge_color_graph$graphAttributes$Edges$color, 2L),
c(paste(ggm_edge_color_from, ggm_edge_color_to, sep = "--"),
paste(ggm_edge_color_to, ggm_edge_color_from, sep = "--")))
ggm_edge_difference_plot <- ggm_difference_plot(ggm_edge_differences,
ggm_boot_nonparam$sampleTable[
ggm_boot_nonparam$sampleTable$type == "edge" &
ggm_boot_nonparam$sampleTable$id %in% ggm_edge_ids, ],
diagonal_colors = ggm_edge_diagonal_colors)
ggplot2::ggsave(file.path(output_dir_14,

"14.Step14_Edge_Differences.pdf"), plot = ggm_edge_difference_plot,
width = 10, height = 9, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")
  ggplot2::ggsave(file.path(output_dir_14,

"15.Step14_Edge_Differences.png"), plot = ggm_edge_difference_plot,
width = 10, height = 9, units = "in", dpi = 300, bg = "white")
} else {
  message("Fewer than two nonzero edges: no edge-pair difference figure.")
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 14.5 Export edge accuracy with native percentile bootstrap intervals
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

ggm_accuracy_plot <- plot(
ggm_boot_nonparam, statistics = "edge", plot = "area",
CIstyle = "quantiles", labels = TRUE, order = "sample", panels = FALSE)

ggplot2::ggsave(file.path(output_dir_14,
"16.Step14_Edge_Accuracy.pdf"), plot = ggm_accuracy_plot,
width = 10, height = 8, units = "in", device = grDevices::pdf,
useDingbats = FALSE, bg = "white")

ggplot2::ggsave(file.path(output_dir_14,
"17.Step14_Edge_Accuracy.png"), plot = ggm_accuracy_plot,
width = 10, height = 8, units = "in", dpi = 300, bg = "white")
