#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                  ┏┓┏┳┓┏┓┏┓  ┏┓                               #
#                                  ┗┓ ┃ ┣ ┃┃  ┣┫                               #
#                                  ┗┛ ┻ ┗┛┣┛  ┗┛                               #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#          Estimating models of Joint Job Crafting measure (JCQ+JCS)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.0 Prepare the common item set and figure dimensions
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 8.0.1 Select the exact 12 JCS and nine JCQ items used in the prior CFAs
jc_items <- c(sprintf("jcs_%02d", c(1:3, 6:8, 12:14, 17:19)),
              sprintf("jcq_%02d", c(2, 3, 5, 6, 9, 10, 11, 13, 15)))
jc_data <- as.data.frame(readRDS(file.path(output_dir_03, 
                         "2.Step03_Analysis_Dataset.rds"))
                         [, jc_items, drop = FALSE])

# 8.0.2 Archive the common input without changing the cleaned global dataset
saveRDS(jc_data, file.path(output_dir_08, "1.Step08_JC21_Input.rds"))


# 8.0.3 Define figure dimensions in millimeters and convert them to inches
jc_width_in_inches <- 180 / 25.4
jc_height_in_inches <- 220 / 25.4

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.1 MODEL 1: Seven Correlated Factors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Source: Makikangas and Schaufeli (2021), adapted to the short JCS e JCQ
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 8.1.1 Define the JCS + JCQ model
model1_jc <- "# JCS items
              ISJR =~ jcs_01 + jcs_02 + jcs_03
              DHJD =~ jcs_06 + jcs_07 + jcs_08
              ISOJR =~ jcs_12 + jcs_13 + jcs_14
              ICJD =~ jcs_17 + jcs_18 + jcs_19
              # JCQ items
              TC =~ jcq_02 + jcq_03 + jcq_05
              CC =~ jcq_06 + jcq_09 + jcq_10
              RC =~ jcq_11 + jcq_13 + jcq_15"

# 8.1.2 Fit the model using Robust Maximum Likelihood (MLR)
model1_jc.fit <- lavaan::sem(model = model1_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")
# Archive the fitted model
saveRDS(model1_jc.fit, file.path(output_dir_08, "2.Step08_JC21_M1_CFA.rds"))

# 8.1.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model1_jc.fit, 
                              fit.measures = TRUE,
                              standardized = TRUE, 
                              ci = TRUE,
                              rsquare  = TRUE),
                              file = file.path(output_dir_08, 
                              "3.Step08_JC21_M1_CFA_Report.txt"),
                              split = TRUE)

# 8.1.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model1_jc.fit, 
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                 file.path(output_dir_08, "4.Step08_JC21_M1_Parameters.csv"), 
                 na = "NA")

# 8.1.3.2 Export all native fit indices
model1_jc.fit_indices <- lavaan::fitMeasures(model1_jc.fit)
readr::write_csv(data.frame(index = names(model1_jc.fit_indices),
                            value = as.numeric(model1_jc.fit_indices)),
                 file.path(output_dir_08, "5.Step08_JC21_M1_Fit_Indices.csv"), 
                 na = "NA")

# 8.1.3.3 Report native sample size, convergence, and post-estimation checks
model1_jc.checks <- data.frame(n_analyzed = lavaan::lavInspect(model1_jc.fit, 
                                                               "nobs"),
                              converged = lavaan::lavInspect(model1_jc.fit, 
                                                            "converged"),
                              post_check = lavaan::lavInspect(model1_jc.fit, 
                                                              "post.check"))
# 8.1.3.4
print(model1_jc.checks, row.names = FALSE)
readr::write_csv(model1_jc.checks, file.path(output_dir_08,
"6.Step08_JC21_M1_Checks.csv"), na = "NA")

# 8.1.5 Prepare semPlot objects and visualize the estimated model
# 8.1.5.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst1_jc <- lavaan::lavInspect(model1_jc.fit, "est.std")$lambda
psiEst1_jc <- lavaan::lavInspect(model1_jc.fit, "est.std")$psi
thetaEst1_jc <- lavaan::lavInspect(model1_jc.fit, "est.std")$theta
factors1_jc <- lavaan::lavNames(model1_jc.fit, type = "lv")
n_factors1_jc <- length(factors1_jc)
# 8.1.5.2 Label items in their native matrix order, followed by latent factors
varnames_custom1_jc <- c(rownames(lambdaEst1_jc), factors1_jc)
# 8.1.5.3 Create a zero Beta matrix: no regressions among latent factors
betaEst1_jc <- matrix(0, nrow = n_factors1_jc,
                         ncol = n_factors1_jc,
                         dimnames = list(factors1_jc, factors1_jc))
# 8.1.5.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object1_jc <- semPlot::lisrelModel(LY = lambdaEst1_jc,
                                          BE = betaEst1_jc,
                                          PS = psiEst1_jc,
                                          TE = thetaEst1_jc)
# 8.1.5.5 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc1 <- matrix(c(1, 10.00, 1,  9.00, 1,  8.00, 1,  7.00, 1,  6.00,
1,  5.00, 1,  4.00, 1, 3.00, 1, 2.00, 1, 1.00,
1,  0.00, 1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00,
1, -5.00, 1, -6.00, 1, -7.00, 1, -8.00, 1, -9.00,
1, -10.00, 0, 9.00, 0,  6.00, 0, 3.00, 0, 0.00,
0, -3.00, 0, -6.00, 0, -9.00),
byrow = TRUE, ncol = 2)
# 8.1.5.6 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08, "7.Step08_JC21_M1_CFA_Plot.pdf"),
                     width = jc_width_in_inches, 
                     height = jc_height_in_inches, 
                     family = "sans")
# 8.1.5.7 Draw uniform blue paths and retain standardized signed labels
Plot_model1_jc <- semPlot::semPaths(object = lisrel_object1_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc1,
                                    nodeLabels = varnames_custom1_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors1_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors1_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    edge.label.bg = "white",
                                    label.cex = 0.8,
                                    curvePivot = 0.06,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE,
                                    layoutScale = c(0.7, 1),
                                    layoutOffset = c(0.3, 0))
# 8.1.5.8 Identify loading paths and unique factor correlations
jc_edges <- Plot_model1_jc$Edgelist
jc_loadings <- jc_edges$from > 21 & jc_edges$to <= 21
jc_correlations <- which(jc_edges$from > 21 & jc_edges$to > jc_edges$from)
jc_from <- jc_edges$from[jc_correlations] - 21L
jc_to <- jc_edges$to[jc_correlations] - 21L
jc_outer <- which(jc_to - jc_from > 1L)
jc_outer <- jc_outer[order(jc_to[jc_outer] - jc_from[jc_outer], -jc_to[jc_outer])]

# Preserve loading anchors, curve sizes, and label positions
jc_style <- Plot_model1_jc$graphAttributes$Edges
jc_style$edgeConnectPoints[jc_loadings, ] <-
  rep(c(0.5, 1.5) * pi, each = sum(jc_loadings))
jc_style$curve[jc_correlations] <- -0.22
jc_style$curve[jc_correlations[jc_outer]] <-
  -seq(0.6, 6.3, length.out = length(jc_outer))
jc_style$edge.label.position[jc_correlations] <-
  ifelse(jc_to - jc_from > 1L, 0.68, 0.5)
jc_style$edge.label.position[jc_correlations[
  factors1_jc[jc_from] == "TC" & factors1_jc[jc_to] == "RC"]] <- 0.60
jc_style$edgeConnectPoints[jc_correlations, ] <-
  rep(c(190, 350) * pi / 180, each = length(jc_correlations))

# Interpolate the same outer anchors for fully correlated factors
stopifnot(length(jc_correlations) == choose(n_factors1_jc, 2))
jc_style$edgeConnectPoints[jc_correlations[jc_outer], ] <- cbind(
  265 - 45 * (n_factors1_jc - jc_to[jc_outer]) /
    pmax(n_factors1_jc - jc_from[jc_outer] - 2, 1),
  320 - 45 * (jc_to[jc_outer] - jc_from[jc_outer] - 2) /
    pmax(jc_to[jc_outer] - 3, 1)) * pi / 180
Plot_model1_jc$graphAttributes$Edges <- jc_style
Plot_model1_jc$plotOptions$curveScale <- FALSE
plot(Plot_model1_jc)
# 8.1.4.14 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.2 MODEL 2: One General Factor
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 8.2.1 Define the model using the definitive database item names
model2_jc <- "GeneralJC =~ jcs_01 + jcs_02 + jcs_03 + jcs_06 + jcs_07 + jcs_08 +
                           jcs_12 + jcs_13 + jcs_14 + jcs_17 + jcs_18 + jcs_19 +
                           jcq_02 + jcq_03 + jcq_05 + jcq_06 + jcq_09 + jcq_10 +
                           jcq_11 + jcq_13 + jcq_15"

# 8.2.2 Fit the model using Robust Maximum Likelihood (MLR)
model2_jc.fit <- lavaan::sem(model = model2_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")
# 8.2.2.1 Archive the fitted model without refitting
saveRDS(model2_jc.fit, file.path(output_dir_08, "8.Step08_JC21_M2_CFA.rds"))

# 8.2.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model2_jc.fit, 
                              fit.measures = TRUE,
                              standardized = TRUE, 
                              ci = TRUE, 
                              rsquare = TRUE), 
                              file = file.path(output_dir_08, 
                              "9.Step08_JC21_M2_CFA_Report.txt"), 
                      split = TRUE)
# 8.2.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model2_jc.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_08, "10.Step08_JC21_M2_Parameters.csv"), na = "NA")
# 8.2.3.2 Export all native fit indices at full precision for Quarto
model2_jc.fit_indices <- lavaan::fitMeasures(model2_jc.fit)
readr::write_csv(data.frame(index = names(model2_jc.fit_indices),
                            value = as.numeric(model2_jc.fit_indices)),
file.path(output_dir_08, "11.Step08_JC21_M2_Fit_Indices.csv"), na = "NA")
# 8.2.3.3 Report native sample size, convergence, and post-estimation checks
model2_jc.checks <- data.frame(n_analyzed = lavaan::lavInspect(model2_jc.fit, 
                                                               "nobs"),
                               converged = lavaan::lavInspect(model2_jc.fit, 
                                                              "converged"),
                               post_check = lavaan::lavInspect(model2_jc.fit, 
                                                               "post.check"))
print(model2_jc.checks, row.names = FALSE)
readr::write_csv(model2_jc.checks, file.path(output_dir_08,
"12.Step08_JC21_M2_Checks.csv"), na = "NA")

# 8.2.4 Prepare semPlot objects and visualize the estimated model
# 8.2.4.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst2_jc <- lavaan::lavInspect(model2_jc.fit, "est.std")$lambda
psiEst2_jc <- lavaan::lavInspect(model2_jc.fit, "est.std")$psi
thetaEst2_jc <- lavaan::lavInspect(model2_jc.fit, "est.std")$theta
factors2_jc <- lavaan::lavNames(model2_jc.fit, type = "lv")
n_factors2_jc <- length(factors2_jc)
# 8.2.4.2 Label items in their native matrix order, followed by latent factors
varnames_custom2_jc <- c(rownames(lambdaEst2_jc), "General\nJC")
# 8.2.4.3 Create a zero Beta matrix: no regressions among latent factors
betaEst2_jc <- matrix(0, nrow = n_factors2_jc,
                         ncol = n_factors2_jc,
                         dimnames = list(factors2_jc, factors2_jc))
# 8.2.4.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object2_jc <- semPlot::lisrelModel(LY = lambdaEst2_jc,
                                          BE = betaEst2_jc,
                                          PS = psiEst2_jc,
                                          TE = thetaEst2_jc)
# 8.2.4.5 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc2 <- matrix(c(1, 10.00, 1,  9.00, 1,  8.00, 1,  7.00, 1, 6.00,
1, 5.00, 1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00, 1, -1.00, 1, -2.00, 1, 
-3.00, 1, -4.00, 1, -5.00, 1, -6.00, 1, -7.00, 1, -8.00, 1, -9.00, 1, -10.00, 
0, 0.00), byrow = TRUE, ncol = 2)
# 8.2.4.6 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08,
                                          "13.Step08_JC21_M2_CFA_Plot.pdf"),
                     width = jc_width_in_inches, 
                     height = jc_height_in_inches, 
                     family = "sans")
# 8.2.4.7 Draw uniform blue paths and retain standardized signed labels
Plot_model2_jc <- semPlot::semPaths(object = lisrel_object2_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc2,
                                    nodeLabels = varnames_custom2_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors2_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors2_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    label.cex = 0.8,
                                    curvePivot = TRUE,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE)
# 8.2.4.8 Identify estimated paths by their endpoints
edge_from2_jc <- Plot_model2_jc$Edgelist$from
edge_to2_jc <- Plot_model2_jc$Edgelist$to
loading_edges2_jc <- which(edge_from2_jc > 21 &
                           edge_to2_jc <= 21)
Plot_model2_jc$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_jc, 1] <- -0.5 * pi
Plot_model2_jc$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_jc, 2] <-  1.5 * pi
plot(Plot_model2_jc)
# 8.2.4.9 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.3 MODEL 3: Six Factors: ISOJR-RC Merged
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Source: Makikangas and Schaufeli (2021), adapted to the short forms.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 8.3.1 Define the model using the definitive database item names
model3_jc <- "ISJR =~ jcs_01 + jcs_02 + jcs_03
              DHJD =~ jcs_06 + jcs_07 + jcs_08
              ISOJR_RC =~ jcs_12 + jcs_13 + jcs_14 + jcq_11 + jcq_13 + jcq_15
              ICJD =~ jcs_17 + jcs_18 + jcs_19
              TC =~ jcq_02 + jcq_03 + jcq_05
              CC =~ jcq_06 + jcq_09 + jcq_10"

# 8.3.2 Fit the model using Robust Maximum Likelihood (MLR)
model3_jc.fit <- lavaan::sem(model = model3_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")
# 8.3.2.1 Archive the fitted model without refitting
saveRDS(model3_jc.fit, file.path(output_dir_08, "14.Step08_JC21_M3_CFA.rds"))

# 8.3.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model3_jc.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                     file = file.path(output_dir_08, 
                                      "15.Step08_JC21_M3_CFA_Report.txt"),
                     split = TRUE)
# 8.3.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model3_jc.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_08, "16.Step08_JC21_M3_Parameters.csv"), na = "NA")
# 8.3.3.2 Export all native fit indices at full precision for Quarto
model3_jc.fit_indices <- lavaan::fitMeasures(model3_jc.fit)
                         readr::write_csv(data.frame(index = 
                                                     names(model3_jc.fit_indices),
                                                     value = as.numeric(
                                                     model3_jc.fit_indices)),
                                          file.path(output_dir_08, 
                                          "17.Step08_JC21_M3_Fit_Indices.csv"), 
                                          na = "NA")
# 8.3.3.3 Report native sample size, convergence, and post-estimation checks
model3_jc.checks <- data.frame(
n_analyzed = lavaan::lavInspect(model3_jc.fit, "nobs"),
converged = lavaan::lavInspect(model3_jc.fit, "converged"),
post_check = lavaan::lavInspect(model3_jc.fit, "post.check"))
                                print(model3_jc.checks, row.names = FALSE)
readr::write_csv(model3_jc.checks, file.path(output_dir_08,
                 "18.Step08_JC21_M3_Checks.csv"), na = "NA")

# 8.3.4 Prepare semPlot objects and visualize the estimated model
# 8.3.4.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst3_jc <- lavaan::lavInspect(model3_jc.fit, "est.std")$lambda
psiEst3_jc <- lavaan::lavInspect(model3_jc.fit, "est.std")$psi
thetaEst3_jc <- lavaan::lavInspect(model3_jc.fit, "est.std")$theta
factors3_jc <- lavaan::lavNames(model3_jc.fit, type = "lv")
n_factors3_jc <- length(factors3_jc)
# 8.3.4.2 Label items in their native matrix order, followed by latent factors
varnames_custom3_jc <- c(rownames(lambdaEst3_jc),
                         "ISJR", "DHJD", "ISOJR\nRC", "ICJD", "TC", "CC")
# 8.3.4.3 Create a zero Beta matrix: no regressions among latent factors
betaEst3_jc <- matrix(0, nrow = n_factors3_jc,
                         ncol = n_factors3_jc,
                         dimnames = list(factors3_jc, factors3_jc))
# 8.3.4.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object3_jc <- semPlot::lisrelModel(LY = lambdaEst3_jc,
                                          BE = betaEst3_jc,
                                          PS = psiEst3_jc,
                                          TE = thetaEst3_jc)
# 8.3.4.6 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc3 <- matrix(c(1, 10.00, 1,  9.00, 1,  8.00, 1,  7.00, 1, 6.00,
1, 5.00, 1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00, 1, -1.00, 1, -2.00, 1, 
-3.00, 1, -4.00, 1, -5.00, 1, -6.00, 1, -7.00, 1, -8.00, 1, -9.00, 1, -10.00, 0, 
9.00, 0, 6.00, 0, 1.50, 0, -3.00, 0, -6.00, 0, -9.00), byrow = TRUE, ncol = 2)
# 8.3.4.7 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08,
                                          "19.Step08_JC21_M3_CFA_Plot.pdf"),
                     width = jc_width_in_inches, 
                     height = jc_height_in_inches, 
                     family = "sans")
# 8.3.4.8 Draw uniform blue paths and retain standardized signed labels
Plot_model3_jc <- semPlot::semPaths(object = lisrel_object3_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc3,
                                    nodeLabels = varnames_custom3_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors3_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors3_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    edge.label.bg = "white",
                                    label.cex = 0.8,
                                    curvePivot = 0.06,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE,
                                    layoutScale = c(0.7, 1),
                                    layoutOffset = c(0.3, 0))
# 8.3.4.9 Identify loading paths and unique factor correlations
jc_edges <- Plot_model3_jc$Edgelist
jc_loadings <- jc_edges$from > 21 & jc_edges$to <= 21
jc_correlations <- which(jc_edges$from > 21 & jc_edges$to > jc_edges$from)
jc_from <- jc_edges$from[jc_correlations] - 21L
jc_to <- jc_edges$to[jc_correlations] - 21L
jc_outer <- which(jc_to - jc_from > 1L)
jc_outer <- jc_outer[order(jc_to[jc_outer] - jc_from[jc_outer], -jc_to[jc_outer])]

# Preserve loading anchors, curve sizes, and label positions
jc_style <- Plot_model3_jc$graphAttributes$Edges
jc_style$edgeConnectPoints[jc_loadings, ] <-
  rep(c(0.5, 1.5) * pi, each = sum(jc_loadings))
jc_style$curve[jc_correlations] <- -0.22
jc_style$curve[jc_correlations[jc_outer]] <-
  -seq(0.6, 6.3, length.out = length(jc_outer))
jc_style$edge.label.position[jc_correlations] <-
  ifelse(jc_to - jc_from > 1L, 0.68, 0.5)
jc_style$edge.label.position[jc_correlations[
  factors3_jc[jc_from] == "ICJD" & factors3_jc[jc_to] == "CC"]] <- 0.60
jc_style$edgeConnectPoints[jc_correlations, ] <-
  rep(c(190, 350) * pi / 180, each = length(jc_correlations))

# Interpolate the same outer anchors for fully correlated factors
stopifnot(length(jc_correlations) == choose(n_factors3_jc, 2))
jc_style$edgeConnectPoints[jc_correlations[jc_outer], ] <- cbind(
  265 - 45 * (n_factors3_jc - jc_to[jc_outer]) /
    pmax(n_factors3_jc - jc_from[jc_outer] - 2, 1),
  320 - 45 * (jc_to[jc_outer] - jc_from[jc_outer] - 2) /
    pmax(jc_to[jc_outer] - 3, 1)) * pi / 180
Plot_model3_jc$graphAttributes$Edges <- jc_style
Plot_model3_jc$plotOptions$curveScale <- FALSE
plot(Plot_model3_jc)
# 8.3.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.4 MODEL 4: Six Factors: ICJD-TC Merged
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 8.4.1 Define the model using the definitive database item names
model4_jc <- "ISJR =~ jcs_01 + jcs_02 + jcs_03
              DHJD =~ jcs_06 + jcs_07 + jcs_08
              ISOJR =~ jcs_12 + jcs_13 + jcs_14
              ICJD_TC =~ jcs_17 + jcs_18 + jcs_19 + jcq_02 + jcq_03 + jcq_05
              CC =~ jcq_06 + jcq_09 + jcq_10
              RC =~ jcq_11 + jcq_13 + jcq_15"

# 8.4.2 Fit the model using Robust Maximum Likelihood (MLR)
model4_jc.fit <- lavaan::sem(model = model4_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")
# 8.4.2.1 Archive the fitted model without refitting
saveRDS(model4_jc.fit, file.path(output_dir_08, "20.Step08_JC21_M4_CFA.rds"))

# 8.4.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model4_jc.fit, 
                              fit.measures = TRUE, 
                              standardized = TRUE, 
                              ci = TRUE,
                              rsquare  = TRUE),
                              file = file.path(output_dir_08, 
                              "21.Step08_JC21_M4_CFA_Report.txt"),
                              split = TRUE)
# 8.4.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model4_jc.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_08, "22.Step08_JC21_M4_Parameters.csv"), na = "NA")
# 8.4.3.2 Export all native fit indices at full precision for Quarto
model4_jc.fit_indices <- lavaan::fitMeasures(model4_jc.fit)
readr::write_csv(data.frame(index = names(model4_jc.fit_indices),
                            value = as.numeric(model4_jc.fit_indices)),
                 file.path(output_dir_08, "23.Step08_JC21_M4_Fit_Indices.csv"), 
                 na = "NA")
# 8.4.3.3 Report native sample size, convergence, and post-estimation checks
model4_jc.checks <- data.frame(n_analyzed = lavaan::lavInspect(model4_jc.fit, 
                                                               "nobs"),
                               converged = lavaan::lavInspect(model4_jc.fit, 
                                                              "converged"),
                               post_check = lavaan::lavInspect(model4_jc.fit, 
                                                               "post.check"))
print(model4_jc.checks, row.names = FALSE)
readr::write_csv(model4_jc.checks, file.path(output_dir_08, "24.Step08_JC21_M4_Checks.csv"), 
                 na = "NA")
# 8.4.4 Prepare semPlot objects and visualize the estimated model
# 8.4.4.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst4_jc <- lavaan::lavInspect(model4_jc.fit, "est.std")$lambda
psiEst4_jc <- lavaan::lavInspect(model4_jc.fit, "est.std")$psi
thetaEst4_jc <- lavaan::lavInspect(model4_jc.fit, "est.std")$theta
factors4_jc <- lavaan::lavNames(model4_jc.fit, type = "lv")
n_factors4_jc <- length(factors4_jc)
# 8.4.4.2 Label items in their native matrix order, followed by latent factors
varnames_custom4_jc <- c(rownames(lambdaEst4_jc),
                         "ISJR", "DHJD", "ISOJR", "ICJD\nTC", "CC", "RC")
# 8.4.4.3 Create a zero Beta matrix: no regressions among latent factors
betaEst4_jc <- matrix(0, nrow = n_factors4_jc,
                         ncol = n_factors4_jc,
                         dimnames = list(factors4_jc, factors4_jc))
# 8.4.4.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object4_jc <- semPlot::lisrelModel(LY = lambdaEst4_jc,
                                          BE = betaEst4_jc,
                                          PS = psiEst4_jc,
                                          TE = thetaEst4_jc)
# 8.4.4.5 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc4 <- matrix(c(1, 10.00, 1,  9.00, 1,  8.00, 1,  7.00, 1,  6.00,
1,  5.00, 1,  4.00, 1,  3.00, 1,  2.00, 1,  1.00, 1,  0.00, 1, -1.00, 1, -2.00, 
1, -3.00, 1, -4.00, 1, -5.00, 1, -6.00, 1, -7.00, 1, -8.00, 1, -9.00, 1, -10.00, 
0,  9.00, 0,  6.00, 0,  3.00, 0, -1.50, 0, -6.00, 0, -9.00), byrow = TRUE, ncol = 2)
# 8.4.4.6 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08,
                                          "25.Step08_JC21_M4_CFA_Plot.pdf"),
                     width = jc_width_in_inches, 
                     height = jc_height_in_inches, 
                     family = "sans")
# 8.4.4.7 Draw uniform blue paths and retain standardized signed labels
Plot_model4_jc <- semPlot::semPaths(object = lisrel_object4_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc4,
                                    nodeLabels = varnames_custom4_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors4_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors4_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    edge.label.bg = "white",
                                    label.cex = 0.8,
                                    curvePivot = 0.06,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE,
                                    layoutScale = c(0.7, 1),
                                    layoutOffset = c(0.3, 0))
# 8.4.4.8 Identify loading paths and unique factor correlations
jc_edges <- Plot_model4_jc$Edgelist
jc_loadings <- jc_edges$from > 21 & jc_edges$to <= 21
jc_correlations <- which(jc_edges$from > 21 & jc_edges$to > jc_edges$from)
jc_from <- jc_edges$from[jc_correlations] - 21L
jc_to <- jc_edges$to[jc_correlations] - 21L
jc_outer <- which(jc_to - jc_from > 1L)
jc_outer <- jc_outer[order(jc_to[jc_outer] - jc_from[jc_outer], -jc_to[jc_outer])]

# Preserve loading anchors, curve sizes, and label positions
jc_style <- Plot_model4_jc$graphAttributes$Edges
jc_style$edgeConnectPoints[jc_loadings, ] <-
  rep(c(0.5, 1.5) * pi, each = sum(jc_loadings))
jc_style$curve[jc_correlations] <- -0.22
jc_style$curve[jc_correlations[jc_outer]] <-
  -seq(0.6, 6.3, length.out = length(jc_outer))
jc_style$edge.label.position[jc_correlations] <-
  ifelse(jc_to - jc_from > 1L, 0.68, 0.5)
jc_style$edge.label.position[jc_correlations[
  factors4_jc[jc_from] == "ICJD_TC" & factors4_jc[jc_to] == "RC"]] <- 0.60
jc_style$edgeConnectPoints[jc_correlations, ] <-
  rep(c(190, 350) * pi / 180, each = length(jc_correlations))

# Interpolate the same outer anchors for fully correlated factors
stopifnot(length(jc_correlations) == choose(n_factors4_jc, 2))
jc_style$edgeConnectPoints[jc_correlations[jc_outer], ] <- cbind(
  265 - 45 * (n_factors4_jc - jc_to[jc_outer]) /
    pmax(n_factors4_jc - jc_from[jc_outer] - 2, 1),
  320 - 45 * (jc_to[jc_outer] - jc_from[jc_outer] - 2) /
    pmax(jc_to[jc_outer] - 3, 1)) * pi / 180
Plot_model4_jc$graphAttributes$Edges <- jc_style
Plot_model4_jc$plotOptions$curveScale <- FALSE
plot(Plot_model4_jc)
# 8.4.4.13 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.5 MODEL 5: One Higher-Order Factor
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 8.5.1 Define the model using the definitive database item names
model5_jc <- "ISJR =~ jcs_01 + jcs_02 + jcs_03
              DHJD =~ jcs_06 + jcs_07 + jcs_08
              ISOJR =~ jcs_12 + jcs_13 + jcs_14
              ICJD =~ jcs_17 + jcs_18 + jcs_19
              TC =~ jcq_02 + jcq_03 + jcq_05
              CC =~ jcq_06 + jcq_09 + jcq_10
              RC =~ jcq_11 + jcq_13 + jcq_15
              GeneralJC =~ ISJR + DHJD + ISOJR + ICJD + TC + CC + RC"
# 8.5.2 Fit the model using Robust Maximum Likelihood (MLR)
model5_jc.fit <- lavaan::sem(model = model5_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")
# 8.5.2.1 Archive the fitted model without refitting
saveRDS(model5_jc.fit, file.path(output_dir_08, "26.Step08_JC21_M5_CFA.rds"))

# 8.5.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model5_jc.fit, 
                              fit.measures = TRUE,
                              standardized = TRUE, 
                              ci = TRUE,
                              rsquare = TRUE),
file = file.path(output_dir_08, "27.Step08_JC21_M5_CFA_Report.txt"),
split = TRUE)
# 8.5.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model5_jc.fit, 
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_08, "28.Step08_JC21_M5_Parameters.csv"), na = "NA")
# 8.5.3.2 Export all native fit indices at full precision for Quarto
model5_jc.fit_indices <- lavaan::fitMeasures(model5_jc.fit)

readr::write_csv(data.frame(index = names(model5_jc.fit_indices),
                            value = as.numeric(model5_jc.fit_indices)),
file.path(output_dir_08, "29.Step08_JC21_M5_Fit_Indices.csv"), na = "NA")
# 8.5.3.3 Report native sample size, convergence, and post-estimation checks
model5_jc.checks <- data.frame(n_analyzed = lavaan::lavInspect(model5_jc.fit, 
                                                               "nobs"),
                               converged = lavaan::lavInspect(model5_jc.fit, 
                                                              "converged"),
                               post_check = lavaan::lavInspect(model5_jc.fit, 
                                                               "post.check"))
print(model5_jc.checks, row.names = FALSE)
readr::write_csv(model5_jc.checks, file.path(output_dir_08,
"30.Step08_JC21_M5_Checks.csv"), na = "NA")

# 8.5.4 Prepare semPlot objects and visualize the estimated model
# 8.5.4.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst5_jc <- lavaan::lavInspect(model5_jc.fit, "est.std")$lambda
psiEst5_jc <- lavaan::lavInspect(model5_jc.fit, "est.std")$psi
thetaEst5_jc <- lavaan::lavInspect(model5_jc.fit, "est.std")$theta
factors5_jc <- lavaan::lavNames(model5_jc.fit, type = "lv")
n_factors5_jc <- length(factors5_jc)

# 8.5.4.2 Label items in their native matrix order, followed by latent factors
varnames_custom5_jc <- c(rownames(lambdaEst5_jc),
                         "ISJR", "DHJD", "ISOJR", "ICJD", "TC", "CC", "RC",
                         "General\nJC")

# 8.5.4.3 Retain the estimated second-order loadings in the Beta matrix
betaEst5_jc <- lavaan::lavInspect(model5_jc.fit, "est.std")$beta
# 8.5.4.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object5_jc <- semPlot::lisrelModel(LY = lambdaEst5_jc,
                                          BE = betaEst5_jc,
                                          PS = psiEst5_jc,
                                          TE = thetaEst5_jc)
# 8.5.4.6 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc5 <- matrix(c(1, 10.00, 1, 9.00, 1, 8.00, 1, 7.00, 1, 6.00,
1,  5.00, 1,  4.00, 1,  3.00, 1,  2.00, 1, 1.00, 1, 0.00, 1, -1.00, 1, -2.00, 
1, -3.00, 1, -4.00, 1, -5.00, 1, -6.00, 1, -7.00, 1, -8.00, 1, -9.00, 1, -10.00, 
0, 9.00, 0, 6.00, 0, 3.00, 0, 0.00, 0, -3.00, 0, -6.00, 0, -9.00, -1, 0.00),
byrow = TRUE, ncol = 2)
# 8.5.4.7 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08,
                                          "31.Step08_JC21_M5_CFA_Plot.pdf"), 
                     width = jc_width_in_inches, 
                     height = jc_height_in_inches, 
                     family = "sans")
# 8.5.4.8 Draw uniform blue paths and retain standardized signed labels
Plot_model5_jc <- semPlot::semPaths(object = lisrel_object5_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc5,
                                    nodeLabels = varnames_custom5_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors5_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors5_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    label.cex = 0.8,
                                    curvePivot = TRUE,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE)

# 8.5.4.9 Identify estimated paths by their endpoints
edge_from5_jc <- Plot_model5_jc$Edgelist$from
edge_to5_jc   <- Plot_model5_jc$Edgelist$to
loading_edges5_jc <- which(edge_from5_jc > 21 &
                           edge_to5_jc <= 21)
Plot_model5_jc$graphAttributes$Edges$edgeConnectPoints[
loading_edges5_jc, 1] <- -0.5 * pi
Plot_model5_jc$graphAttributes$Edges$edgeConnectPoints[
loading_edges5_jc, 2] <-  1.5 * pi
# Retain the seven directed GeneralJC-to-first-order paths as straight lines.
higher_edges5_jc <- which(edge_from5_jc > 21 &
                          edge_to5_jc > 21)
Plot_model5_jc$graphAttributes$Edges$edgeConnectPoints[
higher_edges5_jc, 1] <- -0.5 * pi
Plot_model5_jc$graphAttributes$Edges$edgeConnectPoints[
higher_edges5_jc, 2] <-  1.5 * pi

plot(Plot_model5_jc)
# 8.5.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.6 MODEL 6: General Plus Seven Orthogonal Specifics
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 8.6.1 Define the model using the definitive database item names
model6_jc <- "GeneralJC =~ jcs_01 + jcs_02 + jcs_03 + jcs_06 + jcs_07 + jcs_08 +
                           jcs_12 + jcs_13 + jcs_14 + jcs_17 + jcs_18 + jcs_19 +
                           jcq_02 + jcq_03 + jcq_05 + jcq_06 + jcq_09 + jcq_10 +
                           jcq_11 + jcq_13 + jcq_15
              ISJR =~ jcs_01 + jcs_02 + jcs_03
              DHJD =~ jcs_06 + jcs_07 + jcs_08
              ISOJR =~ jcs_12 + jcs_13 + jcs_14
              ICJD =~ jcs_17 + jcs_18 + jcs_19
              TC =~ jcq_02 + jcq_03 + jcq_05
              CC =~ jcq_06 + jcq_09 + jcq_10
              RC =~ jcq_11 + jcq_13 + jcq_15
              GeneralJC ~~ 0*ISJR
              GeneralJC ~~ 0*DHJD
              GeneralJC ~~ 0*ISOJR
              GeneralJC ~~ 0*ICJD
              GeneralJC ~~ 0*TC
              GeneralJC ~~ 0*CC
              GeneralJC ~~ 0*RC
              ISJR ~~ 0*DHJD
              ISJR ~~ 0*ISOJR
              ISJR ~~ 0*ICJD
              ISJR ~~ 0*TC
              ISJR ~~ 0*CC
              ISJR ~~ 0*RC
              DHJD ~~ 0*ISOJR
              DHJD ~~ 0*ICJD
              DHJD ~~ 0*TC
              DHJD ~~ 0*CC
              DHJD ~~ 0*RC
              ISOJR ~~ 0*ICJD
              ISOJR ~~ 0*TC
              ISOJR ~~ 0*CC
              ISOJR ~~ 0*RC
              ICJD ~~ 0*TC
              ICJD ~~ 0*CC
              ICJD ~~ 0*RC
              TC ~~ 0*CC
              TC ~~ 0*RC
              CC ~~ 0*RC"

# 8.6.2 Fit the model using Robust Maximum Likelihood (MLR)
model6_jc.fit <- lavaan::sem(model = model6_jc,
                             data = jc_data,
                             estimator = "MLR",
                             std.lv = FALSE,
                             missing = "ML")

# 8.6.2.1 Archive the fitted model without refitting
saveRDS(model6_jc.fit, file.path(output_dir_08, "32.Step08_JC21_M6_CFA.rds"))

# 8.6.3 Summarize model fit, standardized estimates, CIs, and R-squared
utils::capture.output(summary(model6_jc.fit, 
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_08, 
                                               "33.Step08_JC21_M6_CFA_Report.txt"),
                              split = TRUE)
# 8.6.3.1 Export native parameter estimates, including fixed paths and R-squared
readr::write_csv(lavaan::parameterEstimates(model6_jc.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_08, "34.Step08_JC21_M6_Parameters.csv"), na = "NA")
# 8.6.3.2 Export all native fit indices at full precision for Quarto
model6_jc.fit_indices <- lavaan::fitMeasures(model6_jc.fit)
readr::write_csv(data.frame(index = names(model6_jc.fit_indices),
                            value = as.numeric(model6_jc.fit_indices)),
                 file.path(output_dir_08, "35.Step08_JC21_M6_Fit_Indices.csv"), 
                 na = "NA")
# 8.6.3.3 Report native sample size, convergence, and post-estimation checks
model6_jc.checks <- data.frame(
n_analyzed = lavaan::lavInspect(model6_jc.fit, "nobs"),
converged = lavaan::lavInspect(model6_jc.fit, "converged"),
post_check = lavaan::lavInspect(model6_jc.fit, "post.check"))
print(model6_jc.checks, row.names = FALSE)
readr::write_csv(model6_jc.checks, file.path(output_dir_08,
                                             "36.Step08_JC21_M6_Checks.csv"), 
                                             na = "NA")

# 8.6.4 Prepare semPlot objects and visualize the estimated model
# 8.6.4.1 Extract standardized loadings, latent covariances, and residuals
lambdaEst6_jc <- lavaan::lavInspect(model6_jc.fit, "est.std")$lambda
psiEst6_jc <- lavaan::lavInspect(model6_jc.fit, "est.std")$psi
thetaEst6_jc <- lavaan::lavInspect(model6_jc.fit, "est.std")$theta
factors6_jc <- lavaan::lavNames(model6_jc.fit, type = "lv")
n_factors6_jc <- length(factors6_jc)
# 8.6.4.2 Label items in their native matrix order, followed by latent factors
varnames_custom6_jc <- c(rownames(lambdaEst6_jc), "General\nJC",
                         "ISJR", "DHJD", "ISOJR", "ICJD", "TC", "CC", "RC")
# 8.6.4.3 Create a zero Beta matrix: no regressions among latent factors
betaEst6_jc <- matrix(0, nrow = n_factors6_jc,
                         ncol = n_factors6_jc,
                         dimnames = list(factors6_jc, factors6_jc))
# 8.6.4.4 Construct the LISREL object from the fitted standardized matrices
lisrel_object6_jc <- semPlot::lisrelModel(LY = lambdaEst6_jc,
                                          BE = betaEst6_jc,
                                          PS = psiEst6_jc,
                                          TE = thetaEst6_jc)
# 8.6.4.5 Define explicit coordinates for the 21 items and latent factors
layout_matrix_jc6 <- matrix(c(0, 10.00, 0, 9.00, 0, 8.00, 0, 7.00, 0, 
6.00, 0, 5.00, 0, 4.00, 0, 3.00, 0, 2.00, 0, 1.00, 0, 0.00, 0, -1.00, 
0, -2.00, 0, -3.00, 0, -4.00, 0, -5.00, 0, -6.00, 0, -7.00, 0, -8.00, 
0, -9.00, 0, -10.00, -1, 0.00, 1, 9.00, 1, 6.00, 1, 3.00, 1, 0.00, 1, 
-3.00, 1, -6.00, 1, -9.00), byrow = TRUE, ncol = 2)

# 8.6.4.6 Open the PDF device with the common joint-model dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_08,
                                          "37.Step08_JC21_M6_CFA_Plot.pdf"),
                     width = jc_width_in_inches, height = jc_height_in_inches, 
                     family = "sans")

# 8.6.4.7 Draw uniform blue paths and retain standardized signed labels
Plot_model6_jc <- semPlot::semPaths(object = lisrel_object6_jc,
                                    what = "path",
                                    whatLabels = "std",
                                    layout = layout_matrix_jc6,
                                    nodeLabels = varnames_custom6_jc,
                                    style = "lisrel",
                                    shapeMan = "rectangle",
                                    shapeLat = "circle",
                                    node.width = c(rep(8.0, 21),
                                                   rep(5.0, n_factors6_jc)),
                                    node.height = c(rep(2.2, 21),
                                                    rep(5.0, n_factors6_jc)),
                                    theme = "Borkulo",
                                    color = "white",
                                    cardinal = FALSE,
                                    borders = TRUE,
                                    border.color = "darkblue",
                                    residuals = FALSE,
                                    covAtResiduals = FALSE,
                                    intercepts = FALSE,
                                    sizeMan = 1.0,
                                    sizeLat = 1.0,
                                    edge.width = 1,
                                    weighted = FALSE,
                                    asize = 1.5,
                                    esize = 1,
                                    edge.label.cex = 0.65,
                                    label.cex = 0.8,
                                    curvePivot = TRUE,
                                    reorder = FALSE,
                                    normalize = FALSE,
                                    edge.color = "darkblue",
                                    fade = FALSE,
                                    DoNotPlot = TRUE)
# 8.6.4.8 Identify estimated paths by their endpoints
edge_from6_jc <- Plot_model6_jc$Edgelist$from
edge_to6_jc <- Plot_model6_jc$Edgelist$to
# GeneralJC is latent node 22; nodes 23-29 are the seven specific factors.
general_edges6_jc <- which(edge_from6_jc == 22 &
                           edge_to6_jc <= 21)
specific_edges6_jc <- which(edge_from6_jc > 22 &
                            edge_to6_jc <= 21)
Plot_model6_jc$graphAttributes$Edges$edgeConnectPoints[
general_edges6_jc, 1] <- -0.5 * pi
Plot_model6_jc$graphAttributes$Edges$edgeConnectPoints[
general_edges6_jc, 2] <-  1.5 * pi
Plot_model6_jc$graphAttributes$Edges$edgeConnectPoints[
specific_edges6_jc, 1] <- 0.5 * pi
Plot_model6_jc$graphAttributes$Edges$edgeConnectPoints[
specific_edges6_jc, 2] <- 0.5 * pi
plot(Plot_model6_jc)
# 8.6.4.9 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8.7 Compare the six prespecified models using native robust fit indices
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 8.7.1 Select native indices without rounding their numerical values
jc_fit_names <- c("npar", "ntotal", "chisq.scaled", "df.scaled",
                  "pvalue.scaled", "cfi.robust", "tli.robust", "rmsea.robust",
                  "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr", 
                  "aic", "bic")

# 8.7.2 Assemble one comparison row per fitted model for Quarto and APA tables
jc_fit_comparison <- data.frame(Model = c("M1: Seven correlated factors",
                                          "M2: One general factor",
                                          "M3: Six factors, ISOJR-RC merged",
                                          "M4: Six factors, ICJD-TC merged",
                                          "M5: One higher-order factor",
                                          "M6: General plus seven orthogonal specifics"),
rbind(model1_jc.fit_indices[jc_fit_names],
      model2_jc.fit_indices[jc_fit_names],
      model3_jc.fit_indices[jc_fit_names],
      model4_jc.fit_indices[jc_fit_names],
      model5_jc.fit_indices[jc_fit_names],
      model6_jc.fit_indices[jc_fit_names]),
      row.names = NULL, check.names = FALSE)

# 8.7.3 Retain the native convergence and post-estimation results in the table
jc_fit_comparison$Converged <- c(model1_jc.checks$converged,
                                 model2_jc.checks$converged,
                                 model3_jc.checks$converged,
                                 model4_jc.checks$converged,
                                 model5_jc.checks$converged,
                                 model6_jc.checks$converged)

# 8.7.4 Record post-estimation checks for each model
jc_fit_comparison$Post_Check <- c(model1_jc.checks$post_check,
                                  model2_jc.checks$post_check,
                                  model3_jc.checks$post_check,
                                  model4_jc.checks$post_check,
                                  model5_jc.checks$post_check,
                                  model6_jc.checks$post_check)

# 8.7.5 Export the full-precision comparison and preserve a readable text copy
readr::write_csv(jc_fit_comparison, file.path(output_dir_08,
                 "38.Step08_JC21_Model_Comparison.csv"), na = "NA")
utils::capture.output(print(jc_fit_comparison, row.names = FALSE),
file = file.path(output_dir_08, "39.Step08_JC21_Model_Comparison.txt"),
                 split = TRUE)

# 8.7.6 Report the location of the saved joint-model results
message("JCS-12 and JCQ-9 joint CFA results saved in: ",
normalizePath(output_dir_08, winslash = "/"))
