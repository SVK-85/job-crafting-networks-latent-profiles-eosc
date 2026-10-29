#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏┓                                   #
#                              ┗┓ ┃ ┣ ┃┃  ┣┓                                   #
#                              ┗┛ ┻ ┗┛┣┛  ┗┛                                   #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#               Confirmatory Factor Analysis: JCS-21 and JCS-12                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 6.1 JCS-21 MODEL 2: four correlated factors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Source: Bakker et al. (2018), modified four-factor model (M6).
# Freely estimate the published residual pairs 6-7, 8-9, and 15-16.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 6.1.1 Define the four-factor correlated model using database item names
model2_jcs <- "jcs_structural =~ jcs_01 + jcs_02 + jcs_03 + jcs_04 + jcs_05
               jcs_hindering =~ jcs_06 + jcs_07 + jcs_08 + jcs_09 +
                                jcs_10 + jcs_11
               jcs_social =~ jcs_12 + jcs_13 + jcs_14 + jcs_15 + jcs_16
               jcs_challenging =~ jcs_17 + jcs_18 + jcs_19 + jcs_20 + jcs_21
               jcs_06 ~~ jcs_07
               jcs_08 ~~ jcs_09
               jcs_15 ~~ jcs_16"

# 6.1.2 Fit the model using Robust Maximum Likelihood (MLR)
model2_jcs.fit <- lavaan::sem(model = model2_jcs,
                              data = data,
                              estimator = "MLR",
                              std.lv = FALSE,
                              missing = "ML")

# 6.1.3 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model2_jcs.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_06, "1.Step06_JCS_21_Parameters.csv"), na = "NA")

# 6.1.3.1 Export native fit indices at full precision for Quarto
model2_jcs.fit_indices <- lavaan::fitMeasures(model2_jcs.fit)
# 6.1.3.1.1 Export the fit indices as a CSV table
readr::write_csv(data.frame(index = names(model2_jcs.fit_indices),
                            value = as.numeric(model2_jcs.fit_indices)),
                 file.path(output_dir_06, "2.Step06_JCS_21_Fit_Indices.csv"), 
                 na = "NA")

# 6.1.3.2 Archive the fitted model without refitting
saveRDS(model2_jcs.fit, file.path(output_dir_06, "3.Step06_JCS_21_CFA.rds"))

# 6.1.3.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model2_jcs.fit,
                             fit.measures = TRUE,
                             standardized = TRUE,
                             ci = TRUE,
                             rsquare = TRUE),
                             file = file.path(output_dir_06, 
                                              "4.Step06_JCS_21_CFA_Report.txt"),
                             split = TRUE)

# 6.1.4 Prepare semPlot objects and visualize the four-factor solution
# 6.1.4.1 Use database item names and short labels for the latent factors
varnames_custom2_jcs <- c(sprintf("jcs_%02d", 1:21),
                          "ISJR", "DHJD", "ISOJR", "ICJD")

# 6.1.4.2 Extract standardized loadings, factor covariances, and residuals
lambdaEst2_jcs <- lavaan::lavInspect(model2_jcs.fit, "est.std")$lambda
psiEst2_jcs    <- lavaan::lavInspect(model2_jcs.fit, "est.std")$psi
thetaEst2_jcs  <- lavaan::lavInspect(model2_jcs.fit, "est.std")$theta

# 6.1.4.3 Create a zero Beta matrix: no regressions among latent factors
factors2_jcs   <- lavaan::lavNames(model2_jcs.fit, type = "lv")
n_factors2_jcs <- length(factors2_jcs)
betaEst2_jcs <- matrix(0, nrow = n_factors2_jcs,
                          ncol = n_factors2_jcs,
                          dimnames = list(factors2_jcs, factors2_jcs))

# 6.1.4.4 Construct the LISREL object using the fitted model matrices
lisrel_object2_jcs <- semPlot::lisrelModel(LY = lambdaEst2_jcs,
                                           BE = betaEst2_jcs,
                                           PS = psiEst2_jcs,
                                           TE = thetaEst2_jcs)

# 6.1.4.5 Use white nodes for the JCS items and factors
jcs_color_vector2_jcs <- rep("white", length(varnames_custom2_jcs))
names(jcs_color_vector2_jcs) <- varnames_custom2_jcs

# 6.1.4.6 Define explicit coordinates: items at x = 1, factors at x = 0.5
layout_matrix_jcs2_jcs <- matrix(c(1,  5.00, 1,  4.57, 1,  4.14, 1,  3.71, 1,  
3.28, 1, 2.85, 1, 2.42, 1, 1.99, 1, 1.56, 1, 1.13, 1, 0.70, 1, 0.27, 1, -0.16, 
1, -0.59, 1, -1.02, 1, -1.45, 1, -1.88, 1, -2.31, 1, -2.74, 1, -3.17, 1, -3.60,
0.5, 4.14, 0.5, 1.78, 0.5, -0.59, 0.5, -2.74), byrow = TRUE, ncol = 2)

# 6.1.4.7 Define figure dimensions (mm to inches) and open the PDF device
width_in_inches  <- 79.953 / 25.4
height_in_inches <- 79.953 / 25.4

grDevices::cairo_pdf(filename = file.path(output_dir_06, 
                                          "5.Step06_JCS_21_CFA_Plot.pdf"),
                    width = width_in_inches,
                    height = height_in_inches,
                    family = "sans", 
                    onefile = TRUE)

# 6.1.4.8 Generate the semPaths diagram with explicit graphical parameters
Plot_model2_jcs <- semPlot::semPaths(object = lisrel_object2_jcs,
                                     what = "path",
                                     whatLabels = "std",
                                     layout = layout_matrix_jcs2_jcs,
                                     nodeLabels = varnames_custom2_jcs,
                                     style = "lisrel",
                                     theme = "Borkulo",
                                     shapeMan = "rectangle",
                                     shapeLat = "circle",
                                     node.width = 4.0,
                                     node.height = 0.7,
                                     color = jcs_color_vector2_jcs,
                                     cardinal = FALSE,
                                     borders = TRUE,
                                     border.color = "darkblue",
                                     residuals = FALSE,
                                     sizeMan = 1.0,
                                     sizeLat = 1.0,
                                     edge.width = 1,
                                     asize = 1.5,
                                     esize = 1,
                                     weighted = FALSE,
                                     edge.label.cex = 0.35,
                                     label.cex = 0.7,
                                     curvePivot = TRUE,
                                     reorder = FALSE,
                                     normalize = FALSE,
                                     edge.color = "darkblue",
                                     fade = FALSE,
                                     DoNotPlot = TRUE,
                                     layoutScale = c(0.9, 1),
                                     layoutOffset = c(0.25, 0))

# 6.1.4.9 Identify paths by their endpoints and retain the supplied curvatures
edge_from2_jcs <- Plot_model2_jcs$Edgelist$from
edge_to2_jcs <- Plot_model2_jcs$Edgelist$to
loading_edges2_jcs <- which(edge_from2_jcs > 21 & edge_to2_jcs <= 21)
latent_edges2_jcs <- which(edge_from2_jcs > 21 &
                           edge_to2_jcs > edge_from2_jcs)
Plot_model2_jcs$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_jcs, 1] <- -0.5 * pi
Plot_model2_jcs$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_jcs, 2] <-  1.5 * pi
Plot_model2_jcs$graphAttributes$Edges$curve[
latent_edges2_jcs[c(1, 4, 6)]] <- -1
Plot_model2_jcs$graphAttributes$Edges$curve[
latent_edges2_jcs[c(2, 5)]] <- -1.7
Plot_model2_jcs$graphAttributes$Edges$curve[
latent_edges2_jcs[3]] <- -2

# Show the three specified residual covariances to the right of the items.
residual_edges2_jcs <- which(edge_from2_jcs <= 21 &
                             edge_to2_jcs <= 21 &
                             edge_from2_jcs < edge_to2_jcs)
Plot_model2_jcs$graphAttributes$Edges$edgeConnectPoints[
residual_edges2_jcs, 1] <- 0.5 * pi
Plot_model2_jcs$graphAttributes$Edges$edgeConnectPoints[
residual_edges2_jcs, 2] <- 0.5 * pi
Plot_model2_jcs$graphAttributes$Edges$curve[residual_edges2_jcs] <- 4

# 6.1.4.10 Render the final diagram
plot(Plot_model2_jcs, GLratio = 2)

# 6.1.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 6.2 JCS-12 MODEL 2A: four correlated factors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Source: Sora et al. (2018), Table 2; original JCS numbering retained.
# Test the published item composition using the present database.
# Residual covariances remain fixed at zero in this specification.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 6.2.1 Define the four-factor correlated model using database item names
model2a_jcs <- "ISJR =~ jcs_01 + jcs_02 + jcs_03
                DHJD =~ jcs_06 + jcs_07 + jcs_08
                ISOJR =~ jcs_12 + jcs_13 + jcs_14
                ICJD =~ jcs_17 + jcs_18 + jcs_19"

# 6.2.2 Fit the model using Robust Maximum Likelihood (MLR)
model2a_jcs.fit <- lavaan::sem(model = model2a_jcs,
                               data = data,
                               estimator = "MLR",
                               std.lv = FALSE,
                               missing = "ML")

# 6.2.3 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model2a_jcs.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                 file.path(output_dir_06, "6.Step06_JCS_12_Parameters.csv"), 
                           na = "NA")

# 6.2.3.1 Export native fit indices at full precision for Quarto
model2a_jcs.fit_indices <- lavaan::fitMeasures(model2a_jcs.fit)

readr::write_csv(data.frame(index = names(model2a_jcs.fit_indices),
                            value = as.numeric(model2a_jcs.fit_indices)),
                file.path(output_dir_06, "7.Step06_JCS_12_Fit_Indices.csv"), 
                na = "NA")

# 6.2.3.2 Archive the fitted model without refitting
saveRDS(model2a_jcs.fit, file.path(output_dir_06, "8.Step06_JCS_12_CFA.rds"))

# 6.2.3.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model2a_jcs.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_06, 
                                               "9.Step06_JCS_12_CFA_Report.txt"),
                              split = TRUE)

# 6.2.4 Prepare semPlot objects and visualize the four-factor solution
# 6.2.4.1 Use database item names and short labels for the latent factors
varnames_custom2a_jcs <- c(sprintf("jcs_%02d", c(1:3, 6:8, 12:14, 17:19)),
                           "ISJR", "DHJD", "ISOJR", "ICJD")

# 6.2.4.2 Extract standardized loadings, factor covariances, and residuals
lambdaEst2a_jcs <- lavaan::lavInspect(model2a_jcs.fit, "est.std")$lambda
psiEst2a_jcs <- lavaan::lavInspect(model2a_jcs.fit, "est.std")$psi
thetaEst2a_jcs <- lavaan::lavInspect(model2a_jcs.fit, "est.std")$theta

# 6.2.4.3 Create a zero Beta matrix: no regressions among latent factors
factors2a_jcs <- lavaan::lavNames(model2a_jcs.fit, type = "lv")
n_factors2a_jcs <- length(factors2a_jcs)
betaEst2a_jcs <- matrix(0, nrow = n_factors2a_jcs,
                           ncol = n_factors2a_jcs,
                           dimnames = list(factors2a_jcs, factors2a_jcs))

# 6.2.4.4 Construct the LISREL object using the fitted model matrices
lisrel_object2a_jcs <- semPlot::lisrelModel(LY = lambdaEst2a_jcs,
                                            BE = betaEst2a_jcs,
                                            PS = psiEst2a_jcs,
                                            TE = thetaEst2a_jcs)

# 6.2.4.5 Use white nodes for the JCS items and factors
jcs_color_vector2a_jcs <- rep("white", length(varnames_custom2a_jcs))
names(jcs_color_vector2a_jcs) <- varnames_custom2a_jcs

# 6.2.4.6 Define explicit coordinates: items at x = 1, factors at x = 0.5
layout_matrix_jcs2a_jcs <- matrix(c(1,  5.00, 1,  4.09, 1,  3.18,1,  2.27, 1,  1.36, 
1,  0.45, 1, -0.45, 1, -1.36, 1, -2.27,1, -3.18, 1, -4.09, 1, -5.00, 0.5, 4.09, 0.5, 
1.36, 0.5, -1.36, 0.5, -4.09), byrow = TRUE, ncol = 2)

# 6.2.4.7 Reuse the figure dimensions and open the PDF device
grDevices::cairo_pdf(filename = file.path(output_dir_06, "10.Step06_JCS_12_CFA_Plot.pdf"),
                     width = width_in_inches,
                     height = height_in_inches,
                     family = "sans", 
                     onefile = TRUE)

# 6.2.4.8 Generate the semPaths diagram with explicit graphical parameters
Plot_model2a_jcs <- semPlot::semPaths(object = lisrel_object2a_jcs,
                                      what = "path",
                                      whatLabels = "std",
                                      layout = layout_matrix_jcs2a_jcs,
                                      nodeLabels = varnames_custom2a_jcs,
                                      style = "lisrel",
                                      theme = "Borkulo",
                                      shapeMan = "rectangle",
                                      shapeLat = "circle",
                                      node.width = 4.0,
                                      node.height = 0.7,
                                      color = jcs_color_vector2a_jcs,
                                      cardinal = FALSE,
                                      borders = TRUE,
                                      border.color = "darkblue",
                                      residuals = FALSE,
                                      sizeMan = 1.0,
                                      sizeLat = 1.0,
                                      edge.width = 1,
                                      asize = 1.5,
                                      esize = 1,
                                      weighted = FALSE,
                                      edge.label.cex = 0.35,
                                      label.cex = 0.7,
                                      curvePivot = TRUE,
                                      reorder = FALSE,
                                      normalize = FALSE,
                                      edge.color = "darkblue",
                                      fade = FALSE,
                                      DoNotPlot = TRUE,
                                      layoutScale = c(0.9, 1),
                                      layoutOffset = c(0.25, 0))

# 6.2.4.9 Identify paths by their endpoints and retain the supplied curvatures
edge_from2a_jcs <- Plot_model2a_jcs$Edgelist$from
edge_to2a_jcs   <- Plot_model2a_jcs$Edgelist$to
loading_edges2a_jcs <- which(edge_from2a_jcs > 12 & edge_to2a_jcs <= 12)
latent_edges2a_jcs <- which(edge_from2a_jcs > 12 &
                            edge_to2a_jcs > edge_from2a_jcs)
Plot_model2a_jcs$graphAttributes$Edges$edgeConnectPoints[
loading_edges2a_jcs, 1] <- -0.5 * pi
Plot_model2a_jcs$graphAttributes$Edges$edgeConnectPoints[
loading_edges2a_jcs, 2] <-  1.5 * pi
Plot_model2a_jcs$graphAttributes$Edges$curve[
latent_edges2a_jcs[c(1, 4, 6)]] <- -1
Plot_model2a_jcs$graphAttributes$Edges$curve[
latent_edges2a_jcs[c(2, 5)]] <- -1.7
Plot_model2a_jcs$graphAttributes$Edges$curve[
latent_edges2a_jcs[3]] <- -2

# 6.2.4.10 Render the final diagram and add its title
plot(Plot_model2a_jcs, GLratio = 2)

# 6.2.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 6.3 Report the saved CFA outputs
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
message("JCS-21 and JCS-12 CFA results saved in: ",
normalizePath(output_dir_06, winslash = "/"))
