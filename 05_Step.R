#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏━                                   #
#                              ┗┓ ┃ ┣ ┃┃  ┗┓                                   #
#                              ┗┛ ┻ ┗┛┣┛  ┗┛                                   #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Confirmatory Factor Analysis: JCQ-15 and JCQ-9  
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 5.1 JCQ-15 MODEL 1 (Three-factor Correlated Model- 15 item)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 5.1.1 Define CFA model: three correlated latent factors
model1 <- "TaskCrafting =~ jcq_01 + jcq_02 + jcq_03 + jcq_04 + jcq_05
           CognitiveCrafting  =~ jcq_06 + jcq_07 + jcq_08 + jcq_09 + jcq_10
           RelationalCrafting =~ jcq_11 + jcq_12 + jcq_13 + jcq_14 + jcq_15"

# 5.1.2 Fit the model using Robust Maximum Likelihood (MLR)
model1.fit <- lavaan::sem(model = model1,
                          data = data,
                          estimator = "MLR",
                          std.lv = FALSE,
                          missing = "ML")

# 5.1.3 Export the native parameter estimates and R-squared
readr::write_csv(lavaan::parameterEstimates(model1.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                 file.path(output_dir_05, 
                           "1.Step05_JCQ_15_Parameters.csv"), 
                           na = "NA")

# 5.1.3.1 Export native fit indices at full precision for Quarto
model1.fit_indices <- lavaan::fitMeasures(model1.fit)

readr::write_csv(data.frame(index = names(model1.fit_indices),
                            value = as.numeric(model1.fit_indices)),
                 file.path(output_dir_05, "2.Step05_JCQ_15_Fit_Indices.csv"), 
                           na = "NA")

# 5.1.3.2 Archive the fitted model without refitting
saveRDS(model1.fit, file.path(output_dir_05, "3.Step05_JCQ_15_CFA.rds"))

# 5.1.3.3 Summarize model fit (fit indices, loadings, confidence intervals, R²)
utils::capture.output(summary(model1.fit, fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_05, 
                                               "4.Step05_JCQ_15_CFA_Report.txt"),
                              split = TRUE)

# 5.1.4 Prepare semPlot objects and visualize the three-factor solution
# 5.1.4.1 Custom labels for the 15 observed variables + 3 latent factors
varnames_custom <- c(paste0("TC ", 1:5),
                     paste0("CC ", 1:5),
                     paste0("RC ", 1:5),
                     "TC", "CC", "RC")

# 5.1.4.1.1 Use white nodes for the JCQ items and factors
jcq_factor_colors <- c(TC = "white", CC = "white", RC = "white")

# 5.1.4.2 Extract standardized loadings, covariances, and error variances
lambdaEst <- lavaan::lavInspect(model1.fit, "est.std")$lambda
psiEst <- lavaan::lavInspect(model1.fit, "est.std")$psi
thetaEst <- lavaan::lavInspect(model1.fit, "est.std")$theta

# 5.1.4.3 Create a Beta matrix as zero
factors <- lavaan::lavNames(model1.fit, type = "lv")
n_factors <- length(factors)
betaEst <- matrix(0, nrow = n_factors,
                     ncol = n_factors,
                     dimnames = list(factors, factors))

# 5.1.4.4 Construct a lisrelModel object for semPlot
lisrel_object <- semPlot::lisrelModel(LY = lambdaEst,
                                      BE = betaEst,
                                      PS = psiEst,
                                      TE = thetaEst)


# 5.1.4.5 Define the item and factor coordinates
layout_matrix_jcq <- matrix(c(1,  4.00, 1, 3.43, 1, 2.86, 1, 2.29, 1, 1.71, 1, 
1.14, 1, 0.57, 1, 0.00, 1, -0.57, 1, -1.14, 1, -1.71,  1, -2.29, 1, -2.86, 
1, -3.43, 1, -4.00, 0, 2.86, 0, 0.00, 0, -2.86), byrow = TRUE, ncol = 2)

# 5.1.4.6 Define the figure dimensions (mm to inches) and open the PDF device
width_in_inches <- 79.953 / 25.4
height_in_inches <- 79.953 / 25.4
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
grDevices::cairo_pdf(filename = file.path(output_dir_05, 
                                          "5.Step05_JCQ_15_CFA_Plot.pdf"),
                    width = width_in_inches, 
                    height = height_in_inches,
                    family = "sans", 
                    onefile = TRUE)

# 5.1.4.7 Generate the semPaths diagram for the three-factor model
Plot_model1 <- semPlot::semPaths(object = lisrel_object,
                                 what = "path",
                                 whatLabels = "std",
                                 layout = layout_matrix_jcq,
                                 nodeLabels = varnames_custom,
                                 style = "lisrel",
                                 theme = "Borkulo",
                                 shapeMan = "rectangle",
                                 shapeLat = "circle",
                                 color = c(rep(jcq_factor_colors, each = 5),
                                            jcq_factor_colors),
                                 node.width = 2.5,
                                 node.height = 1.0,
                                 borders = TRUE,
                                 border.color = "darkblue",
                                 residuals = FALSE,
                                 sizeMan = 1.2,
                                 sizeLat = 1.5,
                                 edge.width = 1,
                                 asize = 1.5,
                                 esize = 1,
                                 weighted = FALSE,
                                 edge.label.cex = 0.4,
                                 label.cex = 1.0,
                                 curvePivot = TRUE,
                                 reorder = FALSE,
                                 normalize = FALSE,
                                 edge.color = "darkblue",
                                 fade = FALSE,
                                 DoNotPlot = TRUE)

# 5.1.4.8 Adjust line connections (in radians) for a clear diagram
#         Adjust curvature of bidirectional lines (factor correlations)
Plot_model1$graphAttributes$Edges$edgeConnectPoints[1:15, 1] <- -0.5 * pi
Plot_model1$graphAttributes$Edges$edgeConnectPoints[1:15, 2] <-  1.5 * pi
bidirectional_edges1 <- c(16,17,18)
Plot_model1$graphAttributes$Edges$curve[bidirectional_edges1] <- -1

# 5.1.4.9 Render final diagram
plot(Plot_model1)

# 5.1.4.10 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 5.2 JCQ-9 MODEL 1a (Three-factor Correlated Model 9 item)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 5.2.1 Define the nine-item CFA model with three latent factors
model1a <- "TaskCrafting =~ jcq_02 + jcq_03 + jcq_05
            CognitiveCrafting  =~ jcq_06 + jcq_09 + jcq_10
            RelationalCrafting =~ jcq_11 + jcq_13 + jcq_15"

# 5.2.2 Fit the correlated model using robust MLR estimation
model1a.fit <- lavaan::sem(model = model1a,
                           data = data,
                           estimator = "MLR",
                           std.lv = FALSE,
                           missing = "ML")

# 5.2.3 Export the native parameter estimates and R-squared
readr::write_csv(lavaan::parameterEstimates(model1a.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                file.path(output_dir_05, 
                          "6.Step05_JCQ_9_Parameters.csv"), 
                          na = "NA")

# 5.2.3.1 Export native fit indices at full precision for Quarto
model1a.fit_indices <- lavaan::fitMeasures(model1a.fit)

readr::write_csv(data.frame(index = names(model1a.fit_indices), 
                            value = as.numeric(model1a.fit_indices)),
                file.path(output_dir_05, 
                          "7.Step05_JCQ_9_Fit_Indices.csv"), 
                na = "NA")

# 5.2.3.2 Archive the fitted model without refitting
saveRDS(model1a.fit, file.path(output_dir_05, "8.Step05_JCQ_9_CFA.rds"))

# 5.2.3.3 Summarize model fit (standardized loadings, CIs, R², fit measures)
utils::capture.output(summary(model1a.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_05, 
                                               "9.Step05_JCQ_9_CFA_Report.txt"),
                                               split = TRUE)

# 5.2.4 Retain the original item numbers within each factor
varnames_custom1a <- c(paste0("TC ", c(2, 3, 5)),
                       paste0("CC ", c(1, 4, 5)),
                       paste0("RC ", c(1, 3, 5)),
                       "TC", "CC", "RC")

# 5.2.5 Extract standardized estimates and build a lisrelModel object
lambdaEst1a <- lavaan::lavInspect(model1a.fit, "est.std")$lambda
psiEst1a <- lavaan::lavInspect(model1a.fit, "est.std")$psi
thetaEst1a <- lavaan::lavInspect(model1a.fit, "est.std")$theta
factors1a <- lavaan::lavNames(model1a.fit, type = "lv")
n_factors1a <- length(factors1a)

# 5.2.6 No regressions among factors (beta is zero)
betaEst1a <- matrix(0, nrow = n_factors1a,
                    ncol = n_factors1a,
                    dimnames = list(factors1a, factors1a))

# 5.2.7 Construct a lisrelModel object for semPlot
lisrel_object1a <- semPlot::lisrelModel(LY = lambdaEst1a,
                                        BE = betaEst1a,
                                        PS = psiEst1a,
                                        TE = thetaEst1a)

# 5.2.8 Define coordinates for the nine items and three factors
layout_matrix_jcq1a <- matrix(c(1,  4.00,  1,  3.00,  1,  2.00,  1,  1.00,  1, 
0.00,  1, -1.00, 1, -2.00,  1, -3.00,  1, -4.00,  0,  3.00,  0,  0.00, 0, -3.00), 
byrow = TRUE, ncol = 2)

# 5.2.9 Plot the correlated three-factor model
# 5.2.9.1 Open the PDF device with the specified figure dimensions
grDevices::cairo_pdf(filename = file.path(output_dir_05, 
                                          "10.Step05_JCQ_9_CFA_Plot.pdf"),
                     width = width_in_inches,
                     height = height_in_inches,
                     family = "sans", 
                     onefile = TRUE)
# 5.2.9.2 Prepare the path diagram without rendering it
Plot_model1a <- semPlot::semPaths(lisrel_object1a,
                                  what = "path",
                                  whatLabels = "std",
                                  layout = layout_matrix_jcq1a,
                                  nodeLabels = varnames_custom1a,
                                  style = "lisrel",
                                  theme = "Borkulo",
                                  shapeMan = "rectangle",
                                  shapeLat = "circle",
                                  color = c(rep(jcq_factor_colors, each = 3),
                                            jcq_factor_colors),
                                  node.width = 2.5,
                                  node.height = 1.0,
                                  borders = TRUE,
                                  border.color = "darkblue",
                                  residuals = FALSE,
                                  sizeMan = 1.2,
                                  sizeLat = 1.5,
                                  edge.width = 1,
                                  asize = 1.5,
                                  esize = 1,
                                  weighted = FALSE,
                                  edge.label.cex = 0.4,
                                  label.cex = 1.0,
                                  curvePivot = TRUE,
                                  reorder = FALSE,
                                  normalize = FALSE,
                                  edge.color = "darkblue",
                                  fade = FALSE,
                                  DoNotPlot = TRUE)

# 5.2.10 Adjust item-factor angles for a cleaner diagram
Plot_model1a$graphAttributes$Edges$edgeConnectPoints[1:9, 1] <- -0.5 * pi
Plot_model1a$graphAttributes$Edges$edgeConnectPoints[1:9, 2] <-  1.5 * pi
bidirectional_edges1a <- c(10, 11, 12)
Plot_model1a$graphAttributes$Edges$curve[bidirectional_edges1a] <- -1

# 5.2.11 Render the final diagram
plot(Plot_model1a)
# 5.2.12 Close the PDF device
dev.off()

# 5.2.13 Report the saved CFA outputs
message("JCQ-15 and JCQ-9 CFA results saved in: ",
normalizePath(output_dir_05, winslash = "/"))
