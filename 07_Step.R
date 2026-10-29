#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                 ┏┓┏┳┓┏┓┏┓  ━┓                                #
#                                 ┗┓ ┃ ┣ ┃┃   ┃                                #
#                                 ┗┛ ┻ ┗┛┣┛   ╹                                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                    Compare five UWES-9 CFAs structures                       # 
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.1 UWES-9 MODEL 0 (Unidimensional Model)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# One latent factor (WorkEngagement) loads on all nine items.
# Investigated by Schaufeli et al. (2006) and Dominguez-Salas et al. (2022).
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.1.1 Define the CFA model using the original database item labels
model0_uwes9 <- "WorkEngagement =~ uwes_01 + uwes_02 + uwes_05 +
                                   uwes_03 + uwes_04 + uwes_07 +
                                   uwes_06 + uwes_08 + uwes_09"

# 7.1.2 Fit the model using Robust Maximum Likelihood (MLR)
model0_uwes9.fit <- lavaan::sem(model = model0_uwes9,
                                data = data,
                                estimator = "MLR",
                                std.lv = FALSE,
                                missing = "ML")

# 7.1.2.1 Archive the fitted model without refitting
saveRDS(model0_uwes9.fit, file = file.path(output_dir_07,
"1.Step07_UWES9_Model0_Fit.rds"))

# 7.1.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model0_uwes9.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07,
                              "2.Step07_UWES9_Model0_Report.txt"),
                              split = TRUE)

# 7.1.3.1 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model0_uwes9.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_07, "3.Step07_UWES9_Model0_Parameters.csv"), na = "NA")

# 7.1.3.2 Export native fit indices at full precision for Quarto
model0_uwes9.fit_indices <- lavaan::fitMeasures(model0_uwes9.fit)

readr::write_csv(data.frame(index = names(model0_uwes9.fit_indices),
                            value = as.numeric(model0_uwes9.fit_indices)),
                 file.path(output_dir_07, "4.Step07_UWES9_Model0_Fit_Indices.csv"), 
                 na = "NA")

# 7.1.4 Print the model using semPlot
# 7.1.4.1 Custom labels: items grouped by dimension, then latent factors
varnames_custom0_uwes9 <- c("uwes_01", "uwes_02", "uwes_05",
                            "uwes_03", "uwes_04", "uwes_07",
                            "uwes_06", "uwes_08", "uwes_09",
                            "WE")

# 7.1.4.2 Extract standardized estimates in the model's variable order
lambdaEst0_uwes9 <- lavaan::lavInspect(model0_uwes9.fit, "est.std")$lambda
psiEst0_uwes9 <- lavaan::lavInspect(model0_uwes9.fit, "est.std")$psi
thetaEst0_uwes9 <- lavaan::lavInspect(model0_uwes9.fit, "est.std")$theta

# 7.1.4.3 Create a zero Beta matrix
factors0_uwes9 <- lavaan::lavNames(model0_uwes9.fit, type = "lv")
betaEst0_uwes9 <- matrix(0, nrow = length(factors0_uwes9),
                            ncol = length(factors0_uwes9),
                            dimnames = list(factors0_uwes9, factors0_uwes9))

# 7.1.4.4 Construct the LISREL object from the standardized matrices
lisrel_object0_uwes9 <- semPlot::lisrelModel(LY = lambdaEst0_uwes9,
                                             BE = betaEst0_uwes9,
                                             PS = psiEst0_uwes9,
                                             TE = thetaEst0_uwes9)

# 7.1.4.5 Use white nodes for the observed items and latent factors
uwes9_color_vector0 <- rep("white", length(varnames_custom0_uwes9))
names(uwes9_color_vector0) <- varnames_custom0_uwes9

# 7.1.4.6 Define the manual layout: nine items followed by latent factors
layout_matrix_uwes9_0 <- matrix(c(1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00,
                                  1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00, 0,
                                  0.00), byrow = TRUE, ncol = 2)

# 7.1.4.7 Define figure dimensions (mm to inches) 
width_in_inches  <- 79.953 / 25.4
height_in_inches <- 79.953 / 25.4
# Open the PDF device
grDevices::cairo_pdf(filename = file.path(output_dir_07, 
                                         "5.Step07_UWES9_Model0_Plot.pdf"),
                                         width = width_in_inches, 
                                         height = height_in_inches,
                                         family = "sans", 
                                         onefile = TRUE)

# 7.1.4.8 Generate the diagram without drawing it immediately
Plot_model0_uwes9 <- semPlot::semPaths(object = lisrel_object0_uwes9,
                                       what = "path",
                                       whatLabels = "std",
                                       layout = layout_matrix_uwes9_0,
                                       nodeLabels = varnames_custom0_uwes9,
                                       style = "lisrel",
                                       theme = "Borkulo",
                                       shapeMan = "rectangle",
                                       shapeLat = "circle",
                                       node.width = 2.5,
                                       node.height = 1.0,
                                       color = uwes9_color_vector0,
                                       borders = TRUE,
                                       border.color = "darkblue",
                                       residuals = FALSE,
                                       covAtResiduals = FALSE,
                                       intercepts = FALSE,
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

# 7.1.4.9 Identify paths by their endpoints and adjust their connections
edge_from0_uwes9 <- Plot_model0_uwes9$Edgelist$from
edge_to0_uwes9 <- Plot_model0_uwes9$Edgelist$to
loading_edges0_uwes9 <- which(edge_from0_uwes9 > 9 &
                              edge_to0_uwes9 <= 9)
Plot_model0_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges0_uwes9, 1] <- -0.5 * pi
Plot_model0_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges0_uwes9, 2] <-  1.5 * pi

# 7.1.4.10 Render the final diagram 
plot(Plot_model0_uwes9)

# 7.1.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.2 UWES-9 MODEL 1 (Three-factor Correlated Model)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Three freely correlated factors: Vigour, Dedication, Absorption.
# Investigated by Schaufeli et al. (2006) and Dominguez-Salas et al. (2022).
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 7.2.1 Define the CFA model using the original database item labels
model1_uwes9 <- "Vigour =~ uwes_01 + uwes_02 + uwes_05
                Dedication =~ uwes_03 + uwes_04 + uwes_07
                Absorption =~ uwes_06 + uwes_08 + uwes_09"

# 7.2.2 Fit the model using Robust Maximum Likelihood (MLR)
model1_uwes9.fit <- lavaan::sem(model = model1_uwes9,
                                data = data,
                                estimator = "MLR",
                                std.lv = FALSE,
                                missing = "ML")

# 7.2.2.1 Archive the fitted model without refitting
saveRDS(model1_uwes9.fit, file = file.path(output_dir_07,
                                           "6.Step07_UWES9_Model1_Fit.rds"))

# 7.2.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model1_uwes9.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07, 
                              "7.Step07_UWES9_Model1_Report.txt"),
                              split = TRUE)

# 7.2.3.1 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model1_uwes9.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_07, "8.Step07_UWES9_Model1_Parameters.csv"), na = "NA")

# 7.2.3.2 Export native fit indices at full precision for Quarto
model1_uwes9.fit_indices <- lavaan::fitMeasures(model1_uwes9.fit)

readr::write_csv(data.frame(index = names(model1_uwes9.fit_indices),
                 value = as.numeric(model1_uwes9.fit_indices)),
                 file.path(output_dir_07, "9.Step07_UWES9_Model1_Fit_Indices.csv"), 
                 na = "NA")

# 7.2.4 Print the model using semPlot
# 7.2.4.1 Custom labels: items grouped by dimension, then latent factors
varnames_custom1_uwes9 <- c("uwes_01", "uwes_02", "uwes_05",
                            "uwes_03", "uwes_04", "uwes_07",
                            "uwes_06", "uwes_08", "uwes_09",
                            "VI", "DE", "AB")

# 7.2.4.2 Extract standardized estimates in the model's variable order
lambdaEst1_uwes9 <- lavaan::lavInspect(model1_uwes9.fit, "est.std")$lambda
psiEst1_uwes9 <- lavaan::lavInspect(model1_uwes9.fit, "est.std")$psi
thetaEst1_uwes9 <- lavaan::lavInspect(model1_uwes9.fit, "est.std")$theta

# 7.2.4.3 Create a zero Beta matrix: there are no latent regressions
factors1_uwes9 <- lavaan::lavNames(model1_uwes9.fit, type = "lv")
betaEst1_uwes9 <- matrix(0, nrow = length(factors1_uwes9),
                            ncol = length(factors1_uwes9),
                            dimnames = list(factors1_uwes9, 
                                            factors1_uwes9))

# 7.2.4.4 Construct the LISREL object from the standardized matrices
lisrel_object1_uwes9 <- semPlot::lisrelModel(LY = lambdaEst1_uwes9,
                                             BE = betaEst1_uwes9,
                                             PS = psiEst1_uwes9,
                                             TE = thetaEst1_uwes9)

# 7.2.4.5 Use white nodes for the observed items and latent factors
uwes9_color_vector1 <- rep("white", length(varnames_custom1_uwes9))
names(uwes9_color_vector1) <- varnames_custom1_uwes9

# 7.2.4.6 Define the manual layout: nine items followed by latent factors
layout_matrix_uwes9_1 <- matrix(c(1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00,
1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00, 0, 3.00, 0, 0.00, 0, -3.00),
byrow = TRUE, ncol = 2)

# 7.2.4.7 Open a PDF device for the model's path diagram
grDevices::cairo_pdf(filename = file.path(output_dir_07, 
                                          "10.Step07_UWES9_Model1_Plot.pdf"),
                     width = width_in_inches, 
                     height = height_in_inches,
                     family = "sans", 
                     onefile = TRUE)

# 7.2.4.8 Generate the diagram without drawing it immediately
Plot_model1_uwes9 <- semPlot::semPaths(object = lisrel_object1_uwes9,
                                       what = "path",
                                       whatLabels = "std",
                                       layout = layout_matrix_uwes9_1,
                                       nodeLabels = varnames_custom1_uwes9,
                                       style = "lisrel",
                                       theme = "Borkulo",
                                       shapeMan = "rectangle",
                                       shapeLat = "circle",
                                       node.width = 2.5,
                                       node.height = 1.0,
                                       color = uwes9_color_vector1,
                                       borders = TRUE,
                                       border.color = "darkblue",
                                       residuals = FALSE,
                                       covAtResiduals = FALSE,
                                       intercepts = FALSE,
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

# 7.2.4.9 Identify paths by their endpoints and adjust their connections
edge_from1_uwes9 <- Plot_model1_uwes9$Edgelist$from
edge_to1_uwes9   <- Plot_model1_uwes9$Edgelist$to
loading_edges1_uwes9 <- which(edge_from1_uwes9 > 9 &
                              edge_to1_uwes9 <= 9)
Plot_model1_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges1_uwes9, 1] <- -0.5 * pi
Plot_model1_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges1_uwes9, 2] <-  1.5 * pi

# Select each factor covariance once, excluding its reversed copy.
latent_edges1_uwes9 <- which(edge_from1_uwes9 > 9 &
                             edge_to1_uwes9 > edge_from1_uwes9)
Plot_model1_uwes9$graphAttributes$Edges$curve[
latent_edges1_uwes9] <- -1

# 7.2.4.10 Render the final diagram
plot(Plot_model1_uwes9)

# 7.2.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.3 UWES-9 MODEL 2 (Higher-order Model)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# WorkEngagement loads on Vigour, Dedication, and Absorption.
# Dominguez-Salas et al. (2022) investigated this model.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.3.1 Define the CFA model using the original database item labels
model2_uwes9 <- "Vigour =~ uwes_01 + uwes_02 + uwes_05
                Dedication =~ uwes_03 + uwes_04 + uwes_07
                Absorption =~ uwes_06 + uwes_08 + uwes_09
                WorkEngagement =~ Vigour + Dedication + Absorption"

# 7.3.2 Fit the model using Robust Maximum Likelihood (MLR)
model2_uwes9.fit <- lavaan::sem(model = model2_uwes9,
                                data = data,
                                estimator = "MLR",
                                std.lv = FALSE,
                                missing = "ML")

# 7.3.2.1 Archive the fitted model without refitting
saveRDS(model2_uwes9.fit, file = file.path(output_dir_07,
"11.Step07_UWES9_Model2_Fit.rds"))

# 7.3.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model2_uwes9.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07, 
                              "12.Step07_UWES9_Model2_Report.txt"),
                              split = TRUE)

# 7.3.3.1 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model2_uwes9.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                 file.path(output_dir_07, "13.Step07_UWES9_Model2_Parameters.csv"), 
                 na = "NA")

# 7.3.3.2 Export native fit indices at full precision for Quarto
model2_uwes9.fit_indices <- lavaan::fitMeasures(model2_uwes9.fit)

readr::write_csv(data.frame(index = names(model2_uwes9.fit_indices),
                            value = as.numeric(model2_uwes9.fit_indices)),
                file.path(output_dir_07, "14.Step07_UWES9_Model2_Fit_Indices.csv"), 
                na = "NA")

# 7.3.4 Print the model using semPlot
# 7.3.4.1 Custom labels: items grouped by dimension, then latent factors
varnames_custom2_uwes9 <- c("uwes_01", "uwes_02", "uwes_05",
                            "uwes_03", "uwes_04", "uwes_07",
                            "uwes_06", "uwes_08", "uwes_09",
                            "VI", "DE", "AB", "WE")

# 7.3.4.2 Extract standardized estimates in the model's variable order
lambdaEst2_uwes9 <- lavaan::lavInspect(model2_uwes9.fit, "est.std")$lambda
psiEst2_uwes9 <- lavaan::lavInspect(model2_uwes9.fit, "est.std")$psi
thetaEst2_uwes9 <- lavaan::lavInspect(model2_uwes9.fit, "est.std")$theta

# 7.3.4.3 Retain the estimated second-order paths in the Beta matrix
betaEst2_uwes9 <- lavaan::lavInspect(model2_uwes9.fit, "est.std")$beta

# 7.3.4.4 Construct the LISREL object from the standardized matrices
lisrel_object2_uwes9 <- semPlot::lisrelModel(LY = lambdaEst2_uwes9,
                                             BE = betaEst2_uwes9,
                                             PS = psiEst2_uwes9,
                                             TE = thetaEst2_uwes9)

# 7.3.4.5 Use white nodes for the observed items and latent factors
uwes9_color_vector2 <- rep("white", length(varnames_custom2_uwes9))
names(uwes9_color_vector2) <- varnames_custom2_uwes9

# 7.3.4.6 Define the manual layout: nine items followed by latent factors
layout_matrix_uwes9_2 <- matrix(c(1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00,
                                  1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00, 0,
                                  3.00, 0, 0.00, 0, -3.00, -1, 0.00),
                                  byrow = TRUE, ncol = 2)

# 7.3.4.7 Open a PDF device for the model's path diagram
grDevices::cairo_pdf(filename = file.path(output_dir_07, 
                                          "15.Step07_UWES9_Model2_Plot.pdf"),
                                          width = width_in_inches,
                                          height = height_in_inches,
                                          family = "sans", 
                                          onefile = TRUE)

# 7.3.4.8 Generate the diagram without drawing it immediately
Plot_model2_uwes9 <- semPlot::semPaths(object = lisrel_object2_uwes9,
                                       what = "path",
                                       whatLabels = "std",
                                       layout = layout_matrix_uwes9_2,
                                       nodeLabels = varnames_custom2_uwes9,
                                       style = "lisrel",
                                       theme = "Borkulo",
                                       shapeMan = "rectangle",
                                       shapeLat = "circle",
                                       node.width = 2.5,
                                       node.height = 1.0,
                                       color = uwes9_color_vector2,
                                       borders = TRUE,
                                       border.color = "darkblue",
                                       residuals = FALSE,
                                       covAtResiduals = FALSE,
                                       intercepts = FALSE,
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

# 7.3.4.9 Identify paths by their endpoints and adjust their connections
edge_from2_uwes9 <- Plot_model2_uwes9$Edgelist$from
edge_to2_uwes9   <- Plot_model2_uwes9$Edgelist$to
loading_edges2_uwes9 <- which(edge_from2_uwes9 > 9 &
                              edge_to2_uwes9 <= 9)
Plot_model2_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_uwes9, 1] <- -0.5 * pi
Plot_model2_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges2_uwes9, 2] <-  1.5 * pi

# Retain the three directed paths from WorkEngagement to first-order factors.
higher_edges2_uwes9 <- which(edge_from2_uwes9 > 9 &
                             edge_to2_uwes9 > 9)
Plot_model2_uwes9$graphAttributes$Edges$edgeConnectPoints[
higher_edges2_uwes9, 1] <- -0.5 * pi
Plot_model2_uwes9$graphAttributes$Edges$edgeConnectPoints[
higher_edges2_uwes9, 2] <-  1.5 * pi

# 7.3.4.10 Render the final diagram
plot(Plot_model2_uwes9)
# 7.3.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.4 UWES-9 MODEL 3 (Two-factor Correlated Model)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Vigour and Dedication merged on one factor; Absorption remains separate.
# The two factors are freely correlated.
# Investigated by Dominguez-Salas et al. (2022)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.4.1 Define the CFA model using the original database item labels
model3_uwes9 <- "VigourDedication =~ uwes_01 + uwes_02 + uwes_05 +
                                     uwes_03 + uwes_04 + uwes_07
                Absorption =~ uwes_06 + uwes_08 + uwes_09"

# 7.4.2 Fit the model using Robust Maximum Likelihood (MLR)
model3_uwes9.fit <- lavaan::sem(model = model3_uwes9,
                                data = data,
                                estimator = "MLR",
                                std.lv = FALSE,
                                missing = "ML")

# 7.4.2.1 Archive the fitted model without refitting
saveRDS(model3_uwes9.fit, file = file.path(output_dir_07,
"16.Step07_UWES9_Model3_Fit.rds"))

# 7.4.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model3_uwes9.fit,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07, "17.Step07_UWES9_Model3_Report.txt"),
                              split = TRUE)

# 7.4.3.1 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model3_uwes9.fit,
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
                 file.path(output_dir_07, "18.Step07_UWES9_Model3_Parameters.csv"), 
                 na = "NA")

# 7.4.3.2 Export native fit indices at full precision for Quarto
model3_uwes9.fit_indices <- lavaan::fitMeasures(model3_uwes9.fit)

readr::write_csv(data.frame(index = names(model3_uwes9.fit_indices), 
                 value = as.numeric(model3_uwes9.fit_indices)),
                 file.path(output_dir_07, "19.Step07_UWES9_Model3_Fit_Indices.csv"), 
                 na = "NA")

# 7.4.4 Print the model using semPlot
# 7.4.4.1 Custom labels: items grouped by dimension, then latent factors
varnames_custom3_uwes9 <- c("uwes_01", "uwes_02", "uwes_05",
                            "uwes_03", "uwes_04", "uwes_07",
                            "uwes_06", "uwes_08", "uwes_09",
                            "VI/DE", "AB")

# 7.4.4.2 Extract standardized estimates in the model's variable order
lambdaEst3_uwes9 <- lavaan::lavInspect(model3_uwes9.fit, "est.std")$lambda
psiEst3_uwes9 <- lavaan::lavInspect(model3_uwes9.fit, "est.std")$psi
thetaEst3_uwes9 <- lavaan::lavInspect(model3_uwes9.fit, "est.std")$theta

# 7.4.4.3 Create a zero Beta matrix: there are no latent regressions
factors3_uwes9 <- lavaan::lavNames(model3_uwes9.fit, type = "lv")
betaEst3_uwes9 <- matrix(0, nrow = length(factors3_uwes9),
                            ncol = length(factors3_uwes9),
                            dimnames = list(factors3_uwes9, factors3_uwes9))

# 7.4.4.4 Construct the LISREL object from the standardized matrices
lisrel_object3_uwes9 <- semPlot::lisrelModel(LY = lambdaEst3_uwes9,
                                             BE = betaEst3_uwes9,
                                             PS = psiEst3_uwes9,
                                             TE = thetaEst3_uwes9)

# 7.4.4.5 Use white nodes for the observed items and latent factors
uwes9_color_vector3 <- rep("white", length(varnames_custom3_uwes9))
names(uwes9_color_vector3) <- varnames_custom3_uwes9

# 7.4.4.6 Define the manual layout: nine items followed by latent factors
layout_matrix_uwes9_3 <- matrix(c(1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00,
                                  1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00, 0,
                                  1.50, 0, -3.00), byrow = TRUE, ncol = 2)

# 7.4.4.7 Open a PDF device for the model's path diagram
grDevices::cairo_pdf(filename = file.path(output_dir_07, 
                                          "20.Step07_UWES9_Model3_Plot.pdf"),
                    width = width_in_inches, 
                    height = height_in_inches,
                    family = "sans", 
                    onefile = TRUE)

# 7.4.4.8 Generate the diagram without drawing it immediately
Plot_model3_uwes9 <- semPlot::semPaths(object = lisrel_object3_uwes9,
                                       what = "path",
                                       whatLabels = "std",
                                       layout = layout_matrix_uwes9_3,
                                       nodeLabels = varnames_custom3_uwes9,
                                       style = "lisrel",
                                       theme = "Borkulo",
                                       shapeMan = "rectangle",
                                       shapeLat = "circle",
                                       node.width = 2.5,
                                       node.height = 1.0,
                                       color = uwes9_color_vector3,
                                       borders = TRUE,
                                       border.color = "darkblue",
                                       residuals = FALSE,
                                       covAtResiduals = FALSE,
                                       intercepts = FALSE,
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

# 7.4.4.9 Identify paths by their endpoints and adjust their connections
edge_from3_uwes9 <- Plot_model3_uwes9$Edgelist$from
edge_to3_uwes9   <- Plot_model3_uwes9$Edgelist$to
loading_edges3_uwes9 <- which(edge_from3_uwes9 > 9 &
                              edge_to3_uwes9 <= 9)
Plot_model3_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges3_uwes9, 1] <- -0.5 * pi
Plot_model3_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges3_uwes9, 2] <-  1.5 * pi

# Select each factor covariance once, excluding its reversed copy.
latent_edges3_uwes9 <- which(edge_from3_uwes9 > 9 &
                             edge_to3_uwes9 > edge_from3_uwes9)
Plot_model3_uwes9$graphAttributes$Edges$curve[
latent_edges3_uwes9] <- -1

# 7.4.4.10 Render the final diagram
plot(Plot_model3_uwes9)
# 7.4.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.5 UWES-9 MODEL 4 (Modified Three-factor Correlated Model)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Three correlated factors with residual covariances 1-2 and 8-9.
# Investigated by Dominguez-Salas et al. (2022).
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.5.1 Define the CFA model using the original database item labels
model4_uwes9 <- "Vigour  =~ uwes_01 + uwes_02 + uwes_05
                Dedication =~ uwes_03 + uwes_04 + uwes_07
                Absorption =~ uwes_06 + uwes_08 + uwes_09
                uwes_01 ~~ uwes_02
                uwes_08 ~~ uwes_09"

# 7.5.2 Fit the model using Robust Maximum Likelihood (MLR)
model4_uwes9.fit <- lavaan::sem(model = model4_uwes9,
                                data = data,
                                estimator = "MLR",
                                std.lv = FALSE,
                                missing = "ML")

# 7.5.2.1 Archive the fitted model without refitting
saveRDS(model4_uwes9.fit, file = file.path(output_dir_07,
"21.Step07_UWES9_Model4_Fit.rds"))

# 7.5.3 Summarize model fit, standardized loadings, CIs, and R-squared
utils::capture.output(summary(model4_uwes9.fit, 
                              fit.measures = TRUE,
                              standardized = TRUE, 
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07, 
                                               "22.Step07_UWES9_Model4_Report.txt"),
                     split = TRUE)

# 7.5.3.1 Export native estimates, including covariances and R-squared
readr::write_csv(lavaan::parameterEstimates(model4_uwes9.fit, 
                                            standardized = TRUE, 
                                            ci = TRUE, 
                                            rsquare = TRUE),
file.path(output_dir_07, "23.Step07_UWES9_Model4_Parameters.csv"), na = "NA")

# 7.5.3.2 Export native fit indices at full precision for Quarto
model4_uwes9.fit_indices <- lavaan::fitMeasures(model4_uwes9.fit)

readr::write_csv(data.frame(index = names(model4_uwes9.fit_indices), 
                            value = as.numeric(model4_uwes9.fit_indices)),
file.path(output_dir_07, "24.Step07_UWES9_Model4_Fit_Indices.csv"), na = "NA")

# 7.5.4 Print the model using semPlot
# 7.5.4.1 Custom labels: items grouped by dimension, then latent factors
varnames_custom4_uwes9 <- c("uwes_01", "uwes_02", "uwes_05",
                            "uwes_03", "uwes_04", "uwes_07",
                            "uwes_06", "uwes_08", "uwes_09",
                            "VI", "DE", "AB")

# 7.5.4.2 Extract standardized estimates in the model's variable order
lambdaEst4_uwes9 <- lavaan::lavInspect(model4_uwes9.fit, "est.std")$lambda
psiEst4_uwes9 <- lavaan::lavInspect(model4_uwes9.fit, "est.std")$psi
thetaEst4_uwes9 <- lavaan::lavInspect(model4_uwes9.fit, "est.std")$theta

# 7.5.4.3 Create a zero Beta matrix: there are no latent regressions
factors4_uwes9 <- lavaan::lavNames(model4_uwes9.fit, type = "lv")
betaEst4_uwes9 <- matrix(0, nrow = length(factors4_uwes9),
                            ncol = length(factors4_uwes9),
                            dimnames = list(factors4_uwes9, factors4_uwes9))

# 7.5.4.4 Construct the LISREL object from the standardized matrices
lisrel_object4_uwes9 <- semPlot::lisrelModel(LY = lambdaEst4_uwes9,
                                             BE = betaEst4_uwes9,
                                             PS = psiEst4_uwes9,
                                             TE = thetaEst4_uwes9)

# 7.5.4.5 Use white nodes for the observed items and latent factors
uwes9_color_vector4 <- rep("white", length(varnames_custom4_uwes9))
names(uwes9_color_vector4) <- varnames_custom4_uwes9

# 7.5.4.6 Define the manual layout: nine items followed by latent factors
layout_matrix_uwes9_4 <- matrix(c(1, 4.00, 1, 3.00, 1, 2.00, 1, 1.00, 1, 0.00,
                                  1, -1.00, 1, -2.00, 1, -3.00, 1, -4.00, 0,
                                  3.00, 0, 0.00, 0, -3.00),
                                  byrow = TRUE, ncol = 2)

# 7.5.4.7 Open a PDF device for the model's path diagram
grDevices::cairo_pdf(filename = file.path(output_dir_07, "25.Step07_UWES9_Model4_Plot.pdf"),
                     width = width_in_inches, 
                     height = height_in_inches,
                     family = "sans", 
                     onefile = TRUE)

# 7.5.4.8 Generate the diagram without drawing it immediately
Plot_model4_uwes9 <- semPlot::semPaths(object = lisrel_object4_uwes9,
                                       what = "path",
                                       whatLabels = "std",
                                       layout = layout_matrix_uwes9_4,
                                       nodeLabels = varnames_custom4_uwes9,
                                       style = "lisrel",
                                       theme = "Borkulo",
                                       shapeMan = "rectangle",
                                       shapeLat = "circle",
                                       node.width = 2.5,
                                       node.height = 1.0,
                                       color = uwes9_color_vector4,
                                       borders = TRUE,
                                       border.color = "darkblue",
                                       residuals = FALSE,
                                       covAtResiduals = FALSE,
                                       intercepts = FALSE,
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
                                       DoNotPlot = TRUE,
                                       layoutScale = c(0.9, 1),
                                       layoutOffset = c(-0.1, 0))

# 7.5.4.9 Identify paths by their endpoints and adjust their connections
edge_from4_uwes9 <- Plot_model4_uwes9$Edgelist$from
edge_to4_uwes9 <- Plot_model4_uwes9$Edgelist$to
loading_edges4_uwes9 <- which(edge_from4_uwes9 > 9 &
                              edge_to4_uwes9 <= 9)
Plot_model4_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges4_uwes9, 1] <- -0.5 * pi
Plot_model4_uwes9$graphAttributes$Edges$edgeConnectPoints[
loading_edges4_uwes9, 2] <-  1.5 * pi

# Select each factor covariance once, excluding its reversed copy.
latent_edges4_uwes9 <- which(edge_from4_uwes9 > 9 &
                             edge_to4_uwes9 > edge_from4_uwes9)
Plot_model4_uwes9$graphAttributes$Edges$curve[
latent_edges4_uwes9] <- -1

# Show the specified residual covariances to the right of the items.
residual_edges4_uwes9 <- which(edge_from4_uwes9 <= 9 &
                               edge_to4_uwes9 <= 9 &
                               edge_from4_uwes9 < edge_to4_uwes9)
Plot_model4_uwes9$graphAttributes$Edges$edgeConnectPoints[
residual_edges4_uwes9, 1] <- 0.5 * pi
Plot_model4_uwes9$graphAttributes$Edges$edgeConnectPoints[
residual_edges4_uwes9, 2] <- 0.5 * pi
Plot_model4_uwes9$graphAttributes$Edges$curve[
residual_edges4_uwes9] <- 4

# 7.5.4.10 Render the final diagram
plot(Plot_model4_uwes9)
# 7.5.4.11 Close the PDF device
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.6 Compare the five UWES-9 models
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.6.1 Define the fit indices to extract from each fitted model
fit_indices_uwes9 <- c("chisq.scaled", "df.scaled", "pvalue.scaled",
                       "cfi.robust", "tli.robust", "rmsea.robust",
                       "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
                       "srmr", "aic", "bic")

# 7.6.2 Extract the same fit indices explicitly for all five models
fit0_uwes9 <- lavaan::fitMeasures(model0_uwes9.fit, fit_indices_uwes9)
fit1_uwes9 <- lavaan::fitMeasures(model1_uwes9.fit, fit_indices_uwes9)
fit2_uwes9 <- lavaan::fitMeasures(model2_uwes9.fit, fit_indices_uwes9)
fit3_uwes9 <- lavaan::fitMeasures(model3_uwes9.fit, fit_indices_uwes9)
fit4_uwes9 <- lavaan::fitMeasures(model4_uwes9.fit, fit_indices_uwes9)

# 7.6.3 Combine the results while retaining the numbering of this script
uwes9_fit_comparison <- data.frame(
Model = c("Model 0: Unidimensional",
         "Model 1: Three-factor correlated", 
         "Model 2: Higher-order",
         "Model 3: Vigour/Dedication vs. Absorption", 
         "Model 4: Three-factor modified"),
rbind(fit0_uwes9, 
      fit1_uwes9, 
      fit2_uwes9, 
      fit3_uwes9, 
      fit4_uwes9),
row.names   = NULL,check.names = FALSE)

# 7.6.4 Add native convergence and post-estimation checks to the comparison
# 7.6.4.1 Record convergence flags
uwes9_fit_comparison$Converged <- c(
lavaan::lavInspect(model0_uwes9.fit, "converged"),
lavaan::lavInspect(model1_uwes9.fit, "converged"),
lavaan::lavInspect(model2_uwes9.fit, "converged"),
lavaan::lavInspect(model3_uwes9.fit, "converged"),
lavaan::lavInspect(model4_uwes9.fit, "converged"))
# 7.6.4.2 Record post-estimation checks
uwes9_fit_comparison$Post_Check <- c(
lavaan::lavInspect(model0_uwes9.fit, "post.check"),
lavaan::lavInspect(model1_uwes9.fit, "post.check"),
lavaan::lavInspect(model2_uwes9.fit, "post.check"),
lavaan::lavInspect(model3_uwes9.fit, "post.check"),
lavaan::lavInspect(model4_uwes9.fit, "post.check"))

# 7.6.5 Print and export the comparison without rounding the saved values
print(uwes9_fit_comparison, row.names = FALSE)
# 7.6.5.1 Export the unrounded comparison table
readr::write_csv(uwes9_fit_comparison, file.path(output_dir_07,
"26.Step07_UWES9_Model_Comparison.csv"), na = "NA")

# 7.6.6 Report the saved CFA outputs
message("UWES-9 CFA results saved in: ",
normalizePath(output_dir_07, winslash = "/"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.7 UWES-3: Loevinger's scalability coefficients using Mokken analysis
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 7.7.1 Select the three published items and their complete-response sample
uwes3_items <- c("uwes_01", "uwes_03", "uwes_08")
uwes3_data <- stats::na.omit(as.data.frame(data[, uwes3_items, drop = FALSE]))

# 7.7.2 Estimate coefficients and their native standard errors
utils::capture.output(uwes3_mokken <- mokken::coefH(X = uwes3_data,
                                                    se = TRUE,
                                                    nice.output = FALSE,
                                                    results = TRUE),
                      file = file.path(output_dir_07, 
                                       "27.Step07_UWES3_Mokken_Report.txt"),
                      split = TRUE)

# 7.7.3 Archive the native result, including its uncertainty estimates
saveRDS(uwes3_mokken, file.path(output_dir_07, "28.Step07_UWES3_Mokken.rds"))

# 7.7.4 Export scale H and document the sample used for this analysis
uwes3_H_scale <- data.frame(scale = "UWES-3", 
                            n_source = nrow(data), 
                            n_analyzed = nrow(uwes3_data),
                            H = as.numeric(uwes3_mokken$H), 
                            se = as.numeric(uwes3_mokken$se.H))

readr::write_csv(uwes3_H_scale, file.path(output_dir_07,
"29.Step07_UWES3_Mokken_Scale.csv"), na = "NA")

# 7.7.5 Export Hi for each item in the same order as the analysis input
uwes3_H_items <- data.frame(variable = uwes3_items,
                            Hi = as.numeric(uwes3_mokken$Hi), 
                            se = as.numeric(uwes3_mokken$se.Hi),
                            row.names = NULL)
readr::write_csv(uwes3_H_items, file.path(output_dir_07,
"30.Step07_UWES3_Mokken_Items.csv"), na = "NA")

# 7.7.6 Export each item pair once, retaining its native Hij and standard error
uwes3_pairs <- which(upper.tri(uwes3_mokken$Hij), arr.ind = TRUE)
uwes3_H_pairs <- data.frame(item1 = uwes3_items[uwes3_pairs[, 1]],
                            item2 = uwes3_items[uwes3_pairs[, 2]],
                            Hij = uwes3_mokken$Hij[uwes3_pairs],
                            se = uwes3_mokken$se.Hij[uwes3_pairs])

readr::write_csv(uwes3_H_pairs, file.path(output_dir_07,
"31.Step07_UWES3_Mokken_Pairs.csv"), na = "NA")

# 7.7.7 Report scale scalability and the location of the saved results
print(uwes3_H_scale, row.names = FALSE)
message("UWES-3 Mokken results saved in: ",
normalizePath(output_dir_07, winslash = "/"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7.8 WAMI-10: Spanish orthogonal bifactor CFA
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Why: Re-estimate the published general and two-specific-factor structure
#      using MLR/FIML
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 7.8.1 Select the ten WAMI items
wami_cfa_items <- c("wami_01", "wami_02", "wami_03", "wami_04", "wami_05",
                    "wami_06", "wami_07", "wami_08", "wami_09", "wami_10")
wami_cfa_data <- data[, wami_cfa_items]
# Item 3 is reverse-keyed; preserve its stored direction and free loadings.
# No scores or reverse-scored responses are created in this section.

# 7.8.2 Specify published orthogonal bifactor model
# Source: Carrasco et al. (2026), Figure 1 and Table 4, model Bi-F2.
# DOI: 10.1007/s44202-026-00586-x.
# General: all items; self: 1, 2, 4, 5, 7, 8; world: 3, 6, 9, 10.
model_wami10_bifactor <- '
wami_general =~ wami_01 + wami_02 + wami_03 + wami_04 + wami_05 +
wami_06 + wami_07 + wami_08 + wami_09 + wami_10
wami_self =~ wami_01 + wami_02 + wami_04 + wami_05 + wami_07 + wami_08
wami_world =~ wami_03 + wami_06 + wami_09 + wami_10 '

# 7.8.3 Estimate MLR with missing responses
# The source study used ULS with robust methods in EQS, not MLR/FIML.
# Unit factor variances leave all loadings free; factors are orthogonal.
fit_wami10_bifactor <- lavaan::cfa(model = model_wami10_bifactor,
                                   data = wami_cfa_data,
                                   estimator = "MLR",
                                   missing = "fiml",
                                   meanstructure = TRUE,
                                   std.lv = TRUE,
                                   orthogonal = TRUE)

# 7.8.3.1 Archive the native fitted model
saveRDS(fit_wami10_bifactor, file.path(output_dir_07,
"32.Step07_WAMI10_BiF2_Fit.rds"))

# 7.8.3.2 Require convergence before extracting results
if (!isTRUE(lavaan::lavInspect(fit_wami10_bifactor, "converged"))) {
stop("WAMI bifactor estimation did not converge; inspect the archived fit.",
call. = FALSE)
}

# 7.8.4 Extract estimates and model diagnostics
# 7.8.4.1 Extract native parameter estimates
wami_parameters <- as.data.frame(lavaan::parameterEstimates(fit_wami10_bifactor, 
                                                            standardized = TRUE, 
                                                            ci = TRUE, 
                                                            level = 0.95,
                                                            rsquare = TRUE))

# 7.8.4.2 Extract standardized estimates and intervals
# These SEs and intervals concern std.all estimates, not raw estimates.
wami_standardized <- as.data.frame(lavaan::standardizedSolution(fit_wami10_bifactor, 
                                                                type = "std.all", 
                                                                ci = TRUE, 
                                                                level = 0.95))

# 7.8.4.3 Separate general and specific loadings
wami_loadings <- wami_standardized[wami_standardized$op == "=~", ]
wami_loadings$component <- ifelse(wami_loadings$lhs == "wami_general",
"General", "Specific")
rownames(wami_loadings) <- NULL
wami_general_loadings <- wami_loadings[wami_loadings$component == "General", ]
wami_specific_loadings <- wami_loadings[wami_loadings$component == "Specific", ]
rownames(wami_general_loadings) <- NULL
rownames(wami_specific_loadings) <- NULL

# 7.8.4.4 Extract all native fit indices
wami_fit_values <- lavaan::fitMeasures(fit_wami10_bifactor)
wami_fit_indices <- data.frame(index = names(wami_fit_values),
value = as.numeric(wami_fit_values))
wami_robust_indices <- c("npar", 
                         "chisq.scaled", 
                         "df.scaled",
                         "pvalue.scaled", 
                         "cfi.robust", 
                         "tli.robust",
                         "rmsea.robust", 
                         "rmsea.ci.lower.robust",
                         "rmsea.ci.upper.robust", 
                         "srmr", 
                         "aic", 
                         "bic")

# 7.8.4.5 Record convergence and admissibility diagnostics
# post.check concerns solution admissibility, not fit or score reliability.
wami_parameter_table <- lavaan::parTable(fit_wami10_bifactor)
wami_free_se <- wami_parameter_table$se[wami_parameter_table$free > 0L]
wami_information <- lavaan::lavInspect(fit_wami10_bifactor, "information")
wami_information_eigenvalues <- if (all(is.finite(wami_information))) {
eigen((wami_information + t(wami_information)) / 2,
symmetric = TRUE, only.values = TRUE)$values
} else {
NA_real_
}
wami_fit_checks <- data.frame(model = "WAMI10_BiF2",
n_items = length(lavaan::lavNames(fit_wami10_bifactor, "ov")),
n_input = nrow(wami_cfa_data),
n_used = lavaan::lavInspect(fit_wami10_bifactor, "ntotal"),
n_complete = sum(stats::complete.cases(wami_cfa_data)),
Converged = lavaan::lavInspect(fit_wami10_bifactor, "converged"),
Post_Check = isTRUE(lavaan::lavInspect(fit_wami10_bifactor, "post.check")),
Finite_SE = length(wami_free_se) > 0L && all(is.finite(wami_free_se)),
Information_PD = all(is.finite(wami_information_eigenvalues)) &&
all(wami_information_eigenvalues > 0),
Min_Information_Eigenvalue = min(wami_information_eigenvalues),
Min_Residual_Variance = min(diag(lavaan::lavInspect(
fit_wami10_bifactor, "est")$theta)))
wami_fit_summary <- data.frame(wami_fit_checks,
as.list(wami_fit_values[wami_robust_indices]),
row.names = NULL, check.names = FALSE)
if (!all(unlist(wami_fit_checks[c("Post_Check", "Finite_SE",
"Information_PD")]))) {
warning("WAMI solution diagnostics require review; no model changes applied.",
call. = FALSE)
}

# 7.8.4.6 Record model and analysis settings
wami_cfa_settings <- list(estimator = "MLR", 
                          missing = "fiml",
                          meanstructure = TRUE, 
                          std.lv = TRUE, 
                          orthogonal = TRUE,
                          parameter_ci_level = 0.95, 
                          rmsea_ci_level = 0.90,
item3_direction = "As stored in data; no direction inferred",
reverse_scoring_applied_step07 = FALSE,
analysis_role = "Re-estimation of the published Spanish bifactor structure",
estimator_note = "MLR/FIML; the source study used ULS with robust methods",
model_source = "Carrasco et al. (2026), Figure 1 and Table 4, Bi-F2",
model_source_doi = "10.1007/s44202-026-00586-x",
factor_labels = c(MW = "Meaningful Work", 
                  SP = "Self-perspective",
                  WP = "World-perspective"),
r_version = as.character(getRversion()),
lavaan_version = as.character(getNamespaceVersion("lavaan")))

# 7.8.5 Export CFA summaries and tables
# 7.8.5.1 Save the native model summary
utils::capture.output(summary(fit_wami10_bifactor,
                              fit.measures = TRUE,
                              standardized = TRUE,
                              ci = TRUE,
                              rsquare = TRUE),
                              file = file.path(output_dir_07, 
                              "33.Step07_WAMI10_BiF2_Report.txt"),
                      split = TRUE)
utils::capture.output(print(wami_fit_summary, row.names = FALSE),
print(wami_general_loadings, row.names = FALSE),
print(wami_specific_loadings, row.names = FALSE), print(wami_cfa_settings),
file = file.path(output_dir_07, "33.Step07_WAMI10_BiF2_Report.txt"),
append = TRUE, split = TRUE)
# 7.8.5.2 Export native parameters and R-squared
readr::write_csv(wami_parameters, file.path(output_dir_07,
"34.Step07_WAMI10_BiF2_Parameters.csv"), na = "NA")
# 7.8.5.3 Export unrounded native fit indices
readr::write_csv(wami_fit_indices, file.path(output_dir_07,
"35.Step07_WAMI10_BiF2_Fit_Indices.csv"), na = "NA")

# 7.8.5.4 Export standardized estimates and intervals
readr::write_csv(wami_standardized, file.path(output_dir_07,
"37.Step07_WAMI10_BiF2_Standardized.csv"), na = "NA")
# 7.8.5.5 Export general and specific loadings
readr::write_csv(wami_loadings, file.path(output_dir_07,
"38.Step07_WAMI10_BiF2_Loadings.csv"), na = "NA")
# 7.8.5.6 Archive reusable numerical reporting objects
wami_cfa <- list(models = list(WAMI10_BiF2 = model_wami10_bifactor),
fits = list(WAMI10_BiF2 = fit_wami10_bifactor), fit_indices = wami_fit_summary, 
                                                native_fit_indices = wami_fit_indices,
                                                parameters = wami_parameters, 
                                                standardized = wami_standardized,
                                                loadings = wami_loadings, 
                                                general_loadings = wami_general_loadings,
                                                specific_loadings = wami_specific_loadings, 
                                                checks = wami_fit_checks,
                                                items = wami_cfa_items, 
                                                settings = wami_cfa_settings)
saveRDS(wami_cfa, file.path(output_dir_07,
"39.Step07_WAMI10_BiF2_Reporting_Objects.rds"))

# 7.8.6 Draw the standardized bifactor diagram
# 7.8.6.1 Extract standardized model matrices
lambdaEst_wami <- lavaan::lavInspect(fit_wami10_bifactor, "est.std")$lambda
psiEst_wami <- lavaan::lavInspect(fit_wami10_bifactor, "est.std")$psi
thetaEst_wami <- lavaan::lavInspect(fit_wami10_bifactor, "est.std")$theta
factors_wami <- lavaan::lavNames(fit_wami10_bifactor, type = "lv")
betaEst_wami <- matrix(0, nrow = length(factors_wami),
ncol = length(factors_wami), dimnames = list(factors_wami, factors_wami))

# 7.8.6.2 Construct the standardized LISREL object
lisrel_object_wami <- semPlot::lisrelModel(LY = lambdaEst_wami,
                                           BE = betaEst_wami,
                                           PS = psiEst_wami,
                                           TE = thetaEst_wami)

# 7.8.6.3 Match labels to model order
node_names_wami <- c(rownames(lambdaEst_wami), colnames(lambdaEst_wami))
node_labels_wami <- c(stats::setNames(wami_cfa_items, wami_cfa_items),
                      wami_general = "WM", wami_self = "SP",
                      wami_world = "WP")[node_names_wami]

# 7.8.6.4 Define white nodes and borders
wami_color_vector <- rep("white", length(node_names_wami))
names(wami_color_vector) <- node_names_wami

# 7.8.6.5 Define general and specific coordinates
# Items are centered; general is left and specifics are right.
layout_matrix_wami <- matrix(c(0, 4.5, 0, 3.5, 0, -1.5, 0, 2.5, 0, 1.5,
0, -2.5, 0, 0.5, 0, -0.5, 0, -3.5, 0, -4.5, -1.8, 0, 1.8, 2, 1.8, -3),
byrow = TRUE, ncol = 2)
rownames(layout_matrix_wami) <- c(wami_cfa_items,
                                  "wami_general", 
                                  "wami_self", 
                                  "wami_world")
layout_matrix_wami <- layout_matrix_wami[node_names_wami, , drop = FALSE]

# 7.8.6.6 Open the matching PDF device
grDevices::cairo_pdf(filename = file.path(output_dir_07,
"36.Step07_WAMI10_BiF2_Plot.pdf"), width = 79.953 / 25.4,
height = 79.953 / 25.4, family = "sans", onefile = TRUE)

tryCatch({
# 7.8.6.7 Prepare the diagram without drawing
Plot_model_wami <- semPlot::semPaths(object = lisrel_object_wami,
                                     what = "path",
                                     whatLabels = "std",
                                     layout = layout_matrix_wami,
                                     nodeLabels = unname(node_labels_wami),
                                     style = "lisrel",
                                     theme = "Borkulo",
                                     shapeMan = "rectangle",
                                     shapeLat = "circle",
                                     node.width = 2.5,
                                     node.height = 1.0,
                                     color = wami_color_vector,
                                     borders = TRUE,
                                     border.color = "darkblue",
                                     residuals = FALSE,
                                     covAtResiduals = FALSE,
                                     intercepts = FALSE,
                                     sizeMan = 1.2,
                                     sizeLat = 1.5,
                                     edge.width = 1,
                                     asize = 1.5,
                                     esize = 1,
                                     weighted = FALSE,
                                     edge.label.cex = 0.4,
                                     edge.label.bg = "white",
                                     label.cex = 1.0,
                                     curvePivot = TRUE,
                                     reorder = FALSE,
                                     normalize = FALSE,
                                     edge.color = "darkblue",
                                     fade = FALSE,
                                     DoNotPlot = TRUE)

# 7.8.6.8 Locate the twenty loading paths
edge_from_wami <- Plot_model_wami$Edgelist$from
edge_to_wami <- Plot_model_wami$Edgelist$to
loading_edges_wami <- which(edge_from_wami > length(wami_cfa_items) &
                            edge_to_wami <= length(wami_cfa_items))

# 7.8.6.9 Route general and specific loadings
factor_x_wami <- layout_matrix_wami[edge_from_wami[loading_edges_wami], 1]
item_x_wami <- layout_matrix_wami[edge_to_wami[loading_edges_wami], 1]
Plot_model_wami$graphAttributes$Edges$edgeConnectPoints[
loading_edges_wami, 1] <- ifelse(factor_x_wami < item_x_wami,
0.5 * pi, 1.5 * pi)
Plot_model_wami$graphAttributes$Edges$edgeConnectPoints[
loading_edges_wami, 2] <- ifelse(factor_x_wami < item_x_wami,
1.5 * pi, 0.5 * pi)

# 7.8.6.10 Render the final diagram
plot(Plot_model_wami)
}, finally = {
# 7.8.6.11 Close the PDF device
grDevices::dev.off()
})
# Note: WM = Work meaning; SP = Self-perspective; WP = World-perspective.

# 7.8.7 Verify and report WAMI outputs
wami_output_files <- file.path(output_dir_07,
c("32.Step07_WAMI10_BiF2_Fit.rds", 
"33.Step07_WAMI10_BiF2_Report.txt",
"34.Step07_WAMI10_BiF2_Parameters.csv",
"35.Step07_WAMI10_BiF2_Fit_Indices.csv", 
"36.Step07_WAMI10_BiF2_Plot.pdf",
"37.Step07_WAMI10_BiF2_Standardized.csv",
"38.Step07_WAMI10_BiF2_Loadings.csv",
"39.Step07_WAMI10_BiF2_Reporting_Objects.rds"))
stopifnot(all(file.exists(wami_output_files)),
all(file.info(wami_output_files)$size > 0))
message("WAMI-10 bifactor CFA results saved in: ",
normalizePath(output_dir_07, winslash = "/", mustWork = TRUE))
