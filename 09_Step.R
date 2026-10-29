#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                               ┏┓┏┳┓┏┓┏┓  ┏┓                                  #
#                               ┗┓ ┃ ┣ ┃┃  ┗┫                                  #
#                               ┗┛ ┻ ┗┛┣┛  ┗┛                                  #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#       Job Crafting: Joint CFA Models and Pruned Latent Network Model         #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.0 Prepare the data and loading pattern
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.0.1 Select the exact JCS-12 and JCQ-9 items used in Step 8
jc9_items <- c(sprintf("jcs_%02d", c(1:3, 6:8, 12:14, 17:19)),
               sprintf("jcq_%02d", c(2, 3, 5, 6, 9, 10, 11, 13, 15)))
jc9_factors <- c("ISJR", "DHJD", "ISOJR", "ICJD", "TC", "CC", "RC")
jc9_data <- readRDS(file.path(output_dir_08, "1.Step08_JC21_Input.rds"))

# 9.0.2 Define free primary loadings and fixed zero cross-loadings
jc9_lambda <- matrix(0, nrow = 21, ncol = 7,
dimnames = list(jc9_items, jc9_factors))
jc9_lambda[c("jcs_01", "jcs_02", "jcs_03"), "ISJR"] <- 1
jc9_lambda[c("jcs_06", "jcs_07", "jcs_08"), "DHJD"] <- 1
jc9_lambda[c("jcs_12", "jcs_13", "jcs_14"), "ISOJR"] <- 1
jc9_lambda[c("jcs_17", "jcs_18", "jcs_19"), "ICJD"] <- 1
jc9_lambda[c("jcq_02", "jcq_03", "jcq_05"), "TC"] <- 1
jc9_lambda[c("jcq_06", "jcq_09", "jcq_10"), "CC"] <- 1
jc9_lambda[c("jcq_11", "jcq_13", "jcq_15"), "RC"] <- 1

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.1 MODEL 1: Seven correlated factors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.1.1 Estimate the seven-factor CFA
jc9_cfa.fit <- psychonetrics::lvm(data = jc9_data,
                                  vars = jc9_items,
                                  latents = jc9_factors,
                                  lambda = jc9_lambda,
                                  beta = "zero",
                                  latent = "cov",
                                  sigma_zeta = "full",
                                  residual = "cov",
                                  sigma_epsilon = "diag",
                                  identification = "loadings",
                                  estimator = "MLR",
                                  missing = "auto",
                                  optimizer = "nlminb",
                                  storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE, 
                        warn_improper = TRUE,
                        bounded = FALSE)

# 9.1.2 Archive M1
saveRDS(jc9_cfa.fit, file.path(output_dir_09, "1.Step09_JC21_CFA_MLR_FIML.rds"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.2 Estimate the pruned seven-node latent network
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.2.1 Estimate the latent network and prune nonsignificant edges
jc9_lnm.fit <- psychonetrics::lvm(data = jc9_data,
                                  vars = jc9_items,
                                  latents = jc9_factors,
                                  lambda = jc9_lambda,
                                  beta = "zero",
                                  latent = "ggm",
                                  omega_zeta = "full",
                                  delta_zeta = "diag",
                                  residual = "cov",
                                  sigma_epsilon = "diag",
                                  identification = "loadings",
                                  estimator = "MLR",
                                  missing = "auto",
                                  optimizer = "nlminb",
                                  storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE, warn_improper = TRUE,
                        bounded = FALSE) |>
psychonetrics::prune(alpha = 0.01, 
                     matrices = "omega_zeta",
                     recursive = TRUE, 
                     verbose = TRUE,
                     addMIs = FALSE, 
                     warn_improper = TRUE,
                     bounded = FALSE)

# 9.2.2 Archive the pruned network
saveRDS(jc9_lnm.fit, file.path(output_dir_09,
                               "2.Step09_JC21_LNM_Pruned_MLR_FIML.rds"))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.3 MODEL 2: One general factor
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.3.1 Load all 21 items on GeneralJC
jc9_lambda2 <- matrix(1, nrow = 21, ncol = 1,
dimnames = list(jc9_items, "GeneralJC"))

# 9.3.2 Estimate the one-factor CFA
jc9_cfa2.fit <- psychonetrics::lvm(data = jc9_data,
                                   vars = jc9_items,
                                   latents = colnames(jc9_lambda2),
                                   lambda = jc9_lambda2,
                                   beta = "zero",
                                   latent = "cov",
                                   sigma_zeta = "diag",
                                   residual = "cov",
                                   sigma_epsilon = "diag",
                                   identification = "loadings",
                                   estimator = "MLR",
                                   missing = "auto",
                                   optimizer = "nlminb",
                                   storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE, 
                        warn_improper = TRUE,
                        bounded = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.4 MODEL 3: Six factors with ISOJR and RC merged
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.4.1 Combine the two loading columns without changing the common item order
jc9_lambda3 <- jc9_lambda[, c("ISJR", "DHJD", "ISOJR", "ICJD", "TC", "CC")]
jc9_lambda3[, "ISOJR"] <- jc9_lambda[, "ISOJR"] + jc9_lambda[, "RC"]
colnames(jc9_lambda3)[3] <- "ISOJR_RC"

# 9.4.2 Estimate the CFA with ISOJR and RC merged
jc9_cfa3.fit <- psychonetrics::lvm(data = jc9_data,
                                   vars = jc9_items,
                                   latents = colnames(jc9_lambda3),
                                   lambda = jc9_lambda3,
                                   beta = "zero",
                                   latent = "cov",
                                   sigma_zeta = "full",
                                   residual = "cov",
                                   sigma_epsilon = "diag",
                                   identification = "loadings",
                                   estimator = "MLR",
                                   missing = "auto",
                                   optimizer = "nlminb",
                                   storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE,
                        warn_improper = TRUE,
                        bounded = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.5 MODEL 4: Six factors with ICJD and TC merged
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.5.1 Combine the challenge and task-crafting loading columns
jc9_lambda4 <- jc9_lambda[, c("ISJR", 
                              "DHJD", 
                              "ISOJR", 
                              "ICJD", 
                              "CC", 
                              "RC")]
jc9_lambda4[, "ICJD"] <- jc9_lambda[, "ICJD"] + jc9_lambda[, "TC"]
colnames(jc9_lambda4)[4] <- "ICJD_TC"

# 9.5.2 Estimate the CFA with ICJD and TC merged
jc9_cfa4.fit <- psychonetrics::lvm(data = jc9_data,
                                   vars = jc9_items,
                                   latents = colnames(jc9_lambda4),
                                   lambda = jc9_lambda4,
                                   beta = "zero",
                                   latent = "cov",
                                   sigma_zeta = "full",
                                   residual = "cov",
                                   sigma_epsilon = "diag",
                                   identification = "loadings",
                                   estimator = "MLR",
                                   missing = "auto",
                                   optimizer = "nlminb",
                                   storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE, 
                        warn_improper = TRUE,
                        bounded = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.6 MODEL 5: One higher-order factor
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.6.1 Add a general factor with no direct loading on the observed items
jc9_lambda5 <- cbind(jc9_lambda, GeneralJC = 0)
jc9_factors5 <- colnames(jc9_lambda5)

# 9.6.2 Specify directed paths from GeneralJC to the seven first-order factors
jc9_beta5 <- matrix(0, nrow = 8, ncol = 8,
dimnames = list(jc9_factors5, jc9_factors5))
jc9_beta5[jc9_factors, "GeneralJC"] <- 1

# 9.6.3 Estimate the higher-order CFA
jc9_cfa5.fit <- psychonetrics::lvm(data = jc9_data,
                                   vars = jc9_items,
                                   latents = jc9_factors5,
                                   lambda = jc9_lambda5,
                                   beta = jc9_beta5,
                                   latent = "cov",
                                   sigma_zeta = "diag",
                                   residual = "cov",
                                   sigma_epsilon = "diag",
                                   identification = "loadings",
                                   simplelambdastart = TRUE,
                                   estimator = "MLR",
                                   missing = "auto",
                                   optimizer = "nlminb",
                                   storedata = TRUE) |>
psychonetrics::fixpar(matrix = "beta", 
                      row = "ISJR",
                      col = "GeneralJC", 
                      value = 1) |>
psychonetrics::runmodel(addMIs = FALSE, 
                        warn_improper = TRUE,
                        bounded = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.7 MODEL 6: General factor plus seven orthogonal specific factors
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.7.1 Add a general loading to every item while retaining its specific factor
jc9_lambda6 <- cbind(GeneralJC = 1, jc9_lambda)

# 9.7.2 Estimate the orthogonal bifactor CFA
jc9_cfa6.fit <- psychonetrics::lvm(data = jc9_data,
                                   vars = jc9_items,
                                   latents = colnames(jc9_lambda6),
                                   lambda = jc9_lambda6,
                                   beta = "zero",
                                   latent = "cov",
                                   sigma_zeta = "diag",
                                   residual = "cov",
                                   sigma_epsilon = "diag",
                                   identification = "loadings",
                                   estimator = "MLR",
                                   missing = "auto",
                                   optimizer = "nlminb",
                                   storedata = TRUE) |>
psychonetrics::runmodel(addMIs = FALSE, 
                        warn_improper = TRUE,
                        bounded = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.8 Archive the seven models and check their solutions
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.8.1 Register the models in the same order used in the comparison table
jc9_models <- list(M1 = jc9_cfa.fit, 
                   M2 = jc9_cfa2.fit, 
                   M3 = jc9_cfa3.fit, 
                   M4 = jc9_cfa4.fit, 
                   M5 = jc9_cfa5.fit, 
                   M6 = jc9_cfa6.fit,
                   LNM_Pruned = jc9_lnm.fit)
saveRDS(jc9_models, file.path(output_dir_09, "3.Step09_JC21_All_Models.rds"))

# 9.8.2 Check positive definiteness using the symmetric eigenvalues
jc9_min_eigen <- function(x) {
if (!all(is.finite(x))) return(NA_real_)
min(eigen((x + t(x)) / 2, symmetric = TRUE, only.values = TRUE)$values)
}

# 9.8.3 Check convergence, covariance matrices, and standard errors
jc9_checks <- lapply(jc9_models, function(model) {
psi <- psychonetrics::getmatrix(model, "sigma_zeta")
theta <- psychonetrics::getmatrix(model, "sigma_epsilon")
sigma <- psychonetrics::getmatrix(model, "sigma")
latent_eigen <- jc9_min_eigen(psi)
residual_variance <- min(diag(theta))
implied_eigen <- jc9_min_eigen(sigma)
information_pd <- FALSE
if (length(model@information) && all(is.finite(model@information))) {
information_values <- eigen((model@information + t(model@information)) / 2,
symmetric = TRUE, only.values = TRUE)$values
information_pd <- min(information_values) >
1e-8 * max(abs(information_values))
}
free_parameters <- model@parameters[!model@parameters$fixed, ]
result <- data.frame(N = sum(model@sample@groups$nobs),
Computed = isTRUE(model@computed),
Converged = isTRUE(model@optim$convergence == 0L),
Admissible = isTRUE(latent_eigen > 1e-8 &&
residual_variance > 1e-8 && implied_eigen > 1e-8),
Information_PD = information_pd,
SE_OK = all(is.finite(free_parameters$se)) && all(free_parameters$se >= 0),
Min_Latent_Eigenvalue = latent_eigen,
Min_Residual_Variance = residual_variance,
Min_Implied_Eigenvalue = implied_eigen)
result$Status <- if (!result$Converged) "Not converged" else
if (!result$Admissible) "Improper or boundary solution" else
if (!result$Information_PD) "Information matrix requires inspection" else
if (!result$SE_OK) "Standard errors require inspection" else "OK"
result
})
jc9_checks <- dplyr::bind_rows(jc9_checks, .id = "model")
readr::write_csv(jc9_checks, file.path(output_dir_09, 
                                  "4.Step09_JC21_Model_Checks.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.9 Compare the seven models
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.9.1 Export each fitted model and its native fit table
jc9_fit_list <- list()
for (jc9_id in names(jc9_models)) {
jc9_model <- jc9_models[[jc9_id]]
jc9_number <- 3L + 2L * match(jc9_id, names(jc9_models))
saveRDS(jc9_model, 
        file.path(output_dir_09, 
        paste0(jc9_number, 
        ".Step09_JC21_", 
        jc9_id, 
        "_psychonetrics.rds")))
utils::capture.output(jc9_fit_list[[jc9_id]] <- psychonetrics::fit(jc9_model),
file = file.path(output_dir_09,
paste0(jc9_number + 1L, ".Step09_JC21_", jc9_id, "_Fit_Report.txt")), split = TRUE)
}
jc9_fit_indices <- dplyr::bind_rows(jc9_fit_list, .id = "model")
readr::write_csv(jc9_fit_indices, file.path(output_dir_09,
"19.Step09_JC21_All_Fit_Indices.csv"), na = "NA")

# 9.9.2 Assemble the comparison table
jc9_fit_names <- c("npar", 
                  "logl", 
                  "df", 
                  "chisq.scaled", 
                  "df.scaled",
                   "pvalue.scaled", 
                   "cfi.robust", 
                   "tli.robust", 
                   "rmsea.robust",
                   "rmsea.ci.lower.robust", 
                   "rmsea.ci.upper.robust", 
                   "srmr", 
                   "aic.ll", 
                   "bic")
jc9_fit_summary <- tidyr::expand_grid(model = names(jc9_models),
Measure = jc9_fit_names) |>
dplyr::left_join(jc9_fit_indices, by = c("model", "Measure")) |>
tidyr::pivot_wider(names_from = Measure, values_from = Value) |>
dplyr::rename(AIC = aic.ll, BIC = bic) |>
dplyr::left_join(jc9_checks, by = "model") |>
dplyr::mutate(Model = unname(c(M1 = "M1: Seven correlated factors",
                               M2 = "M2: One general factor", 
                               M3 = "M3: Six factors, ISOJR-RC merged",
                               M4 = "M4: Six factors, ICJD-TC merged", 
                               M5 = "M5: One higher-order factor",
                               M6 = "M6: General plus seven orthogonal specifics",
                               LNM_Pruned = "LNM: Recursively pruned")[model])
                               , .after = model) |>
dplyr::relocate(N, .after = Model)

# 9.9.3 Compute information-criterion differences for usable solutions only
jc9_eligible <- with(jc9_fit_summary,
Computed & Converged & Admissible & Information_PD & SE_OK)
jc9_eligible[is.na(jc9_eligible)] <- FALSE
for (jc9_criterion in c("AIC", "BIC")) {
jc9_values <- jc9_fit_summary[[jc9_criterion]]
jc9_valid <- jc9_eligible & is.finite(jc9_values)
jc9_fit_summary[[paste0("Delta_", jc9_criterion)]] <- NA_real_
if (any(jc9_valid)) {
jc9_fit_summary[[paste0("Delta_", jc9_criterion)]][jc9_valid] <-
jc9_values[jc9_valid] - min(jc9_values[jc9_valid])
}
}

# 9.9.4 Export the comparison table as CSV, RDS, and Excel
readr::write_csv(jc9_fit_summary, file.path(output_dir_09,
"20.Step09_JC21_Fit_Comparison.csv"), na = "NA")
saveRDS(jc9_fit_summary, file.path(output_dir_09,
"21.Step09_JC21_Fit_Comparison.rds"))
writexl::write_xlsx(as.data.frame(jc9_fit_summary),
                    file.path(output_dir_09,
                    "22.Step09_JC21_Fit_Comparison.xlsx"))

# 9.9.5 Display and archive the same table without rounding the saved data
utils::capture.output(print(jc9_fit_summary, width = Inf, n = Inf),
file = file.path(output_dir_09, "23.Step09_JC21_Fit_Comparison.txt"),
                 split = TRUE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.10 Export model parameters and pruned edges
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.10.1 Print native parameter estimates and export their numerical values
jc9_parameters <- list()
for (jc9_id in names(jc9_models)) {
jc9_model <- jc9_models[[jc9_id]]
jc9_number <- 22L + 2L * match(jc9_id, names(jc9_models))
jc9_standardize <- isTRUE(jc9_checks$Admissible[
match(jc9_id, jc9_checks$model)])
utils::capture.output({
print(jc9_model)
jc9_parameters[[jc9_id]] <- psychonetrics::parameters(jc9_model,
standardized = jc9_standardize)
}, file = file.path(output_dir_09,
paste0(jc9_number, ".Step09_JC21_", jc9_id,
"_Parameters_Report.txt")), split = TRUE)
readr::write_csv(jc9_parameters[[jc9_id]],
file.path(output_dir_09,
paste0(jc9_number + 1L, ".Step09_JC21_", jc9_id,
"_Parameters.csv")), na = "NA")
}

# 9.10.2 Preserve all 21 candidate edges, including paths fixed to zero
jc9_pruned_parameters <- jc9_parameters[["LNM_Pruned"]]
jc9_pruned_edges <- subset(jc9_pruned_parameters, 
                           matrix == "omega_zeta" & row > col,
                           select = c(var1, var2, fixed, est, se, p))
readr::write_csv(jc9_pruned_edges, 
                 file.path(output_dir_09,"38.Step09_JC21_LNM_Pruned_Edges.csv"), 
                 na = "NA")

# 9.10.3 Retain the CFA and pruned-network fit tables
jc9_cfa_fit <- jc9_fit_list[["M1"]]
jc9_lnm_pruned_fit <- jc9_fit_list[["LNM_Pruned"]]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.11 Prepare the CFA diagrams and pruned-network layout
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.11.1 Define the item order for display without changing estimation input
jc9_plot_items <- stats::setNames(rep(list(jc9_items), 6), paste0("M", 1:6))
jc9_plot_items$M3 <- c(jc9_items[1:9], 
                       jc9_items[19:21], 
                       jc9_items[10:18])
jc9_plot_factors <- list(M1 = jc9_factors, 
                         M2 = "GeneralJC",
                         M3 = colnames(jc9_lambda3), 
                         M4 = colnames(jc9_lambda4),
                         M5 = jc9_factors5, 
                         M6 = colnames(jc9_lambda6))

# 9.11.2 Define the same item and factor coordinates used in Step 8
jc9_item_coordinates <- cbind(x = 1, y = 10:-10)
jc9_layouts <- list(M1 = rbind(jc9_item_coordinates, 
                               cbind(0, seq(9, -9, by = -3))),
                    M2 = rbind(jc9_item_coordinates, c(0, 0)),
                    M3 = rbind(jc9_item_coordinates, 
                               cbind(0, c(9, 6, 1.5, -3, -6, -9))),
                    M4 = rbind(jc9_item_coordinates, 
                               cbind(0, c(9, 6, 3, -1.5, -6, -9))),
                    M5 = rbind(jc9_item_coordinates, 
                               cbind(0, seq(9, -9, by = -3)), c(-1, 0)),
                    M6 = rbind(cbind(0, 10:-10), c(-1, 0), 
                               cbind(1, seq(9, -9, by = -3))))

# 9.11.3 Define the CFA plotting function
jc9_draw_model <- function(id, first_file) {
model <- jc9_models[[id]]
prefix <- paste0(".Step09_JC21_", id)
items <- jc9_plot_items[[id]]
factors <- jc9_plot_factors[[id]]
sem_model <- semPlot::semPlotModel(model, delta = "ignore")

# 9.11.3.1 Orient latent pairs consistently before semPaths duplicates curves
undirected <- which(sem_model@Pars$edge %in% c("<->", "--") &
sem_model@Pars$lhs %in% factors & sem_model@Pars$rhs %in% factors &
sem_model@Pars$lhs != sem_model@Pars$rhs)
swap <- undirected[
match(sem_model@Pars$lhs[undirected], factors) >
match(sem_model@Pars$rhs[undirected], factors)]
sem_model@Pars[swap, c("lhs", "rhs")] <-
sem_model@Pars[swap, c("rhs", "lhs")]

# 9.11.3.2 Define the item and factor labels
labels <- c(items, factors)
labels[labels == "GeneralJC"] <- "General\nJC"
labels[labels == "ISOJR_RC"] <- "ISOJR\nRC"
labels[labels == "ICJD_TC"] <- "ICJD\nTC"
curved <- id %in% c("M1", "M3", "M4")

# 9.11.3.3 Select the available coefficient scale
shown <- sem_model@Pars$edge != "int" &
sem_model@Pars$lhs != sem_model@Pars$rhs
standardized <- all(is.finite(sem_model@Pars$std[shown]))
metric <- if (standardized) "std" else "est"

# 9.11.3.4 Draw uniform blue paths and retain the signed coefficient labels
graph <- semPlot::semPaths(object = sem_model,
                            what = "path",
                            whatLabels = metric,
                            manifests = items,
                            latents = factors,
                            layout = jc9_layouts[[id]],
                            nodeLabels = labels,
                            style = "lisrel",
                            shapeMan = "rectangle",
                            shapeLat = "circle",
                            node.width = c(rep(8.0, 21),
                                           rep(5.0, length(factors))),
                            node.height = c(rep(2.2, 21),
                                            rep(5.0, length(factors))),
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
                            layoutScale = if (curved) c(0.7, 1) else c(1, 1),
                            layoutOffset = if (curved) c(0.3, 0) else c(0, 0))

# 9.11.3.5 Keep loading paths straight and honor the bifactor's reversed side
edge_from <- graph$Edgelist$from
edge_to <- graph$Edgelist$to
loading_edges <- which(edge_from > 21 & edge_to <= 21)
factor_x <- jc9_layouts[[id]][edge_from[loading_edges], 1]
item_x <- jc9_layouts[[id]][edge_to[loading_edges], 1]
graph$graphAttributes$Edges$edgeConnectPoints[loading_edges, 1] <-
ifelse(factor_x < item_x, 0.5 * pi, 1.5 * pi)
graph$graphAttributes$Edges$edgeConnectPoints[loading_edges, 2] <-
ifelse(factor_x < item_x, 1.5 * pi, 0.5 * pi)

# 9.11.3.6 Retain directed higher-order paths from GeneralJC to each dimension
if (id == "M5") {
higher_edges <- which(edge_from > 21 & edge_to > 21 & edge_from != edge_to)
graph$graphAttributes$Edges$edgeConnectPoints[higher_edges, 1] <- 0.5 * pi
graph$graphAttributes$Edges$edgeConnectPoints[higher_edges, 2] <- 1.5 * pi
}

# 9.11.3.7 Route the latent correlations to the left
if (curved) {
latent_edges <- which(edge_from > 21 & edge_to > edge_from)
routes <- data.frame(edge = latent_edges,
from = edge_from[latent_edges] - 21L, to = edge_to[latent_edges] - 21L)
routes$gap <- routes$to - routes$from
routes <- routes[order(routes$gap, -routes$to, -routes$from), ]
adjacent <- which(routes$gap == 1L)
outer <- which(routes$gap > 1L)
routes$curve <- rep(-0.22, nrow(routes))
routes$label_position <- rep(0.5, nrow(routes))
routes$curve[outer] <- -seq(0.6, 6.3, length.out = length(outer))
routes$label_position[outer] <- 0.68
routes$label_position[routes$from == length(factors) - 2L &
routes$to == length(factors)] <- 0.60
graph$plotOptions$curveScale <- FALSE
graph$graphAttributes$Edges$curve[routes$edge] <- routes$curve
graph$graphAttributes$Edges$edge.label.position[routes$edge] <-
routes$label_position
graph$graphAttributes$Edges$edgeConnectPoints[routes$edge[adjacent], 1] <-
190 * pi / 180
graph$graphAttributes$Edges$edgeConnectPoints[routes$edge[adjacent], 2] <-
350 * pi / 180

# 9.11.3.8 Separate outer connections along the left border of each factor
for (factor in seq_along(factors)) {
lower <- outer[routes$from[outer] == factor]
lower <- lower[order(-routes$to[lower])]
upper <- outer[routes$to[outer] == factor]
upper <- upper[order(-routes$from[upper])]
graph$graphAttributes$Edges$edgeConnectPoints[routes$edge[lower], 1] <-
seq(265, 220, length.out = length(lower)) * pi / 180
graph$graphAttributes$Edges$edgeConnectPoints[routes$edge[upper], 2] <-
seq(320, 275, length.out = length(upper)) * pi / 180
}
}

# 9.11.3.9 Export one PDF and one 300-dpi PNG per model
for (extension in c("pdf", "png")) {
number <- first_file + match(extension, c("pdf", "png")) - 1L
filename <- file.path(output_dir_09,
paste0(number, prefix, "_Path_Diagram.", extension))
if (extension == "pdf") {
grDevices::cairo_pdf(filename, width = 180 / 25.4,
height = 220 / 25.4, family = "sans")
} else {
grDevices::png(filename, width = 180 / 25.4,
height = 220 / 25.4, units = "in", res = 300, bg = "white")
}
tryCatch(plot(graph, GLratio = 2), finally = grDevices::dev.off())
}

# 9.11.3.10 Archive the plotted parameters and graph
readr::write_csv(sem_model@Pars, file.path(output_dir_09,
paste0(first_file + 2L, prefix, "_Diagram_Parameters.csv")),
na = "NA")
saveRDS(graph, file.path(output_dir_09,
paste0(first_file + 3L, prefix, "_Path_Diagram.rds")))
if (interactive()) plot(graph, GLratio = 2)
invisible(graph)
}

# 9.11.4 Extract the pruned latent partial correlations
jc9_omega <- psychonetrics::getmatrix(jc9_lnm.fit, "omega_zeta",
                                      threshold = FALSE)
dimnames(jc9_omega) <- list(jc9_factors, jc9_factors)

# 9.11.5 Compute and archive the spring layout
jc9_network_layout <- qgraph::qgraph(jc9_omega, layout = "spring",
                                     DoNotPlot = TRUE)$layout
dimnames(jc9_network_layout) <- list(jc9_factors, c("x", "y"))
saveRDS(jc9_network_layout, file.path(output_dir_09,
"39.Step09_JC21_LNM_Pruned_Layout.rds"))
readr::write_csv(data.frame(factor = jc9_factors, jc9_network_layout),
file.path(output_dir_09, "40.Step09_JC21_LNM_Pruned_Layout.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.12 Export the six CFA figures
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.12.1 Draw the six CFA models in comparison-table order
Plot_jc9_M1 <- jc9_draw_model("M1", first_file = 41L)
Plot_jc9_M2 <- jc9_draw_model("M2", first_file = 45L)
Plot_jc9_M3 <- jc9_draw_model("M3", first_file = 49L)
Plot_jc9_M4 <- jc9_draw_model("M4", first_file = 53L)
Plot_jc9_M5 <- jc9_draw_model("M5", first_file = 57L)
Plot_jc9_M6 <- jc9_draw_model("M6", first_file = 61L)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.13 Plot the pruned latent network
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 9.13.1 Position the edge labels
jc9_label_positions <- matrix(0.5, nrow = 7, ncol = 7,
dimnames = list(jc9_factors, jc9_factors))
jc9_label_positions["ISOJR", "CC"] <-
jc9_label_positions["CC", "ISOJR"] <- 0.35
jc9_label_positions["ISJR", "CC"] <-
jc9_label_positions["CC", "ISJR"] <- 0.55

# 9.13.2 Draw the seven latent dimensions and their partial correlations
Plot_jc9_LNM_Pruned <- qgraph::qgraph(jc9_omega,
                                     layout = jc9_network_layout,
                                     theme = "Borkulo",
                                     labels = jc9_factors,
                                     color = "white",
                                     border.color = "darkblue",
                                     edge.color = "darkblue",
                                     directed = FALSE,
                                     weighted = TRUE,
                                     diag = FALSE,
                                     minimum = 0,
                                     threshold = 0,
                                     maximum = 1,
                                     cut = 0,
                                     vsize = 8,
                                     label.cex = 1,
                                     label.scale = FALSE,
                                     esize = 8,
                                     edge.labels = TRUE,
                                     edge.label.cex = 0.7,
                                     edge.label.bg = "white",
                                     edge.label.color = "black",
                                     edge.label.position = jc9_label_positions,
                                     fade = FALSE,
                                     legend = FALSE,
                                     mar = c(4, 2, 4, 2),
                                     DoNotPlot = TRUE)

# 9.13.3 Export the network as PDF and PNG
for (extension in c("pdf", "png")) {
number <- if (extension == "pdf") 65L else 66L
filename <- file.path(output_dir_09,
paste0(number, ".Step09_JC21_LNM_Pruned_Path_Diagram.", extension))
if (extension == "pdf") {
grDevices::cairo_pdf(filename, width = 180 / 25.4,
height = 180 / 25.4, family = "sans")
} else {
grDevices::png(filename, width = 180 / 25.4,
height = 180 / 25.4, units = "in", res = 300, bg = "white")
}
tryCatch(plot(Plot_jc9_LNM_Pruned), finally = grDevices::dev.off())
}
if (interactive()) plot(Plot_jc9_LNM_Pruned)

# 9.13.4 Archive the matrix, candidate edges, and plotted graph
saveRDS(jc9_omega, file.path(output_dir_09,"67.Step09_JC21_LNM_Pruned_Omega.rds"))
readr::write_csv(data.frame(factor = jc9_factors, jc9_omega),
file.path(output_dir_09, "68.Step09_JC21_LNM_Pruned_Omega.csv"), na = "NA")
jc9_pairs <- which(upper.tri(jc9_omega), arr.ind = TRUE)
jc9_edges <- data.frame(from = jc9_factors[jc9_pairs[, 1]],
to = jc9_factors[jc9_pairs[, 2]], partial_correlation = jc9_omega[jc9_pairs],
displayed = jc9_omega[jc9_pairs] != 0)
readr::write_csv(jc9_edges, file.path(output_dir_09,
"69.Step09_JC21_LNM_Pruned_Diagram_Parameters.csv"), na = "NA")
saveRDS(Plot_jc9_LNM_Pruned, file.path(output_dir_09,
"70.Step09_JC21_LNM_Pruned_Path_Diagram.rds"))
