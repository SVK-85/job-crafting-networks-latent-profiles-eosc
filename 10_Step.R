#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                  ┏┓┏┳┓┏┓┏┓  ┓┏┓                              #
#                                  ┗┓ ┃ ┣ ┃┃  ┃┃┫                              #
#                                  ┗┛ ┻ ┗┛┣┛  ┻┗┛                              #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#            Scale Scores, Reliability, and Descriptive Statistics             #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.0 Retain the full cleaned dataset
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
data_scored <- data 
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.1 Define the items used for the nine observed scores
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 10.1.1 JCQ-9
jc11_tc_items <- c("jcq_02", "jcq_03", "jcq_05")
jc11_cc_items <- c("jcq_06", "jcq_09", "jcq_10")
jc11_rc_items <- c("jcq_11", "jcq_13", "jcq_15")

# 10.1.2 JCS-12
jc11_isjr_items <- c("jcs_01", "jcs_02", "jcs_03")
jc11_dhjd_items <- c("jcs_06", "jcs_07", "jcs_08")
jc11_isojr_items <- c("jcs_12", "jcs_13", "jcs_14")
jc11_icjd_items <- c("jcs_17", "jcs_18", "jcs_19")

# 10.1.3 WAMI-10 and UWES-3 
jc11_wami_items <- sprintf("wami_%02d", 1:10)
jc11_uwes_items <- c("uwes_01", "uwes_03", "uwes_08")

# 10.1.4 Reverse WAMI item 3 on a separate scoring copy
jc11_wami_scoring <- as.data.frame(data[, jc11_wami_items, drop = FALSE])
jc11_wami_scoring$wami_03 <- psych::reverse.code(keys = -1, 
                                                 items = jc11_wami_scoring["wami_03"],
                                                 mini = 1, 
                                                 maxi = 5)[, 1]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.2 Calculate complete-response sum scores
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 10.2.1 Sum the three items of each JCQ-9 dimension
data_scored$JCQ_TC <- rowSums(data[, jc11_tc_items], na.rm = FALSE)
data_scored$JCQ_CC <- rowSums(data[, jc11_cc_items], na.rm = FALSE)
data_scored$JCQ_RC <- rowSums(data[, jc11_rc_items], na.rm = FALSE)

# 10.2.2 Sum the three items of each JCS-12 dimension
data_scored$JCS_ISJR <- rowSums(data[, jc11_isjr_items], na.rm = FALSE)
data_scored$JCS_DHJD <- rowSums(data[, jc11_dhjd_items], na.rm = FALSE)
data_scored$JCS_ISOJR <- rowSums(data[, jc11_isojr_items], na.rm = FALSE)
data_scored$JCS_ICJD <- rowSums(data[, jc11_icjd_items], na.rm = FALSE)

# 10.2.3 Sum all WAMI items after reverse-keying and the three UWES items
data_scored$WAMI_total <- rowSums(jc11_wami_scoring, na.rm = FALSE)
data_scored$UWES3_total <- rowSums(data[, jc11_uwes_items], na.rm = FALSE)


# 10.2.4 Retain the nine scores
jc11_score_names <- c("JCQ_TC", 
                      "JCQ_CC", 
                      "JCQ_RC",
                      "JCS_ISJR", 
                      "JCS_DHJD", 
                      "JCS_ISOJR", 
                      "JCS_ICJD",
                      "WAMI_total", 
                      "UWES3_total")
jc11_scores <- as.data.frame(data_scored[, jc11_score_names])

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.3 Define complete responses for reliability
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

jc11_tc_data <- stats::na.omit(as.data.frame(data[, jc11_tc_items]))
jc11_cc_data <- stats::na.omit(as.data.frame(data[, jc11_cc_items]))
jc11_rc_data <- stats::na.omit(as.data.frame(data[, jc11_rc_items]))
jc11_isjr_data <- stats::na.omit(as.data.frame(data[, jc11_isjr_items]))
jc11_dhjd_data <- stats::na.omit(as.data.frame(data[, jc11_dhjd_items]))
jc11_isojr_data <- stats::na.omit(as.data.frame(data[, jc11_isojr_items]))
jc11_icjd_data <- stats::na.omit(as.data.frame(data[, jc11_icjd_items]))
jc11_wami_data <- stats::na.omit(jc11_wami_scoring)
jc11_uwes_data <- stats::na.omit(as.data.frame(data[, jc11_uwes_items]))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.4 Calculate Cronbach's alpha using psych
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
jc11_tc_alpha <- psych::alpha(jc11_tc_data,
                              check.keys = FALSE,
                              use = "complete.obs")
jc11_cc_alpha <- psych::alpha(jc11_cc_data,
                              check.keys = FALSE,
                              use = "complete.obs")
jc11_rc_alpha <- psych::alpha(jc11_rc_data,
                              check.keys = FALSE,
                              use = "complete.obs")
jc11_isjr_alpha <- psych::alpha(jc11_isjr_data,
                                check.keys = FALSE,
                                use = "complete.obs")
jc11_dhjd_alpha <- psych::alpha(jc11_dhjd_data,
                                check.keys = FALSE,
                                use = "complete.obs")
jc11_isojr_alpha <- psych::alpha(jc11_isojr_data,
                                 check.keys = FALSE,
                                 use = "complete.obs")
jc11_icjd_alpha <- psych::alpha(jc11_icjd_data,
                                check.keys = FALSE,
                                use = "complete.obs")
jc11_wami_alpha <- psych::alpha(jc11_wami_data,
                                check.keys = FALSE,
                                use = "complete.obs")
jc11_uwes_alpha <- psych::alpha(jc11_uwes_data,
                                check.keys = FALSE,
                                use = "complete.obs")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.5 Calculate McDonald's omega total using psych
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
jc11_tc_omega <- psych::omega(jc11_tc_data,
                              nfactors = 1,
                              fm = "ml",
                              covar = TRUE,
                              flip = FALSE,
                              plot = FALSE)
jc11_cc_omega <- psych::omega(jc11_cc_data,
                              nfactors = 1,
                              fm = "ml",
                              covar = TRUE,
                              flip = FALSE,
                              plot = FALSE)
jc11_rc_omega <- psych::omega(jc11_rc_data,
                              nfactors = 1,
                              fm = "ml",
                              covar = TRUE,
                              flip = FALSE,
                              plot = FALSE)
jc11_isjr_omega <- psych::omega(jc11_isjr_data,
                                nfactors = 1,
                                fm = "ml",
                                covar = TRUE,
                                flip = FALSE,
                                plot = FALSE)
jc11_dhjd_omega <- psych::omega(jc11_dhjd_data,
                                nfactors = 1,
                                fm = "ml",
                                covar = TRUE,
                                flip = FALSE,
                                plot = FALSE)
jc11_isojr_omega <- psych::omega(jc11_isojr_data,
                                 nfactors = 1,
                                 fm = "ml",
                                 covar = TRUE,
                                 flip = FALSE,
                                 plot = FALSE)
jc11_icjd_omega <- psych::omega(jc11_icjd_data,
                                nfactors = 1,
                                fm = "ml",
                                covar = TRUE,
                                flip = FALSE,
                                plot = FALSE)
jc11_wami_omega <- psych::omega(jc11_wami_data,
                                nfactors = 3,
                                fm = "ml",
                                covar = TRUE,
                                flip = FALSE,
                                plot = FALSE)
jc11_uwes_omega <- psych::omega(jc11_uwes_data,
                                nfactors = 1,
                                fm = "ml",
                                covar = TRUE,
                                flip = FALSE,
                                plot = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.6 Assemble the score-level descriptive and reliability report
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 10.6.1 Describe the calculated sums using adjusted skew and excess kurtosis
jc11_descriptives <- psych::describe(jc11_scores, na.rm = TRUE, type = 2)

# 10.6.2 Collect the two reliability coefficients in the same score order
jc11_alpha_values <- c(jc11_tc_alpha$total$raw_alpha,
                       jc11_cc_alpha$total$raw_alpha,
                       jc11_rc_alpha$total$raw_alpha,
                       jc11_isjr_alpha$total$raw_alpha,
                       jc11_dhjd_alpha$total$raw_alpha,
                       jc11_isojr_alpha$total$raw_alpha,
                       jc11_icjd_alpha$total$raw_alpha,
                       jc11_wami_alpha$total$raw_alpha,
                       jc11_uwes_alpha$total$raw_alpha)
jc11_omega_values <- c(jc11_tc_omega$omega.tot,
                       jc11_cc_omega$omega.tot,
                       jc11_rc_omega$omega.tot,
                       jc11_isjr_omega$omega.tot,
                       jc11_dhjd_omega$omega.tot,
                       jc11_isojr_omega$omega.tot,
                       jc11_icjd_omega$omega.tot,
                       jc11_wami_omega$omega.tot,
                       jc11_uwes_omega$omega.tot)

# 10.6.3 Extract missing-data statistics and create one report row per score
jc11_missing <- misty::na.descript(jc11_scores, table = TRUE,
                                   output = FALSE)$result$L1
jc11_missing_table <- jc11_missing$table.miss.l1
jc11_report <- data.frame(score = jc11_score_names,
                          instrument = c(rep("JCQ-9", 3), rep("JCS-12", 4),
                                         "WAMI-10", "UWES-3"),
                          n_items = c(rep(3L, 7), 10L, 3L),
                          N_total = jc11_missing$no.cases.l1,
                          N_valid = jc11_descriptives$n, 
                          N_missing = jc11_missing_table$nNA,
                          missing_percent = jc11_missing_table$pNA, 
                          mean = jc11_descriptives$mean,
                          sd = jc11_descriptives$sd, 
                          min = jc11_descriptives$min,
                          max = jc11_descriptives$max, 
                          skewness = jc11_descriptives$skew,
                          kurtosis = jc11_descriptives$kurtosis,
                          cronbach_alpha_raw = jc11_alpha_values,
                          mcdonald_omega_total = jc11_omega_values,
                          omega_factors = c(rep(1L, 7), 3L, 1L),
                          row.names = NULL)
print(jc11_report, row.names = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10.7 Export the extended dataset and the report for Quarto
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
readr::write_csv(data_scored, file.path(output_dir_10,
"1.Step10_Analysis_Dataset_With_Scores.csv"), na = "NA")

readr::write_csv(jc11_report, file.path(output_dir_10,
"2.Step10_Score_Descriptives_and_Reliability.csv"), na = "NA")
