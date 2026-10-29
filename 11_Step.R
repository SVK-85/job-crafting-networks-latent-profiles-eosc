#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                                  ┏┓┏┳┓┏┓┏┓  ┓┓                               #
#                                  ┗┓ ┃ ┣ ┃┃  ┃┃                               #
#                                  ┗┛ ┻ ┗┛┣┛  ┻┻                               #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                Zero-Order Pearson Correlations Between Scores                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 11.1 Calculate Pearson correlations using psych
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
jc12_correlations <- psych::corr.test(jc11_scores,
                                      use = "pairwise",
                                      method = "pearson",
                                      adjust = "none",
                                      alpha = 0.05,
                                      ci = TRUE,
                                      minlength = 20)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 11.2 Extract one row per variable pair and export the CSV
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
jc12_table <- tibble::rownames_to_column(jc12_correlations$ci, var = "pair")
# Count available observations for each pair
jc12_n <- psych::pairwiseCount(jc11_scores)
# Add pairwise sample sizes to the table
jc12_table$n <- jc12_n[lower.tri(jc12_correlations$r)]
# Export the correlation table as CSV
readr::write_csv(jc12_table, file.path(output_dir_11,
"1.Step11_Zero_Order_Pearson_Correlations.csv"), na = "NA")
print(jc12_table)
