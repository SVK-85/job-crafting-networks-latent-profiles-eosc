#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏┓                                   #
#                              ┗┓ ┃ ┣ ┃┃  ┃┃                                   #
#                              ┗┛ ┻ ┗┛┣┛  ┗╋                                   #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Missing Data Diagnostics: Little's MCAR Tests                                #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 4.1 Run Little's MCAR test on each instrument's full item set
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 4.1.1 Select the full instrument item sets and run Little's MCAR tests
mcar_item_sets <- descriptive_sets[c("JCS", "JCQ", "WAMI", "UWES")]
# 4.1.1.1 Process each instrument
mcar_results <- lapply(mcar_item_sets, function(items) {
# 4.1.1.2 Extract item responses
block_data <- as.data.frame(data[, items, drop = FALSE])
# 4.1.1.3 Run Little's MCAR test
misty::na.test(block_data, print = "little", digits = 3, p.digits = 4,
               output = FALSE)
})

# 4.1.2 Combine the native test statistics into one row per instrument
mcar_summary <- do.call(rbind, lapply(names(mcar_results), function(name) {
                        data.frame(instrument = name, 
                                   mcar_results[[name]]$result$little,
                                   row.names = NULL)
}))

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 4.2 Save the results and display the combined test summary
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 4.2.1 Capture the combined summary without displaying positional row names
utils::capture.output(print(mcar_summary, 
                            row.names = FALSE), 
                            file = file.path(output_dir_04,
# 4.2.1.1 Save the TXT report in Step 04 and mirror its output in the console
              "1.Step04_Full_Instrument_Little_MCAR_Report.txt"), split = TRUE)

# 4.2.2 Export the numerical test summary as a CSV table for Quarto
readr::write_csv(mcar_summary, file.path(output_dir_04,
                "2.Step04_Full_Instrument_Little_MCAR_Summary.csv"), na = "NA")

# 4.2.3 Archive the item sets, native test objects, and combined summary
saveRDS(list(item_sets = mcar_item_sets, 
             results = mcar_results,
             summary = mcar_summary), 
        file.path(output_dir_04, "3.Step04_Full_Instrument_Little_MCAR.rds"))
