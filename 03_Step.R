#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏┓                                   #
#                              ┗┓ ┃ ┣ ┃┃   ┫                                   #
#                              ┗┛ ┻ ┗┛┣┛  ┗┛                                   #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Data Cleaning, Missingness, and Descriptive Statistics                       #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 3.1 Replace values outside the registered response categories with NA
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 3.1.1 Reload the Step 02 inputs to make cleaning counts reproducible
data <- readRDS(file.path(output_dir_02, "2.Step02_Analysis_Dataset.rds"))
# 3.1.1.1 Restore the variable registry and its valid response categories
variable_registry <- readRDS(file.path(output_dir_02,
"6.Step02_Variable_Registry.rds"))$variable_registry

# 3.1.2 Record missingness before validating registered response categories
missing_before <- misty::na.descript(data,
output = FALSE)$result$L1$table.miss.l1

# 3.1.2.1 Replace invalid responses with NA and assemble the cleaning audit
cleaning_report <- list()
for (block in variable_registry) {
# 3.1.2.2 Retrieve valid categories, when defined.
allowed <- block$allowed_values %||% unname(block$value_labels)
for (variable in unname(block$variable_labels)) {
# 3.1.2.3 Extract the current variable values.
values <- data[[variable]]
# 3.1.2.4 Initialize the invalid-response flag.
invalid <- rep(FALSE, length(values))
if (!is.null(allowed)) {
# 3.1.2.5 Flag observed values outside the registered categories.
invalid <- !is.na(values) & !(values %in% allowed)
}
# 3.1.2.6 Recode invalid responses as NA.
values[invalid] <- NA
# 3.1.2.7 Store the cleaned variable.
data[[variable]] <- values
# 3.1.2.8 Record counts for the variable-level audit.
cleaning_report[[variable]] <- data.frame(
variable = variable, n_total = length(values),
categories_checked = !is.null(allowed),
n_missing_before = missing_before$nNA[
match(variable, missing_before$Var)],
n_invalid_to_na = sum(invalid))
}
}
# 3.1.2.9 Combine all variable-level audits.
cleaning_report <- do.call(rbind, cleaning_report)
rownames(cleaning_report) <- NULL

# 3.1.3 Recalculate variable-level missingness after category validation
missing_after <- misty::na.descript(data, 
output = FALSE)$result$L1$table.miss.l1
# 3.1.3.1 Match variables in the post-cleaning missingness table
missing_index <- match(cleaning_report$variable, missing_after$Var)
# 3.1.3.2 Append post-cleaning missing counts
cleaning_report$n_missing_after <- missing_after$nNA[missing_index]
# 3.1.3.3 Append post-cleaning missing percentages
cleaning_report$missing_percent <- missing_after$pNA[missing_index]
# 3.1.3.4 Export the cleaning and missingness audit as CSV
readr::write_csv(cleaning_report, file.path(output_dir_03,
"1.Step03_Cleaning_and_Missing.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 3.2 Archive the cleaned dataset for subsequent analyses
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 3.2.1 Save the cleaned dataset as RDS with names and row order unchanged
saveRDS(data, file.path(output_dir_03, "2.Step03_Analysis_Dataset.rds"))
# 3.2.2 Export the cleaned dataset as CSV with explicit NA values
readr::write_csv(data, file.path(output_dir_03,
"3.Step03_Analysis_Dataset.csv"), na = "NA")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 3.3 Describe each instrument's items and participant age
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 3.3.1 Select the registered item sets
descriptive_sets <- list( 
JCS = unname(variable_registry[["VAR4"]]$variable_labels),
JCQ = unname(variable_registry[["VAR5"]]$variable_labels),
WAMI = unname(variable_registry[["VAR6"]]$variable_labels),
UWES = unname(variable_registry[["VAR7"]]$variable_labels),
Age = "Age")

# 3.3.2 Export one numeric descriptive table per instrument and one for Age
descriptive_participants <- list()
for (group in names(descriptive_sets)) {
values <- as.data.frame(data[, descriptive_sets[[group]], drop = FALSE])
# 3.3.3 Select Age explicitly because misty 0.8.3 fails on one-column data frames.
missing <- if (group == "Age") {
misty::na.descript(values, Age, output = FALSE)$result$L1
} else {
misty::na.descript(values, output = FALSE)$result$L1
}
# 3.3.4 Compute variable-level descriptive statistics.
descriptives <- as.data.frame(psych::describe(values, na.rm = TRUE))
# 3.3.4.1 Move variable names from row names into a column.
descriptives <- data.frame(variable = rownames(descriptives),
descriptives, row.names = NULL)
# 3.3.4.2 Remove the positional variable index.
descriptives$vars <- NULL
# 3.3.4.3 Match variables to the missingness table.
missing_index <- match(descriptives$variable, missing$table.miss.l1$Var)
# 3.3.4.4 Add missing-response counts.
descriptives$n_missing <- missing$table.miss.l1$nNA[missing_index]
# 3.3.4.5 Add missing-response percentages.
descriptives$missing_percent <- missing$table.miss.l1$pNA[missing_index]
# 3.3.4.6 Export the descriptive table for this group.
readr::write_csv(descriptives, file.path(output_dir_03,
paste0(match(group, names(descriptive_sets)) + 3L,
".Step03_", group, "_Descriptives.csv")), na = "NA")
# 3.3.4.7 Record completeness within each descriptive set.
descriptive_participants[[group]] <- data.frame(
# 3.3.4.8 Record group name and number of variables.
group = group, n_variables = missing$no.var.l1,
# 3.3.4.9 Record total and complete cases.
n_total = missing$no.cases.l1, n_complete = missing$no.complete.l1,
# 3.3.4.10 Record cases with missing values.
n_with_missing = missing$no.incomplete.l1,
# 3.3.4.11 Count cases with all variables missing.
n_all_missing = sum(rowSums(!is.na(values)) == 0),
# 3.3.4.12 Record the total number of missing cells.
n_missing_cells = missing$no.missing.values.l1,
# 3.3.4.13 Record the overall missingness percentage.
missing_percent = missing$perc.missing.values.l1)
}

# 3.3.5 Save the completeness summary for manuscript reporting
# 3.3.5.1 Combine the group-level summaries
descriptive_participants <- do.call(rbind, descriptive_participants)
# 3.3.5.2 Remove inherited row names
rownames(descriptive_participants) <- NULL
# 3.3.5.3 Export the completeness summary
readr::write_csv(descriptive_participants, file.path(output_dir_03,
"9.Step03_Descriptive_Participants.csv"), na = "NA")
# 3.3.5.4 Display the completeness summary
print(descriptive_participants, row.names = FALSE)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 3.4 Describe gender and educational level using category frequencies
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 3.4.1 Count cleaned demographic responses using registered labels
# 3.4.1.1 Initialize the demographic frequency tables
sociodemographic_frequencies <- list()
# 3.4.1.2 Process gender and educational level
for (block in variable_registry[c("VAR2", "VAR3")]) {
# 3.4.1.3 Retrieve the variable name and cleaned responses
variable <- unname(block$variable_labels)
values <- data[[variable]]
# 3.4.1.4 Retrieve all registered response codes
codes <- unname(block$value_labels)
# 3.4.1.5 Count registered categories and missing values
counts <- misty::freq(factor(values, levels = codes), output = FALSE)$result
# 3.4.1.6 Assemble labels, counts, and percentages
frequencies <- data.frame(variable = variable, code = c(codes, NA_real_),
category = c(names(block$value_labels), "Missing"), n = counts$Freq,
percent_total = counts$Perc, percent_valid = counts$V.Perc)
# 3.4.1.7 Store the variable-level frequency table
sociodemographic_frequencies[[variable]] <- frequencies
}
# 3.4.2 Export one long-format demographic table for reporting
# 3.4.2.1 Combine the variable-level frequency tables
sociodemographic_frequencies <- do.call(rbind, sociodemographic_frequencies)
# 3.4.2.2 Remove inherited row names
rownames(sociodemographic_frequencies) <- NULL
# 3.4.2.3 Export the combined demographic table
readr::write_csv(sociodemographic_frequencies, file.path(output_dir_03,
"10.Step03_Sociodemographic_Frequencies.csv"), na = "NA")
