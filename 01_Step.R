#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#                              ┏┓┏┳┓┏┓┏┓  ┏                                    #
#                              ┗┓ ┃ ┣ ┃┃  ┃                                    #
#                              ┗┛ ┻ ┗┛┣┛  ┻                                    #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 1.1 Download and import the original dataset
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 1.1.1 Define the Figshare file identifier
figshare_file_id <- "46740349"
# 1.1.1 Construct the dataset download URL
data_url <- paste0("https://ndownloader.figshare.com/files/", figshare_file_id)
# 1.1.1 Define the local path for the original SPSS dataset
source_data_file <- file.path(output_dir_01, "1. BaseDatos814.sav")
# Reference of the dataset:
# Letona-Ibañez, O., Martinez-Rodriguez, S., Ortiz-Marques, N.,
#   Carrasco, M., & Amillano, A. (2021). Job crafting and work engagement:
#   The mediating role of work meaning. International Journal of
#   Environmental Research and Public Health, 18(10), Article 5383.
#   https://doi.org/10.3390/ijerph18105383

# 1.1.2 Download once and import the archived SPSS file
if (!file.exists(source_data_file)) {
# 1.1.1 Download the original SPSS file in binary mode
utils::download.file(data_url, source_data_file, mode = "wb")
}
# 1.1.1 Import the archived SPSS file while preserving variable and value labels
raw_imported_data <- haven::read_sav(source_data_file)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 1.2 Archive the imported data and its provenance
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

# 1.2.1 Save the imported dataset with its original SPSS attributes
saveRDS(raw_imported_data, file.path(output_dir_01,
        "2. Step01_Imported_Data_With_SPSS_Labels.rds"))

# 1.2.2 Export the original values and source column names
# 1.2.2.1 .csv uses NA for missing values 
readr::write_csv(raw_imported_data, file.path(output_dir_01,
                 "3. Step01_Original_Data.csv"), na = "NA")
# 1.2.2.2 .xlxs uses empty cells
writexl::write_xlsx(list(Data = raw_imported_data), 
                    file.path(output_dir_01,
                    "4. Step01_Original_Data.xlsx"))

# 1.2.3 Record source identity and dataset dimensions
source_provenance <- data.frame(figshare_file_id = figshare_file_id,
                                source_url = data_url, 
                                source_file = basename(source_data_file),
                                md5 = unname(tools::md5sum(source_data_file)), 
                                imported_utc = format(Sys.time(),
                                                      "%Y-%m-%dT%H:%M:%SZ", 
                                                      tz = "UTC"), 
                                participants = nrow(raw_imported_data),
                                variables = ncol(raw_imported_data))
# 1.2.3.1 Save source provenance and dataset dimensions as a CSV file
readr::write_csv(source_provenance, file.path(output_dir_01,
                 "5. Step01_Source_Provenance.csv"))
# 1.2.3.2 Display the source provenance report in the console
print(source_provenance, row.names = FALSE)
