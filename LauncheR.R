###############################################################################
#####.██╗......█████╗.██╗...██╗███╗...██╗.██████╗██╗..██╗███████╗██████╗..#####
#####.██║.....██╔══██╗██║...██║████╗..██║██╔════╝██║..██║██╔════╝██╔══██╗.#####
#####.██║.....███████║██║...██║██╔██╗.██║██║.....███████║█████╗..██████╔╝.#####
#####.██║.....██╔══██║██║...██║██║╚██╗██║██║.....██╔══██║██╔══╝..██╔══██╗.#####
#####.███████╗██║..██║╚██████╔╝██║.╚████║╚██████╗██║..██║███████╗██║..██║.#####
#####.╚══════╝╚═╝..╚═╝.╚═════╝.╚═╝..╚═══╝ ╚═════╝╚═╝..╚═╝╚══════╝╚═╝..╚═╝.#####
###############################################################################
####                  Full replication launcher for:                       ####
#####     "Psychometric Networks and Latent Profiles of Job Crafting:     #####
######          A Reproducible Data-to-Publication Workflow Using        ######
#######             the European Open Science Cloud EU Node"            #######
###############################################################################
#########                     Andrea Svicher, PhD                     #########
###############################################################################
###########            info at: andrea.svicher@unifi.it             ###########
###############################################################################
############           OSF: https://osf.io/b4hwr/overview          ############
#############        Registration DOI: 10.17605/OSF.IO/B4HWR      #############
###############################################################################
# GitHub: https://github.com/SVK-85/job-crafting-networks-latent-profiles-eosc
jc_eosc_launcher_main <- function() {
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 1. Interpret the supported command-line arguments
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
args <- as.character(getOption("job-crafting-eosc.launcher.args",
commandArgs(trailingOnly = TRUE)))
# 1.2 Detect the help request
help <- "--help" %in% args
# 1.3 Detect the script listing request
show_list <- "--list" %in% args
# 1.4 Detect the syntax check request
check_only <- "--check" %in% args
# 1.5 Detect pauses between executed steps
pause <- "--pause" %in% args
# 1.6 Detect explicit GitHub mode
github_mode <- "--github" %in% args
# 1.7 Initialize automatic GitHub mode
github_mode_automatic <- FALSE
# 1.8 Read the final step argument
through <- grep("^--through=[0-9]{1,2}$", args, value = TRUE)
# 1.9 Read the repository argument
repository_arg <- grep("^--repo=.+$", args, value = TRUE)
# 1.10 Read the repository reference
ref_arg <- grep("^--ref=.+$", args, value = TRUE)
# 1.11 Validate supported command line arguments
known <- args %in% c("--help", "--list", "--check", "--pause",
"--github") |
grepl("^--through=[0-9]{1,2}$", args) |
grepl("^--repo=.+$", args) |
grepl("^--ref=.+$", args)
if (length(through) > 1L || length(repository_arg) > 1L ||
length(ref_arg) > 1L || any(!known)) {
stop("Invalid arguments. Use --help for usage.", call. = FALSE)
}
# 1.12 Enable GitHub for explicit repositories
github_mode <- github_mode || length(repository_arg) == 1L
# 1.13 Select requested or default repository
repository <- if (length(repository_arg)) {
sub("^--repo=", "", repository_arg[[1L]])
} else {
"SVK-85/job-crafting-networks-latent-profiles-eosc"
}
# 1.14 Remove trailing repository slash
repository <- sub("/$", "", repository)
# 1.15 Remove the git repository suffix
repository <- sub("\\.git$", "", repository)
# 1.16 Remove GitHub URL prefix
repository <- sub("^https://github\\.com/", "", repository)
# 1.17 Select requested or default reference
repository_ref <- if (length(ref_arg)) {
sub("^--ref=", "", ref_arg[[1L]])
} else {
"main"
}
# 1.18 Validate repository and reference options
if (length(ref_arg) && !github_mode) {
stop("--ref requires --github or --repo.", call. = FALSE)
}
if (github_mode &&
!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
stop("--repo must be OWNER/REPOSITORY or a GitHub repository URL.",
call. = FALSE)
}
if (github_mode &&
(!grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*$", repository_ref) ||
grepl("(^|/)\\.\\.(/|$)", repository_ref))) {
stop("--ref is not a valid GitHub branch, tag, or commit.", call. = FALSE)
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 2 Use Steps 00-15 unless overridden
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 2.1 Set the default final step
default_final_id <- 15L
# 2.2 Resolve and validate requested steps
final_id <- if (length(through)) {
as.integer(sub("^--through=", "", through[[1L]]))
} else {
default_final_id
}
if (!final_id %in% 0:15) {
stop("--through must be between 00 and 15.", call. = FALSE)
}
required_ids <- 0:final_id
# 2.3 Define required script filenames
required_files <- sprintf("%02d_Step.R", required_ids)
# 2.4 Reject conflicting usage modes
exclusive_modes <- sum(c(help, show_list, check_only))
if (exclusive_modes > 1L) {
stop("Use only one of --help, --list, or --check.", call. = FALSE)
}
# 2.5 Display help and exit
if (help) {
cat("JC-EOSC - Step Launcher\n\n")
cat("  --list        Show the detected requested steps\n")
cat("  --check       Parse all selected steps without running them\n")
cat("  --through=NN  Download, check, or run through Step NN (default: 15; allowed: 00-15)\n")
cat("  --pause       Wait for ENTER between executed steps\n")
cat("  --github      Download the default GitHub repository snapshot\n")
cat("  --repo=...    Use OWNER/REPOSITORY or its GitHub URL\n")
cat("  --ref=main    Use a branch, tag, or commit (default: main)\n")
cat("  --help        Show this help\n")
cat("\nGitHub mode writes results in the current directory.\n")
cat("It is selected automatically when requested local steps are absent.\n")
cat("For a private repository, set GH_TOKEN or GITHUB_TOKEN first.\n")
cat("\nFrom source(), arguments can be supplied with:\n")
cat("  options(\"job-crafting-eosc.launcher.args\" = '--check')\n")
cat(
"  options(\"job-crafting-eosc.launcher.args\" = c('--github', '--check'))\n")
return(invisible(NULL))
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 3 Locate the launcher and project root
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
source_files <- unlist(lapply(rev(sys.frames()), function(frame) {
# 3.1 Read and validate source path
source_file <- frame$ofile
if (!is.character(source_file) || length(source_file) != 1L ||
is.na(source_file) || !nzchar(source_file)) {
return(character(0))
}
# 3.2 Expand and check source path
source_file <- path.expand(source_file)
if (file.exists(source_file)) return(source_file)
# 3.3 Read the original source directory
original_directory <- frame$owd
if (is.character(original_directory) &&
length(original_directory) == 1L &&
!is.na(original_directory) && nzchar(original_directory)) {
# 3.4 Check source in original directory
source_from_original_directory <- file.path(original_directory, source_file)
if (file.exists(source_from_original_directory)) {
return(source_from_original_directory)
}
}
character(0)
}), use.names = FALSE)
# 3.5 Read the Rscript file argument
launcher_flag <- grep("^--file=", commandArgs(FALSE), value = TRUE)
# 3.6 Resolve the Rscript launcher path
rscript_file <- if (length(launcher_flag)) {

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 4. R replaces spaces in --file paths with ~+~ on Unix-like systems.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 4.1 Restore spaces in launcher path
gsub("~+~", " ", sub("^--file=", "", launcher_flag[[1L]]), fixed = TRUE)
} else {
character(0)
}
# 4.2 Collect candidate launcher paths
launcher_candidates <- c(source_files, path.expand(rscript_file))
# 4.3 Require an existing launcher path
launcher_candidates <- launcher_candidates[
file.exists(launcher_candidates)]
if (!length(launcher_candidates)) {
stop("Launcher file path could not be determined. Use Rscript or source().",
call. = FALSE)
}
# 4.4 Normalize launcher and read configuration
launcher_file <- normalizePath(launcher_candidates[[1L]],
winslash = "/", mustWork = TRUE)
configured_root <- getOption("job-crafting-eosc.project_root",
Sys.getenv("JC_EOSC_PROJECT_ROOT", unset = ""))
# 4.5 Convert configured root to text
configured_root <- as.character(configured_root)[1L]
# 4.6 Select candidate local project roots
local_root_candidates <- if (nzchar(configured_root)) {
# 4.7 Normalize the configured project path
normalizePath(path.expand(configured_root),
# 4.8 Configure path normalization options
winslash = "/", mustWork = TRUE)
} else {
unique(c(normalizePath(getwd(), winslash = "/", mustWork = TRUE),
         dirname(launcher_file)))
}
# 4.9 Define all workflow script filenames
workflow_files <- sprintf("%02d_Step.R", 0:15)
# 4.10 Find complete local script sets
complete_local_roots <- local_root_candidates[vapply(
local_root_candidates,
function(root) {
# 4.11 Define candidate source directories
candidate_directories <- c(root,
                           file.path(root, "steps"),
                           file.path(root, "code"))
# 4.12 Inspect existing directories for scripts
candidate_directories <- candidate_directories[
dir.exists(candidate_directories)]
any(vapply(
candidate_directories,
function(directory) {
# 4.13 Verify required and unexpected filenames
local_files <- sort(list.files(directory, pattern = "^[0-9]{2}_Step\\.R$"))
all(required_files %in% local_files) &&
!length(setdiff(local_files, workflow_files))
},
logical(1)))
},
logical(1))]
# 4.14 Select source and project directory
complete_local_set <- length(complete_local_roots) > 0L
local_project_root <- if (complete_local_set) {
complete_local_roots[[1L]]
} else {
local_root_candidates[[1L]]
}
if (!github_mode && !complete_local_set) {
github_mode <- TRUE
github_mode_automatic <- TRUE
message(
sprintf(
"Required local Steps 00-%02d not found; switching to GitHub mode.",
final_id))
}
project_root <- if (nzchar(configured_root)) {
local_project_root
} else if (github_mode) {
normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
local_project_root
}
original_directory <- getwd()
on.exit(setwd(original_directory), add = TRUE)
setwd(project_root)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 5. Download one fixed GitHub snapshot when remote mode is requested
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 5.1 Define the step filename pattern
step_pattern <- "^[0-9]{2}_Step\\.R$"
# 5.2 Define all workflow step identifiers
workflow_ids <- 0:15
# 5.3 Record the required step identifiers
expected_ids <- required_ids
# 5.4 Record the required script filenames
expected_files <- required_files
# 5.5 Initialize the source repository description
source_repository <- "local files"
# 5.6 Initialize the source reference description
source_ref <- "not applicable"
# 5.7 Initialize the resolved commit identifier
resolved_sha <- "not available"
# 5.8 Initialize the local source directory
source_root <- project_root
if (github_mode) {
# 5.9 Read the primary GitHub token
github_token <- Sys.getenv("GH_TOKEN", unset = "")
if (!nzchar(github_token)) {
# 5.10 Read the fallback GitHub token
github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
}
github_headers <- function(accept) {
# 5.11 Build standard GitHub request headers
headers <- c(Accept = accept,
             `X-GitHub-Api-Version` = "2022-11-28",
             `User-Agent` = "JC-EOSC-Step-Launcher")
if (nzchar(github_token)) {
# 5.12 Add the token authorization header
headers <- c(headers, Authorization = paste("Bearer", github_token))
}
headers
}
github_download <- function(url, destination, accept,
resource = "requested resource") {
status <- tryCatch(
suppressWarnings(utils::download.file(url = url, destfile = destination,
method = "libcurl", mode = "wb", quiet = TRUE,
headers = github_headers(accept))), error = function(e) e)
valid_download <- !inherits(status, "error") &&
isTRUE(as.integer(status) == 0L) &&
file.exists(destination) &&
isTRUE(file.info(destination)$size > 0L)
if (!valid_download) {
stop(paste0("GitHub download failed for ", resource,
". Check that the file exists and that the repository is ",
"accessible. Private repositories require a read-only GH_TOKEN ",
"or GITHUB_TOKEN with Contents access."), call. = FALSE)
}
invisible(destination)
}
# 5.13 Define the repository API endpoint
github_api <- paste0("https://api.github.com/repos/", repository)
if (grepl("^[0-9a-fA-F]{40}$", repository_ref)) {
# 5.14 Normalize the supplied commit SHA
resolved_sha <- tolower(repository_ref)
} else {
# 5.15 Prepare temporary response and cleanup
sha_file <- tempfile("jc_commit_", fileext = ".txt")
on.exit(unlink(sha_file, force = TRUE), add = TRUE)
# 5.16 Request the repository commit identifier
github_download(
paste0(github_api, "/commits/",
utils::URLencode(repository_ref, reserved = TRUE)),
sha_file, "application/vnd.github.sha",
paste("repository reference", repository_ref))
# 5.17 Read and validate commit SHA
resolved_sha <- trimws(paste(
readLines(sha_file, warn = FALSE, encoding = "UTF-8"),
collapse = ""))
}
if (!grepl("^[0-9a-f]{40}$", resolved_sha)) {
stop("GitHub returned an invalid commit identifier.", call. = FALSE)
}
# 5.18 Create source cache parent directory
source_parent <- file.path(project_root, ".jc_sources")
if (!dir.exists(source_parent) &&
!dir.create(source_parent, recursive = TRUE,
showWarnings = FALSE)) {
stop("Cannot create the GitHub source-cache directory.", call. = FALSE)
}
# 5.19 Define commit and range directory
source_root <- file.path(source_parent,
paste0(resolved_sha, "_through_", sprintf("%02d", final_id)))
# 5.20 Prepare staging and automatic cleanup
if (!dir.exists(source_root)) {
staging_root <- tempfile("download_", tmpdir = source_parent)
if (!dir.create(staging_root, showWarnings = FALSE)) {
stop("Cannot create the temporary GitHub download directory.", call. = FALSE)
}
staging_complete <- FALSE
on.exit({
if (!staging_complete && dir.exists(staging_root)) {
unlink(staging_root, recursive = TRUE, force = TRUE)
}
}, add = TRUE)
# 5.21 Download all requested step scripts
for (step_file in expected_files) {
github_download(paste0(github_api, "/contents/", step_file,
"?ref=", resolved_sha),
file.path(staging_root, step_file),
"application/vnd.github.raw+json", step_file)
}
# 5.22 Collect and parse downloaded scripts
staged_files <- file.path(staging_root, expected_files)
invisible(lapply(staged_files, parse, keep.source = FALSE))
# 5.23 Record provenance and script checksums
source_manifest <- data.frame(repository = repository,
requested_ref = repository_ref, commit_sha = resolved_sha,
script = expected_files,
md5 = unname(tools::md5sum(staged_files)),
stringsAsFactors = FALSE)
# 5.24 Save manifest and finalize snapshot
utils::write.csv(source_manifest, file.path(staging_root,
"source_manifest.csv"), row.names = FALSE, na = "")
if (!file.rename(staging_root, source_root)) {
stop("Cannot finalize the verified GitHub source snapshot.", call. = FALSE)
}
# 5.25 Mark snapshot staging as complete
staging_complete <- TRUE
}
# 5.26 Validate and parse cached scripts
cached_files <- file.path(source_root, expected_files)
if (!all(file.exists(cached_files)) ||
any(file.info(cached_files)$size <= 0L)) {
stop("The cached GitHub source snapshot is incomplete.", call. = FALSE)
}
invisible(lapply(cached_files, parse, keep.source = FALSE))
# 5.27 Record the selected repository URL
source_repository <- paste0("https://github.com/", repository)
# 5.28 Record reference and announce selection
source_ref <- repository_ref
if (github_mode_automatic) {
message("GitHub source selected automatically: ", source_repository)
}
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 6. Download one fixed GitHub snapshot when remote mode is requested
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 6.1 Define the candidate source directories
candidate_directories <- c(source_root,
                           file.path(source_root, "steps"),
                           file.path(source_root, "code"))
# 6.2 Retain the existing candidate directories
candidate_directories <- candidate_directories[
dir.exists(candidate_directories)]
# 6.3 List numbered scripts by directory
files_by_directory <- lapply(candidate_directories, function(directory) {
sort(list.files(directory, pattern = step_pattern, full.names = TRUE))
})
# 6.4 Locate scripts or report absence
active_directories <- which(lengths(files_by_directory) > 0L)

if (!length(active_directories)) {
stop("No NN_Step.R scripts found in the root, steps/, or code/.", call. = FALSE)
}
if (length(active_directories) > 1L) {
# 6.5 Report directory conflicts and stop
locations <- paste(candidate_directories[active_directories],
collapse = "\n  ")
stop(
"Step scripts were found in multiple directories. Keep one canonical set:\n  ",
locations, call. = FALSE)
}
# 6.6 Select the step script directory
step_directory <- candidate_directories[active_directories]
# 6.7 Select the detected script paths
step_files <- files_by_directory[[active_directories]]
# 6.8 Extract the detected script filenames
detected_files <- basename(step_files)
# 6.9 Extract the numeric step identifiers
ids <- as.integer(sub("_Step\\.R$", "", detected_files))
# 6.10 Identify the missing required scripts
missing_files <- setdiff(expected_files, detected_files)
# 6.11 Identify the unexpected numbered scripts
unexpected_files <- setdiff(detected_files, workflow_files)
# 6.12 List scripts or validate sequence
if (show_list) {
cat("Source: ", source_repository, "\n", sep = "")
cat("Requested ref: ", source_ref, "\n", sep = "")
cat("Resolved commit: ", resolved_sha, "\n", sep = "")
cat(sprintf("Detected steps in %s:\n", step_directory))
for (i in seq_along(step_files)) {
cat(sprintf("  %02d  %s\n", ids[i], detected_files[i]))
}
if (length(missing_files)) {
cat("Missing required steps: ",
paste(missing_files, collapse = ", "), "\n", sep = "")
}
if (length(unexpected_files)) {
cat("Unexpected numbered steps: ",
paste(unexpected_files, collapse = ", "), "\n", sep = "")
}
return(invisible(NULL))
}
if (anyDuplicated(ids) || length(missing_files) ||
length(unexpected_files) ||
!identical(ids[ids %in% expected_ids], expected_ids)) {
details <- c(
if (length(missing_files))
paste("Missing:", paste(missing_files, collapse = ", ")),
if (length(unexpected_files))
paste("Unexpected:", paste(unexpected_files, collapse = ", ")))
stop(sprintf(
"The launcher requires a continuous sequence from 00_Step.R through %02d_Step.R. %s",
final_id, paste(details, collapse = " | ")), call. = FALSE)
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 7. Select the final requested step
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 7.1 Select steps through requested limit
selected <- which(ids <= final_id)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 8. Parse every selected script before starting any analysis
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
for (i in selected) {
parse(file = step_files[[i]], keep.source = FALSE)
if (check_only) {
cat(sprintf("  OK  %s\n", detected_files[i]))
}
}
if (check_only) {
cat("Source: ", source_repository, "\n", sep = "")
cat("Requested ref: ", source_ref, "\n", sep = "")
cat("Resolved commit: ", resolved_sha, "\n", sep = "")
cat("Syntax validation completed. No analysis was run.\n")
return(invisible(NULL))
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 9.Create an immutable run identifier and execution report
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 9.1 Generate the UTC run timestamp
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
# 9.2 Create a unique run directory
run_directory <- file.path(project_root, "logs",
paste0("run_", run_stamp, "_", Sys.getpid()))
if (!dir.create(run_directory, recursive = TRUE,
showWarnings = FALSE)) {
stop("Cannot create the run log directory.", call. = FALSE)
}
# 9.3 Select the available repository commit
git_sha <- if (github_mode) {
resolved_sha
} else {
# 9.4 Read local commit when available
local_sha <- tryCatch(
suppressWarnings(system2(
"git", c("-C", shQuote(step_directory),
"rev-parse", "--short=12", "HEAD"),
stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
if (length(local_sha) != 1L ||
!is.null(attr(local_sha, "status"))) {
"not available"
} else {
local_sha
}
}
# 9.5 Define the UTC timestamp helper
utc_now <- function() {
format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}
# 9.6 Collect and save execution metadata
metadata <- c(paste("Start:", utc_now()),
              paste("Git commit:", git_sha),
              paste("R version:", R.version.string),
              paste("Project root:", project_root),
              paste("Source repository:", source_repository),
              paste("Requested ref:", source_ref),
              paste("Resolved commit:", resolved_sha),
              paste("Step directory:", step_directory),
              paste("Selected steps: 00 through", sprintf("%02d", final_id)))
writeLines(metadata, file.path(run_directory, "run_metadata.txt"))
# 9.7 Initialize the step status table
report <- data.frame(step = ids, script = detected_files,
status = ifelse(ids %in% ids[selected], "PENDING", "NOT_SELECTED"),
duration_sec = rep(NA_real_, length(ids)),
started_utc = rep("", length(ids)), finished_utc = rep("", length(ids)),
stringsAsFactors = FALSE)
# 9.8 Configure and save execution report
report_file <- file.path(run_directory, "step_status.csv")
save_report <- function() {
utils::write.csv(report, file = report_file,
row.names = FALSE, na = "")
}
save_report()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
## 10. Use a lightweight terminal display without extra packages
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# 10.1 Define optional terminal color formatting
colored <- function(x, code) {
# 10.2 Inspect terminal and color preferences
terminal <- Sys.getenv("TERM", unset = "")
if (nzchar(Sys.getenv("NO_COLOR")) ||
# 10.3 Return plain or colored text
terminal %in% c("", "dumb")) return(x)
paste0("\033[", code, "m", x, "\033[0m")
}
progress_bar <- function(n, total, width = 24L) {
# 10.4 Build the text progress bar
filled <- as.integer(round(width * n / total))
paste0("[", strrep("#", filled), strrep("-", width - filled), "]")
}
# 10.5 Display banner and run details
cat("\n")
cat(colored("===============================================\n", "36"))
cat(colored("   JC-EOSC | R ANALYSIS LAUNCHER\n", "1;36"))
cat(colored("===============================================\n", "36"))
cat("Repository commit: ", git_sha, "\n", sep = "")
cat("Source:            ", source_repository, "\n", sep = "")
cat("Virtual machine:    ", Sys.info()[["nodename"]], "\n", sep = "")
cat("R version:          ", as.character(getRversion()), "\n", sep = "")
cat("Steps selected:     ", length(selected),
" / ", length(ids), "\n", sep = "")
cat("Run record:         ", run_directory, "\n\n", sep = "")
# 9. Run all selected steps in one shared R environment
shared_workspace <- new.env(parent = globalenv())
# 10.6 Record total execution start time
total_start <- Sys.time()
# 10.7 Iterate through selected analysis steps
for (position in seq_along(selected)) {
# 10.8 Select the current step index
i <- selected[[position]]
# 10.9 Select the current script path
current_script <- step_files[[i]]
# 10.10 Restore the project working directory
setwd(project_root)
# 10.11 Announce the current analysis step
cat(colored(sprintf("\n>> STEP %02d (%d/%d): %s\n",
ids[i], position, length(selected),
detected_files[i]), "1;34"))
# 10.12 Save step start and status
started <- Sys.time()
report$status[i] <- "RUNNING"
report$started_utc[i] <- utc_now()
save_report()
# 10.13 Run script and capture errors
failure <- tryCatch({
source(file = current_script, local = shared_workspace,
echo = FALSE, chdir = FALSE)
NULL
}, error = function(e) e)
# 10.14 Record duration and finish timestamp
report$duration_sec[i] <- round(as.numeric(difftime(
Sys.time(), started, units = "secs")), 2L)
report$finished_utc[i] <- utc_now()
# 10.15 Report failures and stop execution
if (inherits(failure, "error")) {
report$status[i] <- "FAILED"
save_report()
writeLines(conditionMessage(failure), file.path(run_directory,
sprintf("Step_%02d_Error.txt", ids[i])))
writeLines(capture.output(sessionInfo()),
file.path(run_directory, "sessionInfo.txt"))
cat(colored(sprintf("FAILED STEP %02d: %s\n",
ids[i], conditionMessage(failure)),
"1;31"), file = stderr())
cat("Report: ", report_file, "\n", sep = "")
stop(sprintf("Step %02d failed; see the run report above.",
ids[i]), call. = FALSE)
}
# 10.16 Record completion and display progress
report$status[i] <- "COMPLETED"
save_report()
cat(colored(sprintf("OK %s %d/%d | %.2f s\n",
progress_bar(position, length(selected)), position, length(selected),
report$duration_sec[i]), "1;32"))
# 10.17 Pause between steps when requested
if (pause && position < length(selected)) {
cat("Press ENTER to continue (q + ENTER to stop): ")
# 10.18 Read and validate continuation response
answer <- readLines(con = stdin(), n = 1L, warn = FALSE)
if (!length(answer)) {
stop("No stdin available for --pause.", call. = FALSE)
}
if (tolower(trimws(answer[[1L]])) %in% c("q", "quit")) {
# 10.19 Record interruption and return report
remaining <- selected[(position + 1L):length(selected)]
report$status[remaining] <- "NOT_RUN"
save_report()
cat("Pipeline interrupted at user request.\n")
writeLines(capture.output(sessionInfo()),
file.path(run_directory, "sessionInfo.txt"))
return(invisible(report))
}
}
}
# 10.20 Save final R session information
writeLines(capture.output(sessionInfo()),
file.path(run_directory, "sessionInfo.txt"))
# 10.21 Calculate duration and summarize completion
total_seconds <- as.numeric(difftime(
Sys.time(), total_start, units = "secs"))
cat(colored("\nPIPELINE COMPLETED SUCCESSFULLY\n", "1;32"))
cat(sprintf("Elapsed: %.2f seconds\n", total_seconds))
cat("Step status: ", report_file, "\n", sep = "")
cat("Git commit:  ", git_sha, "\n", sep = "")
invisible(report)
}
# 10.22 Start the analysis launcher
jc_eosc_launcher_main()
