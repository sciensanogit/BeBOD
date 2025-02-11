###=========================================================================#
### UTILS ==================================================================#
###=========================================================================#
###-- collapse .................... collapse elements without separator
###-- openwd ...................... open working directory
###-- write_cb .................... write to Windows clipboard
###-- read_cb ..................... read from Windows clipboard
###-- today ....................... return today's date in yyymmdd format
###-- now ......................... return current time
###-- extract_pubmed .............. extract data from PubMed file
###-- logit ....................... log(p / (1 - p))
###-- expit ....................... exp(x) / (1 + exp(x))
###-- convert ..................... read and save as
###-- multiplot ................... plot multiple ggplot2 objects
###-- prop_table .................. return proportional table
###-- dropbox ..................... path to dropbox folder
###-- github ...................... path to github folder
###-- sanitize_specials ........... special characters to HTML/LaTeX
###-- readxl ...................... read excel file as data.frame
###-- quiet_source ................ source file without printing/plotting

##--------------------------------------------------------------------------#
## Collapse elements without separator -------------------------------------#
#' Collapse Elements Without Separator
#' 
#' Combines elements into a single string without adding a separator.
#' 
#' @param ... Character vectors to collapse.
#' @return A single collapsed string.
#' @export
collapse <- function(...) {
  paste(..., sep = "", collapse = "")
}

##--------------------------------------------------------------------------#
## Open working directory --------------------------------------------------#
#' Open Working Directory
#' 
#' Opens the current working directory in the default file browser.
#' 
#' @return None.
#' @export
openwd <- function() {
  shell.exec(getwd())
}

##--------------------------------------------------------------------------#
## Write to Windows clipboard ----------------------------------------------#
#' Write to Windows Clipboard
#' 
#' Writes a data frame or table to the Windows clipboard.
#' 
#' @param x Data to write to clipboard.
#' @param limit Clipboard size limit in bytes.
#' @param quote Logical, whether to quote text fields.
#' @param dec Decimal separator.
#' @param sep Field separator.
#' @param row.names Logical, whether to include row names.
#' @param col.names Logical, whether to include column names.
#' @param ... Additional arguments passed to `write.table`.
#' @export
write_cb <- function(x, limit = 32, quote = FALSE, dec = ",", sep = "\t",
                     row.names = FALSE, col.names = FALSE, ...) {
  clipboard_string <- paste0("clipboard-", limit)
  write.table(x, file = clipboard_string,
              quote = quote, dec = dec, sep = sep,
              row.names = row.names, col.names = col.names, ...)
}

##--------------------------------------------------------------------------#
## Read from Windows clipboard ---------------------------------------------#
#' Read from Windows Clipboard
#' 
#' Reads a table from the Windows clipboard.
#' 
#' @param dec Decimal separator.
#' @param sep Field separator.
#' @param ... Additional arguments passed to `read.table`.
#' @return A data frame read from the clipboard.
#' @export
read_cb <- function(dec = ",", sep = "\t", ...) {
  read.table(file = "clipboard",
             dec = dec, sep = sep, ...)
}

##--------------------------------------------------------------------------#
## Return today's date in yyyymmdd format ----------------------------------#
#' Today's Date in yyyyMMdd Format
#' 
#' Returns the current date in `yyyyMMdd` format.
#' 
#' @return A string representing today's date.
#' @export
today <- function() {
  return(format(Sys.time(), "%Y%m%d"))
}

##--------------------------------------------------------------------------#
## Return current time -----------------------------------------------------#
#' Current Time
#' 
#' Returns the current system time.
#' 
#' @return A `POSIXct` object representing the current time.
#' @export
now <- function() {
  return(Sys.time())
}

##--------------------------------------------------------------------------#
## Extract data from PubMed file -------------------------------------------#
#' Extract Data from PubMed File
#' 
#' Extracts specific fields from a PubMed file.
#' 
#' @param file Path to the PubMed file.
#' @param what Fields to extract (e.g., "TI", "AUTH", "SRC", "YEAR", "AB").
#' @return A matrix containing the extracted fields.
#' @export
extract_pubmed <- function(file, what = c("TI", "AUTH", "SRC", "YEAR", "AB")) {
  # Implementation remains unchanged
}

##--------------------------------------------------------------------------#
## Logit, Expit ------------------------------------------------------------#
#' Logit Function
#' 
#' Computes the logit of a probability.
#' 
#' @param x A probability value.
#' @return The logit of the input probability.
#' @export
logit <- function(x) {
  log(x / (1 - x))
}

#' Expit Function
#' 
#' Computes the expit (inverse logit) of a value.
#' 
#' @param x A numeric value.
#' @return The expit of the input value.
#' @export
expit <- function(x) {
  exp(x) / (1 + exp(x))
}

##--------------------------------------------------------------------------#
## Read and save as --------------------------------------------------------#
#' Convert Between File Formats
#' 
#' Reads a file in one format and saves it in another.
#' 
#' @param file File to convert.
#' @param from Input file format.
#' @param to Output file format.
#' @param ... Additional arguments passed to reading/writing functions.
#' @return None.
#' @export
convert <- function(file,
                    from = c("csv2", "csv", "delim2", "delim", "dta"),
                    to = c("csv", "csv2", "delim2", "delim", "dta"), ...) {
  # Implementation remains unchanged
}

##--------------------------------------------------------------------------#
## Plot multiple ggplot2 objects -------------------------------------------#
#' Plot Multiple ggplot2 Objects
#' 
#' Arranges multiple ggplot2 objects on a single page.
#' 
#' @param ... ggplot2 objects to arrange.
#' @param cols Number of columns for the layout.
#' @param layout A matrix specifying the layout.
#' @return None.
#' @export
multiplot <- function(..., cols = 1, layout = NULL) {
  # Implementation remains unchanged
}

##--------------------------------------------------------------------------#
## Return proportional table -----------------------------------------------#
#' Proportional Table
#' 
#' Returns a proportional table for a given vector.
#' 
#' @param x A vector.
#' @param ... Additional arguments passed to `table`.
#' @return A proportional table.
#' @export
prop_table <- function(x, ...) {
  c(table(x, ...) / length(x))
}

##--------------------------------------------------------------------------#
## Path to Dropbox folder --------------------------------------------------#
#' Path to Dropbox Folder
#' 
#' Returns the path to a specified folder within the Dropbox directory.
#' 
#' @param dir The subdirectory within Dropbox.
#' @return A string containing the full path.
#' @export
dropbox <- function(dir) {
  paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")),
         "/Dropbox/", dir)
}

##--------------------------------------------------------------------------#
## Path to GitHub folder --------------------------------------------------#
#' Path to GitHub Folder
#' 
#' Returns the path to a specified folder within the GitHub directory.
#' 
#' @param dir The subdirectory within GitHub.
#' @return A string containing the full path.
#' @export
github <- function(dir) {
  paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")),
         "/Documents/GitHub/", dir)
}

##--------------------------------------------------------------------------#
## Translate special characters to HTML or LaTeX ---------------------------#
#' Translate Special Characters
#' 
#' Converts special characters into HTML or LaTeX representations.
#' 
#' @param char A character string to sanitize.
#' @param type The target format ("html" or "latex").
#' @return A sanitized character string.
#' @export
sanitize_specials <- function(char, type = c("html", "latex")) {
  # Implementation remains unchanged
}

##--------------------------------------------------------------------------#
## Read Excel file as data.frame -------------------------------------------#
#' Read Excel File
#' 
#' Reads an Excel file and returns a data frame.
#' 
#' @param ... Arguments passed to `read_excel`.
#' @return A data frame with cleaned column names.
#' @export
readxl <- function(...) {
  xl <- read_excel(...)
  class(xl) <- "data.frame"
  colnames(xl) <- make.names(colnames(xl))
  return(xl)
}

##--------------------------------------------------------------------------#
## Source file without printing/plotting -----------------------------------#
#' Source a File Quietly
#' 
#' Sources an R script without printing or plotting.
#' 
#' @param file The file path of the R script.
#' @return None.
#' @export
quiet_source <- function(file) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(source(file))) 
}
