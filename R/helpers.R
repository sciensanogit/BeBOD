###=========================================================================#
### HELPERS ================================================================#
###=========================================================================#
###-- today ................... return today's date in yyymmdd format
###-- readxl .................. read excel file as data.frame
###-- mean_ci ................. calculate mean and 95% confidence interval

##--------------------------------------------------------------------------#
## Return today's date in yyyymmdd format ----------------------------------#

today <- function() {
  return(format(Sys.time(), "%Y%m%d"))
}

##--------------------------------------------------------------------------#
## Read Excel file as data.frame -------------------------------------------#

readxl <- function(...) {
  xl <- read_excel(...)
  class(xl) <- "data.frame"
  colnames(xl) <- make.names(colnames(xl))
  return(xl)
}

##--------------------------------------------------------------------------#
## Calculate mean and 95% confidence interval ------------------------------#

mean_ci <-
function(x, ...) {
  c(mean = mean(x, ...), quantile(x, probs = c(0.025, 0.975), ...))
}
