#' Assign Colors to Causes Based on Hierarchical Levels
#'
#' This function assigns colors to a vector of causes based on a specified hierarchical level. It uses pre-defined colors from the internal dataset `bebod_colors` within the BeBOD package.
#'
#' @param cause_vector A character vector containing the names of causes. This should match the "CAUSE" column in the `bebod_colors` or `causelist` dataset.
#' @param level An integer specifying the hierarchical level of the causes:
#'   \itemize{
#'     \item 0: Assigns a single green color for "All causes".
#'     \item 1 or 2: Assigns dedicated colors based on the `bebod_colors` dataset.
#'     \item >2: Assigns grouped colors with varying alpha transparency for sub-causes.
#'   }
#'
#' @return A named character vector of HEX color codes, where the names correspond to the `cause_vector`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with mock data (requires bebod_colors dataset)
#' cause_vector <- c("Tuberculosis", "HIV/AIDS", "Diarrheal diseases")
#' color_cause(cause_vector, level = 1)
#' }
color_cause <- function(cause_vector, level) {
  # Validate inputs
  if (!is.character(cause_vector)) {
    stop("`cause_vector` must be a character vector.")
  }
  
  if (!is.numeric(level) || length(level) != 1 || level < 0) {
    stop("`level` must be a single non-negative integer.")
  }
  
  # Level 0: Single color for "All causes"
  if (level == 0) {
    cols <- "#3AAA35FF"
    names(cols) <- "All causes"
    reordered_cols <- cols
  }
  
  # Level 1 or 2: Use dedicated colors from the bebod_colors dataset
  if (level %in% 1:2) {
    col_subset <- subset(BeBOD:::bebod_colors, CAUSE %in% cause_vector)
    if (nrow(col_subset) == 0) {
      stop("No matching causes found in the `bebod_colors` dataset.")
    }
    cols <- col_subset$COL
    names(cols) <- col_subset$CAUSE
    
    # Reorder the colors to match the input cause_vector
    reordered_cols <- cols[cause_vector]
  }
  
  # Level 3 and above: Group and apply alpha transparency
  if (level > 2) {
    # Subset data for Level 3
    col_subset <- subset(BeBOD:::bebod_colors, CAUSE %in% cause_vector & LEVEL == paste0("Level", 3))
    if (nrow(col_subset) == 0) {
      stop("No matching causes found for Level 3 in the `bebod_colors` dataset.")
    }
    col_unique <- unique(col_subset$COL)
    
    cols <- c()
    
    for (i in seq_along(col_unique)) {
      L3.i <- subset(col_subset, COL == col_unique[i])
      n.i <- nrow(L3.i)
      alpha <- seq(from = 100, to = 25, length.out = n.i)
      # Convert alpha to HEX
      alpha_hex <- sprintf("%02X", round(alpha / 100 * 255))
      # Adjust color HEX values with alpha
      col_with_alpha <- paste0(substr(L3.i$COL, 1, 7), alpha_hex)
      cols <- c(cols, col_with_alpha)
    }
    names(cols) <- col_subset$CAUSE
    
    # Reorder the colors to match the input cause_vector
    reordered_cols <- cols[cause_vector]
  }
  
  # Return the resulting colors
  return(reordered_cols)
}