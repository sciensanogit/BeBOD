#' Select the best INLA model based on WAIC or DIC
#'
#' This function fits multiple INLA models provided in a list and compares them based on WAIC or DIC.
#' It selects the best-fitting model and returns both the comparison of models and the best model itself.
#' It can handle binomial and poisson likelihoods, and optionally takes Ntrials for grouped binomial data.
#'
#' @param data A data frame or data.table containing the data for model fitting.
#' @param model_list A list of model formulas. Each model formula should be a valid formula that can be used in INLA.
#' @param likelihood A character string specifying the likelihood model to be used. Defaults to "zeroinflatedbinomial0".
#'        Accepted values are for binomial and poisson families (e.g., "binomial", "poisson").
#' @param metric A character string specifying the metric for model comparison. Either "WAIC" or "DIC". Defaults to "WAIC".
#' @param Ntrials Optional. Specifies the number of trials for binomial models. If set to "N", it will use the "N" column from the data.
#' @param verbose Logical. If TRUE, provides verbose output during the INLA model fitting process.
#' @param ... Additional arguments passed to the `inla()` function.
#'
#' @return A list containing:
#'   \item{model_comparisons}{A tibble with comparison of the models based on the chosen metric (WAIC or DIC).}
#'   \item{selected_model}{The name of the selected best-fitting model.}
#'   \item{best_inla_model}{The INLA model object for the best-fitting model.}
#'   \item{best_formula}{The formula used for the best-fitting model.}
#'   \item{original_data}{The original data used for fitting the models.}
#'
#' @examples
#' \dontrun{
#' # Define a list of models
#' model_list <- list(
#'   FIXED = ID ~ 1 + YEAR + SEX + AGEGR + REGION,
#'   TWOxWAYxYEAR = ID ~ 1 + YEAR*AGEGR + YEAR*REGION + YEAR*SEX
#' )
#'
#' # Run the model selection
#' best_model_output <- select_best_inla_model(data, model_list, likelihood = "binomial", metric = "WAIC")
#'
#' # Access the best model
#' best_inla_model <- best_model_output$best_inla_model
#'}
#'
#' @export
select_best_inla_model <- function(data, model_list, likelihood = "zeroinflatedbinomial0", metric = c("WAIC", "DIC"), Ntrials = NULL, verbose = TRUE, ...) {
  
  # Ensure data is not empty
  if (nrow(data) == 0) {
    stop("The provided data is empty. Please provide a valid dataset.")
  }
  
  # Allowed likelihood options for binomial and poisson families
  binomial_likelihoods <- c("binomial", "zeroinflatedbinomial0", "zeroinflatedbinomial1", "zeroinflatedbinomial2")
  poisson_likelihoods <- c("poisson", "zeroinflatedpoisson0", "zeroinflatedpoisson1", "zeroinflatedpoisson2", "xpoisson")
  gamma_likelihoods <- c("gamma")
  
  # Check if the provided likelihood is valid
  if (!(likelihood %in% c(binomial_likelihoods, poisson_likelihoods, gamma_likelihoods))) {
    stop("Invalid likelihood provided. Please choose a valid binomial or poisson likelihood.")
  }
  
  # Use match.arg to ensure metric is either "WAIC" or "DIC"
  metric <- match.arg(metric)
  
  # If Ntrials is provided as "N", use the "N" column from the data
  if (!is.null(Ntrials)) {
    if (is.character(Ntrials) && Ntrials == "N") {
      if ("N" %in% names(data)) {
        inla_Ntrials <- data$N
      } else {
        stop("N column not found in data.")
      }
    }
  }
  
  # Remove rows where Ntrials is NA or 0 and warn the user
  if (!is.null(Ntrials)) {
    invalid_rows <- which(is.na(inla_Ntrials) | inla_Ntrials == 0)
    if (length(invalid_rows) > 0) {
      warning(sprintf("Removing %d rows with NA or 0 values in Ntrials.", length(invalid_rows)))
      data <- data[-invalid_rows, ]
      inla_Ntrials <- inla_Ntrials[-invalid_rows]
    }
  }
  
  # For binomial models, ensure Ntrials is valid
  if (likelihood %in% binomial_likelihoods && !is.null(Ntrials)) {
    if (any(inla_Ntrials <= 0, na.rm = TRUE)) {
      stop("Ntrials must be a positive integer greater than 0 for all binomial observations.")
    }
  }
  
  # Check if Ntrials is required but not provided for grouped binomial data
  if (is.null(Ntrials) && likelihood %in% binomial_likelihoods) {
    if ("N" %in% names(data) && any(data$N > 1)) {
      stop("Ntrials is required for grouped binomial data but is not provided. Please specify the Ntrials argument.")
    }
  }
  
  # Initialize an empty table to store model results
  model_compare <- tibble()
  
  # Initialize a list to store the inla models
  inla_models_list <- list()
  
  # Loop over the models provided in the model_list
  for (m in names(model_list)) {
    print(paste0("Running model: ", m))
    
    # Check for the Ntrials argument
    
    if (!is.null(Ntrials)) {
      
      # Try running the INLA model, capturing any errors
      inla_model <- tryCatch({
        inla(
          model_list[[m]],               # Use the formula from the model_list
          family = likelihood,           # Use the specified likelihood family
          data = data,                   # Use the input dataset
          quantile = c(0.025, 0.5, 0.975),  # Compute the 95% credible intervals
          verbose = verbose,             # Verbose output if requested
          Ntrials = N,        # Specify Ntrials for binomial models (if applicable)
          control.compute = list(
            dic = TRUE,                 # Compute DIC
            waic = TRUE,                # Compute WAIC
            cpo = TRUE,                 # Compute Conditional Predictive Ordinate (CPO)
            hyperpar = TRUE,            # Include hyperparameters in the output
            mlik = TRUE,                # Compute marginal likelihood
            config = TRUE               # Include configuration information
          ),
          control.predictor = list(compute = TRUE, link = 1),  # Compute predictions
          control.inla = list(cmin = 0),  # Control INLA options
          ...
        )
      }, error = function(e) {
        message(sprintf("Error in model %s: %s", m, e$message))
        return(NULL)
      })
      
    } else {
      
      # Try running the INLA model, capturing any errors
      inla_model <- tryCatch({
        inla(
          model_list[[m]],               # Use the formula from the model_list
          family = likelihood,           # Use the specified likelihood family
          data = data,                   # Use the input dataset
          quantile = c(0.025, 0.5, 0.975),  # Compute the 95% credible intervals
          verbose = verbose,             # Verbose output if requested
          control.compute = list(
            dic = TRUE,                 # Compute DIC
            waic = TRUE,                # Compute WAIC
            cpo = TRUE,                 # Compute Conditional Predictive Ordinate (CPO)
            hyperpar = TRUE,            # Include hyperparameters in the output
            mlik = TRUE,                # Compute marginal likelihood
            config = TRUE               # Include configuration information
          ),
          control.predictor = list(compute = TRUE, link = 1),  # Compute predictions
          control.inla = list(cmin = 0),  # Control INLA options
          ...
        )
      }, error = function(e) {
        message(sprintf("Error in model %s: %s", m, e$message))
        return(NULL)
      })
    }

    
    # Skip the model if there was an error
    if (is.null(inla_model)) next
    
    # Store the model and formula in a list
    inla_models_list[[m]] <- list(
      model = inla_model,
      formula = model_list[[m]]  # Store the formula directly
    )
    
    # Save the comparison metrics for this model
    model_compare_tmp <- tibble(
      MODEL = m,
      DIC = inla_model$dic$dic,
      WAIC = inla_model$waic$waic
    )
    
    # Add the current model's comparison metrics to the overall comparison table
    model_compare <- bind_rows(model_compare, model_compare_tmp)
    
    # Clean up the inla_model to free memory
    rm(inla_model)
  }
  
  # Select the best model based on the chosen metric (WAIC or DIC)
  if (metric == "WAIC") {
    model_best <- model_compare %>%
      filter(!is.na(WAIC), WAIC != Inf, WAIC != -Inf) %>%
      slice(which.min(WAIC))  # Select the model with the minimum WAIC
  } else if (metric == "DIC") {
    model_best <- model_compare %>%
      filter(!is.na(DIC), DIC != Inf, DIC != -Inf) %>%
      slice(which.min(DIC))  # Select the model with the minimum DIC
  }
  
  # Get the name and the corresponding INLA model for the best model
  best_model_name <- model_best$MODEL
  best_inla_model <- inla_models_list[[best_model_name]]
  
  # Print the selected model name
  print(sprintf("The selected model is: %s", best_model_name))
  
  # Return a list with model comparison, selected model, best INLA model, and original data
  return(list(
    model_comparisons = model_compare,            # Comparison of all models
    selected_model = best_model_name,             # Name of the selected best model
    best_inla_model = best_inla_model$model,      # The best INLA model object
    best_formula = best_inla_model$formula,       # The formula used for the best model
    original_data = data                          # The original data used for model fitting
  ))
}
