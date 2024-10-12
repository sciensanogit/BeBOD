#' Predict new outcomes using the best INLA model
#'
#' This function takes the output of the `select_best_inla_model()` function and a new dataset,
#' and performs predictions for the outcome variable using the best INLA model.
#' It refits the model with the original data combined with the new data, where the outcome variable
#' in the new data is set to NA.
#'
#' @param best_model_output A list containing the output from the `select_best_inla_model()` function. 
#'        This list should include the best INLA model (`best_inla_model`), the model formula (`best_formula`),
#'        and the original data used for model fitting (`original_data`).
#' @param new_data A data frame containing the new data for which predictions are needed.
#'        This should match the structure of the original data, excluding the outcome variable.
#' @param outcome_var A character string specifying the name of the outcome variable in the original data.
#'        The outcome variable in `new_data` will be set to `NA` for the purpose of prediction.
#' @param Ntrials Optional. Specifies the number of trials for binomial models. If `Ntrials = "N"`,
#'        the function will use the "N" column from the new data. If not provided and required, an error will be thrown.
#' @param verbose Logical. If TRUE, provides verbose output from the `inla()` function during the model refitting.
#' @param ... Additional arguments passed to the `inla()` function.
#'
#' @return A data frame containing the new data combined with predicted values and credible intervals (0.025quant, 0.5quant, 0.975quant).
#'         These values represent the fitted predictions along with their 95% credible intervals.
#'         The predictions are for the rows in `new_data`, and the original data rows are excluded from the returned output.
#'
#' @examples
#'\dontrun{
#' # Assuming `best_model_output` is the result of `select_best_inla_model()`
#' # and `new_data` is a data frame for prediction:
#' predictions <- predict_with_best_inla_model(best_model_output, new_data, outcome_var = "outcome")
#'}
#'
#' @export
predict_with_best_inla_model <- function(best_model_output, new_data, outcome_var, Ntrials = NULL, verbose = TRUE, ...) {
  
  # Extract the best INLA model, formula, and original data from the output
  best_inla_model <- best_model_output$best_inla_model
  best_formula <- best_model_output$best_formula  # Get the formula from the output
  original_data <- best_model_output$original_data
  
  # Check if the best INLA model and original data are provided
  if (is.null(best_inla_model)) {
    stop("No best INLA model provided. Please ensure that the best model output contains an INLA model.")
  }
  if (is.null(best_formula)) {
    stop("Formula is missing. Please ensure that the best model output contains the correct formula.")
  }
  if (is.null(original_data)) {
    stop("Original data used for model fitting is missing. Please provide the original data.")
  }
  
  # Add a column with NA values for the outcome variable in the new_data
  # The outcome is set to NA because we are predicting these values.
  new_data[[outcome_var]] <- NA
  
  # Combine the original data (used for model fitting) with the new data (for prediction)
  combined_data <- dplyr::bind_rows(original_data, new_data)
  
  # Handle Ntrials if specified
  inla_Ntrials <- NULL
  if (!is.null(Ntrials)) {
    # If Ntrials is set to "N", use the "N" column from the data
    if (is.character(Ntrials) && Ntrials == "N") {
      if ("N" %in% names(combined_data)) {
        inla_Ntrials <- combined_data$N
      } else {
        stop("N column not found in combined data.")
      }
    } else {
      # Otherwise, use the Ntrials argument provided
      inla_Ntrials <- Ntrials
    }
  }
  
  if (!is.null(Ntrials)) {
    
    # Refit the INLA model using the combined dataset (original + new data)
    prediction_model <- tryCatch({
      inla(
        formula = best_formula,  # Use the stored formula from the best model
        data = combined_data,    # Use the combined data (original + new data)
        family = best_inla_model$.args$family,  # Use the same likelihood family as in the best model
        Ntrials = N,  # Use Ntrials if provided
        quantile = c(0.025, 0.5, 0.975),  # Compute predictions with credible intervals
        verbose = verbose,  # Control verbosity of the INLA output
        control.compute = list(
          dic = TRUE,
          waic = TRUE,
          cpo = TRUE,
          hyperpar = TRUE,
          mlik = TRUE,
          config = TRUE
        ),
        control.predictor = list(compute = TRUE, link = 1),  # Compute predictions
        control.inla = list(cmin = 0),  # Set control options for INLA fitting
        ...
      )
    }, error = function(e) {
      message(sprintf("Error during prediction: %s", e$message))
      return(NULL)
    })
    
    # If the prediction model fails, return NULL
    if (is.null(prediction_model)) {
      return(NULL)
    }
    
  } else {
    
    # Refit the INLA model using the combined dataset (original + new data)
    prediction_model <- tryCatch({
      inla(
        formula = best_formula,  # Use the stored formula from the best model
        data = combined_data,    # Use the combined data (original + new data)
        family = best_inla_model$.args$family,  # Use the same likelihood family as in the best model
        quantile = c(0.025, 0.5, 0.975),  # Compute predictions with credible intervals
        verbose = verbose,  # Control verbosity of the INLA output
        control.compute = list(
          dic = TRUE,
          waic = TRUE,
          cpo = TRUE,
          hyperpar = TRUE,
          mlik = TRUE,
          config = TRUE
        ),
        control.predictor = list(compute = TRUE, link = 1),  # Compute predictions
        control.inla = list(cmin = 0),  # Set control options for INLA fitting
        ...
      )
    }, error = function(e) {
      message(sprintf("Error during prediction: %s", e$message))
      return(NULL)
    })
    
    # If the prediction model fails, return NULL
    if (is.null(prediction_model)) {
      return(NULL)
    }
    
  }
  
  
  # Extract the fitted values (predictions) from the fitted model
  tmp_proj_fitted <- prediction_model$summary.fitted.values
  
  # Combine the new data with the predicted values
  tmp_pred_final <- bind_cols(combined_data, tmp_proj_fitted)
  
  # Return the predictions for the new data (excluding the original data rows)
  tmp_pred_final <- tmp_pred_final[-(1:nrow(original_data)), ]
  
  # Return only the relevant columns: new data and credible intervals (0.025quant, 0.5quant, 0.975quant)
  tmp_pred_final <- subset(tmp_pred_final, select = c(names(new_data), "0.025quant", "0.5quant", "0.975quant"))
  
  return(tmp_pred_final)
}
