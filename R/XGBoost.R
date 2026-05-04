#' XGBoost for Binary Classification
#'
#' A comprehensive function to train, tune, and evaluate an XGBoost model
#' for binary classification tasks. Features include automatic threshold
#' optimization via F1 score, feature importance analysis, and built-in
#' evaluation metrics (AUC, log loss).
#'
#' @name xgboost_model
#' @author ZHANG Yibing
NULL

#' Train XGBoost Model for Binary Classification
#'
#' Trains an XGBoost model with predefined hyperparameters, evaluates
#' performance on test data, automatically finds the optimal probability
#' threshold that maximizes F1 score, and returns predictions with feature
#' importance.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item Converts input data to matrix format and creates xgb.DMatrix objects
#'   \item Trains XGBoost with binary logistic objective and AUC/log loss metrics
#'   \item Generates prediction probabilities on test set
#'   \item Finds optimal threshold (0.1-0.9 by 0.01) that maximizes F1 score
#'   \item Computes confusion matrix and feature importance
#'   \item Returns predictions and metadata in standard format
#' }
#'
#' @param X_train Training feature matrix or data frame. Will be converted to matrix.
#' @param y_train Training labels (0/1).
#' @param X_test Test feature matrix or data frame. Will be converted to matrix.
#' @param y_test Test labels (0/1) for evaluation.
#' @return A list containing:
#'   \item{model}{Trained XGBoost model object}
#'   \item{best_threshold}{Optimal probability threshold maximizing F1 score}
#'   \item{importance}{Feature importance data frame from xgb.importance()}
#'   \item{model_name}{Character string "XGBoost"}
#'   \item{predictions}{Factor vector of predicted classes (0/1)}
#'   \item{pred_prob}{Numeric vector of prediction probabilities for class 1}
#'
#' @importFrom xgboost xgb.DMatrix xgb.train xgb.importance
#'
#' @examples
#' \dontrun{
#' # Preprocess data first
#' result <- preprocess_data("Breast_Cancer.csv", tag_column = 1)
#'
#' # Train XGBoost model
#' xgb_result <- train_xgb(
#'   X_train = result$X_train_norm,
#'   y_train = result$y_train,
#'   X_test  = result$X_test_norm,
#'   y_test  = result$y_test
#' )
#'
#' # Access results
#' predictions <- xgb_result$predictions
#' pred_prob   <- xgb_result$pred_prob
#' importance  <- xgb_result$importance
#' }
#' @export
train_xgb <- function(X_train, y_train, X_test, y_test) {
  convert_to_numeric <- function(y) {
    if (is.factor(y)) {
      # Check if there are 2 levels
      if (length(levels(y)) != 2) {
        stop("y must be binary (2 levels). Found: ", paste(levels(y), collapse = ", "))
      }
      return(as.numeric(y) - 1)
    } else if (is.character(y)) {
      unique_vals <- unique(y)
      if (length(unique_vals) != 2) {
        stop("y must be binary. Found: ", paste(unique_vals, collapse = ", "))
      }
      # map to 0/1
      sorted <- sort(unique_vals)
      return(as.numeric(factor(y, levels = sorted)) - 1)
    } else if (is.logical(y)) {
      return(as.numeric(y))
    } else if (is.numeric(y)) {
      # check if there is only 0/1
      if (!all(y %in% c(0, 1))) {
        warning("Numeric y contains values other than 0/1. Converting to binary.")
        y <- as.numeric(y > median(y))
      }
      return(y)
    } else {
      stop("y must be factor, character, logical, or numeric. Found: ", class(y))
    }
  }

  y_train_num <- convert_to_numeric(y_train)
  y_test_num <- convert_to_numeric(y_test)


  # Convert input data to matrix format
  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)

  # Create xgb.DMatrix objects for efficient training and evaluation
  dtrain <- xgb.DMatrix(X_train, label = y_train_num)
  dtest  <- xgb.DMatrix(X_test, label = y_test_num)

  # Set up watchlist to monitor training and test metrics
  evals <- list(
    train = dtrain,
    test = dtest
  )

  # Set up watchlist to monitor training and test metrics
  params <- list(
    objective = "binary:logistic",      # Binary classification
    eval_metric = c("auc", "logloss"),  # Evaluation metrics
    eta = 0.05,                         # Learning rate (smaller = more conservative)
    max_depth = 4,                      # Maximum tree depth
    subsample = 0.8,                    # Row sampling ratio
    colsample_bytree = 0.8              # Column sampling ratio
  )

  cat("\nTraining XGBoost...\n")

  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 300,          # Number of boosting rounds
    evals = evals,          # Monitor both train and test metrics
    print_every_n = 10,     # Print progress every 10 rounds
    verbose = 1
  )

  # Generate prediction probabilities on test set
  prob <- predict(model, dtest)

  # Find best probability threshold that maximizes F1 score）
  find_best_threshold <- function(prob, y_true) {

    thresholds <- seq(0.1, 0.9, 0.01)

    f1_scores <- sapply(thresholds, function(t) {

      # Find best probability threshold that maximizes F1 score
      pred <- ifelse(prob > t, 1, 0)

      # Build confusion matrix with explicit factor levels
      cm <- table(factor(pred, levels = c(0,1)),
                  factor(y_true, levels = c(0,1)))

      # Extract confusion matrix components
      TP <- cm["1","1"]
      FP <- cm["1","0"]
      FN <- cm["0","1"]

      # Calculate precision and recall
      precision <- ifelse(TP+FP==0,0,TP/(TP+FP))
      recall    <- ifelse(TP+FN==0,0,TP/(TP+FN))

      # Calculate F1 score (harmonic mean of precision and recall)
      if (precision + recall == 0) return(0)

      2 * precision * recall / (precision + recall)
    })

    # Return best threshold and corresponding F1 score
    list(
      best_threshold = thresholds[which.max(f1_scores)],
      best_f1 = max(f1_scores)
    )
  }

  # Find optimal threshold using F1 score maximization
  th <- find_best_threshold(prob, y_test)

  cat("\nBest threshold:", th$best_threshold)
  cat("\nBest F1:", th$best_f1, "\n")

  # Apply best threshold to get final class predictions
  pred <- ifelse(prob > th$best_threshold, 1, 0)

  # Print confusion matrix
  cm <- table(Predicted = pred, Actual = y_test)

  cat("\nConfusion Matrix\n")
  print(cm)

  # Calculate and display feature importance
  imp <- xgb.importance(model = model)

  cat("\nFeature Importance:\n")
  print(imp)

  # Return results in standard format
  return(list(
    model = model,
    best_threshold = th$best_threshold,
    importance = imp,
    model_name = "XGBoost",
    predictions = factor(pred, levels=c("0", "1")),
    pred_prob = prob
  ))
}

# Example usage (commented out)

# result <- preprocess_data("Breast_Cancer.csv", tag_column = 1, split_ratio = 0.7)
#
# xgb_result <- train_xgb(
#   X_train = result$X_train_norm,
#   y_train = result$y_train,
#   X_test  = result$X_test_norm,
#   y_test  = result$y_test
# )
#
# # Access results
# predictions <- xgb_result$predictions
# pred_prob   <- xgb_result$pred_prob
#
# # Generate predictions with trained model
# prob <- predict(xgb_result$model, xgb.DMatrix(as.matrix(X_test)))
