#' Support Vector Machine for Binary Classification
#'
#' A collection of functions to train, tune, and evaluate a Support Vector Machine
#' (SVM) model for binary classification tasks. Features include cross-validation
#' for hyperparameter tuning and class weighting for imbalanced data.
#'
#' @name svm_model
#' @author ZHANG Yibing
NULL

#' Train SVM Model with Optional Hyperparameter Tuning
#'
#' Trains a support vector machine on training data, optionally performs
#' 5-fold cross-validation to tune cost and gamma parameters, and generates
#' predictions with class probabilities for test data.
#'
#' The function automatically:
#' \itemize{
#'   \item Converts categorical predictors to dummy variables using model.matrix
#'   \item Applies inverse frequency class weights to handle imbalanced datasets
#'   \item Returns prediction probabilities for the positive class (class "1")
#' }
#'
#' @param X_train Training feature matrix or data frame. Each column is a feature,
#'        each row is a sample.
#' @param y_train Training label vector (0/1).
#' @param X_test Optional test feature matrix or data frame. If provided,
#'        predictions will be generated. Default is NULL.
#' @param y_test Optional test label vector (0/1). Used for evaluation only.
#'        Default is NULL.
#' @param kernel_type Kernel type for SVM. Options: "radial" (default),
#'        "linear", "polynomial", or "sigmoid".
#' @param tune Logical indicating whether to perform hyperparameter tuning
#'        using 5-fold cross-validation. If TRUE, searches over cost = c(0.1,1,10)
#'        and gamma = c(0.01,0.1,1). Default is TRUE.
#' @return A list containing:
#'   \item{model_name}{Character string "SVM"}
#'   \item{predictions}{Factor vector of predicted classes (0/1) for test data}
#'   \item{pred_prob}{Numeric vector of prediction probabilities for class "1"}
#'   \item{model}{The trained SVM model object from e1071::svm()}
#'
#' @importFrom e1071 svm tune tune.control
#' @importFrom stats model.matrix
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' X_train <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' y_train <- sample(0:1, 100, replace = TRUE)
#' X_test <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' y_test <- sample(0:1, 20, replace = TRUE)
#'
#' # Train with tuning
#' result <- train_svm(X_train, y_train, X_test, y_test, tune = TRUE)
#'
#' # Make predictions manually
#' pred_class <- result$predictions
#' pred_prob <- result$pred_prob
#' }
#' @export
train_svm <- function(X_train, y_train,
                            X_test = NULL, y_test = NULL,
                            kernel_type = "radial",
                            tune = TRUE) {

  # Missing value in case
  fill_na <- function(df) {
    for (i in 1:ncol(df)) {
      if (any(is.na(df[, i]))) {
        if (is.numeric(df[, i])) {
          median_val <- median(df[, i], na.rm = TRUE)
          df[is.na(df[, i]), i] <- median_val
        } else {
          # For non-numeric columns, fill with the mode value
          mode_val <- names(sort(table(df[, i]), decreasing = TRUE))[1]
          df[is.na(df[, i]), i] <- mode_val
        }
      }
    }
    return(df)
  }

  # Combine training and test data to ensure consistent dummy variable encoding
  valid_cols <- sapply(X_train, function(x) length(unique(na.omit(x))) > 1)

  # If all columns have zero variance, retain at least one column
  if (sum(valid_cols) == 0) {
    valid_cols <- rep(TRUE, ncol(X_train))
  }

  X_train <- X_train[, valid_cols, drop = FALSE]
  if (!is.null(X_test)) {
    X_test <- X_test[, valid_cols, drop = FALSE]
  }

  X_train <- fill_na(X_train)
  if (!is.null(X_test)) {
    X_test <- fill_na(X_test)
  }

  # Handle test data when NULL
  if (is.null(X_test)) {
    # If no test data, create empty matrix for dummy variable encoding
    full_data <- X_train
    X_all <- model.matrix(~ . -1, data = full_data)
    X_train <- X_all[1:nrow(X_train), ]
    X_test <- NULL
  } else {
    # Combine training and test data for consistent dummy variable encoding
    full_data <- rbind(X_train, X_test)
    X_all <- model.matrix(~ . -1, data = full_data)
    X_train <- X_all[1:nrow(X_train), ]
    X_test <- X_all[(nrow(X_train) + 1):nrow(X_all), ]
  }

  # Convert labels to factors (required by e1071::svm)
  y_train <- as.factor(y_train)
  if (!is.null(y_test)) {
    y_test <- as.factor(y_test)
    # Check whether the test set contains any levels that did not appear in the training set
    train_levels <- levels(y_train)
    test_levels <- levels(y_test)

    if (!all(test_levels %in% train_levels)) {
      stop("Test set contains factor levels not seen in training set: ",
           paste(setdiff(test_levels, train_levels), collapse = ", "))
    }
  }

  # Calculate inverse frequency class weights for imbalanced data
  # Formula: weight = total_samples / (n_classes * class_count)
  class_counts <- table(y_train)
  class_weights <- sum(class_counts) / (length(class_counts) * class_counts)


  # Hyperparameter tuning using 5-fold cross-validation
  if (tune) {
    tune_result <- tryCatch({
      tune(
        svm,
        train.x = X_train,
        train.y = y_train,
        kernel = kernel_type,
        ranges = list(
          cost = c(0.1, 1, 10),
          gamma = c(0.01, 0.1, 1)
        ),
        class.weights = class_weights,
        probability = TRUE,
        tunecontrol = tune.control(cross = 5)
      )
    }, error = function(e) {
      # 如果调参失败，使用默认参数
      warning("Tuning failed, using default parameters: ", e$message)
      return(NULL)
    })

    if (!is.null(tune_result)) {
      best_model <- tune_result$best.model
    } else {
      best_model <- svm(
        x = X_train,
        y = y_train,
        kernel = kernel_type,
        probability = TRUE,
        class.weights = class_weights
      )
    }
  } else {
    best_model <- svm(
      x = X_train,
      y = y_train,
      kernel = kernel_type,
      probability = TRUE,
      class.weights = class_weights
    )
  }

  # Check if nSV is 0
  if (is.null(best_model$nSV) || all(best_model$nSV == 0)) {
    message("No support vectors found. Model may not have converged properly.")
  }

  # Generate predictions for test set (if provided)
  if (!is.null(X_test) && !is.null(y_test)) {
    if (is.null(dim(X_test))) {
      X_test <- as.data.frame(t(X_test))
    }
    X_test <- as.data.frame(X_test)
    colnames(X_test) <- colnames(X_train)
    test_pred <- predict(best_model, X_test, probability = TRUE)
    prob_attr <- attr(test_pred, "probabilities")
    # Ensure that probability exists
    if (!is.null(prob_attr) && "1" %in% colnames(prob_attr)) {
      pred_prob <- prob_attr[, "1"]
    } else {
      pred_prob <- rep(0.5, length(test_pred))
    }
  } else {
    test_pred <- NULL
    pred_prob <- NULL
  }

  # Return results in same format as NN_performance()
  return(list(
    model_name = "SVM",
    predictions = test_pred,
    pred_prob = pred_prob,
    model = best_model
  ))
}

# Example usage (commented out)

# svm_result <- train_svm(
#   X_train = result$X_train,
#   y_train = result$y_train,
#   X_test  = result$X_test,
#   y_test  = result$y_test,
#   tune = TRUE
# )
#
# # Access results
# svm_result$predictions
# svm_result$pred_prob
#
# head(svm_result$predictions)
# head(svm_result$pred_prob)
