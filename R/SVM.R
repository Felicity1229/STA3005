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

  # Combine training and test data to ensure consistent dummy variable encoding
  full_data <- rbind(X_train, X_test)

  # Convert categorical variables to dummy variables (remove intercept)
  X_all <- model.matrix(~ . -1, data = full_data)

  # Split back into training and test sets
  X_train <- X_all[1:nrow(X_train), ]
  X_test  <- X_all[(nrow(X_train)+1):nrow(X_all), ]

  # Convert labels to factors (required by e1071::svm)
  y_train <- as.factor(y_train)
  y_test  <- as.factor(y_test)


  # Calculate inverse frequency class weights for imbalanced data
  # Formula: weight = total_samples / (n_classes * class_count)
  class_counts <- table(y_train)
  class_weights <- sum(class_counts) / (length(class_counts) * class_counts)


  # Hyperparameter tuning using 5-fold cross-validation
  if (tune) {

    tune_result <- tune(
      svm,
      train.x = X_train,
      train.y = y_train,
      kernel = kernel_type,
      ranges = list(
        cost = c(0.1, 1, 10),     # Regularization parameter
        gamma = c(0.01, 0.1, 1)   # Kernel width parameter (for radial kernel)
      ),
      class.weights = class_weights,
      probability = TRUE,         # Enable probability predictions
      tunecontrol = tune.control(cross = 5)  # 5-fold cross-validation
    )

    best_model <- tune_result$best.model

  } else {
    # Train without tuning, using default parameters
    best_model <- svm(
      x = X_train,
      y = y_train,
      kernel = kernel_type,
      probability = TRUE,
      class.weights = class_weights
    )
  }

  # Generate predictions for test set
  test_pred <- predict(best_model, X_test, probability = TRUE)

  # Extract probability attribute
  prob_attr <- attr(test_pred, "probabilities")

  # Get probability for the positive class (class "1")
  pred_prob <- prob_attr[, "1"]

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
