#' Decision Tree Model
#'
#' This function trains a decision tree classifier using 10-fold cross-validation to
#' tune the complexity parameter (cp). It automatically identifies and removes
#' zero-variance features (columns with only one unique value) from the datasets.
#' Finally, it evaluates the model on a testing set and plots the tree structure.
#'
#' @param X_train A data frame containing the training set features.
#' @param y_train A vector containing the training set labels (numeric 0/1 or factor).
#' @param X_test A data frame containing the testing set features.
#' @param y_test A vector containing the testing set labels (numeric 0/1 or factor).
#' @param cp A numeric value specifying the default complexity parameter. Default is 0.01.
#'   Note: The function still performs grid search tuning between 0.001 and 0.1.
#'
#' @return A list containing the following Decision Tree components and evaluation results:
#' \itemize{
#'   \item \code{model}: The final trained decision tree model object (`rpart` object).
#'   \item \code{predictions}: A factor vector of predicted class labels for the test set.
#'   \item \code{metrics}: A \code{confusionMatrix} object containing detailed performance metrics.
#'   \item \code{pred_prob}: A numeric vector of predicted probabilities for the positive class.
#'   \item \code{model_name}: A character string indicating the model name ("Decision Tree").
#' }
#'
#' @author Xinyi Hu
#'
#' @import caret
#' @import rpart
#' @import rpart.plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming X_train, y_train, X_test, and y_test are already prepared
#' dt_results <- decision_tree(X_train, y_train, X_test, y_test)
#'
#' # View the overall accuracy
#' print(dt_results$metrics$overall["Accuracy"])
#' }
decision_tree <- function(X_train, y_train, X_test, y_test, cp = 0.01) {

  # 1. Data restructuring
  # Drop columns with only one character
  valid_cols <- sapply(X_train, function(x) length(unique(na.omit(x))) > 1)
  removed_cols <- names(valid_cols)[!valid_cols]
  if (length(removed_cols) > 0) {
    message("Removed columns: ", paste(removed_cols, collapse = ", "))
  }
  X_train <- X_train[, valid_cols, drop = FALSE]
  X_test <- X_test[, valid_cols, drop = FALSE]
  # To ensure generality for classification, force the target variable to be a factor
  # Rename the label to a unified "target"
  y_train <- as.factor(y_train)
  y_test <- as.factor(y_test)
  train_data <- data.frame(X_train, target = y_train)
  test_data  <- data.frame(X_test, target = y_test)

  # 2. Model training
  print("Training Decision Tree Model...")

  # Set up 10-fold cross-validation
  train_control <- trainControl(method = "cv", number = 10)

  cv_model <- train(
    x = X_train,
    y = y_train,
    method = "rpart",
    trControl = train_control,
    # Find the best Complexity Parameter `cp` between 0.001 and 0.1
    tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.005)),
    # Prevent the tree growing infinitely
    # Set minimum split and maximum depth
    control = rpart.control(minsplit = 20, maxdepth = 15)
  )

  # Extract the best decision tree
  dt_model <- cv_model$finalModel
  print(paste("Best tuned cp selected by model:", cv_model$bestTune$cp))

  # 3. Model Prediction
  # Return the predicted class labels
  predictions <- predict(cv_model, newdata = X_test)
  # Return the predicted probability of belonging to each class
  pred_prob <- predict(cv_model, newdata = X_test, type = "prob")

  # 4. Performance Evaluation
  # Ensure prediction and y_test factor levels are consistent to avoid caret errors
  # Caret errors occur when the test set is small so that it only contains partial levels
  levels_pred <- levels(predictions)
  test_target <- factor(test_data$target, levels = levels_pred)

  conf_matrix <- confusionMatrix(predictions, test_target) # Compare test results with predictions

  # Print core evaluation results
  print("===== Decision Tree Evaluation =====")
  cat("Accuracy :", round(conf_matrix$overall["Accuracy"], 4), "\n")
  cat("Kappa    :", round(conf_matrix$overall["Kappa"], 4), "\n") # avoid the imbalance of datasets
  print(conf_matrix$table)

  # 5. Plot the decision tree structure
  rpart.plot(dt_model, main = "Decision Tree Structure", type = 4, extra = 104)

  # Return the result
  return(list(
    model = dt_model,
    predictions = predictions,
    pred_prob = pred_prob[, ncol(pred_prob)],
    model_name = "Decision Tree"
  ))
}
