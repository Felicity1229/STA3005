# Install Packages
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

library(rpart)
library(rpart.plot)
library(caret)

#' Decision Tree
#' @param X_train Training set features (data.frame)
#' @param y_train Training set labels (vector, 0/1 numeric or factor)
#' @param y_train Training set labels (vector, 0/1 numeric or factor)
#' @param X_test Testing set features (data.frame)
#' @param y_test Testing set labels (vector, 0/1 numeric or factor)
#' @return Returns a list containing the trained model object, predictions, and detailed evaluation metrics
decision_tree <- function(X_train, y_train, X_test, y_test, cp = 0.01) {

  # 1. Data restructuring
  # To ensure generality for classification, force the target variable to be a factor
  # Rename the label to a unified "target"
  train_data <- data.frame(X_train, target = as.factor(y_train))
  test_data  <- data.frame(X_test, target = as.factor(y_test))

  # 2. Model training
  print("Training Decision Tree Model...")

  # Set up 10-fold cross-validation
  train_control <- trainControl(method = "cv", number = 10)

  cv_model <- train(
    target ~ .,
    data = train_data,
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
  predictions <- predict(cv_model, newdata = test_data, type = "class")
  # Return the predicted probability of belonging to each class
  pred_prob <- predict(cv_model, newdata = test_data, type = "prob")

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
    metrics = conf_matrix,
    pred_prob = pred_prob[, 2],
    model_name = "Decision Tree"
  ))
}
