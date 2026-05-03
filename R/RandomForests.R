#' Random Forest for Binary Classification (Scratch Implementation)
#'
#' A collection of functions to build, train, and evaluate a Random Forest
#' model for binary classification tasks from scratch. Features include Gini
#' impurity splitting, bootstrap aggregation (bagging), and probability
#' estimation via majority voting.
#'
#' @name random_forest_model
#' @author ZHANG Yibing
#' @version 1.0
#' @date 2026-05-03
NULL

#' Compute Gini Impurity
#'
#' Calculates the Gini impurity of a set of labels, which measures the
#' probability of misclassifying a randomly chosen element.
#'
#' @param y Vector of class labels (0/1)
#' @return Gini impurity value. Lower values indicate purer nodes.
#'
#' @examples
#' gini_impurity(c(1,1,1,0,0))  # Returns 0.48
#' @export
gini_impurity <- function(y) {
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}

#' Find Best Split
#'
#' Randomly selects a subset of features (mtry) and finds the feature-value
#' split that minimizes the weighted Gini impurity.
#'
#' @param X Feature data frame. Each column is a feature, each row is a sample.
#' @param y Vector of class labels (0/1).
#' @param mtry Number of features randomly sampled as candidates for splitting.
#' @return A list containing:
#'   \item{feature}{Name of the best splitting feature}
#'   \item{value}{Threshold value for the best split}
#'
#' @export
best_split <- function(X, y, mtry) {

  features <- sample(colnames(X), mtry)  # Randomly select features

  best_feature <- NULL
  best_value <- NULL
  best_gini <- Inf

  for (feature in features) {

    values <- unique(X[[feature]])

    for (v in values) {

      left_idx  <- X[[feature]] <= v
      right_idx <- X[[feature]] > v

      # Skip splits that don't separate the data
      if (sum(left_idx) == 0 || sum(right_idx) == 0) next

      gini_left  <- gini_impurity(y[left_idx])
      gini_right <- gini_impurity(y[right_idx])

      # Weighted Gini impurity
      gini_total <- (sum(left_idx)/length(y)) * gini_left +
        (sum(right_idx)/length(y)) * gini_right

      if (gini_total < best_gini) {
        best_gini <- gini_total
        best_feature <- feature
        best_value <- v
      }
    }
  }

  return(list(feature = best_feature, value = best_value))
}

#' Build Decision Tree
#'
#' Recursively builds a binary decision tree using Gini impurity as the
#' splitting criterion. Stopping occurs when either max_depth is reached
#' or the node becomes pure.
#'
#' @param X Feature data frame. Each column is a feature, each row is a sample.
#' @param y Vector of class labels (0/1).
#' @param depth Current depth in the tree (starts at 0).
#' @param max_depth Maximum depth of the tree.
#' @param mtry Number of features to consider at each split. Default is
#'        floor(sqrt(ncol(X))).
#' @return A list representing a decision tree node:
#'   \item{label}{If leaf node, the predicted class}
#'   \item{feature}{If internal node, the splitting feature name}
#'   \item{value}{If internal node, the splitting threshold}
#'   \item{left}{Left child subtree}
#'   \item{right}{Right child subtree}
#'
#' @export
build_tree <- function(X, y, depth = 0, max_depth = 5, mtry = NULL) {

  # Stopping criteria: pure node or max depth reached
  if (length(unique(y)) == 1 || depth >= max_depth) {
    label <- names(sort(table(y), decreasing = TRUE))[1]
    return(list(label = label))
  }

  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(X)))
  }

  split <- best_split(X, y, mtry)

  # If no valid split found, create leaf node
  if (is.null(split$feature)) {
    label <- names(sort(table(y), decreasing = TRUE))[1]
    return(list(label = label))
  }

  left_idx  <- X[[split$feature]] <= split$value
  right_idx <- X[[split$feature]] > split$value

  return(list(
    feature = split$feature,
    value = split$value,
    left = build_tree(X[left_idx, ], y[left_idx], depth + 1, max_depth, mtry),
    right = build_tree(X[right_idx, ], y[right_idx], depth + 1, max_depth, mtry)
  ))
}


#' Predict with Single Decision Tree
#'
#' Traverses a decision tree to make a prediction for a single sample.
#'
#' @param tree A decision tree object from \code{\link{build_tree}}
#' @param x A single-row data frame or named vector of features
#' @return Predicted class label (0 or 1)
#'
#' @export
predict_tree <- function(tree, x) {

  if (!is.null(tree$label)) {
    return(tree$label)
  }

  if (x[[tree$feature]] <= tree$value) {
    return(predict_tree(tree$left, x))
  } else {
    return(predict_tree(tree$right, x))
  }
}

#' Train Random Forest from Scratch
#'
#' Builds an ensemble of decision trees using bootstrap aggregation (bagging).
#' Each tree is trained on a bootstrap sample of the original data.
#'
#' @param X Feature data frame. Each column is a feature, each row is a sample.
#' @param y Vector of class labels (0/1).
#' @param n_trees Number of trees in the forest. Default is 20.
#' @param max_depth Maximum depth of each tree. Default is 5.
#' @return A list of decision trees (length = n_trees)
#'
#' @export
train_rf_scratch <- function(X, y, n_trees = 20, max_depth = 5) {

  trees <- list()

  for (i in 1:n_trees) {

    idx <- sample(1:nrow(X), replace = TRUE)  # bootstrap

    X_sample <- X[idx, ]
    y_sample <- y[idx]

    tree <- build_tree(X_sample, y_sample, max_depth = max_depth)

    trees[[i]] <- tree
  }

  return(trees)
}

#' Random Forest Prediction
#'
#' Makes predictions for test data using a trained random forest. Returns
#' both the majority vote class and the probability of belonging to class 1.
#'
#' @param trees List of decision trees from \code{\link{train_rf_scratch}}
#' @param X Test feature data frame. Each row is a sample.
#' @return A list containing:
#'   \item{model_name}{Character string "Random Forest"}
#'   \item{predictions}{Factor vector of predicted classes (0/1)}
#'   \item{pred_prob}{Numeric vector of prediction probabilities for class "1"}
#'   \item{model}{The trained random forest model (list of trees)}
#'
#' @export
predict_rf <- function(trees, X) {
  results <- lapply(1:nrow(X), function(i){

    votes <- sapply(trees, function(tree) {
      predict_tree(tree, X[i, ])
    })

    vote_table <- table(votes)

    # Majority vote class
    pred_class <- names(sort(vote_table, decreasing = TRUE))[1]

    # Probability for positive class (class 1)
    prob_1 <- ifelse("1" %in% names(vote_table),
                     vote_table["1"] / length(trees),
                     0)

    return(list(class = pred_class, prob = prob_1))
  })
  # Extract results
  predictions <- as.factor(sapply(results, function(x) x$class))
  pred_prob   <- as.numeric(sapply(results, function(x) x$prob))

  return(list(
    model_name = "Random Forest",
    predictions = predictions,
    pred_prob = pred_prob,
    model = rf_scratch
  ))
}

#' Evaluate Model Performance
#'
#' Computes classification metrics including confusion matrix, accuracy,
#' precision, recall, and F1 score.
#'
#' @param y_true Actual class labels (0/1)
#' @param y_pred Predicted class labels (0/1)
#' @return Prints confusion matrix and metrics to console. No return value.
#'
#' @examples
#' y_true <- c(1,1,0,0,1,0)
#' y_pred <- c(1,0,0,0,1,1)
#' evaluate_model(y_true, y_pred)
#' @export
evaluate_model <- function(y_true, y_pred) {

  cm <- table(Predicted = y_pred, Actual = y_true)
  print(cm)

  TP <- cm["1","1"]
  FP <- cm["1","0"]
  FN <- cm["0","1"]
  TN <- cm["0","0"]

  acc <- (TP + TN) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2 * precision * recall / (precision + recall)

  cat("\nAccuracy:", round(acc, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall:", round(recall, 4), "\n")
  cat("F1 Score:", round(f1, 4), "\n")
}


# Example usage (commented out)

# rf_scratch <- train_rf_scratch(
#   X = result$X_train_norm,
#   y = result$y_train,
#   n_trees = 20,
#   max_depth = 5
# )
#
# rf_result <- predict_rf(rf_scratch, result$X_test_norm)
#
# predictions <- rf_result$predictions
# pred_prob   <- rf_result$pred_prob
#
# evaluate_model(result$y_test, predictions)
# head(pred_prob)
