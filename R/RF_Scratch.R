# Gini计算----
gini_impurity <- function(y) {
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}

# 找最佳分裂----
best_split <- function(X, y, mtry) {

  features <- sample(colnames(X), mtry)  # 随机选特征

  best_feature <- NULL
  best_value <- NULL
  best_gini <- Inf

  for (feature in features) {

    values <- unique(X[[feature]])

    for (v in values) {

      left_idx  <- X[[feature]] <= v
      right_idx <- X[[feature]] > v

      if (sum(left_idx) == 0 || sum(right_idx) == 0) next

      gini_left  <- gini_impurity(y[left_idx])
      gini_right <- gini_impurity(y[right_idx])

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

# 构建决策树----
build_tree <- function(X, y, depth = 0, max_depth = 5, mtry = NULL) {

  # 停止条件
  if (length(unique(y)) == 1 || depth >= max_depth) {
    label <- names(sort(table(y), decreasing = TRUE))[1]
    return(list(label = label))
  }

  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(X)))
  }

  split <- best_split(X, y, mtry)

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


# 单棵树预测----
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

# 随机森林训练----
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

# RF预测----
predict_rf <- function(trees, X) {
  results <- lapply(1:nrow(X), function(i){

  votes <- sapply(trees, function(tree) {
    predict_tree(tree, X[i, ])
  })

  vote_table <- table(votes)

  pred_class <- names(sort(vote_table, decreasing = TRUE))[1]

  # 假设正类是1，正类概率
  prob_1 <- ifelse("1" %in% names(vote_table),
                   vote_table["1"] / length(trees),
                   0)

  return(list(class = pred_class, prob = prob_1))
})
  # 提取
  predictions <- as.factor(sapply(results, function(x) x$class))
  pred_prob   <- as.numeric(sapply(results, function(x) x$prob))

  return(list(
    model_name = "Random Forest",
    predictions = predictions,
    pred_prob = pred_prob,
    model = rf_scratch
  ))
}

# 评估函数----
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

# 测试----

rf_scratch <- train_rf_scratch(
  X = result$X_train_norm,
  y = result$y_train,
  n_trees = 20,
  max_depth = 5
)

# 调用

pred_result <- predict_rf(rf_scratch, result$X_test_norm)

predictions <- pred_result$predictions
pred_prob   <- pred_result$pred_prob

evaluate_model(result$y_test, predictions)
pred_prob
