# 辅助函数：生成基础干净的测试数据，支持自定义特征维度
generate_dummy_data <- function(n_train = 50, n_test = 20, n_features = 2) {
  set.seed(123)
  X_train <- as.data.frame(matrix(rnorm(n_train * n_features), ncol = n_features))
  colnames(X_train) <- paste0("f", 1:n_features)
  y_train <- factor(sample(c("0", "1"), n_train, replace = TRUE))

  X_test <- as.data.frame(matrix(rnorm(n_test * n_features), ncol = n_features))
  colnames(X_test) <- paste0("f", 1:n_features)
  y_test <- factor(sample(c("0", "1"), n_test, replace = TRUE))

  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

# Base Cases
# Case A: Valid Model Structure
test_that("train_svm returns a valid model structure", {
  data <- generate_dummy_data()

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = FALSE)
  })

  # 验证返回的是否是我们定义的 list 结构
  expect_type(res, "list")
  expect_true(all(c("model", "predictions", "pred_prob", "model_name") %in% names(res)))

  # 验证内部的 model 是否是正宗的 svm 对象
  expect_s3_class(res$model, "svm")

  # 深入检查 svm 模型的内部组件
  # 任何一棵成功的 svm，其内部必然包含 nSV (支持向量数量)
  expect_true(all(res$model$nSV > 0))
  expect_true(!is.null(res$model$gamma))
  expect_true(!is.null(res$model$cost))
})

# Case B: Correct Predictions
test_that("train_svm returns correct predictions", {
  data <- generate_dummy_data()

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = FALSE)
  })

  # 检查预测标签
  expect_s3_class(res$predictions, "factor")
  expect_equal(length(res$predictions), nrow(data$X_test))

  # 检查预测概率
  expect_type(res$pred_prob, "double")
  expect_equal(length(res$pred_prob), nrow(data$X_test))
  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))
})

# Case C: High-Dimensional Data
test_that("train_svm handles high-dimensional data and runs without error", {
  data_high_dim <- generate_dummy_data(n_train = 100, n_test = 30, n_features = 15)

  capture.output({
    res_high <- train_svm(data_high_dim$X_train, data_high_dim$y_train,
                          data_high_dim$X_test, data_high_dim$y_test,
                          tune = FALSE)
  })

  expect_equal(length(res_high$predictions), nrow(data_high_dim$X_test))
  expect_s3_class(res_high$model, "svm")
})

# Edge Cases
# Scenario A: Handles zero-variance columns safely
test_that("Scenario A: Handles zero-variance columns safely", {
  data <- generate_dummy_data()
  data$X_train$const_col <- rep(0, nrow(data$X_train))
  data$X_test$const_col <- rep(0, nrow(data$X_test))

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = FALSE)
  })
  expect_s3_class(res$model, "svm")
})

# Scenario B: Auto-converts numeric targets to factors
test_that("Scenario B: Auto-converts numeric targets to factors", {
  data <- generate_dummy_data()
  y_train_num <- as.numeric(as.character(data$y_train))
  y_test_num <- as.numeric(as.character(data$y_test))

  capture.output({
    res <- train_svm(data$X_train, y_train_num, data$X_test, y_test_num, tune = FALSE)
  })
  expect_s3_class(res$predictions, "factor")
})

# Scenario C: Generates output with extreme test set imbalance
test_that("Scenario C: Generates output with extreme test set imbalance", {
  data <- generate_dummy_data()
  data$y_test <- factor(rep("0", nrow(data$X_test)), levels = c("0", "1"))

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = FALSE)
  })
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario D: Handles missing values (NA) in test set - 修改期望长度
test_that("Scenario D: Handles missing values (NA) in test set", {
  data <- generate_dummy_data()
  data$X_test[1, 1] <- NA
  data$X_test[5, 2] <- NA

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = FALSE)
  })
  # 期望长度不变（因为填充了缺失值）
  expect_equal(length(res$predictions), nrow(data$X_test))
})

# Scenario E: Handles scrambled columns order
test_that("Scenario E: Handles scrambled columns order", {
  data <- generate_dummy_data()

  X_test_scrambled <- data$X_test[, c("f2", "f1")]

  capture.output({
    res_scrambled <- train_svm(data$X_train, data$y_train, X_test_scrambled, data$y_test, tune = FALSE)
  })
  expect_length(res_scrambled$predictions, nrow(data$X_test))
})

# Scenario F: Errors on missing features
test_that("Scenario F: Errors on missing features", {
  data <- generate_dummy_data()

  X_test_missing <- data$X_test[, "f1", drop = FALSE]

  capture.output({
    expect_error(train_svm(data$X_train, data$y_train, X_test_missing, data$y_test, tune = FALSE))
  })
})

# Scenario G: Survives the 1-row data frame trap
test_that("Scenario G: Survives the 1-row data frame trap", {
  data <- generate_dummy_data()
  X_test_single <- data$X_test[1, , drop = FALSE]
  y_test_single <- data$y_test[1]

  capture.output({
    res <- train_svm(data$X_train, data$y_train, X_test_single, y_test_single, tune = FALSE)
  })
  expect_length(res$predictions, 1)
})

# Scenario H: Errors out on unseen factor levels in target
test_that("Scenario H: Errors out on unseen factor levels in target", {
  data <- generate_dummy_data()
  y_test_alien <- factor(sample(c("0", "1", "2"), nrow(data$X_test), replace = TRUE),
                         levels = c("0", "1", "2"))

  capture.output({
    expect_error(train_svm(data$X_train, data$y_train, data$X_test, y_test_alien, tune = FALSE))
  })
})

# Scenario I: Works with tuning enabled
test_that("Scenario I: Works with hyperparameter tuning", {
  data <- generate_dummy_data(n_train = 50, n_test = 20, n_features = 3)

  capture.output({
    res <- train_svm(data$X_train, data$y_train, data$X_test, data$y_test, tune = TRUE)
  })

  expect_s3_class(res$model, "svm")
  expect_true(all(c("predictions", "pred_prob") %in% names(res)))
})

# Scenario J: Handles NULL test set gracefully
test_that("Scenario J: Handles NULL test set gracefully", {
  data <- generate_dummy_data()

  capture.output({
    res <- train_svm(data$X_train, data$y_train, X_test = NULL, y_test = NULL, tune = FALSE)
  })

  expect_s3_class(res$model, "svm")
  expect_null(res$predictions)
  expect_null(res$pred_prob)
})
