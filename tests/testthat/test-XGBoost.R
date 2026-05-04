
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
test_that("train_xgb returns a valid model structure", {
  data <- generate_dummy_data()

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  expect_type(res, "list")
  expect_true(all(c("model", "predictions", "pred_prob", "model_name",
                    "best_threshold", "importance") %in% names(res)))

  expect_s3_class(res$model, "xgb.Booster")

  expect_true(!is.null(res$importance))
  expect_true(nrow(res$importance) > 0)
})

# Case B: Correct Predictions
test_that("train_xgb returns correct predictions", {
  data <- generate_dummy_data()

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  # 检查预测标签
  expect_s3_class(res$predictions, "factor")
  expect_equal(length(res$predictions), nrow(data$X_test))

  # 检查预测概率
  expect_type(res$pred_prob, "double")
  expect_equal(length(res$pred_prob), nrow(data$X_test))
  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))

  # 检查最佳阈值
  expect_type(res$best_threshold, "double")
  expect_true(res$best_threshold >= 0.1 && res$best_threshold <= 0.9)
})

# Case C: High-Dimensional Data
test_that("train_xgb handles high-dimensional data and runs without error", {
  data_high_dim <- generate_dummy_data(n_train = 100, n_test = 30, n_features = 15)

  capture.output({
    res_high <- train_xgb(data_high_dim$X_train, data_high_dim$y_train,
                          data_high_dim$X_test, data_high_dim$y_test)
  })

  # Check the length
  expect_equal(length(res_high$predictions), nrow(data_high_dim$X_test))
  expect_true(nrow(res_high$importance) > 0)
})

# Edge Cases
# Scenario A: Handles zero-variance columns safely
test_that("Scenario A: Handles zero-variance columns safely", {
  data <- generate_dummy_data()
  data$X_train$const_col <- rep(0, nrow(data$X_train))
  data$X_test$const_col <- rep(0, nrow(data$X_test))

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_s3_class(res$model, "xgb.Booster")
})

# Scenario B: Auto-converts numeric targets to factors
test_that("Scenario B: Auto-converts numeric targets to factors", {
  data <- generate_dummy_data()
  y_train_num <- as.numeric(as.character(data$y_train))
  y_test_num <- as.numeric(as.character(data$y_test))

  capture.output({
    res <- train_xgb(data$X_train, y_train_num, data$X_test, y_test_num)
  })
  expect_s3_class(res$predictions, "factor")
})

# Scenario C: Generates output with extreme test set imbalance
test_that("Scenario C: Generates output with extreme test set imbalance", {
  data <- generate_dummy_data()
  data$y_test <- factor(rep("0", nrow(data$X_test)), levels = c("0", "1"))

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario D: Handles missing values (NA) in test set
test_that("Scenario D: Handles missing values (NA) in test set", {
  data <- generate_dummy_data()
  data$X_test[1, 1] <- NA
  data$X_test[5, 2] <- NA

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario E: Handles scrambled columns order
test_that("Scenario E: Handles scrambled columns order", {
  data <- generate_dummy_data()

  X_test_scrambled <- data$X_test[, c("f2", "f1")]

  capture.output({
    res_scrambled <- train_xgb(data$X_train, data$y_train, X_test_scrambled, data$y_test)
  })
  expect_length(res_scrambled$predictions, nrow(data$X_test))
})

# Scenario F: Works with subset of features (uses available columns)
test_that("Scenario F: Works with subset of features", {
  data <- generate_dummy_data()

  X_test_missing <- data$X_test[, "f1", drop = FALSE]

  expect_silent(capture.output({
    res <- train_xgb(data$X_train, data$y_train, X_test_missing, data$y_test)
  }))
  expect_length(res$predictions, nrow(X_test_missing))
})

# Scenario G: Survives the 1-row data frame trap
test_that("Scenario G: Survives the 1-row data frame trap", {
  data <- generate_dummy_data()
  X_test_single <- data$X_test[1, , drop = FALSE]
  y_test_single <- data$y_test[1]

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, X_test_single, y_test_single)
  })
  expect_length(res$predictions, 1)
})

# Scenario H: Errors out on unseen factor levels in target
test_that("Scenario H: Errors out on unseen factor levels in target", {
  data <- generate_dummy_data()
  y_test_alien <- factor(sample(c("0", "1", "2"), nrow(data$X_test), replace = TRUE),
                         levels = c("0", "1", "2"))

  capture.output({
    expect_error(train_xgb(data$X_train, data$y_train, data$X_test, y_test_alien))
  })
})

# Scenario I: Works with different label formats
test_that("Scenario I: Works with different label formats (logical, character)", {
  data <- generate_dummy_data()

  # logical labels
  y_train_logical <- as.logical(as.numeric(as.character(data$y_train)))
  y_test_logical <- as.logical(as.numeric(as.character(data$y_test)))

  capture.output({
    res_logical <- train_xgb(data$X_train, y_train_logical, data$X_test, y_test_logical)
  })
  expect_s3_class(res_logical$predictions, "factor")

  # character labels
  y_train_char <- as.character(data$y_train)
  y_test_char <- as.character(data$y_test)

  capture.output({
    suppressWarnings(res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test))
    res_char <- train_xgb(data$X_train, y_train_char, data$X_test, y_test_char)
  })
  expect_s3_class(res_char$predictions, "factor")
})

# Scenario J: Returns feature importance properly
test_that("Scenario J: Returns feature importance properly", {
  data <- generate_dummy_data(n_features = 5)

  capture.output({
    res <- train_xgb(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  expect_true(is.data.frame(res$importance))
  expect_true(all(c("Feature", "Gain", "Cover", "Frequency") %in% colnames(res$importance)))
  expect_true(nrow(res$importance) <= ncol(data$X_train))
})
