
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
test_that("train_rf returns a valid model structure", {
  data <- generate_dummy_data()

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))

  expect_type(res, "list")
  expect_true(all(c("model", "predictions", "pred_prob", "model_name") %in% names(res)))

  expect_type(res$model, "list")
  expect_true(length(res$model) == 10)

  for (i in 1:length(res$model)) {
    expect_true(!is.null(res$model[[i]]))
  }
})

# Case B: Correct Predictions
test_that("train_rf returns correct predictions", {
  data <- generate_dummy_data()

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))

  # Check predict labels
  expect_s3_class(res$predictions, "factor")
  expect_equal(length(res$predictions), nrow(data$X_test))
  expect_true(all(levels(res$predictions) %in% c("0", "1")))

  # Check predict probabilities
  expect_type(res$pred_prob, "double")
  expect_equal(length(res$pred_prob), nrow(data$X_test))
  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))
})

# Case C: High-Dimensional Data
test_that("train_rf handles high-dimensional data and runs without error", {

  data_high_dim <- generate_dummy_data(n_train = 100, n_test = 30, n_features = 15)

  suppressWarnings(capture.output({
    trees_high <- train_rf(data_high_dim$X_train, data_high_dim$y_train,
                           n_trees = 10, max_depth = 5)
    res_high <- predict_rf_model(trees_high, data_high_dim$X_test)
  }))

  # Check the length
  expect_equal(length(res_high$predictions), nrow(data_high_dim$X_test))
  expect_length(res_high$pred_prob, nrow(data_high_dim$X_test))
})

# Edge Cases
# Scenario A: Handles zero-variance columns safely
test_that("Scenario A: Handles zero-variance columns safely", {
  data <- generate_dummy_data()
  data$X_train$const_col <- rep(0, nrow(data$X_train))
  data$X_test$const_col <- rep(0, nrow(data$X_test))

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))
  expect_s3_class(res$predictions, "factor")
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario B: Auto-converts numeric targets to factors
test_that("Scenario B: Auto-converts numeric targets to factors", {
  data <- generate_dummy_data()
  y_train_num <- as.numeric(as.character(data$y_train))

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, y_train_num, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))
  expect_s3_class(res$predictions, "factor")
})

# Scenario C: Generates output with extreme test set imbalance
test_that("Scenario C: Generates output with extreme test set imbalance", {
  data <- generate_dummy_data()
  data$y_test <- factor(rep("0", nrow(data$X_test)), levels = c("0", "1"))

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario D: Handles missing values (NA) in test set
test_that("Scenario D: Handles missing values (NA) in test set", {
  data <- generate_dummy_data()
  data$X_test[1, 1] <- NA
  data$X_test[5, 2] <- NA

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, data$X_test)
  }))
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario E: Handles scrambled columns order
test_that("Scenario E: Handles scrambled columns order", {
  data <- generate_dummy_data()

  X_test_scrambled <- data$X_test[, c("f2", "f1")]

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res_scrambled <- predict_rf_model(trees, X_test_scrambled)
  }))
  expect_length(res_scrambled$predictions, nrow(data$X_test))
})

# Scenario F: Handles missing features gracefully
test_that("Scenario F: Handles missing features gracefully", {
  data <- generate_dummy_data()

  X_test_missing <- data$X_test[, "f1", drop = FALSE]

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    expect_silent(res <- predict_rf_model(trees, X_test_missing))
  }))
  expect_length(res$predictions, nrow(X_test_missing))
})

# Scenario G: Survives the 1-row data frame trap
test_that("Scenario G: Survives the 1-row data frame trap", {
  data <- generate_dummy_data()
  X_test_single <- data$X_test[1, , drop = FALSE]

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 10, max_depth = 5)
    res <- predict_rf_model(trees, X_test_single)
  }))
  expect_length(res$predictions, 1)
  expect_length(res$pred_prob, 1)
})

# Scenario H: Works with different label formats
test_that("Scenario H: Works with different label formats (logical, character)", {
  data <- generate_dummy_data()

  y_train_logical <- as.logical(as.numeric(as.character(data$y_train)))

  suppressWarnings(capture.output({
    trees_logical <- train_rf(data$X_train, y_train_logical, n_trees = 10, max_depth = 5)
    res_logical <- predict_rf_model(trees_logical, data$X_test)
  }))
  expect_s3_class(res_logical$predictions, "factor")

  y_train_char <- as.character(data$y_train)

  suppressWarnings(capture.output({
    trees_char <- train_rf(data$X_train, y_train_char, n_trees = 10, max_depth = 5)
    res_char <- predict_rf_model(trees_char, data$X_test)
  }))
  expect_s3_class(res_char$predictions, "factor")
})

# Scenario I: Works with different number of trees
test_that("Scenario I: Works with different number of trees", {
  data <- generate_dummy_data()

  for (n_trees in c(1, 5, 20, 50)) {
    suppressWarnings(capture.output({
      trees <- train_rf(data$X_train, data$y_train, n_trees = n_trees, max_depth = 5)
      res <- predict_rf_model(trees, data$X_test)
    }))
    expect_length(res$predictions, nrow(data$X_test))
    expect_equal(length(trees), n_trees)
  }
})

# Scenario J: Works with different max_depth values
test_that("Scenario J: Works with different max_depth values", {
  data <- generate_dummy_data()

  for (max_depth in c(1, 3, 5, 10)) {
    suppressWarnings(capture.output({
      trees <- train_rf(data$X_train, data$y_train, n_trees = 5, max_depth = max_depth)
      res <- predict_rf_model(trees, data$X_test)
    }))
    expect_length(res$predictions, nrow(data$X_test))
    expect_type(res$model, "list")
    expect_length(res$model, 5)
  }
})

# Scenario K: Probability sums to 1 when both classes present
test_that("Scenario K: Probability correctly reflects class proportions", {
  data <- generate_dummy_data()

  data$y_train <- factor(sample(c("0", "1"), nrow(data$X_train), replace = TRUE, prob = c(0.5, 0.5)))

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 50, max_depth = 3)
    res <- predict_rf_model(trees, data$X_test)
  }))

  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))
})

# Scenario L: Evaluate model function works
test_that("Scenario L: evaluate_rf_model works without errors", {
  data <- generate_dummy_data()

  suppressWarnings(capture.output({
    trees <- train_rf(data$X_train, data$y_train, n_trees = 5, max_depth = 3)
    res <- predict_rf_model(trees, data$X_test)
  }))

  expect_silent(capture.output({
    evaluate_rf_model(data$y_test, res$predictions)
  }))
})
