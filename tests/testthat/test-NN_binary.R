# Helper Function: Generate matrices for Neural Network (features as rows, samples as columns)
generate_nn_data <- function(n_train = 50, n_test = 20, n_features = 3) {
  set.seed(123)

  # X matrices: n_features rows, n_samples columns
  X_train <- matrix(rnorm(n_features * n_train), nrow = n_features, ncol = n_train)
  X_test  <- matrix(rnorm(n_features * n_test),  nrow = n_features, ncol = n_test)

  # y matrices: 1 row, n_samples columns (0 or 1)
  y_train <- matrix(sample(0:1, n_train, replace = TRUE), nrow = 1, ncol = n_train)
  y_test  <- matrix(sample(0:1, n_test, replace = TRUE),  nrow = 1, ncol = n_test)

  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test, hidden = 4))
}

# Base Cases
test_that("trainModel returns valid parameters and cost history", {
  data <- generate_nn_data()

  # Train model (verbose = FALSE to keep test console clean)
  capture.output({
    nn_model <- trainModel(data$X_train, data$y_train, num_iteration = 10,
                           hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
  })

  # Check structure
  expect_type(nn_model, "list")
  expect_true(all(c("updated_params", "cost_hist") %in% names(nn_model)))

  # Check parameters matrices dimensions
  params <- nn_model$updated_params
  expect_equal(dim(params$W1), c(data$hidden, nrow(data$X_train)))
  expect_equal(dim(params$W2), c(1, data$hidden))

  # Cost history should have length equal to num_iteration
  expect_length(nn_model$cost_hist, 10)
})

test_that("NN_performance returns valid predictions and probabilities", {
  data <- generate_nn_data()

  capture.output({
    nn_model <- trainModel(data$X_train, data$y_train, num_iteration = 10,
                           hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
    res <- NN_performance(model_name = "Test_NN", test_data = data$X_test,
                          train_model = nn_model, hidden_neurons = data$hidden)
  })

  # Check output structure
  expect_type(res, "list")
  expect_equal(res$model_name, "Test_NN")

  # Predictions must be factor and have length equal to test samples
  expect_s3_class(res$predictions, "factor")
  expect_equal(length(res$predictions), ncol(data$X_test))

  # Probabilities must be numeric and bounded between 0 and 1
  expect_type(res$pred_prob, "double")
  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))
})

# Edge Cases
# Scenario A: Extreme class imbalance triggers custom warning
test_that("Scenario A: Warns when training data is completely imbalanced (only one class)", {
  data <- generate_nn_data()
  # Force all training labels to be 0
  data$y_train[1, ] <- 0

  capture.output({
    suppressWarnings(
      expect_warning(
        trainModel(data$X_train, data$y_train, num_iteration = 5,
                   hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE),
        "missing category, using equal weights"
      )
    )
  })
})

# Scenario B: Survives the 1-sample trap (Matrix drop issue)
test_that("Scenario B: Predicts successfully on a single sample", {
  data <- generate_nn_data()

  capture.output({
    nn_model <- trainModel(data$X_train, data$y_train, num_iteration = 5,
                           hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
  })

  # Extract exactly 1 sample as a column matrix (MUST use drop=FALSE in R)
  X_test_single <- data$X_test[, 1, drop = FALSE]

  pred <- makePrediction(X_test_single, nn_model$updated_params, data$hidden)

  # Should return exactly 1 probability without matrix dimension crash
  expect_length(as.numeric(pred), 1)
})

# Scenario C: Matrix dimension mismatch error (Missing feature)
test_that("Scenario C: Errors out on feature dimension mismatch", {
  data <- generate_nn_data()

  capture.output({
    nn_model <- trainModel(data$X_train, data$y_train, num_iteration = 5,
                           hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
  })

  # Drop one feature row from test set (e.g., test has 2 features, train had 3)
  X_test_missing_feature <- data$X_test[-1, , drop = FALSE]

  # Matrix multiplication W1 %*% X will inherently fail here
  expect_error(makePrediction(X_test_missing_feature, nn_model$updated_params, data$hidden))
})

# Scenario D: Zero variance feature doesn't crash calculations
test_that("Scenario D: Calculates smoothly even with a zero-variance feature", {
  data <- generate_nn_data()

  # Force the first feature (row) to be all zeros
  data$X_train[1, ] <- 0

  capture.output({
    nn_model <- trainModel(data$X_train, data$y_train, num_iteration = 5,
                           hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
  })

  # Check if the cost history generated valid numbers (no NaNs caused by 0 variance)
  expect_true(all(!is.nan(nn_model$cost_hist)))
})

# Scenario E: Character data trap
test_that("Scenario H: Neural network fails gracefully when character data is present", {
  # 1. Prepare data with a character column
  data <- generate_nn_data()
  # Convert X_train to a data frame and add a character column
  X_df <- as.data.frame(t(data$X_train)) # Transpose for data frame format
  X_df$char_col <- sample(c("A", "B", "C"), nrow(X_df), replace = TRUE)

  # 2. Re-transpose back to Neural Network format (features as rows)
  X_train_char <- t(as.matrix(X_df))

  # 3. Expect Error: Matrix multiplication (%*%) will fail with character strings
  capture.output({
    expect_error(
      trainModel(X_train_char, data$y_train, num_iteration = 5,
                 hidden_neurons = data$hidden, lr = 0.01, verbose = FALSE)
    )
  })
})
