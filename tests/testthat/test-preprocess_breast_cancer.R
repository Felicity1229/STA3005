
load_breast_cancer_data <- function() {
  data("breast_cancer_dataset", package = "ClassifyR")
  return(breast_cancer_dataset)
}

# Base Cases
# Case A: Valid preprocessing structure
test_that("preprocessBreastCancerData returns a valid structure", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_type(result, "list")

  expected_names <- c("X_train", "y_train", "X_test", "y_test",
                      "train_data", "test_data")
  expect_true(all(expected_names %in% names(result)))

  expect_true(is.matrix(result$X_train))
  expect_true(is.matrix(result$X_test))
  expect_true(is.matrix(result$y_train))
  expect_true(is.matrix(result$y_test))
})

# Case B: Correct dimensions after splitting
test_that("preprocessBreastCancerData splits data correctly", {
  test_df <- load_breast_cancer_data()
  n_total <- nrow(test_df)
  train_ratio <- 0.8
  expected_train_size <- floor(n_total * train_ratio)
  expected_test_size <- n_total - expected_train_size

  result <- preprocessBreastCancerData(test_df, train_ratio = train_ratio)

  expect_equal(ncol(result$X_train), expected_train_size)
  expect_equal(ncol(result$X_test), expected_test_size)
  expect_equal(ncol(result$y_train), expected_train_size)
  expect_equal(ncol(result$y_test), expected_test_size)
})

# Case C: Target encoding works correctly
test_that("preprocessBreastCancerData encodes target correctly", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_true(all(result$y_train %in% c(0, 1)))
  expect_true(all(result$y_test %in% c(0, 1)))
})

# Edge Cases
# Scenario A: Handles different train ratios
test_that("Scenario A: Works with different train ratios", {
  test_df <- load_breast_cancer_data()
  n_total <- nrow(test_df)

  for (ratio in c(0.6, 0.7, 0.8, 0.9)) {
    result <- preprocessBreastCancerData(test_df, train_ratio = ratio)
    expected_train_size <- floor(n_total * ratio)

    expect_equal(ncol(result$X_train), expected_train_size)
    expect_equal(ncol(result$X_test), n_total - expected_train_size)
  }
})

# Scenario B: Feature engineering (Positive_Ratio) works
test_that("Scenario B: Positive_Ratio feature engineering works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_true("Positive_Ratio" %in% colnames(result$train_data))
  expect_true("Positive_Ratio" %in% colnames(result$test_data))

  expect_false("Regional.Node.Examined" %in% colnames(result$train_data))
  expect_false("Regional.Node.Positive" %in% colnames(result$train_data))
})

# Scenario C: One-hot encoding worked correctly
test_that("Scenario C: One-hot encoding for Race and Marital.Status works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_false("Race" %in% colnames(result$train_data))
  expect_false("Marital.Status" %in% colnames(result$train_data))

  one_hot_cols <- grep("^Race|^Marital.Status", colnames(result$train_data), value = TRUE)
  expect_true(length(one_hot_cols) > 0)
})

# Scenario D: Ordinal encoding works
test_that("Scenario D: Ordinal encoding works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  # T.Stage, N.Stage, X6th.Stage, differentiate should be numeric
  ordinal_cols <- c("T.Stage", "N.Stage", "X6th.Stage", "differentiate")
  for (col in ordinal_cols) {
    expect_true(is.numeric(result$train_data[[col]]))
    expect_true(is.numeric(result$test_data[[col]]))
  }
})

# Scenario E: Binary encoding works
test_that("Scenario E: Binary encoding works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  binary_cols <- c("Estrogen.Status", "Progesterone.Status", "A.Stage")
  for (col in binary_cols) {
    expect_true(all(result$train_data[[col]] %in% c(0, 1)))
    expect_true(all(result$test_data[[col]] %in% c(0, 1)))
  }
})

# Scenario F: Grade column cleaning works
test_that("Scenario F: Grade column cleaning works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  # Grade should be 1-4
  expect_true(is.numeric(result$train_data$Grade))
  expect_true(is.numeric(result$test_data$Grade))
  expect_true(all(result$train_data$Grade %in% c(1, 2, 3, 4, NA), na.rm = TRUE))
})

# Scenario G: Z-score scaling works
test_that("Scenario G: Z-score scaling works", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  scaled_cols <- c("Age", "Tumor.Size", "Survival.Months")
  for (col in scaled_cols) {
    scaled_vals <- result$train_data[[col]]
    expect_true(abs(mean(scaled_vals, na.rm = TRUE)) < 0.1)
    expect_true(abs(sd(scaled_vals, na.rm = TRUE) - 1) < 0.1)
  }
})

# Scenario H: Matrix orientation (features as rows, samples as columns)
test_that("Scenario H: Matrix orientation is correct", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  n_features_train <- ncol(result$train_data) - 1
  expect_equal(nrow(result$X_train), n_features_train)
  expect_equal(nrow(result$X_test), ncol(result$test_data) - 1)
})

# Scenario I: Works with different seed (reproducibility)
test_that("Scenario I: Reproducible with same seed", {
  test_df <- load_breast_cancer_data()

  result1 <- preprocessBreastCancerData(test_df, train_ratio = 0.8)
  result2 <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_equal(names(result1), names(result2))
  expect_equal(dim(result1$X_train), dim(result2$X_train))
})

# Scenario J: Labels are in last column after preprocessing
test_that("Scenario J: Status (label) is in last column", {
  test_df <- load_breast_cancer_data()

  result <- preprocessBreastCancerData(test_df, train_ratio = 0.8)

  expect_equal(names(result$train_data)[ncol(result$train_data)], "Status")
  expect_equal(names(result$test_data)[ncol(result$test_data)], "Status")
})
