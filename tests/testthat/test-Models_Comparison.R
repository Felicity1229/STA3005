# Helper Function: Create a mock models_list compatible with comparison
generate_mock_models <- function(n_samples = 100) {
  set.seed(456)
  y_true <- factor(sample(c("0", "1"), n_samples, replace = TRUE), levels = c("0", "1"))

  # Mock Model 1: High performance
  m1_preds <- y_true
  m1_probs <- ifelse(y_true == "1", runif(n_samples, 0.6, 0.9), runif(n_samples, 0.1, 0.4))

  # Mock Model 2: Random performance
  m2_preds <- factor(sample(c("0", "1"), n_samples, replace = TRUE), levels = c("0", "1"))
  m2_probs <- runif(n_samples)

  models_list <- list(
    "SuperModel" = list(predictions = m1_preds, pred_prob = m1_probs),
    "RandomModel" = list(predictions = m2_preds, pred_prob = m2_probs)
  )

  return(list(true_labels = y_true, models = models_list))
}

# Base Cases
test_that("models_comparison returns correct dataframe structure", {
  data <- generate_mock_models(50)

  # models_comparison prints 3 plots, use capture.output to keep console clean
  capture.output({
    result <- models_comparison(data$true_labels, data$models, positive_class = "1")
  })

  # Check if return value is a data.frame
  expect_s3_class(result, "data.frame")

  # Check if it has the expected number of rows (2 models)
  expect_equal(nrow(result), 2)

  # Check for mandatory columns
  required_cols <- c("Model", "Accuracy", "F1_Score", "Precision", "Recall", "AUC")
  expect_true(all(required_cols %in% colnames(result)))

  # AUC should be between 0 and 1
  expect_true(all(result$AUC >= 0 & result$AUC <= 1))
})

test_that("models_comparison handles side effects (plotting) without error", {
  data <- generate_mock_models(20)

  # Testing if the function runs without error despite the heavy ggplot2/pROC side effects
  # NA as the second argument to expect_error means "no error expected"
  capture.output({
    expect_error(models_comparison(data$true_labels, data$models), NA)
  })
})

# Edge Cases
# Scenario A: Handles a single model in the list
test_that("Scenario A: Handles a single model in the list", {
  data <- generate_mock_models(30)
  single_model <- data$models["SuperModel"] # List of length 1

  capture.output({
    result <- models_comparison(data$true_labels, single_model)
  })

  expect_equal(nrow(result), 1)
  expect_equal(result$Model[1], "SuperModel")
})

# Scenario B: Handles extreme class imbalance in true_labels
test_that("Scenario B: Handles extreme class imbalance in true_labels", {
  # All true labels are "1"
  y_true_imbalanced <- factor(rep("1", 40), levels = c("0", "1"))

  m_list <- list(
    "StableModel" = list(
      predictions = factor(rep("1", 40), levels = c("0", "1")),
      pred_prob = runif(40, 0.6, 0.8)
    )
  )

  capture.output({
    # 1. Use suppressWarnings to swallow the ggplot2 "Removed 1 row" warning
    suppressWarnings({
      # 2. Use expect_message to catch our intentional Note
      expect_message(
        res <- models_comparison(y_true_imbalanced, m_list),
        "Only one class present"
      )
    })
  })

  # AUC should be NA as per our defensive logic
  expect_true(is.na(res$AUC[1]))
  expect_equal(nrow(res), 1)
})

# Scenario C: Errors out if positive_class does not exist in labels
test_that("Scenario C: Errors out if positive_class does not exist in labels", {
  data <- generate_mock_models(20)

  # User provides a positive_class that is not "0" or "1"
  capture.output({
    # confusionMatrix will error because the positive class must be a level of the factor
    expect_error(models_comparison(data$true_labels, data$models, positive_class = "YES"))
  })
})

# Scenario D: Column name consistency in the heatmap data
test_that("Scenario D: Column name consistency in the heatmap data", {
  data <- generate_mock_models(10)

  # This tests the internal logic where we rename columns for the Faceted Heatmap
  # We check if the final output metrics match the model names provided
  capture.output({
    result <- models_comparison(data$true_labels, data$models)
  })

  # The "Model" column in result should match the names of our input list
  expect_setequal(result$Model, names(data$models))
})
