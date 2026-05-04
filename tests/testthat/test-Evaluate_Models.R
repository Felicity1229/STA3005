# Helper Function: Create a mock single model result
generate_mock_single_model <- function(n_samples = 60) {
  set.seed(789)
  y_true <- factor(sample(c("0", "1"), n_samples, replace = TRUE), levels = c("0", "1"))

  # Mock high-performance predictions
  preds <- y_true
  # Inject some random noise to make it realistic
  noise_idx <- sample(1:n_samples, size = floor(n_samples * 0.1))
  preds[noise_idx] <- factor(ifelse(y_true[noise_idx] == "1", "0", "1"), levels = c("0", "1"))

  probs <- ifelse(y_true == "1", runif(n_samples, 0.7, 1.0), runif(n_samples, 0.0, 0.3))

  model_result <- list(
    model_name = "MockDecisionTree",
    predictions = preds,
    pred_prob = probs
  )

  return(list(true_labels = y_true, model_result = model_result))
}

# Base Cases
test_that("evaluate_models returns correct dataframe and handles graphics", {
  data <- generate_mock_single_model(40)

  # capture.output is used to silence messages and prevent console flooding
  capture.output({
    # We expect the function to run and produce plots without error
    expect_error(
      res <- evaluate_models(data$true_labels, data$model_result, positive_class = "1"),
      NA
    )
  })

  # Check structure
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Model[1], "MockDecisionTree")

  # Verify metrics are within logical bounds [0, 1]
  metrics_to_check <- c("Accuracy", "F1_Score", "Precision", "Recall", "AUC")
  for (m in metrics_to_check) {
    expect_true(res[[m]] >= 0 && res[[m]] <= 1)
  }
})

# Edge Cases
# Scenario A: Character Labels Input
test_that("Scenario A: Handles character vector input for true_labels", {
  data <- generate_mock_single_model(20)
  char_labels <- as.character(data$true_labels) # "0", "1" as characters

  capture.output({
    res <- evaluate_models(char_labels, data$model_result, positive_class = "1")
  })

  expect_equal(res$Accuracy, 0.9, tolerance = 0.1)
})

# Scenario B: Extreme Class Imbalance (pROC Safety)
test_that("Scenario B: Handles single-class labels gracefully", {
  # All true labels are "1", no controls for ROC
  y_imbalanced <- factor(rep("1", 30), levels = c("0", "1"))

  mock_res <- list(
    model_name = "ImbalancedModel",
    predictions = factor(rep("1", 30), levels = c("0", "1")),
    pred_prob = runif(30, 0.6, 0.9)
  )

  capture.output({
    suppressWarnings({
      expect_message(
        res <- evaluate_models(y_imbalanced, mock_res, positive_class = "1"),
        "Only one class present"
      )
    })
  })

  expect_true(is.na(res$AUC[1]))
})

# Scenario C: Incorrect positive_class
test_that("Scenario C: Errors out when positive_class is not in levels", {
  data <- generate_mock_single_model(20)

  capture.output({
    # "Yes" is not in c("0", "1")
    expect_error(evaluate_models(data$true_labels, data$model_result, positive_class = "Yes"))
  })
})

# Scenario D: Checks if it correctly identifies the Model Name
test_that("Scenario D: Corrects reflects the model_name from the input list", {
  data <- generate_mock_single_model(10)
  data$model_result$model_name <- "Custom_Model_X"

  capture.output({
    res <- evaluate_models(data$true_labels, data$model_result, positive_class = "1")
  })

  expect_equal(res$Model[1], "Custom_Model_X")
})
