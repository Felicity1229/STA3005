# Helper Function: Generate mixed dataset for visualization testing
generate_visual_data <- function(n = 30) {
  set.seed(123)
  data.frame(
    Potability = sample(c(0, 1), n, replace = TRUE),
    Num1 = rnorm(n),
    Num2 = runif(n),
    Num3 = rnorm(n, mean = 5),
    Cat1 = sample(c("A", "B", "C"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Base Cases
test_that("processed_data_visual runs smoothly with default parameters", {
  data <- generate_visual_data()

  # 1. Open a NULL PDF device to prevent plot windows from popping up during tests
  pdf(NULL)

  # 2. Capture and silence all console cat() and print() outputs
  capture.output({
    # We expect NO error (NA) when running with correct defaults
    expect_error(processed_data_visual(data), NA)
  })

  # 3. Close the null device
  dev.off()
})

test_that("processed_data_visual accepts custom parameters without error", {
  data <- generate_visual_data()

  pdf(NULL)

  capture.output({
    expect_error(
      processed_data_visual(
        df = data,
        target_col = "Potability",
        class_labels = c("Safe", "Unsafe"),
        feature_cols = c("Num1", "Cat1"),
        correlation_cols = c("Num1", "Num2", "Num3")
      ),
      NA
    )
  })
  dev.off()
})

# Edge Cases & Error Handling
# Scenario A: Errors out when target column is missing
test_that("Scenario A: Errors out when target column is missing", {
  data <- generate_visual_data()

  capture.output({
    expect_error(
      processed_data_visual(data, target_col = "NonExistent"),
      "Target column 'NonExistent' not found"
    )
  })
})

# Scenario B: Warns when target column is not binary
test_that("Scenario B: Warns when target column is not binary", {
  data <- generate_visual_data()
  # Force target to have 3 unique values instead of 2
  data$Potability <- sample(c(0, 1, 2), 30, replace = TRUE)

  pdf(NULL)
  capture.output({
    expect_warning(
      processed_data_visual(data),
      "Expected binary"
    )
  })
  dev.off()
})

# Scenario C: Warns and handles dataset with NO numeric features
test_that("Scenario C: Warns and handles dataset with NO numeric features", {
  data <- generate_visual_data()
  # Subset to only include the target and a character column
  data_no_numeric <- data[, c("Potability", "Cat1")]

  pdf(NULL)
  capture.output({
    expect_warning(
      processed_data_visual(data_no_numeric),
      "No numeric features found"
    )
  })
  dev.off()
})

# Scenario D: Handles invalid correlation_cols gracefully
test_that("Scenario D: Handles invalid correlation_cols gracefully", {
  data <- generate_visual_data()

  pdf(NULL)
  capture.output({
    # Provide only 1 column for correlation (needs at least 2)
    expect_warning(
      processed_data_visual(data, correlation_cols = c("Num1")),
      "Not enough valid columns for correlation matrix"
    )
  })
  dev.off()
})

# Scenario E: Handles correlation_cols as numeric indices
test_that("Scenario E: Handles correlation_cols as numeric indices", {
  data <- generate_visual_data()

  pdf(NULL)
  capture.output({
    # Provide numeric indices for the correlation matrix (e.g., column 2 and 3)
    expect_error(
      processed_data_visual(data, correlation_cols = c(2, 3, 4)),
      NA
    )
  })
  dev.off()
})
