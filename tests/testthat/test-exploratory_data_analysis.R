# Helper Function: Generate mixed dataset for EDA testing
generate_eda_data <- function() {
  set.seed(42)

  # f1: Normal distribution but with one extreme outlier (value = 1000)
  f1 <- c(rnorm(99, mean = 50, sd = 5), 1000)

  # f2: Numeric with NAs
  f2 <- sample(c(1, 2, 3, NA), 100, replace = TRUE)

  # f3: Non-numeric character feature
  f3 <- sample(c("Type_A", "Type_B"), 100, replace = TRUE)

  # target: Binary target variable
  target <- sample(c(0, 1), 100, replace = TRUE)

  return(data.frame(f1 = f1, f2 = f2, f3 = f3, target = target))
}

# Base Cases
test_that("exploratory_data_analysis returns correct structure and components", {
  data <- generate_eda_data()

  # Silence all console prints and messages
  capture.output({
    suppressMessages({
      res <- exploratory_data_analysis(data, target_name = "target")
    })
  })

  # 1. Check return type and component names
  expect_type(res, "list")
  expected_names <- c("Dimensions", "Descriptive_Statistics", "Target_Distribution",
                      "Group_Means", "Outlier_Counts")
  expect_true(all(expected_names %in% names(res)))

  # 2. Check Dimensions
  expect_equal(res$Dimensions, c(100, 4))

  # 3. Check Descriptive Statistics format
  expect_true(is.matrix(res$Descriptive_Statistics))
  # Should have 7 columns (Min, Q1, Median, Mean, Q3, Max, SD)
  expect_equal(ncol(res$Descriptive_Statistics), 7)
  # Should only include f1 and f2 (numeric features)
  expect_equal(nrow(res$Descriptive_Statistics), 2)

  # 4. Check Target Distribution
  expect_s3_class(res$Target_Distribution, "data.frame")
  expect_equal(sum(res$Target_Distribution$Count), 100)
  expect_equal(sum(res$Target_Distribution$Percentage), 100)

  # 5. Check Group Means
  expect_s3_class(res$Group_Means, "data.frame")
  expect_true("target" %in% colnames(res$Group_Means))
})

# Edge Cases
# Scenario A: Errors out when target column is missing
test_that("Scenario A: Errors out when target column is missing", {
  data <- generate_eda_data()

  # "wrong_target" does not exist in the dataframe
  capture.output({
    expect_error(
      exploratory_data_analysis(data, target_name = "wrong_target"),
      "Target column 'wrong_target' not found"
    )
  })
})

# Scenario B: Handles datasets with NO numeric features gracefully
test_that("Scenario B: Handles datasets with NO numeric features gracefully", {
  data <- generate_eda_data()
  # Subset to only include character feature and target
  data_no_numeric <- data[, c("f3", "target")]

  capture.output({
    expect_message(
      res <- exploratory_data_analysis(data_no_numeric, target_name = "target"),
      "No numeric features found"
    )
  })

  # Descriptive_Statistics should be an empty 0x7 matrix
  expect_equal(nrow(res$Descriptive_Statistics), 0)
  expect_equal(ncol(res$Descriptive_Statistics), 7)

  # Outlier counts should be a numeric vector of length 0
  expect_length(res$Outlier_Counts, 0)
})

# Scenario C: Handles NA values correctly without propagating NA
test_that("Scenario C: Handles NA values correctly without propagating NA", {
  data <- generate_eda_data()

  capture.output({
    suppressMessages({
      res <- exploratory_data_analysis(data, target_name = "target")
    })
  })

  # f2 contains NAs. If na.rm = TRUE works, mean and sd should be valid numbers
  expect_false(is.na(res$Descriptive_Statistics["f2", "Mean"]))
  expect_false(is.na(res$Descriptive_Statistics["f2", "SD"]))
})

# Scenario D: Correctly detects outliers using the IQR method
test_that("Scenario D: Correctly detects outliers using the IQR method", {
  data <- generate_eda_data()

  capture.output({
    suppressMessages({
      res <- exploratory_data_analysis(data, target_name = "target")
    })
  })

  # We injected the value 1000 into f1, so it MUST have at least 1 outlier
  expect_true(res$Outlier_Counts["f1"] >= 1)
})

# Scenario E: Handles character target variables
test_that("Scenario E: Handles character target variables", {
  data <- generate_eda_data()
  # Change target from 0/1 to "Yes"/"No"
  data$target <- ifelse(data$target == 1, "Yes", "No")

  capture.output({
    suppressMessages({
      res <- exploratory_data_analysis(data, target_name = "target")
    })
  })

  # Group means should still work and group by "Yes" / "No"
  expect_setequal(res$Group_Means$target, c("Yes", "No"))
})
