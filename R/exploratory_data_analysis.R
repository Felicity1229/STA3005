#' Exploratory Analysis for Datasets
#'
#' This function performs a comprehensive exploratory data analysis on the
#' unnormalized training dataset. It calculates descriptive statistics, evaluates the
#' distribution of the target variable, computes group means, and identifies outliers.
#'
#' @param data A data frame containing the unnormalized training features and target variable.
#' @param target_name A character string naming the target variable.
#'   The target column should be binary (e.g., 0/1, "Yes"/"No").
#'
#' @return A list containing the following Exploratory Data Analysis components:
#' \itemize{
#'   \item \code{Dimensions}: The dimensions of the combined dataset.
#'   \item \code{Descriptive_Statistics}: A matrix of summary statistics for numeric variables.
#'   \item \code{Target_Distribution}: A data frame showing the count and percentage of the target classes.
#'   \item \code{Group_Means}: The mean of each numeric feature grouped by the target variable.
#'   \item \code{Outlier_Counts}: The number of outliers in each numeric feature based on the IQR method.
#' }
#'
#' @author Xinyi Hu
#'
#' @importFrom dplyr select where group_by summarise across mutate .data %>%
#' @importFrom stats median sd quantile
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'result' is the list returned by preprocess_data()
#' eda_results <- exploratory_data_analysis(data = result$train_data, target_name = "Class")
#'
#' # View the descriptive statistics
#' print(eda_results$Descriptive_Statistics)
#' }
exploratory_data_analysis <- function(data, target_name) {

  # 1. Parameter Validation
  if (!(target_name %in% colnames(data))) {
    stop(paste("Error: Target column '", target_name, "' not found in the dataset.", sep = ""))
  }

  # Initialize a list to store and return all exploratory analysis results
  eda_results <- list()

  # 2. Extract Basic Information
  eda_results$Dimensions <- dim(data)

  features_only <- data[, colnames(data) != target_name, drop = FALSE]
  numeric_data <- select(features_only, where(is.numeric))

  if (ncol(numeric_data) == 0) {
    warning("No numeric features found in the dataset for descriptive statistics.")
  }

  # 3. Detailed Descriptive Statistics
  calc_stats <- function(x) {
    c(
      Min = min(x, na.rm = TRUE),
      Q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
      Max = max(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE)
    )
  }

  # Transpose to matrix format
  if (ncol(numeric_data) > 0) {
    eda_results$Descriptive_Statistics <- t(sapply(numeric_data, calc_stats))
  } else {
    eda_results$Descriptive_Statistics <- matrix(ncol = 7, nrow = 0)
  }

  # 4. Target Variable Distribution
  target_vector <- data[[target_name]]
  counts <- table(target_vector)
  props <- prop.table(counts) * 100

  dist_df <- data.frame(
    Class = names(counts),
    Count = as.numeric(counts),
    Percentage = round(as.numeric(props), 2)
  )
  eda_results$Target_Distribution <- dist_df

  # 5. Group Means by Target
  eda_results$Group_Means <- data %>%
    group_by(.data[[target_name]]) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

  colnames(eda_results$Group_Means)[1] <- target_name

  # 6. Outlier Detection using IQR method
  detect_outliers_iqr <- function(x) {
    q_vals <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    Q1 <- q_vals[1]
    Q3 <- q_vals[2]
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val

    outliers <- x < lower_bound | x > upper_bound
    return(sum(outliers, na.rm = TRUE))
  }

  if (ncol(numeric_data) > 0) {
    eda_results$Outlier_Counts <- sapply(numeric_data, detect_outliers_iqr)
  } else {
    eda_results$Outlier_Counts <- numeric(0)
  }

  # 7. Print brief summary
  message("=== Exploratory Data Analysis ===")
  message("Data Source: Unnormalized Training Set")

  # 7.1 Basic Information
  cat("\n[1] Dataset Overview\n")
  cat("Total Observations :", eda_results$Dimensions[1], "\n")
  cat("Total Features     :", eda_results$Dimensions[2] - 1, "(excluding target)\n")
  cat("Target Variable    :", target_name, "\n")

  # 7.2 Target Variable Distribution
  cat("\n[2] Target Variable Distribution\n")
  print(eda_results$Target_Distribution, row.names = FALSE)

  # 7.3 Descriptive Statistics & Outliers
  cat("\n[3] Descriptive Statistics & Outliers\n")
  if (ncol(numeric_data) > 0) {
    stats_df <- as.data.frame(eda_results$Descriptive_Statistics)
    stats_df$Outliers <- as.numeric(eda_results$Outlier_Counts)

    stats_df[] <- lapply(stats_df, function(x) round(as.numeric(x), 3))
    print(stats_df)
  }

  # 7.4 Group Means Analysis
  cat("\n[4] Feature Means Grouped By Target (", target_name, ")\n", sep = "")

  formatted_means <- eda_results$Group_Means %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  print(as.data.frame(formatted_means), row.names = FALSE)

  return(eda_results)
}
