#' Exploratory Analysis for Water Quality Data
#'
#' This function performs a comprehensive exploratory data analysis on the
#' unnormalized training dataset. It calculates descriptive statistics, evaluates the
#' distribution of the target variable, computes group means, and identifies outliers.
#'
#' @param data A data frame containing the unnormalized training features and target variable.
#' @param target_name A character string naming the target variable. Default is "Potability".
#'   The Potability column should be binary (0 = non-potable, 1 = potable).
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
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'result' is the list returned by preprocess_data()
#' eda_results <- Exploratory_Data_analysis(data = result$train_data)
#'
#' # View the descriptive statistics
#' print(eda_results$Descriptive_Statistics)
#' }

Exploratory_Data_analysis <- function(data, target_name = "Potability") {

  # 1. 参数校验
  if (!(target_name %in% colnames(data))) {
    stop(paste("Error: Target column '", target_name, "' not found in the dataset.", sep = ""))
  }

  # 初始化一个列表，用于存储并返回所有的探索性分析结果
  eda_results <- list()

  # 2. 基础信息提取
  eda_results$Dimensions <- dim(data)
  numeric_data <- data %>%
    select(where(is.numeric))

  # 3. 详细的描述性统计 (Descriptive Statistics)
  calc_stats <- function(x) {
    c(
      Min = min(x, na.rm = TRUE),
      Q1 = unname(quantile(x, 0.25, na.rm = TRUE)), # 25% quantile
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Q3 = unname(quantile(x, 0.75, na.rm = TRUE)), # 75% quantile
      Max = max(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE)
    )
  }

  # 转置为矩阵格式
  eda_results$Descriptive_Statistics <- t(sapply(numeric_data, calc_stats))

  # 4. 目标变量分布 (Target Variable Distribution)
  target_vector <- data[[target_name]]
  counts <- table(target_vector)
  props <- prop.table(counts) * 100

  dist_df <- data.frame(
    Class = names(counts),
    Count = as.numeric(counts),
    Percentage = round(as.numeric(props), 2)
  )
  eda_results$Target_Distribution <- dist_df

  # 5. 分组均值分析 (Group Means by Target)
  group_means <- data %>%
    group_by(.data[[target_name]]) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  colnames(group_means)[1] <- target_name
  eda_results$Group_Means <- group_means

  # 6. 离群值检测 (Outlier Detection using IQR method)
  detect_outliers_iqr <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val

    outliers <- x < lower_bound | x > upper_bound
    return(sum(outliers, na.rm = TRUE))
  }

  eda_results$Outlier_Counts <- sapply(numeric_data, detect_outliers_iqr)

  # 7. 打印简要总结
  cat("=== Exploratory Data Analysis ===\n")
  cat("Data Source: Unnormalized Training Set\n")

  # 7.1 基础信息
  cat("\n[1] Dataset Overview\n")
  cat("Total Observations :", eda_results$Dimensions[1], "\n")
  cat("Total Features     :", eda_results$Dimensions[2] - 1, "(excluding target)\n")
  cat("Target Variable    :", target_name, "\n")

  # 7.2 目标变量分布
  cat("\n[2] Target Variable Distribution\n")
  print(eda_results$Target_Distribution, row.names = FALSE)

  # 7.3 描述性统计与离群值
  cat("\n[3] Descriptive Statistics & Outliers\n")
  stats_with_outliers <- cbind(eda_results$Descriptive_Statistics,
                               Outliers = eda_results$Outlier_Counts)
  print(round(stats_with_outliers, 3))

  # 7.4 分组均值分析
  cat("\n[4] Feature Means Grouped By Target (", target_name, ")\n", sep = "")
  print(as.data.frame(
    eda_results$Group_Means %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  ), row.names = FALSE)

  return(eda_results)
}
