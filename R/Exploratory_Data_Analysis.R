#' Exploratory Analysis for Water Quality Data
#'
#' This function performs a comprehensive exploratory data analysis on the
#' unnormalized training dataset. It calculates descriptive statistics, evaluates the
#' distribution of the target variable, computes group means, and identifies outliers.
#'
#' @param X A data frame containing the unnormalized numeric features (e.g., X_train).
#' @param y A numeric vector or factor containing the target variable (e.g., y_train).
#' @param target_name A character string naming the target variable. Default is "Potability".
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
#' eda_results <- Exploratory_Data_analysis(X = result$X_train, y = result$y_train)
#'
#' # View the descriptive statistics
#' print(eda_results$Descriptive_Statistics)
#' }

library(dplyr)

Exploratory_Data_analysis <- function(X, y, target_name = "Potability") {

  # 1. 数据合并与参数校验：将特征和标签组合成一个完整的数据框，方便后续统一分析
  if (nrow(X) != length(y)) {
    stop("Error: The number of rows in X must match the length of y.")
  }

  data <- X
  data[[target_name]] <- y

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
      Q1 = quantile(x, 0.25, na.rm = TRUE), # 25% quantile
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE), # 75% quantile
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
  cat("Total Observations:", eda_results$Dimensions[1], "\n")
  cat("Total Features:", eda_results$Dimensions[2] - 1, "(excluding target)\n")
  cat("\nTarget Distribution (", target_name, "):\n", sep = "")
  print(eda_results$Target_Distribution, row.names = FALSE)

  return(eda_results)
}
