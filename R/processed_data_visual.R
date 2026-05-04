#' Visualize Dataset with Binary Target Variable
#'
#' This function generates a comprehensive set of visualizations for dataset analysis,
#' including pie chart, boxplots, histograms, and correlation matrix.
#'
#' @param df A data frame containing features and a binary target column.
#' @param target_col Name of the binary target column (default: "Potability").
#'   The target column should be binary (0 = negative class, 1 = positive class).
#' @param class_labels Optional character vector of length 2 for class labels.
#'   If NULL, uses c("Class 0", "Class 1") or detects from data.
#' @param feature_cols Optional character vector of feature column names.
#'   If NULL, all columns except target_col will be used as features.
#' @param correlation_cols Optional integer vector or character vector for correlation plot.
#'   If NULL, uses first 9 features or all numeric features.
#'
#' @return NULL. The function prints plots to the graphics device and returns nothing invisibly.
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_col geom_label coord_polar scale_fill_discrete
#' @importFrom ggplot2 theme_void ggtitle geom_boxplot labs theme_minimal
#' @importFrom ggplot2 geom_histogram theme_bw element_text
#' @importFrom GGally ggpairs wrap
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default usage (assumes "Potability" column exists)
#' processed_data_visual(df)
#'
#' # Custom target column
#' processed_data_visual(df, target_col = "Status")
#'
#' # Custom target column with labels
#' processed_data_visual(df, target_col = "Success",
#'                       class_labels = c("Failure", "Success"))
#'
#' # Specify which columns to visualize
#' processed_data_visual(df, target_col = "Potability",
#'                       feature_cols = c("pH", "Hardness", "Solids"))
#' }
#'
#' @seealso \code{\link{exploratory_data_analysis}} for the EDA function
processed_data_visual <- function(df,
                                  target_col = "Potability",
                                  class_labels = NULL,
                                  feature_cols = NULL,
                                  correlation_cols = NULL) {

  # ========== Input Validation ==========

  # Check if target column exists
  if (!target_col %in% colnames(df)) {
    stop("Target column '", target_col, "' not found in data frame. ",
         "Available columns: ", paste(colnames(df), collapse = ", "))
  }

  # Check if target column is binary
  target_values <- unique(na.omit(df[[target_col]]))
  if (length(target_values) != 2) {
    warning("Target column '", target_col, "' has ", length(target_values),
            " unique values. Expected binary (2 values).")
  }

  # Determine feature columns
  if (is.null(feature_cols)) {
    feature_cols <- colnames(df)[colnames(df) != target_col]
  }

  # Filter out non-numeric features for some plots
  numeric_features <- feature_cols[sapply(df[feature_cols], is.numeric)]
  categorical_features <- feature_cols[!sapply(df[feature_cols], is.numeric)]

  if (length(numeric_features) == 0) {
    warning("No numeric features found. Some plots will be skipped.")
  }

  # Set default class labels
  if (is.null(class_labels)) {
    # Try to detect meaningful labels
    if (is.numeric(df[[target_col]])) {
      class_labels <- c(paste(target_col, "= 0"), paste(target_col, "= 1"))
    } else if (is.factor(df[[target_col]])) {
      class_labels <- levels(df[[target_col]])
    } else {
      class_labels <- c("Class 0", "Class 1")
    }
  }

  # ========== Helper Functions ==========

  # Get target distribution
  get_target_distribution <- function() {
    if (is.numeric(df[[target_col]])) {
      dist_df <- as.data.frame(table(df[[target_col]]))
      colnames(dist_df) <- c("Class", "Count")
      dist_df$Class <- as.character(dist_df$Class)
    } else {
      dist_df <- as.data.frame(table(df[[target_col]], useNA = "ifany"))
      colnames(dist_df) <- c("Class", "Count")
    }
    return(dist_df)
  }

  # ========== 1. Pie Chart of Target Distribution ==========

  dist_df <- get_target_distribution()

  if (nrow(dist_df) > 0) {
    pie_chart <- ggplot(dist_df, aes(x = "", y = Count, fill = Class)) +
      geom_col() +
      geom_label(aes(label = Count),
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y") +
      scale_fill_discrete(labels = class_labels) +
      theme_void() +
      ggtitle(paste("Target Distribution:", target_col))

    print(pie_chart)
    cat("\n=== Pie Chart of Target Distribution ===\n\n")
  }

  # ========== 2. Box Plots for Numeric Features ==========

  if (length(numeric_features) > 0) {
    cat("\n=== Box Plots by", target_col, "===\n\n")

    box_plot_indicator <- function(indicator) {
      p <- ggplot(df, aes(x = factor(.data[[target_col]]),
                          y = .data[[indicator]],
                          fill = factor(.data[[target_col]]))) +
        geom_boxplot(alpha = 0.7, na.rm = TRUE) +
        labs(
          title = paste(indicator, "- Distribution by", target_col),
          x = target_col,
          y = indicator,
          fill = target_col
        ) +
        scale_fill_discrete(labels = class_labels) +
        theme_minimal()

      return(p)
    }

    box_plots <- lapply(numeric_features, box_plot_indicator)
    for(p in box_plots) {
      print(p)
    }
  } else {
    cat("\n=== No numeric features for box plots ===\n\n")
  }

  # ========== 3. Histograms for Numeric Features ==========

  if (length(numeric_features) > 0) {
    cat("\n=== Histograms by", target_col, "===\n\n")

    hist_plot_indicator <- function(indicator) {
      p <- ggplot(df, aes(x = .data[[indicator]],
                          fill = factor(.data[[target_col]]))) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 30, na.rm = TRUE) +
        labs(
          title = paste(indicator, "- Distribution by", target_col),
          x = indicator,
          y = "Count",
          fill = target_col
        ) +
        scale_fill_discrete(labels = class_labels) +
        theme_minimal()

      return(p)
    }

    hist_plots <- lapply(numeric_features, hist_plot_indicator)
    for(p in hist_plots) {
      print(p)
    }
  }

  # ========== 4. Correlation Matrix ==========

  # Determine which columns to use for correlation
  if (is.null(correlation_cols)) {
    # Default: use first 9 numeric features or all numeric features
    if (length(numeric_features) >= 2) {
      if (length(numeric_features) > 9) {
        corr_cols <- numeric_features[1:9]
        cat("\n=== Using first 9 numeric features for correlation matrix ===\n")
      } else {
        corr_cols <- numeric_features
      }
    } else {
      corr_cols <- NULL
    }
  } else if (is.character(correlation_cols)) {
    # Character vector of column names
    corr_cols <- correlation_cols[correlation_cols %in% colnames(df)]
    if (length(corr_cols) < 2) {
      warning("Not enough valid columns for correlation matrix")
      corr_cols <- NULL
    }
  } else if (is.numeric(correlation_cols)) {
    # Numeric vector of column indices
    corr_cols <- colnames(df)[correlation_cols[correlation_cols <= ncol(df)]]
    if (length(corr_cols) < 2) {
      warning("Not enough valid columns for correlation matrix")
      corr_cols <- NULL
    }
  }

  # Generate correlation plot
  if (!is.null(corr_cols) && length(corr_cols) >= 2) {
    cat("\n=== Correlation Matrix ===\n\n")

    # Check if all selected columns are numeric
    numeric_corr_cols <- corr_cols[sapply(df[corr_cols], is.numeric)]

    if (length(numeric_corr_cols) >= 2) {
      # Get column indices for ggpairs
      col_indices <- which(colnames(df) %in% numeric_corr_cols)

      corr_plot <- ggpairs(
        df,
        columns = col_indices,
        aes(color = factor(.data[[target_col]]), alpha = 0.5),
        title = paste("Correlation Matrix - Colored by", target_col),
        lower = list(continuous = wrap("points", size = 0.8, alpha = 0.3)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.6)),
        upper = list(continuous = wrap("cor", size = 3, stars = FALSE)),
        legend = 5
      ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))

      print(corr_plot)
    } else {
      cat("No numeric columns available for correlation matrix\n")
    }
  } else {
    cat("\n=== Skipping correlation matrix (need at least 2 numeric features) ===\n\n")
  }
  return(invisible(NULL))
}
