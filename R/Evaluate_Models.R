# Required packages: caret, pROC, ggplot2, reshape2
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(caret)
library(pROC)
library(ggplot2)
library(reshape2)

#' Comprehensive Evaluation and Comparison for Multiple Models
#' @param true_labels The actual labels of the test set (factor)
#' @param models_results A list of results from different models
#' @param positive_class The string representing the positive class (e.g., "1" or "Yes")
evaluate_models <- function(true_labels, models_results, positive_class) {

  # Initialize an empty dataframe to store metrics
  metrics_df <- data.frame(
    Model = character(),
    Accuracy = numeric(),
    F1_Score = numeric(),
    Precision = numeric(),
    Recall = numeric(),
    AUC = numeric(),
    stringsAsFactors = FALSE
  )

  roc_list <- list() # Store ROC objects for plotting

  # 1. Loop through each model to calculate metrics
  for (res in models_results) {
    name <- res$model_name
    pred_c <- res$predictions
    pred_p <- res$pred_prob

    # Calculate Confusion Matrix (mode="everything" gives F1, Precision, Recall)
    cm <- confusionMatrix(pred_c, true_labels, positive = positive_class, mode = "everything")

    # Calculate ROC and AUC
    roc_obj <- roc(true_labels, pred_p, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    roc_list[[name]] <- roc_obj

    # Append to dataframe
    metrics_df <- rbind(metrics_df, data.frame(
      Model = name,
      Accuracy = cm$overall["Accuracy"],
      F1_Score = cm$byClass["F1"],
      Precision = cm$byClass["Precision"],
      Recall = cm$byClass["Recall"],
      AUC = auc_val
    ))
  }

  print("===== Multi-Model Comparison Metrics =====")
  print(metrics_df)

  # 2. Plot 1: Overlay ROC Curves for all models
  print("Generating combined ROC Curve...")
  plot(roc_list[[1]], col = 1, main = "ROC Curve Comparison", lwd = 2)
  if (length(roc_list) > 1) {
    for (i in 2:length(roc_list)) {
      plot(roc_list[[i]], col = i, add = TRUE, lwd = 2)
    }
  }
  legend("bottomright", legend = names(roc_list), col = 1:length(roc_list), lwd = 2)

  # 3. Plot 2: Bar Chart for Metrics Comparison (Acc, F1, AUC)
  print("Generating Metrics Bar Chart...")
  # Melt dataframe for ggplot
  melted_df <- melt(metrics_df, id.vars = "Model",
                    measure.vars = c("Accuracy", "F1_Score", "AUC"))

  bar_plot <- ggplot(melted_df, aes(x = Model, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal() +
    labs(title = "Model Performance Comparison", y = "Score", x = "Models") +
    scale_fill_brewer(palette = "Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(bar_plot)

  return(metrics_df)
}
