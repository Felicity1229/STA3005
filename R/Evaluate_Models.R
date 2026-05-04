#' Comprehensive Evaluation for a Single Model
#'
#' This function computes core classification metrics (Accuracy, F1-Score,
#' Precision, Recall, AUC) for a single model's predictions against true test labels.
#' It also generates visualizations, including an ROC curve and a Confusion Matrix Heatmap.
#'
#' @param true_labels A factor or numeric vector containing the actual labels of the test set.
#' @param model_result A list containing the results of a trained model. It must
#'   include \code{model_name}, \code{predictions} (class labels), and \code{pred_prob} (probabilities).
#' @param positive_class A character string representing the positive class level
#'   in the target variable (e.g., "1", "Yes", or "X1").
#'
#' @return A data frame containing the computed evaluation metrics.
#'
#' @author Xinyi Hu
#'
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc auc
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient theme_minimal labs theme element_text geom_bar scale_fill_brewer element_blank
#' @importFrom reshape2 melt
#' @importFrom graphics plot text
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'dtr_result' is the output from decision_tree()
#' evaluation_df <- evaluate_models(true_labels = y_test,
#'                                  model_result = dtr_result,
#'                                  positive_class = "1")
#' }
evaluate_models <- function(true_labels, model_result, positive_class) {

  # 1. Extract inputs
  # Use specific levels to ensure consistency
  labels_factor <- factor(true_labels, levels = c("0", "1"))
  name <- model_result$model_name
  pred_c <- model_result$predictions
  pred_p <- model_result$pred_prob

  # 2. Calculate Confusion Matrix
  cm <- confusionMatrix(pred_c, labels_factor, positive = positive_class, mode = "everything")

  # 3. Calculate ROC and AUC
  roc_obj <- roc(labels_factor, pred_p, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  # 4. Store Metrics
  metrics_df <- data.frame(
    Model = name,
    Accuracy = cm$overall["Accuracy"],
    F1_Score = cm$byClass["F1"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    AUC = auc_val,
    stringsAsFactors = FALSE
  )

  message(paste("===== Evaluation Metrics for", name, "====="))
  print(metrics_df)

  # 5. Plot 1: ROC Curve (Using standard graphics via pROC)
  message("Generating ROC Curve...")
  plot(roc_obj, col = "darkblue", main = paste("ROC Curve -", name), lwd = 2)
  text(x = 0.2, y = 0.2, labels = paste("AUC =", round(auc_val, 4)), col = "darkblue", cex = 1.2, font = 2)

  # 6. Plot 2: Confusion Matrix Heatmap
  message("Generating Confusion Matrix Heatmap...")
  cm_table <- as.data.frame(cm$table)
  colnames(cm_table) <- c("Prediction", "Reference", "Freq")

  heatmap_plot <- ggplot(cm_table, aes(x = .data$Reference, y = .data$Prediction, fill = .data$Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = .data$Freq), vjust = 0.5, fontface = "bold", size = 6) +
    scale_fill_gradient(low = "#F0F8FF", high = "#4682B4") +
    theme_minimal() +
    labs(
      title = paste("Confusion Matrix Heatmap -", name),
      x = "Actual Class (Reference)",
      y = "Predicted Class"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 12)
    )

  print(heatmap_plot)

  # 7. Plot 3: Bar Chart for Metrics (Acc, F1, AUC)
  message("Generating Metrics Bar Chart...")
  melted_df <- melt(metrics_df, id.vars = "Model",
                    measure.vars = c("Accuracy", "F1_Score", "AUC"))

  bar_plot <- ggplot(melted_df, aes(x = .data$Model, y = .data$value, fill = .data$variable)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal() +
    labs(title = paste("Performance Metrics -", name), y = "Score", x = "Metrics") +
    scale_fill_brewer(palette = "Set2") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  print(bar_plot)

  return(metrics_df)
}
