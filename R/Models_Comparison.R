#' Comprehensive Comparison for Multiple Models
#'
#' This function evaluates and visually compares the performance of multiple
#' trained binary classification models simultaneously. It calculates core metrics
#' and generates a suite of comparative plots, including an overlaid ROC curve,
#' a bar chart of metrics, and faceted confusion matrix heatmaps.
#'
#' @param true_labels A factor or numeric vector containing the actual, ground-truth labels
#'   of the test dataset. Must contain exactly two distinct levels.
#' @param models_list A named list containing the result objects of all trained models.
#'   The names of the list elements will be directly used as the model names in the
#'   output tables and plot legends.
#' @param positive_class A character string representing the positive class level.
#'   Default is \code{"X1"}. This must strictly match one of the levels in \code{true_labels}.
#'
#' @return A \code{data.frame} containing the computed evaluation metrics for all models,
#'   with the following columns:
#'   \itemize{
#'     \item \code{Model}: The name of the evaluated model.
#'     \item \code{Accuracy}: The overall accuracy of the predictions.
#'     \item \code{F1_Score}: The F1 score, representing the harmonic mean of precision and recall.
#'     \item \code{Precision}: The precision metric (Positive Predictive Value).
#'     \item \code{Recall}: The recall metric (Sensitivity/True Positive Rate).
#'     \item \code{AUC}: The Area Under the Receiver Operating Characteristic Curve.
#'   }
#'   \emph{Side effect}: Prints three \code{ggplot2} objects to the current graphics device.
#'
#' @author Xinyi Hu
#'
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc ggroc auc
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient theme_minimal labs theme element_text facet_wrap geom_abline ggtitle geom_bar scale_fill_brewer
#' @importFrom reshape2 melt
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Assume 'y_test' contains true labels (e.g., "X0", "X1")
#' # 2. Prepare the named list of model results
#' my_models <- list(
#'   "Decision Tree"  = list(predictions = dtr_preds, pred_prob = dtr_probs),
#'   "Random Forest"  = list(predictions = rf_preds, pred_prob = rf_probs),
#'   "Neural Network" = list(predictions = nn_preds, pred_prob = nn_probs)
#' )
#'
#' # 3. Run the comprehensive comparison
#' final_comparison <- compare_multiple_models(
#'   true_labels = y_test,
#'   models_list = my_models,
#'   positive_class = "X1"
#' )
#'
#' # 4. View the numerical results
#' print(final_comparison)
#' }
compare_models <- function(true_labels, models_list, positive_class = "X1") {

  # Initialize dataframes to store overall metrics and confusion matrix data
  metrics_df <- data.frame()
  cm_data_all <- data.frame()
  roc_list <- list()

  # Ensure true labels are properly formatted
  labels_factor <- factor(true_labels, levels = c("X0", "X1"))

  # 1. Loop through the named list of models
  for (model_name in names(models_list)) {
    model_res <- models_list[[model_name]]
    pred_c <- model_res$predictions
    pred_p <- model_res$pred_prob

    # Calculate Confusion Matrix
    cm <- confusionMatrix(pred_c, labels_factor, positive = positive_class, mode = "everything")

    # Extract CM table for Faceted Heatmap
    cm_table <- as.data.frame(cm$table)
    cm_table$Model <- model_name
    cm_data_all <- rbind(cm_data_all, cm_table)

    # Calculate ROC and AUC
    roc_obj <- roc(labels_factor, pred_p, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    roc_list[[model_name]] <- roc_obj

    # Append to metrics dataframe
    metrics_df <- rbind(metrics_df, data.frame(
      Model = model_name,
      Accuracy = cm$overall["Accuracy"],
      F1_Score = cm$byClass["F1"],
      Precision = cm$byClass["Precision"],
      Recall = cm$byClass["Recall"],
      AUC = auc_val,
      stringsAsFactors = FALSE
    ))
  }

  # Print the final comparison table
  print("===== Multi-Model Comparison Metrics =====")
  print(metrics_df)

  # Visualization
  # Plot 1: Overlay ROC Curves for all models
  print("Generating combined ROC Curve...")
  roc_plot <- ggroc(roc_list, size = 1) +
    theme_minimal() +
    ggtitle("ROC Curve Comparison") +
    geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "darkgrey") +
    labs(color = "Models", x = "Specificity", y = "Sensitivity") +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  print(roc_plot)

  # Plot 2: Bar Chart for Metrics Comparison
  print("Generating Metrics Bar Chart...")
  melted_df <- melt(metrics_df, id.vars = "Model", measure.vars = c("Accuracy", "F1_Score", "AUC"))

  bar_plot <- ggplot(melted_df, aes(x = Model, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal() +
    labs(title = "Model Performance Comparison", y = "Score", x = "") +
    scale_fill_brewer(palette = "Set2", name = "Metrics") +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
      legend.position = "bottom"
    )
  print(bar_plot)

  # Plot 3: Faceted Confusion Matrix Heatmaps
  print("Generating Faceted Confusion Matrix Heatmaps...")
  colnames(cm_data_all) <- c("Prediction", "Reference", "Freq", "Model")

  heatmap_plot <- ggplot(cm_data_all, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 4) +
    scale_fill_gradient(low = "#F0F8FF", high = "#4682B4") +
    theme_minimal() +
    labs(title = "Confusion Matrix Across Models", x = "Actual Class", y = "Predicted Class") +
    facet_wrap(~ Model, ncol = 3) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      strip.text = element_text(face = "bold", size = 12, color = "darkblue"),
      strip.background = element_rect(fill = "#E6E6FA", color = NA)
    )
  print(heatmap_plot)

  return(metrics_df)
}

# test
# my_all_models <- list(
#   "Decision Tree"   = dtr_result,
#   "Neural Network"  = NN,
#   "Random Forest"   = rf_result,
#   "SVM"             = svm_result,
#   "XGBoost"         = xgb_result
# )
#
# final_comparison <- compare_models(true_labels = y_test,
#                                            models_list = my_all_models,
#                                            positive_class = "X1")
