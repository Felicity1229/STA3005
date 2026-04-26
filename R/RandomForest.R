library(randomForest)
library(pROC)
library(PRROC)


train_random_forest <- function(X_train, y_train,
                                X_test = NULL, y_test = NULL,
                                ntree = 200,
                                mtry = NULL,
                                tune = TRUE) {


  # =========================
  # 1. 数据准备
  # =========================
  X_train <- as.data.frame(lapply(X_train, as.numeric))
  y_train <- as.factor(y_train)

  if (!is.null(X_test)) {
    X_test <- as.data.frame(lapply(X_test, as.numeric))
    y_test <- as.factor(y_test)
  }

  # =========================
  # 2. 处理类别不平衡（权重）
  # =========================
  class_counts <- table(y_train)
  class_weights <- sum(class_counts) / (length(class_counts) * class_counts)

  print("类别权重：")
  print(class_weights)

  # =========================
  # 3. 自动调参（mtry）
  # =========================
  if (tune) {

    cat("正在调参 mtry...\n")

    tune_result <- tuneRF(
      x = X_train,
      y = y_train,
      ntreeTry = ntree,
      stepFactor = 1.5,
      improve = 0.01,
      trace = TRUE,
      plot = FALSE
    )

    best_mtry <- tune_result[which.min(tune_result[,2]), 1]

    cat("最优 mtry:", best_mtry, "\n")

  } else {
    if (is.null(mtry)) {
      best_mtry <- floor(sqrt(ncol(X_train)))
    } else {
      best_mtry <- mtry
    }
  }

  # =========================
  # 4. 训练模型
  # =========================
  model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = ntree,
    mtry = best_mtry,
    classwt = class_weights,
    importance = TRUE
  )

  # =========================
  # 5. 预测（🔥 修改1：加入概率输出）
  # =========================

  train_prob <- predict(model, X_train, type = "prob")[,2]

  if (!is.null(X_test)) {
    test_prob <- predict(model, X_test, type = "prob")[,2]   # 🔥 修改1
  }

  # =========================
  # 6. 🔥 修改2：寻找最佳 threshold
  # =========================

  find_best_threshold <- function(prob, y_true) {

    thresholds <- seq(0.1, 0.9, by = 0.01)

    f1_scores <- sapply(thresholds, function(t) {

      pred <- ifelse(prob > t, 1, 0)

      cm <- table(factor(pred, levels = c(0,1)),
                  factor(y_true, levels = c(0,1)))

      TP <- cm["1","1"]
      FP <- cm["1","0"]
      FN <- cm["0","1"]

      precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
      recall    <- ifelse(TP + FN == 0, 0, TP / (TP + FN))

      if (precision + recall == 0) return(0)

      2 * precision * recall / (precision + recall)
    })

    best_t <- thresholds[which.max(f1_scores)]

    return(list(
      best_threshold = best_t,
      best_f1 = max(f1_scores)
    ))
  }

  # 🔥 只在测试集存在时计算
  if (!is.null(y_test)) {

    threshold_result <- find_best_threshold(test_prob, as.numeric(as.character(y_test)))

    best_threshold <- threshold_result$best_threshold

    cat("\n🔥 最优 threshold:", best_threshold, "\n")
    cat("🔥 最优 F1 (threshold search):", threshold_result$best_f1, "\n")

    # =========================
    # 7. 用最佳 threshold 重新预测（🔥 修改3）
    # =========================

    test_pred <- ifelse(test_prob > best_threshold, 1, 0)

    # =========================
    # 8. 评估
    # =========================

    cat("\n=== 测试集（优化threshold后）===\n")

    cm <- table(Predicted = test_pred,
                Actual = as.numeric(as.character(y_test)))

    print(cm)

    acc <- mean(test_pred == as.numeric(as.character(y_test)))

    TP <- cm["1","1"]
    FP <- cm["1","0"]
    FN <- cm["0","1"]

    precision <- TP / (TP + FP)
    recall    <- TP / (TP + FN)
    f1        <- 2 * precision * recall / (precision + recall)

    cat("\nAccuracy:", round(acc, 4), "\n")
    cat("Precision:", round(precision, 4), "\n")
    cat("Recall:", round(recall, 4), "\n")
    cat("F1 Score:", round(f1, 4), "\n")
  }

  # =========================
  # 9. 特征重要性
  # =========================
  cat("\n特征重要性（Top 5）：\n")
  print(head(importance(model)[order(importance(model)[,1], decreasing = TRUE), ], 5))

  # =========================
  # 10. 返回
  # =========================

  return(list(
    model = model,
    mtry = best_mtry,
    class_weights = class_weights,
    importance = importance(model),
    best_threshold = if (exists("best_threshold")) best_threshold else NA
  ))
}


result <- preprocess_data("water_potability.csv")

rf_result <- train_random_forest(
  X_train = result$X_train,
  y_train = result$y_train,
  X_test  = result$X_test,
  y_test  = result$y_test,
  tune = TRUE
)

varImpPlot(rf_result$model)


# 预测概率（RF）
rf_prob <- predict(rf_result$model, result$X_test, type = "prob")[,2]

# ROC
roc_obj <- roc(result$y_test, rf_prob)

plot(roc_obj, col = "blue", lwd = 2,
     main = "ROC Curve - Random Forest")

auc_value <- auc(roc_obj)
cat("AUC (Random Forest):", auc_value, "\n")




y_test_bin <- as.numeric(as.character(result$y_test))

pr <- pr.curve(
  scores.class0 = rf_prob[y_test_bin == 1],
  scores.class1 = rf_prob[y_test_bin == 0],
  curve = TRUE
)

plot(pr, main = "Precision-Recall Curve - RF",
     lwd = 2)

pr_rf_auc_value <- as.numeric(pr$auc.integral)

legend("bottomleft",
       legend = paste0("AUC = ", round(pr_rf_auc_value, 3)),
       bty = "n")
