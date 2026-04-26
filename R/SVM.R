library(e1071)

train_svm_model <- function(X_train, y_train,
                            X_test = NULL, y_test = NULL,
                            kernel_type = "radial",
                            cost = 1, gamma = NULL,
                            tune = TRUE) {

  # =========================
  # 1. 标签转为因子（分类任务必须）
  # =========================
  X_train <- as.data.frame(lapply(X_train, as.numeric))
  y_train <- as.factor(y_train)

  if (!is.null(X_test)) {
    X_test <- as.data.frame(lapply(X_test, as.numeric))
    y_test <- as.factor(y_test)
  }

  # =========================
  # 2. 自动计算类别权重（核心）
  # =========================
  class_counts <- table(y_train)

  # 反比例权重（少数类权重大）
  class_weights <- sum(class_counts) / (length(class_counts) * class_counts)

  print("类别权重：")
  print(class_weights)

  # =========================
  # 2. 自动调参（可选）
  # =========================
  if (tune) {

    cat("正在进行参数调优...\n")

    tune_result <- tune(
      svm,
      train.x = X_train,
      train.y = y_train,
      kernel = kernel_type,
      ranges = list(
        cost = c(0.1, 1, 10),
        gamma = c(0.01, 0.1, 1)
      ),
      class.weights = class_weights,
      tunecontrol = tune.control(cross = 5)  # 加速
    )

    best_model <- tune_result$best.model

    cat("最优参数：\n")
    print(tune_result$best.parameters)

  } else {

    # =========================
    # 3. 普通训练
    # =========================
    best_model <- svm(
      x = X_train,
      y = y_train,
      kernel = kernel_type,
      cost = cost,
      gamma = gamma,
      class.weights = class_weights
    )
  }


  # =========================
  # 4. 模型预测
  # =========================

  train_pred <- predict(best_model, X_train)

  if (!is.null(X_test)) {
    test_pred <- predict(best_model, X_test)
  }


  # =========================
  # 5. 模型评估
  # =========================
  cat("\n=== 训练集 ===\n")
  print(table(Predicted = train_pred, Actual = y_train))

  train_acc <- mean(train_pred == y_train)

  if (!is.null(y_test)) {
    cat("\n=== 测试集 ===\n")
    cm <- table(Predicted = test_pred, Actual = y_test)
    print(cm)

    test_acc <- mean(test_pred == y_test)

    # Precision / Recall / F1（针对类别1）
    TP <- cm["1","1"]
    FP <- cm["1","0"]
    FN <- cm["0","1"]
    precision <- TP / (TP + FP)
    recall    <- TP / (TP + FN)
    f1        <- 2 * precision * recall / (precision + recall)


    cat("训练集准确率:", round(train_acc, 4), "\n")
    cat("测试集准确率:", round(test_acc, 4), "\n")
    cat("Precision:", round(precision, 4), "\n")
    cat("Recall:", round(recall, 4), "\n")
    cat("F1 Score:", round(f1, 4), "\n")

    # 混淆矩阵
    cat("\n测试集混淆矩阵：\n")
    print(table(Predicted = test_pred, Actual = y_test))

  } else {
    cat("训练集准确率:", round(train_acc, 4), "\n")
  }


  return(list(
    model = best_model,
    train_accuracy = train_acc,
    test_accuracy = ifelse(exists("test_acc"), test_acc, NA),
    train_pred = train_pred,
    test_pred = ifelse(exists("test_pred"), test_pred, NULL),
    class_weights = class_weights
  ))
}

result <- preprocess_data("water_potability.csv")
svm_result <- train_svm_model(
  X_train = result$X_train,
  y_train = result$y_train,
  X_test  = result$X_test,
  y_test  = result$y_test,
  tune = TRUE
)

# 模型
model <- svm_result$model

# 准确率
svm_result$train_accuracy
svm_result$test_accuracy

# 预测结果
head(svm_result$test_pred)

