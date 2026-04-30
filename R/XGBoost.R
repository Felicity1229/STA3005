library(xgboost)
# library(pROC)

train_xgb_full <- function(X_train, y_train, X_test, y_test) {

  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest  <- xgb.DMatrix(X_test, label = y_test)

  watchlist <- list(
    train = dtrain,
    test = dtest
  )

  params <- list(
    objective = "binary:logistic",
    eval_metric = c("auc", "logloss"),
    eta = 0.05,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  cat("\nTraining XGBoost...\n")

  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 300,
    watchlist = watchlist,
    print_every_n = 10,
    verbose = 1
  )

  # 预测概率----
  prob <- predict(model, dtest)

  # ROC + AUC
  # =========================
  # roc_obj <- roc(y_test, prob)
  # cat("\nXGBoost AUC:", auc(roc_obj), "\n")

  # 自动找 threshold（F1）
  find_best_threshold <- function(prob, y_true) {

    thresholds <- seq(0.1, 0.9, 0.01)

    f1_scores <- sapply(thresholds, function(t) {

      pred <- ifelse(prob > t, 1, 0)

      cm <- table(factor(pred, levels = c(0,1)),
                  factor(y_true, levels = c(0,1)))

      TP <- cm["1","1"]
      FP <- cm["1","0"]
      FN <- cm["0","1"]

      precision <- ifelse(TP+FP==0,0,TP/(TP+FP))
      recall    <- ifelse(TP+FN==0,0,TP/(TP+FN))

      if (precision + recall == 0) return(0)

      2 * precision * recall / (precision + recall)
    })

    list(
      best_threshold = thresholds[which.max(f1_scores)],
      best_f1 = max(f1_scores)
    )
  }

  th <- find_best_threshold(prob, y_test)

  cat("\nBest threshold:", th$best_threshold)
  cat("\nBest F1:", th$best_f1, "\n")

  # 用最佳 threshold 分类
  pred <- ifelse(prob > th$best_threshold, 1, 0)

  cm <- table(Predicted = pred, Actual = y_test)

  cat("\nConfusion Matrix\n")
  print(cm)

  # feature importance
  imp <- xgb.importance(model = model)

  cat("\nFeature Importance：\n")
  print(imp)

  return(list(
    model = model,
    auc = auc(roc_obj),
    best_threshold = th$best_threshold,
    importance = imp
  ))
}

# result <- preprocess_data("Breast_Cancer.csv")

X_train <- result$X_train_norm
X_test  <- result$X_test_norm
y_train <- result$y_train
y_test  <- result$y_test

xgb_result <- train_xgb_full(
  X_train = X_train,
  y_train = y_train,
  X_test  = X_test,
  y_test  = y_test
)

prob <- predict(xgb_result$model,
                xgb.DMatrix(as.matrix(X_test)))

# roc_xgb <- roc(y_test, prob)

# plot(roc_xgb, col = "red", lwd = 2,
#      main = "ROC - XGBoost")




# 预测概率（RF）
# rf_prob <- predict(rf_result$model, result$X_test, type = "prob")[,2]

# ROC
# roc_obj <- roc(result$y_test, rf_prob)

# plot(roc_xgb, col = "red", lwd = 2,
#     main = "ROC Comparison (RF vs XGBoost)")

# lines(roc_obj, col = "blue", lwd = 2)

# legend("bottomright",
#        legend = c("XGBoost", "Random Forest"),
#        col = c("red", "blue"),
#        lwd = 2)

# auc(roc_xgb)
