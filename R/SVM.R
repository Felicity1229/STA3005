library(e1071)

train_svm_model <- function(X_train, y_train,
                            X_test = NULL, y_test = NULL,
                            kernel_type = "radial",
                            tune = TRUE) {

  full_data <- rbind(X_train, X_test)

  X_all <- model.matrix(~ . -1, data = full_data)

  X_train <- X_all[1:nrow(X_train), ]
  X_test  <- X_all[(nrow(X_train)+1):nrow(X_all), ]

  y_train <- as.factor(y_train)
  y_test  <- as.factor(y_test)


  # 类别权重

  class_counts <- table(y_train)
  class_weights <- sum(class_counts) / (length(class_counts) * class_counts)


  # 调参

  if (tune) {

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
      probability = TRUE,
      tunecontrol = tune.control(cross = 5)
    )

    best_model <- tune_result$best.model

  } else {

    best_model <- svm(
      x = X_train,
      y = y_train,
      kernel = kernel_type,
      probability = TRUE,
      class.weights = class_weights
    )
  }

  # 预测 + 概率
  test_pred <- predict(best_model, X_test, probability = TRUE)

  prob_attr <- attr(test_pred, "probabilities")

  pred_prob <- prob_attr[, "1"]

  return(list(
    model_name = "SVM",
    predictions = test_pred,
    pred_prob = pred_prob,
    model = best_model
  ))
}

svm_result <- train_svm_model(
  X_train = result$X_train,
  y_train = result$y_train,
  X_test  = result$X_test,
  y_test  = result$y_test,
  tune = TRUE
)


svm_result$predictions
svm_result$pred_prob

head(svm_result$predictions)
head(svm_result$pred_prob)
