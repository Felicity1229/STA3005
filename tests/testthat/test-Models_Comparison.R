library(testthat)
library(caret)
library(pROC)
library(e1071)
library(rpart)

test_that("End-to-End Integration: Preprocess -> Train -> Compare works correctly", {

  # 1. 设置随机种子
  set.seed(3005)

  # 2. 直接使用包内自带的 .rda 数据集
  mock_data <- heart_dataset

  # 3. 运行预处理管道 (传入数据框)
  data_res <- pre_process_data(data = mock_data, tag_column = 14, split_ratio = 0.7, seed = 3005)

  X_train <- data_res$X_train
  y_train <- data_res$y_train
  X_test <- data_res$X_test
  y_test <- data_res$y_test

  # 4. 训练模型
  dt_res <- decision_tree(X_train, y_train, X_test, y_test, cp = 0.01)
  svm_res <- train_svm(X_train, y_train, X_test, y_test, kernel_type = "radial", tune = FALSE)

  # 5. 组装 models_list
  # 由于前后端标签统一为了 0 和 1，现在可以直接将模型输出放入 list
  my_models <- list(
    "Decision Tree" = list(
      predictions = dt_res$predictions,
      pred_prob = dt_res$pred_prob
    ),
    "SVM" = list(
      predictions = svm_res$predictions,
      pred_prob = svm_res$pred_prob
    )
  )

  # 将真实的 y_test 转换为 factor，对齐比较函数的期望格式
  y_test_formatted <- factor(y_test, levels = c("0", "1"))

  # 6. 运行对比函数 (以 "1" 为正类)
  comparison_result <- models_comparison(true_labels = y_test_formatted,
                            models_list = my_models,
                            positive_class = "1")

  # 7. 核心断言
  expect_s3_class(comparison_result, "data.frame")
  expect_equal(nrow(comparison_result), 2)
  expect_true("AUC" %in% colnames(comparison_result))
  expect_true(all(!is.na(comparison_result$AUC)))
  expect_true(all(comparison_result$AUC >= 0 & comparison_result$AUC <= 1))
})
