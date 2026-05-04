# 辅助函数：生成基础干净的测试数据，支持自定义特征维度
generate_dummy_data <- function(n_train = 50, n_test = 20, n_features = 2) {
  set.seed(123)
  X_train <- as.data.frame(matrix(rnorm(n_train * n_features), ncol = n_features))
  colnames(X_train) <- paste0("f", 1:n_features)
  y_train <- factor(sample(c("0", "1"), n_train, replace = TRUE))

  X_test <- as.data.frame(matrix(rnorm(n_test * n_features), ncol = n_features))
  colnames(X_test) <- paste0("f", 1:n_features)
  y_test <- factor(sample(c("0", "1"), n_test, replace = TRUE))

  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

# Base Cases
# Case A: Valid Model Structure
test_that("decision_tree returns a valid model structure", {
  data <- generate_dummy_data()

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  # 验证返回的是否是我们定义的 list 结构
  expect_type(res, "list")
  expect_true(all(c("model", "predictions", "pred_prob", "model_name") %in% names(res)))

  # 验证内部的 model 是否是正宗的 rpart 对象
  expect_s3_class(res$model, "rpart")

  # 深入检查 rpart 模型的内部组件（类似 SVM 检查 nSV）
  # 任何一棵成功的决策树，其内部必然包含 frame (节点信息) 和 splits (分裂规则)
  expect_true(nrow(res$model$frame) > 0)
  expect_true(!is.null(res$model$control))
})

# Case B：Correct Predictions
test_that("decision_tree returns correct predictions", {
  data <- generate_dummy_data()

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  # 检查预测标签
  # 注意：factor 的底层 typeof 是 "integer"，但规范的写法是检查它的 S3 class 是否为 factor
  expect_s3_class(res$predictions, "factor")
  expect_equal(length(res$predictions), nrow(data$X_test))

  # 检查预测概率
  expect_type(res$pred_prob, "double")
  expect_equal(length(res$pred_prob), nrow(data$X_test))
})

# Case C：High-Dimensional & Plotting
test_that("decision_tree handles high-dimensional data and plot runs without error", {
  # 生成高维数据 (15个特征) 来模拟复杂情况
  data_high_dim <- generate_dummy_data(n_train = 100, n_test = 30, n_features = 15)

  capture.output({
    res_high <- decision_tree(data_high_dim$X_train, data_high_dim$y_train,
                              data_high_dim$X_test, data_high_dim$y_test)
  })

  # 确保高维数据下模型依然正常吐出正确长度的结果
  expect_equal(length(res_high$predictions), nrow(data_high_dim$X_test))

  # 测试绘图功能是否能顺利执行（不抛出 Error）
  # 如果你的包引入了 rpart.plot，直接用它来画返回的模型即可
  expect_error(rpart.plot::rpart.plot(res_high$model), NA)
})


# Edge Cases
# Scenario A: Handles zero-variance columns safely
test_that("Scenario A: Handles zero-variance columns safely", {
  data <- generate_dummy_data()
  data$X_train$const_col <- rep(0, nrow(data$X_train))
  data$X_test$const_col <- rep(0, nrow(data$X_test))

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_s3_class(res$model, "rpart")
})

# Scenario B: Auto-converts numeric targets to factors
test_that("Scenario B: Auto-converts numeric targets to factors", {
  data <- generate_dummy_data()
  y_train_num <- as.numeric(as.character(data$y_train))
  y_test_num <- as.numeric(as.character(data$y_test))

  capture.output({
    res <- decision_tree(data$X_train, y_train_num, data$X_test, y_test_num)
  })
  expect_s3_class(res$predictions, "factor")
})

# Scenario C: Generates output with extreme test set imbalance
test_that("Scenario C: Generates output with extreme test set imbalance", {
  data <- generate_dummy_data()
  data$y_test <- factor(rep("0", nrow(data$X_test)), levels = c("0", "1"))

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario D: Handles missing values (NA) in test set
test_that("Scenario D: Handles missing values (NA) in test set", {
  data <- generate_dummy_data()
  data$X_test[1, 1] <- NA
  data$X_test[5, 2] <- NA

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_length(res$predictions, nrow(data$X_test))
})

# Scenario E: Errors on missing features, handles scrambled columns
test_that("Scenario E: Errors on missing features, handles scrambled columns", {
  data <- generate_dummy_data()

  X_test_scrambled <- data$X_test[, c("f2", "f1")]
  capture.output({
    res_scrambled <- decision_tree(data$X_train, data$y_train, X_test_scrambled, data$y_test)
  })
  expect_length(res_scrambled$predictions, nrow(data$X_test))

  X_test_missing <- data$X_test[, "f1", drop = FALSE]
  capture.output({
    expect_error(decision_tree(data$X_train, data$y_train, X_test_missing, data$y_test))
  })
})

# Scenario F: Survives the 1-row data frame trap
test_that("Scenario F: Survives the 1-row data frame trap", {
  data <- generate_dummy_data()
  X_test_single <- data$X_test[1, , drop = FALSE]
  y_test_single <- data$y_test[1]

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, X_test_single, y_test_single)
  })
  expect_length(res$predictions, 1)
})

# Scenario G: Errors out on unseen factor levels in target
test_that("Scenario G: Errors out on unseen factor levels in target", {
  data <- generate_dummy_data()
  y_test_alien <- factor(sample(c("0", "1", "2"), nrow(data$X_test), replace = TRUE), levels = c("0", "1", "2"))

  capture.output({
    expect_error(decision_tree(data$X_train, data$y_train, data$X_test, y_test_alien))
  })
})
