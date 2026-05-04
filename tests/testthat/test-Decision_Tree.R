# tests/testthat/test-decision_tree.R

# =====================================================================
# 辅助函数：生成基础干净的测试数据，避免在每个测试块里重复写太多代码
# =====================================================================
generate_dummy_data <- function(n_train = 50, n_test = 20) {
  set.seed(123)
  X_train <- data.frame(f1 = rnorm(n_train), f2 = runif(n_train))
  y_train <- factor(sample(c("0", "1"), n_train, replace = TRUE))
  X_test <- data.frame(f1 = rnorm(n_test), f2 = runif(n_test))
  y_test <- factor(sample(c("0", "1"), n_test, replace = TRUE))
  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

# =====================================================================
# 场景 A：零方差 (Zero Variance)
# =====================================================================
test_that("Scenario A: Handles zero-variance columns safely", {
  data <- generate_dummy_data()
  # 故意注入一列全是 0 的零方差特征
  data$X_train$const_col <- rep(0, nrow(data$X_train))
  data$X_test$const_col <- rep(0, nrow(data$X_test))

  # 期望：函数能正常运行不崩溃（内部应已自动剔除该列）
  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })
  expect_type(res, "list")
  expect_s3_class(res$model, "rpart")
})

# =====================================================================
# 场景 B：类型不符 (Type Mismatch)
# =====================================================================
test_that("Scenario B: Auto-converts numeric targets to factors", {
  data <- generate_dummy_data()
  # 剥夺 Factor 身份，退化为纯数值
  y_train_num <- as.numeric(as.character(data$y_train))
  y_test_num <- as.numeric(as.character(data$y_test))

  capture.output({
    res <- decision_tree(data$X_train, y_train_num, data$X_test, y_test_num)
  })

  # 期望：输出的预测结果仍然被自动强制转换成了规范的 Factor
  expect_s3_class(res$predictions, "factor")
})

# =====================================================================
# 场景 C：极度不平衡 (Extreme Imbalance)
# =====================================================================
test_that("Scenario C: Generates confusion matrix with extreme test set imbalance", {
  data <- generate_dummy_data()
  # 让测试集只有 "0" 这一种标签
  data$y_test <- factor(rep("0", nrow(data$X_test)), levels = c("0", "1"))

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  # 期望：即使测试集只有一类，因为我们锁定了 levels，依然能输出正常的结果
  expect_length(res$predictions, nrow(data$X_test))
  expect_true(all(res$pred_prob >= 0 & res$pred_prob <= 1))
})

# =====================================================================
# 场景 D：存在缺失值 (Missing Values)
# =====================================================================
test_that("Scenario D: Handles missing values (NA) in test set", {
  data <- generate_dummy_data()
  # 在测试集中随机注入 NA
  data$X_test$f1[1] <- NA
  data$X_test$f2[5] <- NA

  # 期望：rpart 自带缺失值代理拆分(surrogate split)功能，预测不该崩溃
  capture.output({
    res <- decision_tree(data$X_train, data$y_train, data$X_test, data$y_test)
  })

  # 依然应该吐出和测试集行数相等的预测值
  expect_length(res$predictions, nrow(data$X_test))
})

# =====================================================================
# 场景 E：特征维度错乱 (Feature Dimension Mismatch)
# =====================================================================
test_that("Scenario E: Errors on missing features, handles scrambled columns", {
  data <- generate_dummy_data()

  # 1. 顺序打乱 (Scrambled) -> 应该没问题，predict会自动按列名匹配
  X_test_scrambled <- data$X_test[, c("f2", "f1")]
  capture.output({
    res_scrambled <- decision_tree(data$X_train, data$y_train, X_test_scrambled, data$y_test)
  })
  expect_length(res_scrambled$predictions, nrow(data$X_test))

  # 2. 缺失列 (Missing Column) -> 必须报错
  X_test_missing <- data$X_test[, "f1", drop = FALSE]

  # 期望：遇到残缺特征矩阵时，主动抛出 Error
  capture.output({
    expect_error(decision_tree(data$X_train, data$y_train, X_test_missing, data$y_test))
  })
})

# =====================================================================
# 场景 F：单行降维陷阱 (The 1-Row Trap)
# =====================================================================
test_that("Scenario F: Survives the 1-row data frame trap", {
  data <- generate_dummy_data()

  # 极其危险的操作：取第一行数据，如果不加 drop=FALSE，R 会把它变成 numeric vector
  X_test_single <- data$X_test[1, , drop = FALSE]
  y_test_single <- data$y_test[1]

  capture.output({
    res <- decision_tree(data$X_train, data$y_train, X_test_single, y_test_single)
  })

  # 期望：能成功返回 1 个预测结果，而不是抛出矩阵维度错误
  expect_length(res$predictions, 1)
  expect_length(res$pred_prob, 1)
})

# =====================================================================
# 场景 G：未见过的类别 (Unseen Factor Levels)
# =====================================================================
test_that("Scenario G: Errors out on unseen factor levels in target", {
  data <- generate_dummy_data()

  # 制造一只“异形”：测试集里突然冒出分类 "2"，而训练集里只有 "0" 和 "1"
  # 我们强行给它赋予 c("0", "1", "2") 的 levels
  y_test_alien <- factor(sample(c("0", "1", "2"), nrow(data$X_test), replace = TRUE), levels = c("0", "1", "2"))

  # 期望：caret 的 confusionMatrix 或者 rpart 会因为 levels 不一致而崩溃
  # 所以这里我们 expect_error，证明这种非法数据会被有效拦截
  capture.output({
    expect_error(decision_tree(data$X_train, data$y_train, data$X_test, y_test_alien))
  })
})
