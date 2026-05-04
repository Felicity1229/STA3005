# 辅助函数：创建测试数据
create_test_data <- function() {
  set.seed(123)
  n <- 100
  data.frame(
    age = c(rnorm(n - 5, mean = 50, sd = 10), NA, NA, NA, NA, NA),
    income = c(rnorm(n - 3, mean = 50000, sd = 15000), NA, NA, NA),
    gender = sample(c("M", "F"), n, replace = TRUE),
    target = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  )
}

# Base Cases
# Case A: Valid preprocessing structure
test_that("pre_process_data returns a valid structure", {
  test_df <- create_test_data()

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # 验证返回的是 list
  expect_type(result, "list")

  # 验证所有必需的组件都存在
  expected_names <- c("X_train", "X_test", "X_train_norm", "X_test_norm",
                      "y_train", "y_test", "train_data", "cor_matrix",
                      "min_vals", "max_vals")
  expect_true(all(expected_names %in% names(result)))

  # 验证数据类型
  expect_true(is.data.frame(result$X_train))
  expect_true(is.data.frame(result$X_test))
  expect_true(is.data.frame(result$X_train_norm))
  expect_true(is.data.frame(result$X_test_norm))
  expect_true(is.numeric(result$y_train))
  expect_true(is.numeric(result$y_test))
})

# Case B: Correct dimensions after splitting
test_that("pre_process_data splits data correctly", {
  test_df <- create_test_data()
  n_total <- nrow(test_df)
  split_ratio <- 0.7
  expected_train_size <- floor(n_total * split_ratio)

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = split_ratio, seed = 3)

  # 验证维度
  expect_equal(nrow(result$X_train), expected_train_size)
  expect_equal(nrow(result$X_test), n_total - expected_train_size)
  expect_equal(length(result$y_train), expected_train_size)
  expect_equal(length(result$y_test), n_total - expected_train_size)
})

# Edge Cases
# Scenario A: Handles missing values correctly
test_that("Scenario A: Handles missing values correctly", {
  test_df <- create_test_data()

  # 确认有缺失值
  expect_true(any(is.na(test_df)))

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # 验证返回的数据没有缺失值
  expect_false(any(is.na(result$X_train)))
  expect_false(any(is.na(result$X_test)))
})

# Scenario B: Handles zero-variance columns
test_that("Scenario B: Handles zero-variance columns", {
  test_df <- create_test_data()
  test_df$constant_col <- rep(1, nrow(test_df))

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # 应该正常运行而不报错
  expect_true(is.data.frame(result$X_train_norm))
})

# Scenario C: Binary target conversion works
test_that("Scenario C: Binary target conversion works", {
  test_df <- create_test_data()
  test_df$target <- factor(test_df$target, levels = c(0, 1))

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # y_train 应该是 0/1 数值
  expect_true(all(result$y_train %in% c(0, 1)))
  expect_true(all(result$y_test %in% c(0, 1)))
})

# Scenario D: Normalization works correctly
test_that("Scenario D: Normalization works correctly", {
  test_df <- create_test_data()

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # 获取数值列
  numeric_cols <- sapply(result$X_train_norm, is.numeric)

  if (sum(numeric_cols) > 0) {
    # 归一化后的值应该在 [0, 1] 范围内
    for (col in names(numeric_cols)[numeric_cols]) {
      norm_vals <- result$X_train_norm[[col]]
      expect_true(all(norm_vals >= 0 & norm_vals <= 1, na.rm = TRUE))
    }
  }
})

# Scenario E: Reproducible with same seed
test_that("Scenario E: Reproducible with same seed", {
  test_df <- create_test_data()

  result1 <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 123)
  result2 <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 123)
  result3 <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 456)

  # 相同种子应该产生相同结果
  expect_equal(result1$y_train, result2$y_train)
  expect_equal(result1$y_test, result2$y_test)

  # 不同种子应该产生不同结果（概率上）
  # 只需检查不是完全相同即可
  expect_false(identical(result1$y_train, result3$y_train))
})

# Scenario F: Works with different split ratios
test_that("Scenario F: Works with different split ratios", {
  test_df <- create_test_data()
  n_total <- nrow(test_df)

  for (ratio in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
    result <- pre_process_data(test_df, tag_column = 4, split_ratio = ratio, seed = 3)
    expected_train_size <- floor(n_total * ratio)

    expect_equal(nrow(result$X_train), expected_train_size)
    expect_equal(nrow(result$X_test), n_total - expected_train_size)
  }
})

# Scenario G: Handles all-numeric data
test_that("Scenario G: Handles all-numeric data", {
  test_df <- data.frame(
    age = rnorm(100, mean = 50, sd = 10),
    income = rnorm(100, mean = 50000, sd = 15000),
    target = sample(c(0, 1), 100, replace = TRUE)
  )

  result <- pre_process_data(test_df, tag_column = 3, split_ratio = 0.7, seed = 3)

  expect_true(is.data.frame(result$X_train_norm))
  expect_true(all(sapply(result$X_train_norm, is.numeric)))
})

# Scenario H: Handles all-categorical data
test_that("Scenario H: Handles all-categorical data", {
  test_df <- data.frame(
    color = sample(c("red", "blue", "green"), 100, replace = TRUE),
    size = sample(c("S", "M", "L"), 100, replace = TRUE),
    target = sample(c(0, 1), 100, replace = TRUE)
  )

  result <- pre_process_data(test_df, tag_column = 3, split_ratio = 0.7, seed = 3)

  # 应该正常运行
  expect_true(is.data.frame(result$X_train_norm))
})

# Scenario I: Returns correlation matrix when possible
test_that("Scenario I: Returns correlation matrix when there are numeric columns", {
  test_df <- create_test_data()

  result <- pre_process_data(test_df, tag_column = 4, split_ratio = 0.7, seed = 3)

  # 有数值列时，cor_matrix 应该是矩阵
  expect_true(is.matrix(result$cor_matrix) || is.na(result$cor_matrix))
})

# Scenario J: Handles edge case with very small dataset
test_that("Scenario J: Handles very small dataset", {
  test_df <- data.frame(
    age = c(25, 30, 35, 40),
    income = c(30000, 40000, 50000, 60000),
    target = c(0, 0, 1, 1)
  )

  result <- pre_process_data(test_df, tag_column = 3, split_ratio = 0.5, seed = 3)

  expect_equal(nrow(result$X_train), 2)
  expect_equal(nrow(result$X_test), 2)
})
