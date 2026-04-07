preprocess_data <- function(file_path, tag_column, split_ratio = 0.7, seed = 3) {
  # 1 读取数据----
  df <- read.csv(file_path)
  head(df)
  str(df)
  summary(df)
  tag_col_name <- names(df)[tag_column]

  # 2 检查缺失值情况----
  missing_counts <- colSums(is.na(df))
  missing_counts

  # 3 用均值填充缺失值----
  if (sum(missing_counts) > 0){
    cols_with_na <- names(missing_counts[missing_counts > 0])
    cols_with_na

    for (col in cols_with_na) {
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    }

    print("Filled:")
    print(paste(cols_with_na, collapse = ", "))
    print(colSums(is.na(df)))
  }else{
    print("No missing value")
  }

  # 4 相关性分析-----
  numeric_cols <- sapply(df, is.numeric)

  if (sum(numeric_cols) >= 2) {  # 至少需要2列才能计算相关性
    cor_matrix <- cor(df[, numeric_cols])
    print(cor_matrix)
  } else {
    print("The correlation matrix cannot be calculated.")
  }

  ##library(corrplot)
  ##corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

  # 5 因变量分布----
  print(table(df[[tag_col_name]]))

  # 6 划分训练集和测试集----
  library(caTools)
  set.seed(seed)

  # 按 70% 训练，30% 测试划分
  split <- sample.split(df[[tag_col_name]], SplitRatio = split_ratio)

  train_data <- subset(df, split == TRUE)
  test_data  <- subset(df, split == FALSE)

  dim(train_data)
  dim(test_data)

  unique(train_data[[tag_col_name]])

  # 7 存储训练集和测试集的特征和标签----
  X_train <- train_data[, -tag_column]
  y_train <- as.numeric(factor(train_data[[tag_col_name]])) - 1

  X_test <- test_data[, -tag_column]
  y_test  <- as.numeric(factor(test_data[[tag_col_name]])) - 1

  # 8 归一化函数----
  numeric_cols_train <- sapply(X_train, is.numeric)

  if (sum(numeric_cols_train) > 0) {
    min_vals <- sapply(X_train[, numeric_cols_train], min)
    max_vals <- sapply(X_train[, numeric_cols_train], max)

    min_max_norm <- function(x) {
      return((x - min(x)) / (max(x) - min(x)))
    }

    X_train_numeric <- as.data.frame(lapply(X_train[, numeric_cols_train], min_max_norm))
    X_train_non_numeric <- X_train[, !numeric_cols_train, drop = FALSE]

    X_train_norm <- cbind(X_train_numeric, X_train_non_numeric)

    # 测试集用训练集的 min 和 max
    X_test_numeric <- as.data.frame(mapply(function(x, min_val, max_val) {
      (x - min_val) / (max_val - min_val)
    }, X_test[, numeric_cols_train], min_vals, max_vals, SIMPLIFY = FALSE))

    X_test_non_numeric <- X_test[, !numeric_cols_train, drop = FALSE]

    X_test_norm <- cbind(X_test_numeric, X_test_non_numeric)
  }else{
    X_train_norm = X_train
    X_test_norm = X_test
    min_vals = max_vals = cor_matrix = NA
  }

  # 注释----
  # 读入原始数据，对数据结构与基本统计特征进行了初步分析。检查缺失值情况，对存在缺失的变量采用mean imputation
  # 按 7:3 的比例将数据划分为训练集（train_data）和测试集（test_data）
  # 输入特征分别存储为 X_train 和 X_test（数据类型为 data.frame），目标变量存储为 y_train 和 y_test（数值型向量，取值为 0/1）
  # 对特征数据进行Min-Max，归一化后的训练集和测试集分别存储为 X_train_norm 和 X_test_norm（data.frame）
  # 输入特征：X_train_norm, X_test_norm; 标签数据：y_train, y_test

  return(list(
    X_train = X_train,
    X_test  = X_test,
    X_train_norm = X_train_norm,
    X_test_norm  = X_test_norm,
    y_train = y_train,
    y_test  = y_test,
    train_data = train_data,
    cor_matrix = cor_matrix,
    min_vals = min_vals,   # 用于新数据
    max_vals = max_vals
  ))

}

# 调用
file_path = "E:/Semester6_Year3Term2/STA3005/STA3005/R/mushrooms.csv"
result <- preprocess_data(file_path, tag_column = 1)

# 特征
X_train <- result$X_train
X_test  <- result$X_test
# 标签
y_train <- result$y_train
y_test  <- result$y_test

y_train
