# 1 读取数据----
df <- read.csv("water_potability.csv")
head(df)
str(df)
summary(df)

# 2 检查缺失值情况----
colSums(is.na(df))

# 3 用均值填充缺失值----

# ph
df$ph[is.na(df$ph)] <- mean(df$ph, na.rm = TRUE)
# Sulfate
df$Sulfate[is.na(df$Sulfate)] <- mean(df$Sulfate, na.rm = TRUE)
# Trihalomethanes
df$Trihalomethanes[is.na(df$Trihalomethanes)] <- mean(df$Trihalomethanes, na.rm = TRUE)

colSums(is.na(df))

# 4 相关性分析-----
cor_matrix <- cor(df)
print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# 5 因变量分布：是否可饮用----
table(df$Potability)

# 6 划分训练集和测试集----
library(caTools)

set.seed(3)

# 按 70% 训练，30% 测试划分
split <- sample.split(df$Potability, SplitRatio = 0.7)

train_data <- subset(df, split == TRUE)
test_data  <- subset(df, split == FALSE)

dim(train_data)
dim(test_data)

# 7 存储训练集和测试集的特征和标签----
X_train <- train_data[, -10]   # 去掉 Potability
y_train <- train_data$Potability

X_test <- test_data[, -10]
y_test <- test_data$Potability

# 8 归一化函数----
min_max_norm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

X_train_norm <- as.data.frame(lapply(X_train, min_max_norm))

# 测试集用训练集的 min 和 max
X_test_norm <- as.data.frame(mapply(function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}, X_test,
min_val = sapply(X_train, min),
max_val = sapply(X_train, max)))

# 注释----
# 读入原始数据，对数据结构与基本统计特征进行了初步分析。检查缺失值情况，对存在缺失的变量采用mean imputation
# 按 7:3 的比例将数据划分为训练集（train_data）和测试集（test_data）
# 输入特征分别存储为 X_train 和 X_test（数据类型为 data.frame），目标变量存储为 y_train 和 y_test（数值型向量，取值为 0/1）
# 对特征数据进行Min-Max，归一化后的训练集和测试集分别存储为 X_train_norm 和 X_test_norm（data.frame）
# 输入特征：X_train_norm, X_test_norm; 标签数据：y_train, y_test
