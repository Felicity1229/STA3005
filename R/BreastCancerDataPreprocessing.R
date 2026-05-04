#' Preprocess Breast Cancer Dataset
#'
#' Performs complete data preprocessing for the breast cancer dataset including:
#' - Target variable encoding (Alive -> 1, Dead -> 0)
#' - One-hot encoding for categorical variables (Race, Marital.Status)
#' - Ordinal encoding for ordered variables (T.Stage, N.Stage, X6th.Stage, differentiate)
#' - Grade column cleaning and encoding
#' - Binary encoding for Estrogen.Status, Progesterone.Status, A.Stage
#' - Feature engineering (Positive_Ratio)
#' - Z-score scaling for continuous variables (Age, Tumor.Size, Survival.Months)
#' - Train/test split with matrix transposition for neural network input
#'
#' @param train_data A data frame containing the Breast Cancer dataset.
#' @param train_ratio Proportion of data to use for training (default: 0.8)
#' @return A list containing:
#'   \item{X_train}{Training feature matrix (features as rows, samples as columns)}
#'   \item{y_train}{Training labels (1 row, samples as columns)}
#'   \item{X_test}{Test feature matrix}
#'   \item{y_test}{Test labels}
#'   \item{train_data}{Processed training data frame}
#'   \item{test_data}{Processed test data frame}
#'
#' @importFrom stats model.matrix
#' @export
#'
#' @examples
#' \dontrun{
#' result <- preprocessBreastCancerData("breast_cancer_dataset")
#' X_train <- result$X_train
#' y_train <- result$y_train
#' }
preprocessBreastCancerData <- function(train_data, train_ratio = 0.8) {

  # 1. Encode target variable Status (Alive -> 1, Dead -> 0)
  train_data$Status <- ifelse(train_data$Status == "Alive", 1, 0)

  # 2. One-hot encoding for Race
  race_dummies <- model.matrix(~ Race - 1, train_data)
  train_data <- cbind(train_data, race_dummies)
  train_data$Race <- NULL

  # 3. One-hot encoding for Marital.Status
  marital_dummies <- model.matrix(~ Marital.Status - 1, train_data)
  train_data <- cbind(train_data, marital_dummies)
  train_data$Marital.Status <- NULL

  # 4. Ordinal encoding for T.Stage (T1 to T4, increasing severity)
  t_stage_order <- c("T1", "T2", "T3", "T4")
  train_data$T.Stage <- as.numeric(factor(train_data$T.Stage,
                                          levels = t_stage_order,
                                          ordered = TRUE))

  # 5. Ordinal encoding for N.Stage (N1 to N3, increasing metastasis)
  n_stage_order <- c("N1", "N2", "N3")
  train_data$N.Stage <- as.numeric(factor(train_data$N.Stage,
                                          levels = n_stage_order,
                                          ordered = TRUE))

  # 6. Ordinal encoding for X6th.Stage
  stage_order <- c("IIA", "IIB", "IIIA", "IIIB", "IIIC")
  train_data$X6th.Stage <- as.numeric(factor(train_data$X6th.Stage,
                                             levels = stage_order,
                                             ordered = TRUE))

  # 7. Ordinal encoding for differentiate
  differentiate_order <- c("Well differentiated",
                           "Moderately differentiated",
                           "Poorly differentiated",
                           "Undifferentiated")
  train_data$differentiate <- as.numeric(factor(train_data$differentiate,
                                                levels = differentiate_order,
                                                ordered = TRUE))

  # 8. Clean and encode Grade column
  train_data$Grade <- ifelse(
    train_data$Grade == " anaplastic; Grade IV", 4,
    ifelse(train_data$Grade == "3", 3,
           ifelse(train_data$Grade == "2", 2,
                  ifelse(train_data$Grade == "1", 1, NA)))
  )

  # 9. Binary encoding for Estrogen.Status (Positive -> 1, Negative -> 0)
  train_data$Estrogen.Status <- ifelse(train_data$Estrogen.Status == "Positive", 1, 0)

  # 10. Binary encoding for Progesterone.Status
  train_data$Progesterone.Status <- ifelse(train_data$Progesterone.Status == "Positive", 1, 0)

  # 11. Binary encoding for A.Stage (Regional -> 1, Distant -> 0)
  train_data$A.Stage <- ifelse(train_data$A.Stage == "Regional", 1, 0)

  # 12. Feature engineering: Positive_Ratio
  train_data$Positive_Ratio <- train_data$Regional.Node.Positive /
    train_data$Regional.Node.Examined
  train_data$Regional.Node.Examined <- NULL
  train_data$Regional.Node.Positive <- NULL

  # 13. Z-score scaling for continuous variables
  train_data$Age <- as.numeric(scale(train_data$Age))
  train_data$Tumor.Size <- as.numeric(scale(train_data$Tumor.Size))
  train_data$Survival.Months <- as.numeric(scale(train_data$Survival.Months))

  # 14. Shuffle data
  train_data <- train_data[sample(nrow(train_data)), ]

  # 15. Move Status (label) to last column
  train_data <- train_data[, c(setdiff(names(train_data), "Status"), "Status")]

  # 16. Train/test split
  split_idx <- floor(train_ratio * nrow(train_data))
  train <- train_data[1:split_idx, ]
  test <- train_data[(split_idx + 1):nrow(train_data), ]

  # 17. Extract features and labels
  X_train <- train[, -ncol(train)]
  y_train <- train[, ncol(train)]
  dim(y_train) <- c(length(y_train), 1)

  X_test <- test[, -ncol(test)]
  y_test <- test[, ncol(test)]
  dim(y_test) <- c(length(y_test), 1)

  # 18. Convert to matrix and transpose (features as rows, samples as columns)
  X_train <- t(as.matrix(X_train))
  y_train <- t(as.matrix(y_train))

  X_test <- t(as.matrix(X_test))
  y_test <- t(as.matrix(y_test))

  # Return results as a list
  return(list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test,
    train_data = train,
    test_data = test
  ))
}
