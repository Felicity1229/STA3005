#' Data Preprocessing Pipeline for Binary Classification
#'
#' A comprehensive data preprocessing function that handles data loading,
#' missing value imputation, correlation analysis, train-test splitting,
#' and Min-Max normalization. Designed for binary classification tasks.
#'
#' @name pre_process_data
#' @author ZHANG Yibing
NULL

#' Preprocess Data for Model Training and Evaluation
#'
#' Performs end-to-end data preprocessing including:
#' \itemize{
#'   \item Reading data from CSV file
#'   \item Checking and imputing missing values with mean imputation
#'   \item Computing correlation matrix for numeric variables
#'   \item Displaying target variable distribution
#'   \item Splitting data into training and test sets (stratified by default)
#'   \item Applying Min-Max normalization on numeric features
#' }
#'
#' @param data A data frame containing the dataset to be processed.
#' @param tag_column Integer indicating which column contains the target variable.
#'        For binary classification, values will be converted to 0/1.
#' @param split_ratio Numeric value between 0 and 1 specifying the proportion
#'        of data to use for training. Default is 0.7 (70% train, 30% test).
#' @param seed Integer seed for reproducible random splitting. Default is 3.
#' @return A list containing:
#'   \item{X_train}{Raw training features (data frame, before normalization)}
#'   \item{X_test}{Raw test features (data frame, before normalization)}
#'   \item{X_train_norm}{Normalized training features (data frame)}
#'   \item{X_test_norm}{Normalized test features (data frame)}
#'   \item{y_train}{Training labels as numeric (0/1)}
#'   \item{y_test}{Test labels as numeric (0/1)}
#'   \item{train_data}{Full training data frame (features + target)}
#'   \item{cor_matrix}{Correlation matrix of numeric variables}
#'   \item{min_vals}{Minimum values for each numeric feature (for normalization)}
#'   \item{max_vals}{Maximum values for each numeric feature (for normalization)}
#'
#' @importFrom caTools sample.split
#' @importFrom stats median quantile sd var
#'
#' @examples
#' \dontrun{
#' # Preprocess diabetes dataset
#' result <- pre_process_data("diabetes_dataset", tag_column = 9, split_ratio = 0.7, seed = 3)
#'
#' # Access preprocessed data
#' X_train <- result$X_train_norm
#' y_train <- result$y_train
#' X_test <- result$X_test_norm
#' y_test <- result$y_test
#' }
#' @export
pre_process_data <- function(data, tag_column, split_ratio = 0.7, seed = 3) {
  # 1. Load and explore data
  head(data, 3)      # Display first few rows
  # str(data)       # Check data types of each column
  # summary(data)   # Summary statistics for all variables
  # Store target column name for later use
  tag_col_name <- names(data)[tag_column]

  # 2. Check for missing values
  # Calculate number of missing values per column
  missing_counts <- colSums(is.na(data))
  missing_counts

  # 3. Impute missing values using mean imputation
  if (sum(missing_counts) > 0){
    cols_with_na <- names(missing_counts[missing_counts > 0])
    cols_with_na

    # Replace NAs with column mean
    for (col in cols_with_na) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }

    print("Filled:")
    print(paste(cols_with_na, collapse = ", "))
    print(colSums(is.na(data)))  # Verify no missing values remain
  }else{
    print("No missing value")
  }

  # 4. Correlation analysis
  # Identify numeric columns for correlation calculation
  numeric_cols <- sapply(data, is.numeric)

  if (sum(numeric_cols) >= 2) {  # Need at least 2 numeric columns
    cor_matrix <- cor(data[, numeric_cols])
    print(cor_matrix)
  } else {
    print("The correlation matrix cannot be calculated.")
  }

  # 5. Check target variable distribution
  # Display frequency table of the target variable
  print(table(data[[tag_col_name]]))

  # 6. Split data into training and test sets
  set.seed(seed)

  # Stratified split based on target variable
  split <- sample.split(data[[tag_col_name]], SplitRatio = split_ratio)

  train_data <- subset(data, split == TRUE)
  test_data  <- subset(data, split == FALSE)

  dim(train_data)   # Check training set dimensions
  dim(test_data)    # Check test set dimensions

  # Verify target variable levels in training set
  unique(train_data[[tag_col_name]])

  # 7. Separate features and target variable
  # Extract features (all columns except target)
  X_train <- train_data[, -tag_column]
  y_train <- as.numeric(factor(train_data[[tag_col_name]])) - 1

  X_test <- test_data[, -tag_column]
  y_test <- as.numeric(factor(test_data[[tag_col_name]])) - 1

  # 8. Apply Min-Max normalization to numeric features
  # Identify numeric columns for normalization
  numeric_cols_train <- sapply(X_train, is.numeric)

  if (sum(numeric_cols_train) > 0) {
    # Store min and max values from training set for later use
    min_vals <- sapply(X_train[, numeric_cols_train], min)
    max_vals <- sapply(X_train[, numeric_cols_train], max)

    # Define Min-Max normalization function
    min_max_norm <- function(x) {
      return((x - min(x)) / (max(x) - min(x)))
    }

    # Normalize numeric columns in training set
    X_train_numeric <- as.data.frame(lapply(X_train[, numeric_cols_train], min_max_norm))
    X_train_non_numeric <- X_train[, !numeric_cols_train, drop = FALSE]

    X_train_norm <- cbind(X_train_numeric, X_train_non_numeric)

    # Normalize test set using training set's min and max values
    X_test_numeric <- as.data.frame(mapply(function(x, min_val, max_val) {
      (x - min_val) / (max_val - min_val)
    }, X_test[, numeric_cols_train], min_vals, max_vals, SIMPLIFY = FALSE))

    X_test_non_numeric <- X_test[, !numeric_cols_train, drop = FALSE]

    X_test_norm <- cbind(X_test_numeric, X_test_non_numeric)
  }else{
    # No numeric columns to normalize
    X_train_norm = X_train
    X_test_norm = X_test
    min_vals = max_vals = cor_matrix = NA
  }

  # Return all preprocessed data and metadata
  return(list(
    X_train = X_train,                    # Raw training features
    X_test  = X_test,                     # Raw test features
    X_train_norm = X_train_norm,          # Normalized training features
    X_test_norm  = X_test_norm,           # Normalized test features
    y_train = y_train,                    # Training labels (0/1)
    y_test  = y_test,                     # Test labels (0/1)
    train_data = train_data,              # Full training data frame
    cor_matrix = cor_matrix,              # Correlation matrix
    min_vals = min_vals,                  # Min values for new data normalization
    max_vals = max_vals                   # Max values for new data normalization
  ))

}

# Example usage (commented out)

# file_path = "diabetes.csv"
# result <- pre_process_data(file_path, tag_column = 9, split_ratio = 0.7, seed = 3)
#
# # Access preprocessed data
# X_train <- result$X_train_norm
# X_test  <- result$X_test_norm
# y_train <- result$y_train
# y_test  <- result$y_test
