#' Neural Network for Binary Classification
#'
#' A collection of functions to build, train, and evaluate a single-hidden-layer
#' neural network for binary classification tasks. Features include Xavier
#' parameter initialization, forward/backward propagation, and weighted loss
#' function for imbalanced data.
#'
#' @name neural_network
#' @author LI Zijin
#' @version 1.0
#' @date 2026-05-03
NULL

#' Get Layer Sizes of Neural Network
#'
#' Determines the number of neurons in each layer based on input and output dimensions.
#'
#' @param X Input feature matrix. Each column is a sample, each row is a feature.
#' @param y Output label matrix. For binary classification, this has 1 row.
#' @param hidden_neurons Number of neurons in the hidden layer.
#' @return A list containing:
#'   \item{n_x}{Number of input neurons (feature dimension)}
#'   \item{n_h}{Number of hidden neurons}
#'   \item{n_y}{Number of output neurons}
#'
#' @examples
#' X <- matrix(rnorm(100), nrow = 5, ncol = 20)
#' y <- matrix(sample(0:1, 20, replace = TRUE), nrow = 1)
#' layer_sizes <- getLayerSize(X, y, hidden_neurons = 4)
#' @export
getLayerSize <- function(X, y, hidden_neurons) {
  # To generate matrices with random parameters, we need to first obtain the size
  # (number of neurons) of all the layers in our neural-net.
  n_x <- dim(X)[1]
  n_h <- hidden_neurons
  n_y <- dim(y)[1]

  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)

  return(size)
}

#' Initialize Neural Network Parameters
#'
#' Initializes weight matrices using Xavier/Glorot initialization and bias
#' vectors with zeros. Xavier variance = 2 / (n_input + n_output).
#'
#' @param X Input feature matrix (used to determine input dimension)
#' @param list_layer_size List of layer sizes from \code{\link{getLayerSize}}
#' @return A list containing the initialized parameters:
#'   \item{W1}{Hidden layer weight matrix, dimension (n_h, n_x)}
#'   \item{b1}{Hidden layer bias vector, dimension (n_h, 1)}
#'   \item{W2}{Output layer weight matrix, dimension (n_y, n_h)}
#'   \item{b2}{Output layer bias vector, dimension (n_y, 1)}
#'
#' @examples
#' X <- matrix(rnorm(100), nrow = 5)
#' layer_info <- list(n_x = 5, n_h = 4, n_y = 1)
#' params <- initializeParameters(X, layer_info)
#' @export
initializeParameters <- function(X, list_layer_size){
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  # Xavier初始化
  W1 <- matrix(rnorm(n_h * n_x, 0, sqrt(2/(n_x + n_h))), nrow = n_h, ncol = n_x)
  b1 <- matrix(0, nrow = n_h, ncol = 1)
  W2 <- matrix(rnorm(n_y * n_h, 0, sqrt(2/(n_h + n_y))), nrow = n_y, ncol = n_h)
  b2 <- matrix(0, nrow = n_y, ncol = 1)

  return(list(W1 = W1, b1 = b1, W2 = W2, b2 = b2))
}

#' Sigmoid Activation Function
#'
#' Applies the sigmoid function to map input values to the (0,1) range.
#' Commonly used for binary classification output layers.
#'
#' @param x A numeric vector, matrix, or array
#' @return A numeric object of the same dimension as x, with values in (0,1)
#'
#' @examples
#' sigmoid(0)  # returns 0.5
#' sigmoid(c(-1, 0, 1))
#' @export
sigmoid <- function(x){
  # This activation function is for the output layer
  return(1 / (1 + exp(-x)))
}

#' Forward Propagation
#'
#' Computes activations for all layers through forward propagation.
#' Returns all intermediate values for use in backpropagation.
#'
#' @param X Input feature matrix, dimension (n_x, m) where m = number of samples
#' @param params Parameter list containing W1, b1, W2, b2 from \code{\link{initializeParameters}}
#' @param list_layer_size List of layer sizes from \code{\link{getLayerSize}}
#' @return A list (cache) containing:
#'   \item{Z1}{Linear output of hidden layer, dimension (n_h, m)}
#'   \item{A1}{Hidden layer activation (sigmoid(Z1)), dimension (n_h, m)}
#'   \item{Z2}{Linear output of output layer, dimension (n_y, m)}
#'   \item{A2}{Output layer prediction probabilities, dimension (n_y, m)}
#'
#' @examples
#' X <- matrix(rnorm(50), nrow = 5, ncol = 10)
#' layer_info <- list(n_x = 5, n_h = 4, n_y = 1)
#' params <- initializeParameters(X, layer_info)
#' cache <- forwardPropagation(X, params, layer_info)
#' @export
forwardPropagation <- function(X, params, list_layer_size){
  m <- ncol(X)  # 样本数
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2

  if(ncol(b1) == 1) {
    b1_expanded <- b1[, rep(1, m)]  # 扩展为 [n_h, m]
  } else {
    b1_expanded <- b1
  }

  if(ncol(b2) == 1) {
    b2_expanded <- b2[, rep(1, m)]
  } else {
    b2_expanded <- b2
  }

  # 广播bias
  Z1 <- W1 %*% X + b1_expanded
  A1 <- sigmoid(Z1)
  Z2 <- W2 %*% A1 + b2_expanded
  A2 <- sigmoid(Z2)

  return(list(Z1 = Z1, A1 = A1, Z2 = Z2, A2 = A2))

  # Even though we only need the value A2 for forward propagation,
  # you’ll notice we return all other calculated values as well.
  # We do this because these values will be needed during backpropagation.
  # Saving them here will reduce the the time it takes for backpropagation
  # because we don’t have to calculate it again.
  # return (cache)
}

# We will use Binary Cross Entropy loss function (aka log loss)

#' Compute Weighted Binary Cross-Entropy Cost
#'
#' Calculates the loss using binary cross-entropy with inverse frequency weighting
#' to handle imbalanced datasets.
#'
#' @param X Input feature matrix (used to get number of samples)
#' @param y True labels (0 or 1), dimension (1, m)
#' @param cache Forward propagation cache from \code{\link{forwardPropagation}}
#' @return Weighted binary cross-entropy cost (scalar)
#'
#' @export
computeCost <- function(X, y, cache) {
  m <- dim(X)[2]
  A2 <- cache$A2
  epsilon <- 1e-15
  A2 <- pmax(pmin(A2, 1 - epsilon), epsilon)
  n0 <- sum(y == 0)
  n1 <- sum(y == 1)
  if (n0 == 0 || n1 == 0) {
    warning("类别缺失，使用等权重")
    weights <- rep(1, m)
  } else {
    w0 <- 1 / n0
    w1 <- 1 / n1
    weights <- ifelse(y == 0, w0, w1)
    weights <- weights / sum(weights) * m
  }
  logprobs <- (log(A2) * y) + (log(1 - A2) * (1 - y))
  cost <- -sum(weights * logprobs) / m
  return(cost)
}


#' Backward Propagation
#'
#' Computes gradients of the loss function with respect to all parameters
#' using the chain rule.
#'
#' @param X Input feature matrix, dimension (n_x, m)
#' @param y True labels (0 or 1), dimension (1, m)
#' @param cache Forward propagation cache from \code{\link{forwardPropagation}}
#' @param params Parameter list containing W1, b1, W2, b2
#' @param list_layer_size List of layer sizes from \code{\link{getLayerSize}}
#' @param use_weights Logical, whether to apply class weights for imbalanced data
#' @return A list containing gradients:
#'   \item{dW1}{Gradient of hidden layer weights}
#'   \item{db1}{Gradient of hidden layer bias}
#'   \item{dW2}{Gradient of output layer weights}
#'   \item{db2}{Gradient of output layer bias}
#'
#' @export
backwardPropagation <- function(X, y, cache, params, list_layer_size, use_weights = TRUE) {
  m <- ncol(X)

  A2 <- cache$A2
  A1 <- cache$A1
  W2 <- params$W2

  # 计算权重（用于反向传播）
  if(use_weights) {
    n0 <- sum(y == 0)
    n1 <- sum(y == 1)
    if(n0 > 0 && n1 > 0) {
      weights <- ifelse(y == 0, 1/n0, 1/n1)
      weights <- weights / sum(weights) * m
    } else {
      weights <- rep(1, m)
    }
  } else {
    weights <- rep(1, m)
  }

  # 输出层梯度（加权）
  dZ2 <- (A2 - y) * weights
  dW2 <- (1/m) * (dZ2 %*% t(A1))
  db2 <- (1/m) * rowSums(dZ2)

  # 隐藏层梯度 - 修复这里！
  # 使用 sigmoid 的导数：A1 * (1 - A1)
  dZ1 <- (t(W2) %*% dZ2) * (A1 * (1 - A1))
  dW1 <- (1/m) * (dZ1 %*% t(X))
  db1 <- (1/m) * rowSums(dZ1)

  return(list(dW1 = dW1, db1 = db1, dW2 = dW2, db2 = db2))
}

#' Update Parameters Using Gradient Descent
#'
#' Updates network parameters by subtracting the gradient times the learning rate.
#'
#' @param grads Gradient list from \code{\link{backwardPropagation}}
#' @param params Current parameter list
#' @param learning_rate Step size for gradient descent
#' @return Updated parameter list
#'
#' @export
updateParameters <- function(grads, params, learning_rate) {
  params$W1 <- params$W1 - learning_rate * grads$dW1
  params$b1 <- params$b1 - learning_rate * grads$db1
  params$W2 <- params$W2 - learning_rate * grads$dW2
  params$b2 <- params$b2 - learning_rate * grads$db2

  return(params)
}

# Here we wrap out all functions above,
# and by calling this function, our NN training starts

#' Train Neural Network Model
#'
#' Main training function that iteratively performs forward/backward propagation
#' and parameter updates.
#'
#' @param X Input feature matrix, dimension (n_x, m)
#' @param y True labels (0 or 1), dimension (1, m)
#' @param num_iteration Number of gradient descent iterations
#' @param hidden_neurons Number of neurons in the hidden layer
#' @param lr Learning rate
#' @param verbose Logical, whether to print cost every 100 iterations
#' @return A list containing:
#'   \item{updated_params}{Trained parameters}
#'   \item{cost_hist}{Vector of cost values at each iteration}
#'
#' @examples
#' X <- matrix(rnorm(200), nrow = 10, ncol = 20)
#' y <- matrix(sample(0:1, 20, replace = TRUE), nrow = 1)
#' model <- trainModel(X, y, num_iteration = 500, hidden_neurons = 5, lr = 0.01)
#' @export
trainModel <- function(X, y, num_iteration, hidden_neurons, lr, verbose = TRUE) {
  layer_size <- getLayerSize(X, y, hidden_neurons)
  params <- initializeParameters(X, layer_size)
  cost_history <- c()

  for (i in 1:num_iteration) {
    fwd_prop <- forwardPropagation(X, params, layer_size)
    cost <- computeCost(X, y, fwd_prop)
    back_prop <- backwardPropagation(X, y, fwd_prop, params, layer_size, use_weights = TRUE)
    params <- updateParameters(back_prop, params, lr)
    cost_history <- c(cost_history, cost)
    if(verbose && i %% 100 == 0) {
      cat("Iteration", i, " | Cost: ", round(cost, 6), "\n")
    }
  }

  return(list(updated_params = params, cost_hist = cost_history))
}

# Test the Model

#' Make Predictions
#'
#' Generates prediction probabilities for new data using trained model.
#'
#' @param X Input feature matrix for test data
#' @param params Trained parameters from \code{\link{trainModel}}
#' @param hidden_neurons Number of neurons in hidden layer (must match training)
#' @return Vector of prediction probabilities in (0,1)
#'
#' @export
makePrediction <- function(X, params, hidden_neurons) {
  layer_size <- list(n_x = nrow(X), n_h = hidden_neurons, n_y = 1)
  fwd_prop <- forwardPropagation(X, params, layer_size)
  return(fwd_prop$A2)
}

#' Evaluate Model Performance
#'
#' Wrapper function to generate predictions and return model performance metrics.
#'
#' @param model_name Character string identifying the model
#' @param test_data Test feature matrix
#' @param train_model Trained model output from \code{\link{trainModel}}
#' @param hidden_neurons Number of hidden neurons used in training
#' @return A list containing:
#'   \item{model_name}{Name of the model}
#'   \item{predictions}{Factor vector of predicted classes (0/1)}
#'   \item{pred_prob}{Vector of prediction probabilities}
#'   \item{model}{The trained model object}
#'
#' @export
NN_performance = function(model_name = "Neural Network",test_data,train_model,hidden_neurons){
  updated_params = train_model$updated_params
  pred_prob = makePrediction(test_data,updated_params,hidden_neurons)
  pred_prob = as.numeric(pred_prob)
  pred_class = ifelse(pred_prob > 0.5,1,0)
  pred_class = factor(pred_class,levels=c("0","1"))
  return(list(model_name = model_name,predictions=pred_class,pred_prob=pred_prob,model=train_model))
}

