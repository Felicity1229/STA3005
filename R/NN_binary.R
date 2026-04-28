set.seed(3005)

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

# initializeParameters <- function(list_layer_size){
#   n_x <- list_layer_size$n_x
#   n_h <- list_layer_size$n_h
#   n_y <- list_layer_size$n_y
#
#   # W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE) * 0.01
#   # b1 <- matrix(rep(0, n_h), nrow = n_h)
#   # W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) * 0.01
#   # b2 <- matrix(rep(0, n_y), nrow = n_y)
#
#   W1 <- matrix(rnorm(n_h * n_x, 0, sqrt(2/(n_x + n_h))), nrow = n_h, ncol = n_x)
#   b1 <- matrix(0, nrow = n_h, ncol = 1)
#   W2 <- matrix(rnorm(n_y * n_h, 0, sqrt(2/(n_h + n_y))), nrow = n_y, ncol = n_h)
#   b2 <- matrix(0, nrow = n_y, ncol = 1)
#
#   params <- list("W1" = W1,
#                  "b1" = b1,
#                  "W2" = W2,
#                  "b2" = b2)
#
#   return (params)
# }

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

sigmoid <- function(x){
  # This activation function is for the output layer
  return(1 / (1 + exp(-x)))
}


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

# computeCost <- function(X, y, cache) {
#   m <- dim(X)[2]
#   A2 <- cache$A2
#   logprobs <- (log(A2) * y) + (log(1-A2) * (1-y))
#   cost <- -sum(logprobs/m)
#   return (cost)
# }

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

# Back propagation part
# backwardPropagation <- function(X, y, cache, params, list_layer_size){
#
#   m <- dim(X)[2]
#
#   n_x <- list_layer_size$n_x
#   n_h <- list_layer_size$n_h
#   n_y <- list_layer_size$n_y
#
#   A2 <- cache$A2
#   A1 <- cache$A1
#   W2 <- params$W2
#
#   dZ2 <- A2 - y
#   dW2 <- 1/m * (dZ2 %*% t(A1))
#   db2 <- matrix(1/m * sum(dZ2), nrow = n_y)
#   db2_new <- matrix(rep(db2, m), nrow = n_y)
#
#   dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
#   dW1 <- 1/m * (dZ1 %*% t(X))
#   db1 <- matrix(1/m * sum(dZ1), nrow = n_h)
#   db1_new <- matrix(rep(db1, m), nrow = n_h)
#
#   grads <- list("dW1" = dW1,
#                 "db1" = db1,
#                 "dW2" = dW2,
#                 "db2" = db2)
#
#   return(grads)
# }

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

# updateParameters <- function(grads, params, learning_rate){
#
#   W1 <- params$W1
#   b1 <- params$b1
#   W2 <- params$W2
#   b2 <- params$b2
#
#   dW1 <- grads$dW1
#   db1 <- grads$db1
#   dW2 <- grads$dW2
#   db2 <- grads$db2
#
#
#   W1 <- W1 - learning_rate * dW1
#   b1 <- b1 - learning_rate * db1
#   W2 <- W2 - learning_rate * dW2
#   b2 <- b2 - learning_rate * db2
#
#   updated_params <- list("W1" = W1,
#                          "b1" = b1,
#                          "W2" = W2,
#                          "b2" = b2)
#
#   return (updated_params)
# }

updateParameters <- function(grads, params, learning_rate) {
  params$W1 <- params$W1 - learning_rate * grads$dW1
  params$b1 <- params$b1 - learning_rate * grads$db1
  params$W2 <- params$W2 - learning_rate * grads$dW2
  params$b2 <- params$b2 - learning_rate * grads$db2

  return(params)
}

# Here we wrap out all functions above, and by calling this function, our NN training starts
# trainModel <- function(X, y, num_iteration, hidden_neurons, lr){
#
#   layer_size <- getLayerSize(X, y, hidden_neurons)
#   init_params <- initializeParameters(X, layer_size)
#   cost_history <- c()
#   for (i in 1:num_iteration) {
#     fwd_prop <- forwardPropagation(X, init_params, layer_size)
#     cost <- computeCost(X, y, fwd_prop)
#     back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
#     update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
#     init_params <- update_params
#     cost_history <- c(cost_history, cost)
#
#     if (i %% 10000 == 0) cat("Iteration", i, " | Cost: ", cost, "\n")
#   }
#
#   model_out <- list("updated_params" = update_params,
#                     "cost_hist" = cost_history)
#   return (model_out)
# }

trainModel <- function(X, y, num_iteration, hidden_neurons, lr, verbose = TRUE) {
  layer_size <- getLayerSize(X, y, hidden_neurons)
  params <- initializeParameters(X, layer_size)
  cost_history <- c()

  for (i in 1:num_iteration) {
    # 前向传播
    fwd_prop <- forwardPropagation(X, params, layer_size)

    # 计算损失
    cost <- computeCost(X, y, fwd_prop)

    # 反向传播
    back_prop <- backwardPropagation(X, y, fwd_prop, params, layer_size, use_weights = TRUE)

    # 更新参数
    params <- updateParameters(back_prop, params, lr)

    cost_history <- c(cost_history, cost)

    if(verbose && i %% 100 == 0) {
      cat("Iteration", i, " | Cost: ", round(cost, 6), "\n")
    }
  }

  return(list(updated_params = params, cost_hist = cost_history))
}

# Test the Model
# makePrediction <- function(X, y, hidden_neurons){
#   layer_size <- getLayerSize(X, y, hidden_neurons)
#   params <- train_model$updated_params
#   fwd_prop <- forwardPropagation(X, params, layer_size)
#   pred <- fwd_prop$A2
#
#   return (pred)
# }

makePrediction <- function(X, params, hidden_neurons) {
  layer_size <- list(n_x = nrow(X), n_h = hidden_neurons, n_y = 1)
  fwd_prop <- forwardPropagation(X, params, layer_size)
  return(fwd_prop$A2)
}

# Build Confusion Matrix and calculate the metrics
calculate_stats <- function(tb, model_name) {
  acc <- (tb[1] + tb[4])/(tb[1] + tb[2] + tb[3] + tb[4])
  recall <- tb[4]/(tb[4] + tb[3])
  precision <- tb[4]/(tb[4] + tb[2])
  f1 <- 2 * ((precision * recall) / (precision + recall))

  cat(model_name, ": \n")
  cat("\tAccuracy = ", acc*100, "%.")
  cat("\n\tPrecision = ", precision*100, "%.")
  cat("\n\tRecall = ", recall*100, "%.")
  cat("\n\tF1 Score = ", f1*100, "%.\n\n")
}

