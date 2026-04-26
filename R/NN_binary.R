set.seed(3005)

getLayerSize <- function(X, y, hidden_neurons, train=TRUE) {
  n_x <- dim(X)[1]
  n_h <- hidden_neurons
  n_y <- dim(y)[1]

  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)

  return(size)
}

initializeParameters <- function(X, list_layer_size){

  m <- dim(data.matrix(X))[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE) * 0.01
  b1 <- matrix(rep(0, n_h), nrow = n_h)
  W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) * 0.01
  b2 <- matrix(rep(0, n_y), nrow = n_y)

  params <- list("W1" = W1,
                 "b1" = b1,
                 "W2" = W2,
                 "b2" = b2)

  return (params)
}

sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}


forwardPropagation <- function(X, params, list_layer_size){

  m <- dim(X)[2]
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2

  b1_new <- matrix(rep(b1, m), nrow = n_h)
  b2_new <- matrix(rep(b2, m), nrow = n_y)

  Z1 <- W1 %*% X + b1_new
  A1 <- sigmoid(Z1)
  Z2 <- W2 %*% A1 + b2_new
  A2 <- sigmoid(Z2)

  cache <- list("Z1" = Z1,
                "A1" = A1,
                "Z2" = Z2,
                "A2" = A2)

  # Even though we only need the value A2 for forward propagation,
  # you’ll notice we return all other calculated values as well.
  # We do this because these values will be needed during backpropagation.
  # Saving them here will reduce the the time it takes for backpropagation
  # because we don’t have to calculate it again.
  return (cache)
}


computeCost <- function(X, y, cache) {
  m <- dim(X)[2]
  A2 <- cache$A2
  logprobs <- (log(A2) * y) + (log(1-A2) * (1-y))
  cost <- -sum(logprobs/m)
  return (cost)
}

# Back propagation part
backwardPropagation <- function(X, y, cache, params, list_layer_size){

  m <- dim(X)[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  A2 <- cache$A2
  A1 <- cache$A1
  W2 <- params$W2

  dZ2 <- A2 - y
  dW2 <- 1/m * (dZ2 %*% t(A1))
  db2 <- matrix(1/m * sum(dZ2), nrow = n_y)
  db2_new <- matrix(rep(db2, m), nrow = n_y)

  dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
  dW1 <- 1/m * (dZ1 %*% t(X))
  db1 <- matrix(1/m * sum(dZ1), nrow = n_h)
  db1_new <- matrix(rep(db1, m), nrow = n_h)

  grads <- list("dW1" = dW1,
                "db1" = db1,
                "dW2" = dW2,
                "db2" = db2)

  return(grads)
}

updateParameters <- function(grads, params, learning_rate){

  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2

  dW1 <- grads$dW1
  db1 <- grads$db1
  dW2 <- grads$dW2
  db2 <- grads$db2


  W1 <- W1 - learning_rate * dW1
  b1 <- b1 - learning_rate * db1
  W2 <- W2 - learning_rate * dW2
  b2 <- b2 - learning_rate * db2

  updated_params <- list("W1" = W1,
                         "b1" = b1,
                         "W2" = W2,
                         "b2" = b2)

  return (updated_params)
}

trainModel <- function(X, y, num_iteration, hidden_neurons, lr){

  layer_size <- getLayerSize(X, y, hidden_neurons)
  init_params <- initializeParameters(X, layer_size)
  cost_history <- c()
  for (i in 1:num_iteration) {
    fwd_prop <- forwardPropagation(X, init_params, layer_size)
    cost <- computeCost(X, y, fwd_prop)
    back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
    update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    cost_history <- c(cost_history, cost)

    if (i %% 10000 == 0) cat("Iteration", i, " | Cost: ", cost, "\n")
  }

  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history)
  return (model_out)
}

# Test the Model

makePrediction <- function(X, y, hidden_neurons){
  layer_size <- getLayerSize(X, y, hidden_neurons)
  params <- train_model$updated_params
  fwd_prop <- forwardPropagation(X, params, layer_size)
  pred <- fwd_prop$A2

  return (pred)
}

# Build Confusion Matrix and calculate the metrics
