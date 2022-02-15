#' Train neural network
#' 
#' Trains simple one-layer feed-forward neural network.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param epochs numeric scalar, number of iterations
#' @param hn numeric scalar, number of hidden layer nodes
#' @param f character vector, names of activation functions
#' @param im character scalar, name of initialization method: stand, rand, he
#' @param lr numeric scalar, learning rate
#' @param trac logical, track progress if TRUE
#'
#' @return list
#' @export
train_nn <- function(x, 
                     y, 
                     epochs, 
                     lr, 
                     hn, 
                     f, 
                     im = c("stand", "rand", "he"),
                     track = F){
  
  # Choose initialization method
  if(length(im) > 1){im <- im[1]}
  if (!(im %in% c("stand", "rand", "he"))){
    im <- "stand"
    cat("warning: defaulting to standard initialization method \n")
  }
  
  # Initialize parameters
  l      <- get_layer_size(x, y, hn)
  p      <- init_params(x, l, im)
  cost_h <- c()           ## variable to store cost
  trackr <- 0.1 * epochs  ## variable to trace progress
  
  # Iterate over epochs
  for (i in 1:epochs){
    fwd    <- forward_propagation(x, p, f, l)
    cost   <- compute_cost(x, y, fwd, l)
    back   <- back_propagation(x, y, p, fwd, l, f)
    p      <- update_params(back, p, lr, l)
    cost_h <- c(cost_h, cost)
    
    if (i %% trackr == 0){cat("Iteration", i, " | Cost: ", cost, "\n")}
  }
  
  res <- list("params" = p, "cost_history" = cost_h, "hn" = hn)
  return(res)
}
