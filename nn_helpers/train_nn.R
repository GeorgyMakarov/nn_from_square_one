train_nn <- function(x, y, epochs, lr, hn, f, track = F){
  
  # Initialize parameters
  l      <- get_layer_size(x, y, hn)
  p      <- init_params(l)
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