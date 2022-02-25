#' Train neural network with Adam optimization
#' 
#' Trains feed forward neural network that can use Adam optimization
#' algorithm.
#'
#' @param x matrix of feature variables
#' @param y matrix of output variable
#' @param epochs numeric scalar, number of iterations
#' @param hn numeric scalar, number of hidden layer nodes
#' @param lr numeric scalar, learning rate
#' @param f character vector, names of activation functions
#' @param im character scalar, name of initialization method: stand, rand, he
#' @param lambda numeric scalar, drop out parameter
#' @param kp numeric scalar, keep probability L2 regularization, must be 0 to 1
#' @param track logical, track progress if TRUE
#' @param beta1 numeric scalar, B1 for Adam computation
#' @param beta2 numeric scalar, B2 for Adam computation
#' @param epsilon numeric scalar, E for Adam computation
#' @param b_size numeric scalar, mini batch size
#'
#' @return list
#' @export
train_nn_adam <- function(x, 
                          y, 
                          epochs, 
                          lr, 
                          hn, 
                          f, 
                          im = c("stand", "rand", "he"),
                          lambda  = 0,
                          kp      = 1,
                          track   = F,
                          beta1   = 0.9,
                          beta2   = 0.999,
                          epsilon = 1e-08,
                          b_size  = 64){
  
  # Choose initialization method
  if(length(im) > 1){im <- im[1]}
  if (!(im %in% c("stand", "rand", "he"))){im <- "stand"}
  
  # Initialize parameters
  l      <- get_layer_size(x, y, hn)
  p      <- init_params(x, l, im) ## TODO: add seed to get reproducible results
  cost_h <- c()           ## variable to store cost
  t      <- 0             ## Adam counter
  trackr <- 0.1 * epochs  ## variable to trace progress
  vs     <- init_adam(p)  ## initialize Adam parameters
  seed   <- 123           ## seed for representative results
  
  # Iterate over epochs
  for (i in 1:epochs){
    
    seed   <- seed + 1
    cost_t <- 0
    
    ### ---        Temporary reproduce multiple batch for dev            --- ###
    x1 <- x
    x2 <- x
    x  <- cbind(x1, x2)
    
    y1 <- y
    y2 <- y
    y  <- cbind(y1, y2)
    ### ---                             End                              --- ###
    
    mini_b <- random_mini_batch(x, y, b_size, seed)
    
    for (mb in mini_b){
      
      mini_x <- mb[['x']]
      mini_y <- mb[['y']]
      
      dim(mini_y) <- c(dim(y)[1], length(mini_y))
      
      # Forward propagation depends on drop out
      if (kp == 1){
        fwd <- forward_propagation(mini_x, p, f, l)
      } else {
        fwd <- forward_propagation_dropout(mini_x, p, f, l, kp)
      }
      
      # Cost depends on regularization
      if (lambda == 0){
        cost   <- compute_cost(mini_x, mini_y, fwd, l)
      } else {
        cost   <- compute_cost_reg(mini_x, mini_y, fwd, l, lambda)
      }
      
      cost_t <- cost_t + cost
      
      # Back propagation depends on drop out and regularization
      if (lambda == 0 && kp == 1){
        back   <- back_propagation(mini_x, mini_y, p, fwd, l, f)
      } else if (lambda != 0){
        back   <- back_propagation_reg(mini_x, mini_y, p, fwd, l, f, lambda)
      } else if (kp < 1){
        back   <- back_propagation_dropout(mini_x, mini_y, p, fwd, l, f, kp)
      }
      
      t        <- t + 1
      adm_upd  <- update_adam(back, p, vs, l, t, lr, beta1, beta2, epsilon)
      p        <- adm_upd$p
      vs       <- adm_upd$vs
      
    }
    
    cost_h <- c(cost_h, cost)
    
    if (i == 1){cat("Iteration", i, " | Cost: ", cost, "\n")}
    if (i %% trackr == 0){cat("Iteration", i, " | Cost: ", cost, "\n")}
  }
  
  res <- list("params" = p, "cost_history" = cost_h, "hn" = hn)
  return(res)
}