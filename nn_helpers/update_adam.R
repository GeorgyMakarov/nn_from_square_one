update_adam <- function(bck, p, vs, l, t, lr, beta1, beta2, eps){
  
  # Define initial variables required for computation
  l <- length(l) - 1
  v <- vs$v
  s <- vs$s
  
  # Define corrected variables required to store the data in
  v_adj <- list()
  s_adj <- list()
  
  # Update all parameters with Adam
  for (i in 1:l){
    
    # Define layer names allows to save printing below
    w  <- paste('w', i)
    b  <- paste('b', i) 
    dw <- paste0('dw', i)
    db <- paste0('db', i)
    
    # Compute moving average of gradients
    v[[dw]] <- (beta1 * v[[dw]]) + ((1 - beta1) * bck[[dw]])
    v[[db]] <- (beta1 * v[[db]]) + ((1 - beta1) * bck[[db]])
    
    # Compute bias-corrected first moment
    v_adj[[dw]] <- v[[dw]] / (1 - (beta1 ^ t))
    v_adj[[db]] <- v[[dw]] / (1 - (beta1 ^ t))
    
    # Compute moving average of squared gradient
    # TODO: find how to power matrix -- below does not work
    s[[dw]] <- (beta2 * s[[dw]]) + ((1 - beta2) * (bck[[dw]] %^% 2))
    s[[db]] <- (beta2 * s[[db]]) + ((1 - beta2) * (bck[[db]] %^% 2))
    
    # Compute bias-corrected second moment
    s_adj[[dw]] <- s[[dw]] / (1 - (beta2 ^ t))
    s_adj[[db]] <- s[[db]] / (1 - (beta2 ^ t))
    
    # Update parameters
    p[[w]]
    p[[b]]
    
  }
  
  return(res)
}