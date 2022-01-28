backward_propagation <- function(x, y, cache, par, ls){
  
  m <- dim(x)[2] ## number of observations
  g <- list()    ## list of gradients
  
  ### -----              Compute gradients of last layer               ----- ###
  
  # Get parameters required for computing the last gradient
  l    <- length(ls) - 1     ## last gradient index
  dzl  <- paste0("dz", l)    ## last dz
  dwl  <- paste0("dw", l)    ## last dw
  dbl  <- paste0("db", l)    ## last db
  al   <- paste0("a", l)     ## last a
  al_1 <- paste0("a", l - 1) ## previous a
  
  browser()
  # Differentiate last layer -- it is differentiated differently than other
  g[[dzl]] <- cache[[al]] - y
  g[[dwl]] <- (1 / m) * (g[[dzl]] %*% t(cache[[al_1]]))
  g[[dbl]] <- matrix((1 / m) * sum(g[[dzl]]), nrow = ls[[l + 1]])
  
  ### -----              Compute gradients of other layers             ----- ###
  
  # The computations will differ if neural network has 3 layers or if it has
  # more than 3 layers. This is because of different differentiation rules. If
  # layers = 3 go to computation of first layer, otherwise got to computations
  # of layers [l-1; 2], then go to computation of first layer.
  if (length(ls) == 3){
    
  } else {
    
  }
  
  
  
}
