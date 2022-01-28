init_simple <- function(x, l_size){
  
  m   <- dim(x)[2]
  n_x <- l_size[['n_x']]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  
  w1  <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = T) * 0.01
  b1  <- matrix(rep(0, n_h), nrow = n_h)
  w2  <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = T) * 0.01
  b2  <- matrix(rep(0, n_y), nrow = n_y)
  
  params <- list("w1" = w1, "b1" = b1, "w2" = w2, "b2" = b2)
  return(params)
}