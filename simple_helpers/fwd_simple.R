fwd_simple <- function(x, param, l_size){
  
  m   <- dim(x)[2]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  
  w1 <- param[['w1']]
  b1 <- param[['b1']]
  w2 <- param[['w2']]
  b2 <- param[['b2']]
  
  b1_adj <- matrix(rep(b1, m), nrow = n_h)
  b2_adj <- matrix(rep(b2, m), nrow = n_y)
  
  z1 <- w1 %*% x + b1_adj
  a1 <- tanh(z1)
  z2 <- w2 %*% a1 + b2_adj
  a2 <- sigmoid(z2)
  
  out <- list("z1" = z1, "a1" = a1, "z2" = z2, "a2" = a2)
  return(out)
}