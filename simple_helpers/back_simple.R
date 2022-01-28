back_simple <- function(x, y, cache, param, l_size){
  
  m   <- dim(x)[2]
  n_x <- l_size[['n_x']]
  n_h <- l_size[['n_h']]
  n_y <- l_size[['n_y']]
  a2  <- cache[['a2']]
  a1  <- cache[['a1']]
  w2  <- param[['w2']]
  
  dz2     <- a2 - y
  dw2     <- (1/m) * (dz2 %*% t(a1))
  db2     <- matrix((1/m) * sum(dz2), nrow = n_y)
  db2_adj <- matrix(rep(db2, m), nrow = n_y)
  
  dz1     <- (t(w2) %*% dz2) * (1 - a1 ^ 2)
  dw1     <- (1 / m) * (dz1 %*% t(x))
  db1     <- matrix((1 / m) * sum(dz1), nrow = n_h)
  db1_adj <- matrix(rep(db1, m), nrow = n_h)
  
  out <- list("dw1" = dw1, "db1" = db1, "dw2" = dw2, "db2" = db2)
  return(out)
}