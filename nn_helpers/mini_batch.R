mini_batch <- function(xs, ys, i, b_size){
  lb <- ((i - 1) * b_size) + 1 ## lower interval
  ub <- i * b_size             ## upper interval
  
  mbx <- xs[, lb:ub]
  mby <- ys[, lb:ub]
  mbi <- list("x" = mbx, "y" = mby)
  
  return(mbi)
}
