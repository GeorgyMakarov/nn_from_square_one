random_mini_batch <- function(x, y, b_size, seed){
  
  if (ncol(x) < b_size){b_size <- ncol(x)}
  
  m <- dim(x)[2]
  
  # Shuffle data
  set.seed(seed)
  idx <- sample(x = 1:ncol(x), m)
  xs  <- x[, idx]  ## shuffle x data
  ys  <- y[, idx]  ## shuffle y data
  
  # Convert ys to matrix
  dim(ys) <- dim(y)
  
  # Partition data to complete mini batches
  mb       <- floor( m / b_size)
  mini_bat <- lapply(X = 1:mb, FUN = function(i){mini_batch(xs, ys, i, b_size)})
  
  # If last mini-batch < b_size
  if (m %% b_size != 0 && mb > 1){
    lb  <- (mb * b_size) + 1 ## lower interval
    ub  <- ncol(xs)          ## upper interval
    mbx <- xs[, lb:ub]
    mby <- ys[, lb:ub]
    mbi <- list("x" = mbx, "y" = mby)
    rm(lb, ub, mbx, mby)
    mini_bat[[mb + 1]] <- mbi
  }
  
  return(mini_bat)
}
