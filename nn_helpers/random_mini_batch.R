random_mini_batch <- function(x, y, b_size, seed){
  
  m <- dim(x)[2]
  
  # Shuffle data
  set.seed(seed)
  idx <- sample(1:ncol(x), m)
  xs  <- x[, idx]
  ys  <- y[, idx]
  
  # Partition data into mini batches for complete mini-batches
  mb_complete <- floor(m / b_size)
  browser()
  
}