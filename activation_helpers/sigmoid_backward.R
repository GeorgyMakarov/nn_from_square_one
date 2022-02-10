sigmoid_backward <- function(z){
  s   <- 1 / (1 + exp(-z))
  res <- s * (1 - s)
  return(res)
}