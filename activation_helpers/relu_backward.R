relu_backward <- function(z){
  g <- z
  g[g < 0] <- 0
  g[g > 0] <- 1
  return(g)
}