get_ls_simple <- function(x, y, hn = NULL){
  
  n_x <- dim(x)[1]
  n_y <- dim(y)[1]
  
  ifelse(test = is.null(hn),
         yes  = n_h <- round(mean(x = c(n_x, n_y)), 0),
         no   = n_h <- hn)
  
  out <- list("n_x" = n_x, "n_h" = n_h, "n_y" = n_y)
  return(out)
}