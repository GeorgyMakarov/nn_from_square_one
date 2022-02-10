activation_backward <- function(da, z, f){
  f <- paste0(f, "_backward")
  f <- paste0(f, "(z)")
  g <- eval(parse(text = f))
  res <- da * g
  return(res)
}