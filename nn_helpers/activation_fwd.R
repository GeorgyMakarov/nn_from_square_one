activation_fwd <- function(z, activation){
  f_txt <- paste0(activation, "(z)")
  a     <- eval(parse(text = f_txt))
  return(a)
}
