linear_act_fwd <- function(a_prev, w, b, act){
  
  lin_fwd <- linear_fwd(a_prev, w, b)
  
  # Prepare formula in text format. This is required to avoid multiple if
  # statements in this formula.
  f_txt <- paste0(act, "(lin_fwd[['z']])")
  a_out <- eval(parse(text = f_txt))
  
  # Combine linear and activation cache to return them as a list
  cache <- combine_cahe(lin_fwd, a_out)
  check <- dim(a_out)[1] == dim(w)[1] && dim(a_out)[2] == dim(a_prev)[2]
  if (!check){stop("Input and output dimensions do not match")}
  
  out <- list("a" = a_out[['a']], "cache" = cache)
  return(out)
}
