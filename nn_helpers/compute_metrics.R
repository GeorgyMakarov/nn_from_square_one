#' Compute accuracy metrics
#' 
#' Computes a set of accuracy metrics for classifier.
#'
#' @param y_test matrix of actual output values
#' @param y_hat matrix of predicted output values
#' @param md_name character, model name
#' @param show print metrics
#'
#' @return list
#' @export
compute_metrics <- function(y_test, y_hat, md_name, show = T){
  tb   <- table(y_test, y_hat)
  acc  <- round((tb[1] + tb[4]) / sum(tb), 2)
  rec  <- round(tb[4] / (tb[4] + tb[3]), 2)
  pre  <- round(tb[4] / (tb[4] + tb[2]), 2)
  f1sc <- round(2 * ((pre * rec) / (pre + rec)), 2)
  res  <- list("accuracy" = acc, "precision" = pre, "recall" = rec, "f1" = f1sc)
  
  res_p <- paste0(md_name, ": \n",
                  paste0("\tAccuracy = ", acc * 100, "% \n"),
                  paste0("\tPrecision = ", pre * 100, "% \n"),
                  paste0("\tRecall = ", rec * 100, "% \n"),
                  paste0("\tF1 score = ", f1sc * 100, "% \n"))
  if (show){cat(res_p)}
  return(res)
}