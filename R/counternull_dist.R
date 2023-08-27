#' Finds counternull test statistic
#'
#' @param x Randomized assignment permutation
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional).
#' @param fun Test statistic function (optional).
#' @param counternull_value Counternull value
#' @return Counternull test statistic
#' @noRd

counternull_dist = function(x,y,w,test_stat = NULL, fun = NULL,
                            counternull_value){

  Y_c<-vector(length=length(y))
  Y_c[w == 1 & x == 1] = y[w == 1 & x == 1]
  Y_c[w == 0 & x == 0] = y[w == 0 & x == 0]
  Y_c[w == 0 & x == 1] = y[w == 0 & x == 1] + counternull_value
  Y_c[w == 1 & x == 0] = y[w == 1 & x == 0] - counternull_value

  # returns test statistic
  return(invisible(find_test_stat(Y_c, x, test_stat, fun )))


}
