#' Computes counts of extreme test statistics for given tau
#'
#' @param x tau
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional).
#' @param fun Test statistic function (optional).
#' @param t_obs Observed test statistic
#' @param rand_matrix Randomization matrix
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less"
#' @return Number of extreme test statistics
#' @noRd

fisher_count_wrapper=function(x, y, w, test_stat,fun, t_obs,
                              rand_matrix, alternative){
  tau <- x
  c<-count(y,w,test_stat, fun, t_obs, tau, rand_matrix, alternative)$counts
  return(c)

}
