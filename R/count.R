#' Finds number of extreme of test statistics in counternull distribution
#'
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test. (Optional).
#' @param fun Test statistic function (optional).
#' @param t_obs Observed test statistic
#' @param counternull_value Counternull value
#' @param rand_matrix Randomization matrix
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less"
#' @return List with number of extreme test statistics and
#' counternull statistics
#' @noRd


count=function(y,w, test_stat, fun, t_obs, counternull_value,
               rand_matrix, alternative){

  if(nrow(rand_matrix) != length(y)){
    stop('Argument "rand_matrix" in "null_rand" must have number of rows
         equal to the length of argument "y".')
  }
  counter_samples= apply(rand_matrix,2,counternull_dist, y=y, w=w, fun = fun,
                          test_stat = test_stat,
                          counternull_value = counternull_value)

  if (alternative=="less"){ # larger test statistics are more extreme.
    c=sum(counter_samples>= t_obs)

  } else if(alternative == "greater"){
    # smaller test statistics are more extreme
    c=sum(counter_samples<= t_obs)

  } else{ # double-sided
    c=sum(abs(counter_samples)<= abs(t_obs))

  }

  return(list(counts = c, perm = counter_samples))

}
