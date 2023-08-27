#' Finds range of counternull values
#'
#' @param counternull_value Counternull value
#' @param search Range of effect sizes to test as counternull values
#' @param t_obs Observed test statistic
#' @param obs_c Number of more extreme test statistics in null distribution
#' @param rand_matrix Randomization matrix
#' @param index Current search index
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test. (Optional).
#' Note: One of either test_stat or fun must be specified.
#' @param fun Test statistic function (Optional).
#' Note: One of either test_stat or fun must be specified.
#' fun must take in two parameters (treated outcomes and
#' control outcomes) and returns a numeric test statistic value.
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less"
#' @return Vector of counternull values
#' @noRd

find_set=function(counternull_value, search, t_obs, obs_c,
                   rand_matrix, index, y, w, test_stat,fun, alternative){

  low = counternull_value
  high = counternull_value
  c = obs_c
  start = index
  while ((any(c == obs_c)) & (index < length(search))){
    high = counternull_value
    index = index + 1
    counternull_value = search[index]
    c = count(y,w,test_stat,fun, t_obs, counternull_value,
               rand_matrix, alternative)$counts

  }

  c = obs_c
  index = start
  counternull_value = low
  while (any(c == obs_c) & (index > 1)){
    low = counternull_value
    index = index - 1
    counternull_value = search[index]
    c = count(y,w,test_stat,fun, t_obs, counternull_value,
               rand_matrix, alternative)$counts

  }

  return(c(low, high))

}
