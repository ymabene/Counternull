#' Calculate Observed Test Statistic
#'
#' Finds observed test statistic using treatment and control outcomes
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional)
#' @param fun Test statistic function (optional)
#' @examples
#' find_test_stat(sample_data$turn_angle, sample_data$w,
#' test_stat = c("diffmeans"))
#'
#' find_test_stat(sample_data$turn_angle, sample_data$w,
#'  test_stat = c("t"))
#' @return Observed test statistic (numeric)
#' @details
#' Assignments must be indicated in argument "w" using numeric 1 or 0.
#'
#' One of either argument "test_stat" or "fun" must be specified.
#'
#' Argument "fun" must take in two parameters (treated outcomes and
#' control outcomes) and returns a numeric test statistic value (scalar).
#'
#'
#' @export
#' @importFrom stats t.test
#' @importFrom effsize cohen.d

find_test_stat=function(y, w, test_stat=NULL,fun = NULL){

  if(!is.vector(y) | !all(is.numeric(y))){
    stop('Argument "y" must be a numeric vector.')
  }

  if(!is.vector(w) | !all(is.numeric(w))){
    stop('Argument "w" must be a numeric vector.')

  }

  if(length(w) != length(y)){
    stop('Length of argument "y" must be same length as argument "w".')
  }

  if(is.null(test_stat) & is.null(fun)){
    stop('Must specifiy either "test_stat" or "fun".')
  }

  if(!all(w == 1 | w == 0)){
    stop('Argument "w" or specified "rand_matrix" must be composed
           of numeric 1 and 0 for indicating assignments.')
  }

  if(is.null(test_stat)){
    if(length(formals(fun)) != 2){
      stop('Argument "fun" must take in 2 parameters.')
    }
    ret = fun(y[w == 1], y[w ==0])
    if(!is.numeric(ret) | length(ret) != 1){
      stop('Argument "fun" must return numeric scalar.')
    }
    return(invisible(ret))

  }

  if(!any(test_stat == c("diffmeans", "t", "paired-t", "cohens-d"))){
    stop('Argument "test_stat" must be one of "diffmeans", "t", "paired-t", or
         "cohens-d".')
  }

  if(test_stat == "diffmeans"){
    return(invisible(mean(y[w == 1]) - mean(y[w == 0])))
  }

  if(test_stat == "t"){
    return(invisible(t.test(y[w == 1], y[w == 0], mu = 0,
                            conf.level = .95)$statistic))
  }

  if(test_stat == "paired-t"){
    return(invisible(t.test(y[w == 1], y[w == 0], mu = 0,
                            paired = TRUE, conf.level = .95)$statistic))
  }

  if(test_stat == "cohens-d"){
    return(invisible(cohen.d(y[w == 1], y[w == 0])$estimate))
  }

}



