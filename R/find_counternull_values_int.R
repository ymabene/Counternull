#' Finds counternull values
#'
#' @param obs_c Number of extreme test statistics in null distribution
#' @param search Range of effect sizes to test as counternull values
#' @param t_obs Observed test statistic
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less"
#' @param rand_matrix Randomization matrix
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional).
#' @param fun Test statistic function (optional).
#' @param s Numeric indicating sign of search values
#' @return List of counternull permuations and lower and upper bounds. NULL if
#' no counternull values are found
#' @noRd



find_counternull_values_int=function(obs_c, search, t_obs, y,w,
                                      alternative,rand_matrix,
                                      test_stat, fun,
                                      s){
  low = 1
  high = 10000
  index = (round((low + high) / 2))
  counternull_value=search[index] # estimated counternull value

  while (low <= high) {

    c_s=count(y,w,test_stat, fun, t_obs,
               counternull_value,rand_matrix, alternative)


    if (any(c_s$counts == obs_c)) { # counternull value is identified
      start=counternull_value
      # find counternull range
      c_range=find_set(counternull_value, search, t_obs, obs_c,
                        rand_matrix, index, y, w, test_stat,fun, alternative)
      counter_samples = c_s$perm
      break

    } else if (min(obs_c) > c_s$counts ) {


      ifelse((alternative == "less" | s == 1),
             low<-index + 1, high<-index - 1)

    } else {  # c_s$counts > obs_c

      ifelse((alternative == "less" | s == 1),
             high<-index - 1, low<-index + 1)

    }

    index = round((low + high) / 2)
    counternull_value=search[index]

  }

  if(low > high){ # no remaining numbers to search
    return(invisible(NULL))
  }
  return(invisible(list(perm = counter_samples,
                        low = c_range[1], high = c_range[2])))
}

