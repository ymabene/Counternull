#' Find bounds of Fisher interval
#'
#'
#' @param search List of effect sizes
#' @param low_c Lower bound to match based on specified alpha value
#' @param high_c Upper bound to match based on specified alpha value
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional).
#' @param fun Test statistic function (optional).
#' @param t_obs Observed test statistic
#' @param rand_matrix Randomization matrix
#' @return List contraining bounds of Fisher interval and p-value bounds
#' @noRd



fisher_binary=function(search, low_c, high_c, y,w,test_stat, fun,
                        t_obs, rand_matrix){
  low = 1
  high = 10000
  bounds=vector(length = 2)
  index = (round((low + high) / 2))

  while (low <= high){ # low

    if(low == high){
      bounds[1] = tau
      pvalue_lower = c / ncol(rand_matrix) # not matched
      break
    }

    tau =search[index]
    tau_n =search[index + 1]

    c=count(y,w,test_stat,fun, t_obs, tau, rand_matrix, "less")$counts
    c_n = count(y,w,test_stat, fun, t_obs, tau_n, rand_matrix, "less")$counts

    if(c == low_c | c_n == low_c){

      if(c == low_c){
        index = index
      }
        else{
          index = index + 1
        }



      while(index > 1){ # find largest interval
        c_l = count(y,w,test_stat, fun, t_obs, search[index - 1],
                     rand_matrix, "less")$counts
        if(c_l == low_c){
          index = index - 1
        } else{

          break
        }

      }

      bounds[1] = search[index] # tau or tau_n
      pvalue_lower = low_c / ncol(rand_matrix) # matched
      break
    }


    if((c < low_c )& (c_n > low_c)){
      bounds[1] = tau_n
      pvalue_lower = c_n / ncol(rand_matrix)
      break
    }

    if(c < low_c){
      low=index + 1
    }

    if(c > low_c){
      high=index - 1
    }

    index = round((low + high) / 2)

  }

  low = 1
  high = 10000
  index = (round((low + high) / 2))

  while (low <= high){ # high

    if(index == high){
      bounds[2] = tau
      pvalue_upper = c / ncol(rand_matrix) # not matched
      break
    }

    tau =search[index]
    tau_n =search[index + 1]

    c=count(y,w,test_stat, fun, t_obs, tau, rand_matrix, "less")$counts
    c_n = count(y,w,test_stat,fun, t_obs, tau_n, rand_matrix, "less")$counts

    if(c == high_c | c_n == high_c){

      if(c == high_c){
        index = index

      } else{
        index = index + 1

      }

      while(index < 10000){ # find largest interval
        c_r = count(y,w,test_stat, fun, t_obs, search[index + 1],
                     rand_matrix, "less")$counts
        if(c_r == high_c){
          index = index + 1
        } else{

          break
        }

      }

      bounds[2] = search[index] # tau or tau_n
      pvalue_upper = high_c / ncol(rand_matrix) # matched
      break
    }


    if((c < high_c )& (c_n > high_c)){
      bounds[2] = tau
      pvalue_upper = c / ncol(rand_matrix)
      break
    }

    if(c < high_c){
      low=index + 1
    }

    if(c > high_c){
      high=index - 1
    }

    index = round((low + high) / 2)

  }


  # if still null pick lowest number and highest number as bounds
  if(bounds[1] == FALSE){
    bounds[1] = search[1]
    c=count(y,w,test_stat,fun, t_obs, search[1], rand_matrix, "less")$counts
    pvalue_lower = c / ncol(rand_matrix)
  }

  if(bounds[2] == FALSE){
    bounds[2] = search[length(search)]
    c=count(y,w,test_stat,fun, t_obs, search[length(search)],
            rand_matrix, "less")$counts
    pvalue_upper = c / ncol(rand_matrix)
  }

  pval_bounds = c(pvalue_lower, pvalue_upper)
  return(invisible(list(bounds, pval_bounds)))

}

