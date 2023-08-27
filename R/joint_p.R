#' Returns adjusted p-values
#'
#' @param x "Observed test statistic"
#' assignment
#' @param p All test statistics
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less"
#' @return Adjusted p-value
#' @noRd


joint_p=function(x, p, alternative){

  if(is.null(alternative)){
    alternative = "two-sided"
  }

  if(!any(alternative == c("less", "greater", "two-sided"))){
    stop('Argument "alternative" must be one of "less", "greater",
         or "two-sided".')
  }

  if(alternative == "less"){
    return(sum(p <= x)/ length(p))

  } else if(alternative == "greater"){

    return(sum(p >= x)/ length(p))
  }

  else{

    return(sum(abs(p)>= abs(x))/length(p))

  }


}
