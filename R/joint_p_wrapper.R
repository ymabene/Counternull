#' Wrapper function for joint_p. Finds adjusted p-value for each null_rand
#' object.
#'
#' @param x null_rand object
#' @param p Joint test statistics
#' @return Adjusted p-value
#' @noRd

joint_p_wrapper=function(x, p){

  adjusted_p =joint_p(x$pvalue, p, "less")
  return(adjusted_p)
}
