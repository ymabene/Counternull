#' Finds randomized p-values distribution for each null_r
#'
#' @param x null_r object
#' @return P-value distribution
#' @noRd

perm_wrapper=function(x){

  if(!inherits(x, "null_rand")){
    stop('Argument "ls" must be a list of "null_rand" objects.')

  }

  perm=x$null_dist
  p_vals=sapply(perm, FUN = joint_p, p = perm, alternative = x$alternative)
  return(p_vals)

}
