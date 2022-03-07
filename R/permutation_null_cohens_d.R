#' Creates Cohen's D vector for null distribution
#'
#' Resamples data to create null distribution. Returns vector with test
#' statistics in null distribution.

#' @param rand_matrix Matrix with unique randomizations for exposure
#' assignment
#' @param variable Variable measured for test statistic
#' Format: sample_data$column
#' @param iterations Numbers of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @examples
#' permutation_null_cohens_d(rand_matrix_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#' @return Vector with all generated test statistics in null distribution
#' @export

permutation_null_cohens_d<-function(rand_matrix,variable,iterations){
  # permuation vector with Cohen's D test statistic
  perm_samples<-matrix(ncol=1,nrow=iterations)
  # creates distribution
  for(k in 1:iterations)
  {
    on<-(variable[rand_matrix[,k]==1]) # exposed
    off<-(variable[rand_matrix[,k]==0]) # not exposed
    cohen_d<-cohen.d(on,off)
    perm_samples[k] <- cohen_d$estimate
  }
  return(invisible(perm_samples))
}
