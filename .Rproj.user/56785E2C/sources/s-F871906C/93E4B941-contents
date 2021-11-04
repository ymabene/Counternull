#' Creates difference of means permutation vector for null distribution
#'
#' Resamples data to create null distribution. Returns vector with test
#' statistics in null distribution.

#' @param rand_matrix Matrix with unique randomizations for exposure
#' assignment
#' @param variable Variable measured
#' Format: sample_data$column
#' @param iterations Numbers of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @examples
#' permutation_null_diff_means(rand_matrix_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#' @return Vector with all generated test statistics in null distribution
#' @export

permutation_null_diff_means<-function(rand_matrix,variable,iterations){
  # permuation vector with differences of means
  perm_samples<-matrix(ncol=1,nrow=iterations)
  # creates distribution
  for(k in 1:iterations)
  {
    on<-mean(variable[rand_matrix[,k]==1]) # exposed
    off<-mean(variable[rand_matrix[,k]==0]) # not exposed
    perm_samples[k]<-on-off
  }
  return(invisible(perm_samples))
}
