#' Creates Paired T statistic vector for null distribution
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
#' permutation_null_paired_t(rand_matrix_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#' @return Vector with all generated test statistics in null distribution
#' @export

permutation_null_paired_t<-function(rand_matrix,variable,iterations){
  # permuation vector with Paired T test statistic
  perm_samples<-matrix(ncol=1,nrow=iterations)
  # creates distribution
  for(k in 1:iterations)
  {
    on<-(variable[rand_matrix[,k]==1]) # exposed
    off<-(variable[rand_matrix[,k]==0]) # not exposed
    test_stat <- t.test(on, off,
                        alternative = c("less"),
                        mu = 0, paired = TRUE,
                        conf.level = 0.95,)

    perm_samples[k] <- test_stat$statistic
  }
  return(invisible(perm_samples))
}
