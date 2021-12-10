#' Creates a null distribution
#'
#' Resamples data to create null distribution. Calculates and prints observed
#' test statistic and P-value. Returns vector with null distribution data points
#' (test statistics created from resampling). Observed test statistic is
#' indicated in null distribution using dashed black line.
#'
#' @param sample_data Sample data set. Data should have column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param extreme Direction which is defined as more extreme for test statistic
#' in distribution (0 for less or 1 for greater)
#' @param rand_matrix Matrix with unique randomizations for exposure
#' assignment
#' @param permutation_null_function Function used to create permutation vector
#' for null distribution
#' @param test_stat Observed test statistic.
#' @param variable Variable measured
#' Format: sample_data$column
#' @param iterations Number of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @examples
#' create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_diff_means,find_test_stat_diff_means(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#'
#' create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_cohens_d,find_test_stat_cohens_d(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#'
#' create_null_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_t,find_test_stat_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128)
#'
#' create_null_distribution(sample_district_1D,0,rand_matrix_1D,
#' permutation_null_diff_means,find_test_stat_diff_means(sample_district_1D,
#' sample_district_1D$charge_prosecuted_1000_rate_post -
#' sample_district_1D$charge_prosecuted_1000_rate_pre),
#' sample_district_1D$charge_prosecuted_1000_rate_post -
#' sample_district_1D$charge_prosecuted_1000_rate_pre,10000)
#' @return Vector with all generated test statistics in null distribution
#' @export

# Creates exposure assignments
create_null_distribution<-function(sample_data, extreme, rand_matrix,
                                   permutation_null_function,test_stat,
                                   variable,iterations){
# Creates permutation vector
perm_samples<-permutation_null_function(rand_matrix,variable,iterations)

# creates histogram and prints p-value
null_hist<-hist(perm_samples,breaks=100,col = "gold",
                main=paste("Null Distribution"), xlab="Test Statistics")
abline(v=test_stat,col="black",lty=2, lwd=5)
if (extreme==0){ # smaller test statistics are more extreme.
  pvalue<-sum(perm_samples<=(test_stat))/iterations

} else { # larger test statistics are more extreme
    pvalue<-sum(perm_samples>=(test_stat))/iterations

}
print(test_stat)
print(pvalue)
return(invisible(perm_samples))
}
