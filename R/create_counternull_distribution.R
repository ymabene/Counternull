#' Plots a counternull distribution for a given counternull value and test
#' statistic
#'
#' Resamples data to create and plot counternull distribution for given test
#' statistic. Calculates and prints P-value. Returns vector with counternull
#' distribution data points (test statistics created from sampling). Observed
#' test statistic is indicated in distribution using dashed black line. No
#'  effect is indicated with gray dashed line. Counternull value is indicated
#'  with red dashed line.
#'
#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' For paired randomization xata must be ordered so that paired units are in
#' subsequent rows.
#' @param extreme Direction which is defined as more extreme for test statistic
#' in distribution (0 for less or 1 for greater)
#' @param rand_matrix Matrix with unique randomizations for exposure
#' assignment
#' @param permutation_counter_function Function used to create permutation
#' vector (Vector of tests statistics created from sampling that are used as
#' datapoints for counternull distribution. Test statistic much match observed
#' test statistic.)
#' @param counternull_value Number to test out as counternull value
#' @param test_stat Observed test statistic.
#' @param variable Variable measured (Format: sample_data$column)
#' @param iterations Number of unique arrangements of exposure assignments
#' used for sampling to generate counternull distribution (At most the number
#' of rows in rand_matrix)
#' @param pairs Number of pairs of units there are to measure in dataset
#' (One pair = control unit + experimental unit)
#' @examples
#' create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_counter_diff_means,
#' -3323,find_test_stat_diff_means(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#'
#' create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_counter_t,
#' -3127,find_test_stat_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#'
#' create_counternull_distribution(sample_district_1DS,0,rand_matrix_1DS,
#' permutation_counter_paired_t,
#' -3127,find_test_stat_paired_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#' @return Counternull test statistics (Vector)
#' @export

create_counternull_distribution<-function(sample_data,extreme,rand_matrix,
                                          permutation_counter_function,
                                          counternull_value,test_stat,
                                          variable,iterations,
                                          pairs){
  # creates permutation vector for given test statistic
  counter_samples<-permutation_counter_function(sample_data,rand_matrix,
                                                counternull_value,
                                                variable, iterations,
                                                pairs)
  # creates histograms
  range<-c(0,counter_samples)
  min_range<-min(range)
  max_range<-max(range)
  counter_hist<-hist(counter_samples,breaks=100,col = "goldenrod",
                     main=paste("Counternull Distribution"),
                     xlab="Test Statistics",xlim=c(min_range,max_range))
  abline(v=test_stat,col="black",lty=2, lwd=5)
  abline(v=counternull_value,col="red",lty=2, lwd=5)
  abline(v=0,col="gray",lty=2, lwd=5)
  if (extreme==0){ # smaller test statistics are more extreme
    pvalue<-sum(counter_samples>=(test_stat))/iterations
  } else { # larger test statistics are more extreme
    pvalue<-sum(counter_samples<=(test_stat))/iterations
  }
  print(pvalue)
  return(invisible(counter_samples))
}
