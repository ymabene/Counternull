#' Finds counternull values
#'
#' Finds and prints full range of counternull values, the test statistic
#' and  p-value along with null and counternull distribution if counternull
#' values are found. Otherwise only null distribution is displayed.
#' Observed test statistic is indicated in distribution using dashed
#' black line. No effect is indicated with gray dashed line. Counternull
#' value is indicated with red dashed line. Counternull values are
#' returned if found. 0 is returned otherwise.
#' @param obs_pval P-value from null distribution
#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param extreme Direction which is defined as more extreme for test statistic
#' in distribution (0 for less or 1 for greater)
#' @param rand_matrix Matrix with all possible randomizations of exposure
#' assignment
#' @param permutation_null_function Function used to create permutation vector
#' for null distribution
#' @param permutation_counter_function Function used to create permutation vector
#' for counternull distribution (must be same test statistic used in null distribution)
#' @param low Lower bound of counternull value search
#' @param high Upper bound of counternull value search
#' @param test_stat Observed test statistic. (You can use built in functions to
#' find various test statistics in given dataset)
#' @param variable Variable measured for test statistic. Format: sample_data$column
#' @param iterations Number of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @param pairs Number of pairs of units there are to measure in dataset
#' (One pair = control unit + experimental unit)
#' @examples
#' \donttest{
#' find_counternull_values(.375,sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_diff_means,permutation_counter_diff_means,
#' -8000,0, find_test_stat_diff_means(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post-
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#'
#' find_counternull_values(.375,sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_t,permutation_counter_t,
#' -8000,0, find_test_stat_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post-
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#'
#' find_counternull_values(.375,sample_district_1DS,0,rand_matrix_1DS,
#' permutation_null_paired_t,permutation_counter_paired_t,
#' -8000,0, find_test_stat_paired_t(sample_district_1DS,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre),
#' sample_district_1DS$charge_prosecuted_1000_rate_post-
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#' }
#' @return Vector of Counternull Values (Numeric 0 if none are found)
#' @export

find_counternull_values<-function(obs_pval,sample_data,extreme,rand_matrix,
                                  permutation_null_function,
                                  permutation_counter_function,low,
                                  high,test_stat,
                                  variable,iterations,pairs){
  counter_samples<-0
  counternull_value<-2*test_stat # estimated counternull value
  while (low <= high) {
    counter_samples<-create_counternull_distribution_no_hist(sample_data,
                                                             rand_matrix,
                                                 permutation_counter_function,
                                                           counternull_value,
                                                             test_stat,
                                                             variable,
                                                            iterations,pairs)
    if (extreme==0){ # larger test statistics are more extreme.
      pval<-sum(counter_samples>=(test_stat))/iterations

    } else { # smaller test statistics are more extreme
      pval<-sum(counter_samples<=(test_stat))/iterations

    }
    if (pval == obs_pval) { # counternull value is identified
      start<-counternull_value
      # find counternull range
      counternull_values<-find_counternull_set(obs_pval,sample_data,extreme,
                                               rand_matrix,
                                               permutation_counter_function,
                                               counternull_value,
                                               test_stat,
                                               variable,
                                               iterations,pairs)
      unique(counternull_values)
      sort(counternull_values)
      print(unique(counternull_values))
      # create histograms
      perm_samples<-create_null_distribution(sample_data,extreme,rand_matrix,
                                             permutation_null_function,test_stat,
                                             variable,
                                             iterations)
      null_hist<-hist(perm_samples,breaks=100,col = "gold",
                      main = paste("Null Distribution"), xlab = "Test Statistics")
      counter_hist<-hist(counter_samples,breaks=100,col = "goldenrod",
                         main = paste("Counternull Distribution"),
                         xlab = "Test Statistics")
      range<-c(perm_samples,counter_samples)
      max_range<-max(range)
      min_range<-min(range)
      plot(null_hist, col="gold", xlim=c(min_range,max_range),
           main = paste("Counternull and Null Distribution"),
            xlab = "Test Statistics")  # first histogram
      plot(counter_hist, col="goldenrod", xlim=c(min_range,max_range),add=T)  # second
      abline(v=test_stat,col="black",lty=2, lwd=5)
      abline(v=start,col="red",lty=2, lwd=5)
      abline(v=0,col="gray",lty=2, lwd=5)
      break

    } else if (pval < obs_pval) {
        if (extreme==0){
          low<-counternull_value + 1 # only search larger numbers
        } else {
          high<-counternull_value - 1 # only search smaller numbers
        }
        counternull_value<-(low + high)%/%2

    } else {  # pval > obs_pval
        if (extreme==0){
          high<-counternull_value - 1 # only search smaller numbers
        } else {
          low<-counternull_value - 1 # only search larger numbers
        }
        counternull_value<-(low + high)%/%2
    }
  }
  if(low > high){ # no remaining numbers to search
    print("No Counternull Values found.")
    perm_samples<-create_null_distribution(sample_data,extreme,
                                           rand_matrix,
                                           permutation_null_function,
                                           test_stat,
                                           variable,iterations)
    null_hist<-hist(perm_samples,breaks=100,col = "gold")
    plot(null_hist, col="gold",xlim=c(min(perm_samples),max(perm_samples)),
         main = paste("Null Distribution"),
    xlab = "Test Statistics")  # first histogram
    abline(v=test_stat,col="black",lty=2, lwd=5)
    return(0)
  }
  return(invisible(counter_samples))
}
