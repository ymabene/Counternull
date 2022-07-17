#' Finds range of counternull values
#'
#' Prints full range of counternull values given one counternull value.
#' @param obs_pvalue P-value from null distribution
#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param extreme Direction which is defined as more extreme for test statistic
#' in distribution (0 for left or 1 for right)
#' @param rand_matrix Matrix with all possible randomizations of exposure
#' assignment
#' @param permuation_counter_function Function used to create permutation vector
#' for counternull distribution
#' @param counternull_value Number to test out as counternull value
#' @param test_stat Observed test statistic. (You can use find_test_stat() to
#' find difference in means from given dataset)
#' @param variable Variable Measurement Format: sample_data$column
#' @param iterations Numbers of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @param pairs Number of pairs of units there are to measure in dataset
#' (One pair=control unit + experimental unit)
#' @param round_p Number of significant digits P-value should be rounded to
#' @param round_c Number of significant digits counternull values
#' should be rounded to
#' @param increment Value to increment numbers by to find counternull values
#' @return Vector of counternull values
#' @keywords internal

find_counternull_set<-function(obs_pvalue,sample_data,extreme,rand_matrix,
                               permutation_counter_function,
                               counternull_value,test_stat,
                               variable, iterations,pairs,round_p,
                               round_c,increment){
  start<-counternull_value
  counternull_values<-start
  pval<-obs_pvalue
  i<-1
  while (signif(pval,round_p) == signif(obs_pvalue,round_p)){ # Tries numbers larger
    # than counternull value
    counternull_values<-c(counternull_values,counternull_value)
    i<-i+1
    counternull_value<-round(counternull_value + increment,round_c)
    counter_samples<-create_counternull_distribution_no_hist(sample_data,
                                                             rand_matrix,
                                                  permutation_counter_function,
                                                            counternull_value,
                                                            test_stat,
                                                            variable,
                                                            iterations,pairs)
    if (extreme==0) {
      pval<-sum(counter_samples>=(test_stat))/iterations
    } else {
      pval<-sum(counter_samples<=(test_stat))/iterations

    }
  }
  # Counternull value was too large. Tries numbers smaller than starting value
  counternull_value<-start
  pval<-obs_pvalue
  while (signif(pval,round_p) == signif(obs_pvalue,round_p)) {
    counternull_values<-c(counternull_values,counternull_value)
    i<-i+1
    counternull_value<-round(counternull_value - increment,round_c)
    counter_samples<-create_counternull_distribution_no_hist(sample_data,
                                                             rand_matrix,
                                                  permutation_counter_function,
                                                             counternull_value,
                                                           test_stat,
                                                           variable,
                                                           iterations,pairs)
    if (extreme==0) {
      pval<-sum(counter_samples>=(test_stat))/iterations
    } else {
      pval<-sum(counter_samples<=(test_stat))/iterations

    }

  }
  return(counternull_values)
}
