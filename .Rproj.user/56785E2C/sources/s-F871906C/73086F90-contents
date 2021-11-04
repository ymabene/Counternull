#' Creates counternull distribution (No Histogram)
#'
#' Resamples data to create counternull distribution.
#' Returns vector with counternull distribution data points (test statistics
#' created from resampling).
#'
#' @param sample_data Sample data set. Data should have column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param rand_matrix Matrix with all possible randomizations of exposure
#' assignment
#' @param permutation_counter_function Function used to create permutation vector
#' for counternull distribution
#' @param counternull_value Number to test out as counternull value
#' @param test_stat Observed test statistic.
#' @param variable Variable measured.
#' Format: sample_data$column
#' @param iterations Numbers of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @param pairs Number of pairs of units there are to measure in dataset
#' (One pair=control unit + experimental unit)
#' @return Counternull distribution data (Vector)
#' @keywords internal

create_counternull_distribution_no_hist<-function(sample_data,rand_matrix,
                                                  permutation_counter_function,
                                                  counternull_value,test_stat,
                                                variable,
                                                iterations,pairs){
  # creates permutation vector with differences of means
  counter_samples<-permutation_counter_function(sample_data,rand_matrix,
                                                  counternull_value,variable,
                                                  iterations,pairs)
  return(invisible(counter_samples))
}
