#' Creates paired T statistic permutation vector for counternull distribution
#'
#' Resamples data to create counternull distribution. Returns vector with test
#' statistics in counternull distribution.

#' @param sample_data Sample data set. Data should have column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' @param rand_matrix Matrix with unique randomizations for exposure
#' assignment
#' @param counternull_value Number to test out as counternull value
#' @param variable Variable measured for test statistic
#' Format: sample_data$column
#' @param iterations Numbers of unique arrangements of exposure assignments
#' used to generate distribution (At most the number of rows in rand_matrix)
#' @param pairs Number of pairs of units there are to measure in dataset
#' (One pair = control unit + experimental unit)
#' @examples
#' permutation_counter_paired_t(sample_district_1DS, rand_matrix_1DS, -3323,
#' sample_district_1DS$charge_prosecuted_1000_rate_post -
#' sample_district_1DS$charge_prosecuted_1000_rate_pre,128,7)
#' @return Vector with all generated test statistics in counternull distribution
#' @export

permutation_counter_paired_t<-function(sample_data, rand_matrix,
                                       counternull_value,variable,
                                       iterations,pairs){
  # permutation vector with paired t statistic
  counter_samples <- matrix(ncol=1,nrow=iterations)
  x<-counternull_value

  for(k in 1:iterations)
  {
    obs_on<-vector(length=(2*pairs)) # observed exposure
    est_on<-vector(length=(2*pairs)) # estimated exposure
    obs_off<-vector(length=(2*pairs)) # observed non exposure
    est_off<-vector(length=(2*pairs)) # estimated non exposure
    t<-1 # index for unit whose exposure=1 in sample_data and =1 in rand_matrix
    u<-1 # index for unit whose exposure=0 in sample_data and =1 in rand_matrix
    v<-1 # index for unit whose exposure=0 in sample_data and =0 in rand_matrix
    w<-1 # index for unit whose exposure=1 in sample_data and =0 in rand_matrix

    for(j in 1:(2*pairs)){ # calculates tests statistics
      if(sample_data[j,1]==1 & rand_matrix[j,k]==1){
        obs_on[t]<-variable[j]
        t <-t+1
      }
      if(sample_data[j,1]==0 & rand_matrix[j,k]==1){
        est_on[u]<-variable[j] + x
        u <-u + 1
      }

      if(sample_data[j,1]==0 & rand_matrix[j,k]==0){
        obs_off[v]<-variable[j]
        v <-v+1
      }
      if(sample_data[j,1]==1 & rand_matrix[j,k]==0){
        est_off[w]<-variable[j] - x
        w <-w+1
      }



    }

    on<-c(obs_on,est_on)
    off<-c(obs_off,est_off)
    test_stat <- t.test(on,off,
                          alternative = c("less"),
                          mu = 0, paired = TRUE,
                          conf.level = .95)

    counter_samples[k] <- test_stat$statistic

  }
  return(invisible(counter_samples))
}

