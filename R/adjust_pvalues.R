#' Compute Fisher-Adjusted P-Values for Multiple Testing
#'
#' Adjusts p-values obtained from multiple comparisons. Computes Fisher-Adjusted
#' P-Values utilizing randomization-based method (Lee et al., 2017).
#'
#' @param ls List of "null_rand" objects
#' @param bw Histogram bin width (optional)
#' @examples
#' y = sample_data$turn_angle
#' w = sample_data$w
#' n_one = create_null_rand(y, w, sample_matrix, test_stat = c("t"))
#' y = sample_data$turn_angle
#' w = sample_data$w
#' fun = function(x,y){
#'   return(invisible(ks.test(x,y)$statistic))
#' }
#' n_two = create_null_rand(y, w, sample_matrix, fun = fun,
#' alternative = c("greater"))
#' adjust_pvalues(list(n_one,n_two))
#' @return Vector with adjusted p-values
#' @details
#' Argument "ls" must have a "null_rand" object for each p-value that needs to
#' be adjusted.
#'
#' Function plots joint p-value distribution.
#' @references \doi{10.5705/ss.202016.0116}
#' @export

adjust_pvalues=function(ls, bw = NULL){

  if(!is.list(ls)){
    stop('Argument "ls" must be a list.')
  }

  if(length(ls) < 2){
    stop('Argument "ls" must have atleast length 2.')
  }

  all_p_values= lapply(ls, FUN = perm_wrapper)

  max_length = max(sapply(all_p_values,length))
  all_p_values = lapply(all_p_values, FUN = extend, max_length=max_length)

  all_p_values_df =as.data.frame(do.call(cbind, all_p_values))

  min_dist=apply(all_p_values_df, 1, FUN = min, na.rm = TRUE)

  adjusted_p =lapply(ls, FUN = joint_p_wrapper,p = min_dist)

  if(is.null(bw)){
    bw <- 2 * IQR(min_dist) / length(min_dist)^(1/3)  # Freedman Diaconis Rule
    if(length(min_dist) >= 30){
      bw<-min(bw,(max(min_dist) - min(min_dist))/30) # min 30 bins
    }

    if(bw == 0){
      bw = length(min_dist)/3
    }

  }

  if(!is.numeric(bw) | length(bw) != 1 | bw <= 0){
    stop('Argument "bw" must be a positive numeric scalar.')
  }

  # make plot
  distribution_test_statistics = NULL
  t = tibble(distribution_test_statistics = min_dist)
  p4 =
    ggplot(t, aes(x = distribution_test_statistics)) +
    geom_histogram(colour = "white", fill = "wheat3",
                   binwidth = bw, alpha = .7) +
    xlab("Randomized P-values") + ylab("Counts")  +
    ggtitle("Joint P-value Distribution") +
    theme_classic()

  # display plot
  plot(p4)

  return(invisible(unlist(adjusted_p)))

}

#' @noRd
extend = function(x, max_length){
  return(c(x, rep(NA, max_length - length(x))))
}

