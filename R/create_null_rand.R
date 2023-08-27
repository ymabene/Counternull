#' Create Null Randomization Distribution
#'
#' Generates null randomization distribution for a given test statistic.
#'
#' @param y Vector of observed outcomes
#' @param w Vector indicating treatment assignments
#' @param rand_matrix Matrix with permutations for experiment assignments
#' @param test_stat Name of built in test statistic function. Provide
#' "diffmeans" for difference of means, "t" for t test, "paired-t" for paired
#' t test, and "cohens-d" for cohen's d test (optional).
#' @param fun Test statistic function (optional).
#' @param alternative Character string specifying alternative hypothesis.
#' Must be one of "two-sided" (default), "greater", or "less".
#' @param bw Bin width for histogram (optional)
#' @examples
#' y = sample_data$turn_angle
#' w = sample_data$w
#' n_r = create_null_rand(y, w, sample_matrix, test_stat = c("t"))
#' summary(n_r)
#' plot(n_r)

#' @return Class "null_rand" with 11 entries:
#' \describe{
#'   \item{null_dist}{Vector of permuted test statistics under the
#'   null hypothesis}
#'   \item{t_obs}{Observed test statistic}
#'   \item{counts}{Number of test statistics more extreme than observed test
#'   statistic}
#'   \item{pvalue}{Fisher-Exact P-value}
#'   \item{alternative}{Specified alternative}
#'   \item{rand_matrix}{Randomization matrix used to generate null distribution}
#'   \item{bin_width}{Specified bin width}
#'   \item{y}{Observed outcomes}
#'   \item{w}{Vector indicating treatment assignments}
#'   \item{test_stat}{Name of built in test statistic function}
#'   \item{fun}{Test statistic function}
#'   }
#' @details
#' Call summary on "null_rand" class to retrieve information on the null
#' randomization distribution. Call plot on "null_rand" class for visualization
#' of null randomization distribution.
#'
#' Assignments must be indicated in arguments "w" and "rand_matrix"
#' using numeric 1 or 0.
#'
#' Argument "rand_matrix" must have assignment permutations in each column
#' and must have the same number of rows as there are entries in "w".
#'
#' One of either argument "test_stat" or "fun" must be specified.
#'
#' Argument "fun" must take in two parameters (treated outcomes and
#' control outcomes) and returns a numeric test statistic value (scalar).
#'
#' @export
#' @import ggplot2

create_null_rand=function(y, w, rand_matrix, test_stat = NULL,
                          fun = NULL, alternative=NULL, bw= NULL){

  if(is.null(alternative)){
    alternative = "two-sided"

  }

    if(!any(alternative == c("less", "greater", "two-sided"))){
    stop('Argument "alternative" must be one of "less", "greater",
         or "two-sided".')
  }


  if(is.null(test_stat) & is.null(fun)){
    stop('Must specifiy either "test_stat" or "fun".')
  }

  if(!is.vector(y) | !all(is.numeric(y))){
    stop('Argument "y" must be a numeric vector.')
  }

  if(!is.vector(w) | !all(is.numeric(w))){
    stop('Argument "w" must be a numeric vector.')

  }

  if(length(w) != length(y)){
    stop('Argument "y" must be same length as argument "w".')
  }

  if(!is.matrix(rand_matrix) & !is.data.frame(rand_matrix)){
    stop('Argument "rand_matrix" must be a matrix or dataframe.')
  }

  if(!is.null(bw)){
    if(!is.numeric(bw) | length(bw) != 1 | bw <= 0){
      stop('Argument "bw" must be a positive numeric scalar.')
    }

  }


  t_obs = find_test_stat(y, w, test_stat, fun)
  if(nrow(rand_matrix) != length(y)){
    stop('Argument "rand_matrix" must have same number of rows as the
         length of argument "y".')
  }
  perm = apply(rand_matrix,2, null_dist, y=y, test_stat = test_stat, fun = fun)

  if (alternative=="less"){ # smaller test statistics are more extreme.
    c=sum(perm <= t_obs)

  } else if(alternative == "greater"){ # larger test statistics are more extreme
    c=sum(perm >= t_obs)

  } else{ # double-sided
    c=sum(abs(perm)>= abs(t_obs))

  }

  null_r = list(null_dist =perm, t_obs = t_obs, counts = c,
                pvalue = c/length(perm), alternative = alternative,
                rand_matrix = rand_matrix, bin_width = bw,
                y=y, w=w, test_stat = test_stat, fun=fun)
  class(null_r) = "null_rand"
  return(invisible(null_r))

}


#' @keywords internal
null_dist=function(x, y, test_stat, fun){
  # calculates test statistic under null hypothesis
  tau = find_test_stat(y, x, test_stat, fun )
  return(tau)
}



#' @export
summary.null_rand = function(object, ...){
  cat("Observed test statistic:", object$t_obs,
      "\nNumber of extreme test statistics:", object$counts,
      "\nP-value:", object$pvalue, "\nAlternative:", object$alternative)
}

#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom stats IQR
plot.null_rand=function(x, ...){ # Plots null randomization distribution
  # Freedman Diaconis Rule
  bw = x$bin_width
  if(is.null(bw)){
    bw = 2 * IQR(x$null_dist) / length(x$null_dist)^(1/3)
    if(length(x$null_dist) >= 30){
      bw=min(bw,(max(x$null_dist) - min(x$null_dist))/30) # min 30 bins
    }
    if(bw == 0){
      bw = length(x$null_dist)/3
    }

  }
  # plot
  distribution_test_statistics = x$null_dist
  t = tibble(distribution_test_statistics = x$null_dist)
  p =
    ggplot(t, aes(x = distribution_test_statistics)) +
    geom_histogram(colour = "white", fill = "steelblue2",
                   binwidth = bw, alpha = .7) +
    geom_vline(xintercept = x$t_obs,
               linewidth = 1.5,
               colour = "black") +
    xlab("Permuted Test Statistics") + ylab("Counts")  +
    ggtitle("Null Randomization Distribution") +
    theme_classic() +
    theme(plot.margin = margin(20, 20, 20, 20))

  # display plot
  p


}



