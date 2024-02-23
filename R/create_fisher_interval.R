#' Compute Fisher Interval
#'
#' Computes Fisher (Fidicual) Interval and returns object of
#' "fisher_interval" class.
#' @param null_r "null_rand" object corresponding to data used for interval
#' @param alpha Significance level for Fisher Interval (default = .05
#' for 95\% confidence)
#' @param width Integer indicating the number of values to search for to construct
#' Fisher Interval. Default value = 10000. (Increasing this argument may result
#' in increased accuracy of interval.) (Optional)
#'
#' @examples
#' \donttest{
#' y = sample_data$turn_angle
#' w = sample_data$w
#' n_r = create_null_rand(y,w, sample_matrix, test_stat = c("diffmeans"))
#' f= create_fisher_interval(n_r)
#' summary(f)
#' plot(f)
#'}
#'
#' @return Class "fisher_interval" with 4 entries:
#' \describe{
#' \item{lower_bound}{Lower bound of Fisher Interval}
#' \item{upper_bound}{Upper bound of Fisher Interval}
#' \item{alpha}{Specified significance value}
#' \item{pvalue_lower}{P-value corresponding to lower bound of interval}
#' \item{pvalue_upper}{P-value corresponding to upper bound of interval}
#' \item{range}{Range of effect values tested}
#' \item{null_r}{Specified "null_rand" object}
#' }
#' @details
#' Call summary on "fisher_interval" class to retrieve information on the
#' Fisher Interval. Call plot on "fisher_interval" class for visualization
#' of Fisher Interval.
#'
#' Use "create_null_rand" function to produce "null_rand" object for first
#' argument.
#'
#' Note: The warning 'Fisher Interval coverage is smaller than specified'
#' indicates that there are no effect sizes found that match the p-value bounds
#' for the specified significance level (ie. .025 and .975 for alpha = .05).
#' In this case, the largest possible interval found under the significance
#' level alpha will be returned. Check "pvalue_lower" and "pvalue_upper"
#' parameters to see which p-value bounds are used.
#' @references \doi{10.48550/arXiv.2105.03996}
#' @export

create_fisher_interval=function(null_r,alpha=NULL,width = NULL){

  if(is.null(alpha)){
    alpha = .05
  }

  if(!(alpha > 0 & alpha < 1)){
    stop('Argument "alpha" must be between 0 and 1.')
  }

  if(!inherits(null_r, "null_rand")){
    stop('Argument "null_r" must be of "null_rand" class.' )
  }

  if(!is.null(width)){
    if(!is.numeric(width) | length(width) != 1){
      stop('Argument "width" must be an integer.')
    }

  }

  bounds=vector(length=2)
  low_c=(alpha/2)*length(null_r$null_dist)
  high_c=(1-(alpha/2))*length(null_r$null_dist)

  #  initial bounds are 4 times the observed effect size
  low = 1
  if(!is.null(width)){
    high = max(10000, width)
  } else{
    high = 10000
  }
  bounds_l = -4*abs(null_r$t_obs)
  bounds_h = 4*abs(null_r$t_obs)
  i = 0
  search=seq(bounds_l,bounds_h, (bounds_h - bounds_l)/(high-1))
  counts_b = sapply(c(search[1], search[length(search)]),fisher_count_wrapper,
                     y =null_r$y, w=null_r$w, test_stat =null_r$test_stat,
                     fun = null_r$fun, t_obs = null_r$t_obs,
                     rand_matrix= null_r$rand_matrix, alternative = c("less"))

  ## expand up to 16 times
  while((counts_b[1] >= low_c | counts_b[2] <= high_c) & i < 2){

    if(counts_b[1] > 0){
      bounds_l = 2 * bounds_l
    }

    if(counts_b[2] < length(null_r$null_dist)){
      bounds_h = 2 * bounds_h

    }

    search=seq(bounds_l,bounds_h, (bounds_h - bounds_l)/(high-1))
    counts_b = sapply(c(search[1], search[length(search)]),
                       fisher_count_wrapper,
                       y =null_r$y, w=null_r$w, test_stat =null_r$test_stat,
                       fun = null_r$fun, t_obs = null_r$t_obs,
                       rand_matrix= null_r$rand_matrix, alternative = c("less"))
    i = i + 1
  }


  ls=fisher_binary(search,high, low_c, high_c, null_r$y,null_r$w,
                        null_r$test_stat,null_r$fun,
                        t_obs=null_r$t_obs, rand_matrix=null_r$rand_matrix)

  bounds = unlist(ls[1])
  pval_bounds = unlist(ls[2])

  if(pval_bounds[1] != (alpha/2) | pval_bounds[2] != (1 - (alpha/2))){
    warning('Fisher Interval coverage is smaller than specified. Use
            summary() for more info.')
  }


  f=list(lower_bound = bounds[1], upper_bound = bounds[2], alpha = alpha,
         pvalue_lower = pval_bounds[1], pvalue_upper = pval_bounds[2],
          range = c(bounds_l, bounds_h), null_r = null_r)
  class(f) = "fisher_interval"
  return(invisible(f))

}


#' @export
summary.fisher_interval = function(object, ...){
  cat("Fisher Interval: [", object$lower_bound,",", object$upper_bound,"]",
      "\nAlpha:", object$alpha,
      "\nP-Value Lower:", object$pvalue_lower, "P-Value Upper:",
      object$pvalue_upper)
}


#' @export
#' @import ggplot2
#' @import dplyr
#' @import tidyr
plot.fisher_interval=function(x, ...){ # Plots Fisher Interval

  null_r = x$null_r
  bounds_l = x$range[1]
  bounds_h = x$range[2]

  search=seq(bounds_l,bounds_h, (bounds_h - bounds_l)/(100-1))
  counts = sapply(search,fisher_count_wrapper,
                   y = null_r$y, w=null_r$w, test_stat = null_r$test_stat,
                   fun = null_r$fun,
                   t_obs = null_r$t_obs, rand_matrix= null_r$rand_matrix,
                   alternative = c("less"))

  ## plot
  effect = NULL
  pvalue = NULL

  data_plot = data.frame(effect = search,
                          pvalue = counts/length(null_r$null_dist))

  p3 =
    ggplot(data_plot, aes(x = effect, y = pvalue)) +
    geom_hline(yintercept = (x$alpha/2), colour = 'darkorange2',
               linewidth = 1.2, linetype = "dashed") +
    geom_hline(yintercept = (1-(x$alpha/2)), colour = 'darkorange2',
               linewidth = 1.2, linetype = "dashed")+
    ylim(0,1) +
    geom_line(colour = 'mediumblue', linewidth = 1.2) +
    xlab("Hypothetical Constant Treatment Effects") + ylab("p-values") +
    ggtitle("Fisher Interval") +
    theme_bw()

  # display the graph
  plot(p3)

}
