#' Find Counternull Values
#'
#' Retrieves counternull value set and returns object of "counternull" class.
#' @param null_r "null_rand" object corresponding to data
#' @param counts Vector containing lower and upper bounds for number of
#' test statistics more extreme than observed test statistic in
#' counternull randomization distribution (optional)
#' @param width Integer indicating the number of values to search for to retrieve
#' counternull set. Default value = 10000. (Increasing this argument may result
#' in additional counternull values being found.) (optional)
#' @param bw Histogram bin width (optional)
#'
#' @examples
#' \donttest{
#' n_r = create_null_rand(sample_data$turn_angle, sample_data$w,
#' sample_matrix, test_stat = c("diffmeans"))
#' c = find_counternull_values(n_r)
#' summary(c)
#' plot(c)
#' c = find_counternull_values(n_r, c(56,60))
#' summary(c)
#'}
#'
#' @return Class "counternull" with 6 entries:
#' \describe{
#' \item{counternull_perm}{Counternull test statistics for first counternull set}
#' \item{low}{Counternull test statistics for second counternull set}
#' \item{high}{Lower bound of counternull set}
#' \item{counternull_perm_two}{Upper bound of counternull set}
#' \item{low_two}{Lower bound of second counternull set}
#' \item{high_two}{Upper bound of second counternull set}
#' \item{null_rand}{Specified "null_rand" object}
#' \item{bw}{Specified bin width}
#' }
#' @details
#' Call summary on "counternull" class to retrieve range of counternull values.
#' Call plot on "counternull" class for visualization
#' of counternull distribution.
#'
#' Argument "counts" must contain whole numbers for bounds.Lower bound must be
#' smaller than upper bound. If argument is not specified, counternull values
#' will be obtained using the "counts" argument from the specified "null_rand"
#' argument.
#'
#' If no counternull values are found, all entries in class are set to null.
#' If only one set of counternull values are found, "perm_two", low_two"
#' and "high_two" are set to null.
#'
#'
#' @references \doi{10.1111/j.1467-9280.1994.tb00281.x}
#' @export


find_counternull_values=function(null_r, counts = NULL,width = NULL, bw = NULL){

  if(!inherits(null_r,"null_rand")){
    stop('Argument "null_r" must be of class "null_rand".')
  }

  if(!is.null(bw)){
    if(!is.numeric(bw) | length(bw) != 1 | bw <= 0){
      stop('Argument "bw" must be a positive numeric scalar.')
    }

  }

  if(!is.null(width)){
    if(!is.numeric(width) | length(width) != 1){
      stop('Argument "width" must be an integer.')
    }

  }

  if(is.null(counts)){
    counts = null_r$counts
  } else {
    if(length(counts) != 2){
      stop('Argument "counts" must be length 2.')
    }
    if(!is.numeric(counts)){
      stop('Argument "counts" must be a numeric.')
    }
    if(!(counts[1] <= counts[2])){
      stop('Lower bound must be <= to upper bound in argument "counts".')
    }
    if((counts[1] < 0) | (counts[2] > length(null_r$null_dist))){
      stop('Argument "counts" must specify a non-negative range of numbers
      no larger than the number of permutations within argument "null_rand".')
    }

    counts = seq(round(counts[1]), round(counts[2]), by = 1)

    if(length(counts) >= .10 * length(null_r$null_dist)){
      warning('Argument "counts" exceeds 10% of null
              randomization distribution.')
    }

  }

  # bounds are 4 times the observed effect size
  low = 1
  if(!is.null(width)){
    high = max(10000, width)
  } else{
    high = 10000
  }
  search=seq(-4*abs(null_r$t_obs),4*abs(null_r$t_obs),
              8*abs(null_r$t_obs)/(high-1))

  if(null_r$alternative == "two-sided"){
    s_p=search[search>=0]
    s_n=search[search<0]
    p=find_counternull_values_int(counts, search,high, null_r$t_obs, null_r$y,
                                   null_r$w, null_r$alternative,
                                   null_r$rand_matrix,null_r$test_stat,
                                   null_r$fun, s=0)

    n=find_counternull_values_int(counts, search, high,null_r$t_obs, null_r$y,
                                   null_r$w, null_r$alternative,
                                  null_r$rand_matrix, null_r$test_stat,
                                   null_r$fun, s=1)
    if((!is.null(p)) & (!is.null(n))){

      # combine the two lists
      c_ob = list(counternull_perm = p$perm, low = p$low, high = p$high,
                  counternull_perm_two= n$perm,
                  low_two = n$low, high_two = n$high, null_r = null_r,
                  bw = bw)


    } else if((!is.null(p)) & (is.null(n))){

      c_ob = list(counternull_perm = p$perm, low = p$low, high = p$high,
                  counternull_perm_two = NULL,
                  low_two = NULL, high_two = NULL, null_r = null_r,
                  bw = bw)



    } else if((is.null(p)) & (!is.null(n))){

      c_ob = list(counternull_perm = NULL, low = NULL, high = NULL,
                  counternull_perm_two = n$perm,
                  low_two= n$low, high_two = n$high, null_r = null_r,
                  bw = bw)


    } else{

      c_ob = list(counternull_perm = NULL, low = NULL, high = NULL,
                  counternull_perm_two = NULL,
                  low_two= NULL, high_two = NULL, null_r = null_r,
                  bw = bw)


    }


  } else{
    c <- find_counternull_values_int(counts, search,high, null_r$t_obs,
                                     null_r$y,null_r$w,
                                 null_r$alternative,null_r$rand_matrix,
                                 null_r$test_stat, null_r$fun, s=0)
    if(!is.null(c)){
      c_ob = list(counternull_perm = c$perm, low = c$low, high = c$high,
                  counternull_perm_two = NULL,
                  low_two = NULL, high_two = NULL, null_r = null_r,
                  bw = bw)
    } else{

      c_ob = list(counternull_perm = NULL, low = NULL, high = NULL,
                  counternull_perm_two = NULL,
                  low_two = NULL, high_two = NULL, null_r = null_r,
                  bw = bw)
    }

  }

  class(c_ob) = "counternull"
  return(invisible(c_ob))

}

#' @export
summary.counternull = function(object, ...){
  if(is.null(object$low) & is.null(object$low_two)){
    message("No counternull values found.")
  }

  if(is.null(object$low) & !is.null(object$low_two)){
    cat("Counternull Set: [", object$low_two,",", object$high_two,"]")
  }

  if(!is.null(object$low) & is.null(object$low_two)){
    cat("Counternull Set: [", object$low,",", object$high,"]")
  }

  if(!is.null(object$low) & !is.null(object$low_two)){

    cat("Counternull Set (Positive): [", object$low,",",
         object$high,"]", "\nCounternull Set (Negative): [", object$low_two,
        ",", object$high_two,"]")
  }


}


#' @export
#' @import ggplot2
#' @import dplyr
plot.counternull=function(x, ...){
  # Plots counternull distribution

  null_r = x$null_r
  bw = x$bw

  if(is.null(x$low) & is.null(x$low_two)){
    message("No counternull values found.")
    return()
  }



  ## Plotting  counter and null distributions
  if(is.null(bw)){
    bw = 2 * IQR(x$counternull_perm) / length(x$counternull_perm)^(1/3)
    # Freedman Diaconis Rule
    if(length(x$counternull_perm) >= 30){ # min 30 bins
      bw=min(bw,(max(x$counternull_perm) - min(x$counternull_perm))/30)
    }

    if(bw == 0){
      bw = length(x$null_dist)/3
    }

  }




  xx= NULL
  group = NULL


  if(!is.null(x$low) & !is.null(x$low_two)){

    dat = data.frame(xx = c(x$counternull_perm, null_r$null_dist),
                      group = rep(1:0, each = length(x$counternull_perm)))


    p1= ggplot(dat,aes(x=xx))+
      geom_histogram(data=subset(dat,group=='0'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw) +
      geom_histogram(data=subset(dat,group=='1'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw)+
      scale_fill_manual(name="group", values=c("steelblue2", "grey69"),
                        labels=c("Null","Counternull")) +
      geom_vline(xintercept = null_r$t_obs,
                 linewidth = 1.5,
                 colour = "black", alpha = .8) +
      xlab("Permuted Test Statistics") + ylab("Counts") +
      guides(fill=guide_legend("Distributions")) +
      ggtitle("Counternull Distribution (Positive Set)") +
      theme_classic()

    plot(p1)


    dat = data.frame(xx = c(x$counternull_perm_two, null_r$null_dist),
                      group = rep(1:0, each = length(x$counternull_perm_two)))


    p2= ggplot(dat,aes(x=xx))+
      geom_histogram(data=subset(dat,group=='0'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw) +
      geom_histogram(data=subset(dat,group=='1'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw)+
      scale_fill_manual(name="group", values=c("steelblue2", "grey69"),
                        labels=c("Null","Counternull")) +
      geom_vline(xintercept = null_r$t_obs,
                 linewidth = 1.5,
                 colour = "black", alpha = .8) +
      xlab("Permuted Test Statistics") + ylab("Counts") +
      guides(fill=guide_legend("Distributions")) +
      ggtitle("Counternull Distribution (Negative Set)") +
      theme_classic()

    plot(p2)



  }

  if(is.null(x$low) & !is.null(x$low_two)){

    dat = data.frame(xx = c(x$counternull_perm_two, null_r$null_dist),
                      group = rep(1:0, each = length(x$counternull_perm_two)))

  }

  if(!is.null(x$low) & is.null(x$low_two)){

    dat=data.frame(xx = c(x$counternull_perm, null_r$null_dist),
                      group = rep(1:0, each = length(x$counternull_perm)))
  }

  if((is.null(x$low) & !is.null(x$low_two)) |
     (!is.null(x$low) & is.null(x$low_two))){
    p1= ggplot(dat,aes(x=xx))+
      geom_histogram(data=subset(dat,group=='0'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw) +
      geom_histogram(data=subset(dat,group=='1'),aes(fill=factor(group)),
                     alpha=0.5, binwidth = bw)+
      scale_fill_manual(name="group", values=c("steelblue2", "grey69"),
                        labels=c("Null","Counternull")) +
      geom_vline(xintercept = null_r$t_obs,
                 linewidth = 1.5,
                 colour = "black", alpha = .7) +
      xlab("Permuted Test Statistics") + ylab("Counts") +
      guides(fill=guide_legend("Distributions")) +
      ggtitle("Counternull Distribution") +
      theme_classic()

    plot(p1)

  }



}




