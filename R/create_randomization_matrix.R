#' Creates randomization matrix for paired data
#'
#' Returns matrix with unique randomized allocations for paired data.

#' @param sample_data Sample data set. Data should have first column indicating
#' exposure (1) or non exposure (0) for each group (row) that is measured. Each
#' measured outcome (variable) should be represented by an additional column.
#' For paired randomization, data must be ordered so that paired units are in
#' subsequent rows.
#' @param arrangements Numbers of arrangements generated of exposure assignments
#' of which unique arrangements are extracted
#' @param units Number of units there are to measure in dataset
#' (One pair = control unit + experimental unit)
#' @param pair 1 for paired randomization. 0 for non-paired randomization
#' @examples
#' create_randomization_matrix(sample_district_1DS,10000,14,1)
#' @return Matrix with unique randomized allocations for paired data
#' @export

create_randomization_matrix<-function(sample_data,
                                         arrangements,units,pair){

  rand<-matrix(ncol=arrangements,nrow=units)

  if(pair==1){
    for(j in 1:arrangements)
    {
      count<-1
      for(i in 1:(units/2)) # for each pair
      {
        a<-sample(c(0,1),2,replace=FALSE)  # unique values
        rand[count,j]<-a[1]  # puts 1 or 0 into matrix
        rand[count+1,j]<-a[2] # puts 1 or 0 into matrix
        count<-count+2 # increments row
      }

    }

    # unique pairs only
    dim(unique(t(rand)))
    rand.t<-unique(t(rand))
    rand<-t(rand.t)
    return(invisible(rand))

  } else {
    v<-vector(length=units)
    v[1:(units/2)]<-1
    for(j in 1:arrangements)
    {
        a<-sample(v) # random arrangment
        rand[,j]<-a
    }

    # unique pairs only
    dim(unique(t(rand)))
    rand.t<-unique(t(rand))
    rand<-t(rand.t)
    return(invisible(rand))

  }


}
