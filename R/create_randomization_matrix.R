#' Create Randomization Matrix
#'
#' Creates randomization matrix of assignments for given number of units and
#' permutations.Returns matrix with unique randomized permutations.
#'
#' @param n Number of permutations
#' @param units Number of units in dataset
#' @param block Numeric vector with length equal to "units"
#' indicating block assignments for each unit (optional)
#' @examples
#' create_randomization_matrix(14,128,rep(1:7, each = 2))
#' @return Matrix with unique randomized permutations
#' @export
#' @import randomizr
#' @details
#' Note, if the number of specified permutations exceeds the maximum number
#' of unique permutations, the matrix returned will contain the
#' maximum number of permutations.


create_randomization_matrix=function(units,n,block = NULL){
  if(!is.null(block)){
    if(!is.numeric(block) | length(block) != units){
      stop('Argument "block" must be a numeric vector with length equal to
           argument "unit".')
    }
    declaration = declare_ra(blocks = block )

  } else{
    if(!is.numeric(units)){
      stop('Argument "units" must be a numeric.')
    }
    declaration =declare_ra(N = units)
  }
  if(!is.numeric(n)){
    stop('Argument "n" must be a numeric.')
  }
  rand = obtain_permutation_matrix(declaration, n)
  return(invisible(rand))

}
