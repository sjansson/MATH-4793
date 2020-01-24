#' Sample Means Function
#'
#' Finds sample mean for a vector of data
#'
#' This function helps us obtain the sample mean for each column of data in matrix
#' @param x a matrix
#'
#' @return a vector of sample means
#' @export
#'

SampleMeans = function(x){

  n = nrow(x) # n measurements in matrix x

  sums = apply(x, MARGIN = 2, FUN = sum) #sum of measurements for each variable

  xbar=(1 / n) * sums #divide by n; get mean of each variable's measurements

  return(xbar)

}






