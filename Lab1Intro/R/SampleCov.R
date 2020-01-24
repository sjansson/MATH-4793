#' Sample Covariance Function
#'
#' Finds biased sample covariance
#'
#' This function helps us obtain the covariance matrix
#' @param x a matrix
#'
#' @return a covariance matrix
#' @export

SampleCov = function(x){

  n = nrow(x) # number of measurements
  p = ncol(x) # number of variables

  sums=apply(x, MARGIN = 2, FUN=sum) #sum of measurements for each variable
  xbar=(1/n)*sums # get vector of means

  vec <- vector(mode = "numeric", length = n) #initialize a vector of length n
  s <- matrix(data = NA, nrow = p, ncol = p) # empty cov. matrix with pxp dimension

  #calculation of cov

  for (k in 1:p){
    for (i in 1:p){

      for (j in 1:n){

        vec[j] <- (x[j,i] - xbar[i]) * (x[j,k] - xbar[k])
      }

      s[i,k] <- (1/(n)) * sum(vec)

    }
  }

  colnames(s) = colnames(x)
  rownames(s) = colnames(x)

  return(s)
}

