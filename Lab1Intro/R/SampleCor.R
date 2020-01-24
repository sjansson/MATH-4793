#' Sample Correlation Function
#'
#' Finds (biased) sample correlation
#'
#' This function helps us obtain the correlation matrix
#' @param x a matrix
#'
#' @return a correlation matrix
#' @export


SampleCor = function(x){
  n = nrow(x)
  p = ncol(x)

  #initialize
  vec <- vector(mode = "numeric", length = n) #initialize a vector of length n
  s <- matrix(data = NA, nrow = p, ncol = p) # empty cov. matrix
  r <- matrix(data = NA, nrow = p, ncol = p) #empty correlation matrix


  sums=apply(x, MARGIN = 2, FUN=sum) #sum of measurements for each variable
  xbar=(1/n)*sums

  #calculation of s matrix

  for (k in 1:p){
    for (i in 1:p){

      for (j in 1:n){

        vec[j] <- (x[j,i] - xbar[i]) * (x[j,k] - xbar[k])
      }

      s[i,k] <- (1/(n)) * sum(vec)

    }
  }

  #calculation of r matrix

  for (i in 1:p){
    for (k in 1:p){

      r[i,k] <- s[i,k] / (sqrt(s[i,i]) * sqrt(s[k,k]))


    }
  }

  colnames(r) = colnames(x)
  rownames(r) = colnames(x)

  return(r)
}



