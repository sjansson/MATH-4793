#' PCA Function
#'
#' Runs Principal Components Analysis on a dataset
#'
#' This function creates an object with unique class "PCA" which includes loadings, eigenvectors
#' @param X a matrix of data
#'
#' @return object of class "PCA"
#' @export

prince <- function(X, cor = T){ ## PCA constructor function
  # prince is a cute name for a PRINCE-ipal components function

  for(i in 1:ncol(X)){ # Remove any grouping variables
    if(is.factor(X[,i])){
      X <- X[ ,-i]
    }
  }

  ## center data around mean
  X <- scale(X, scale = FALSE)

  ## obtain S
  if(cor == T){
    S <- cor(X)
  } else{
    S <- cov(X)
  }

  ## Eigenvalues
  lambda <- eigen(S)$values
  names(lambda) <- paste("Comp", 1:length(lambda))

  ## Eigenvectors Matrix
  e <- eigen(S)$vectors
  rownames(e) <- colnames(X)
  colnames(e) <- paste("Comp", 1:ncol(e))

  ## Cumulative percentage of total variance
  percent <- c()

  for (i in 1:length(lambda)){
    percent[i] <- lambda[i]/sum(lambda)
  }

  cumut <- c()
  cumut[1] <- percent[1]
  for (i in 2:length(lambda)){
    cumut[i] <- cumut[i-1] + percent[i]
  }


  ## Make loadings output
  loading <- as.table(rbind(lambda, percent, cumut))
  colnames(loading) <- paste("Comp", 1:ncol(loading))
  rownames(loading) <- c("Eigenvalue", "Proportion Var", "Cumulative Var")

  obj <- list("Eigenvalues" = lambda, "Eigenvectors" = e, "Loadings" = loading)
  class(obj) <- "PCA" # make up a new class
  obj # prints output

}





