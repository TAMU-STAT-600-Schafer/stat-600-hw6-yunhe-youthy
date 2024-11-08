#' @title K-means Cluster with Coordinate Descent
#'
#' @description
#' This function takes n samples and compute K cluster centroids after given iterations.
#' 
#' @param X A matrix with samples stored in rows (dim: \code{n * p}). 
#' @param K An integer representing the number of cluster.
#' @param M A matrix with initial centroids stored in rows (dim: \code{K * p}, optional).
#' @param numIter The number of iterations. 
#'
#' @return A vector with integers from \code{1} to \code{K}, representing the cluster each sample belongs to.
#' 
#' @export
#' 
#' @examples
#' # Kmeans Algorithm Implementation Example.1
#' set.seed(123)
#' X1 <- matrix(rnorm(100), nrow = 20)
#' MyKmeans(X = X1, K = 3)
#' 
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  
  ##Part A: Data checking and processing
  # Check if K is numeric and an integer
  if (K != as.integer(K)) {
    stop("The input data K is not numeric or integer.")
  }
  # Check if X is NULL
  if (is.null(X)) {
    stop("The input data matrix X is NULL.")
  }
  X <- as.matrix(X)    #Convert the data structure of X into matrix
  colnames(X) <- NULL  #Delete the column names from the original data
  # Check if X is empty
  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("The input data matrix X is empty.")
  }
  # Check if the number of data points is less than the number of clusters
  if (nrow(X) < K) {
    stop("Number of data points is less than the number of clusters.")
  }
  # Check if X has NA values
  if (any(is.na(X))) {
    stop("The input data X has NA values.")
  }
  p <- ncol(X)  # Number of features/Dimension of data points
  
  # Check whether M is NULL or not and compatibility with X dimensions and K
  if (is.null(M)) {
    # If NULL, initialize based on K randomly selected points from X.
    M <- X[sample(1:n, K), , drop = FALSE]
  } else if (nrow(M) != K || ncol(M) != p) {
    # If not NULL, check for compatibility with X dimensions.
    stop("Dimension mismatch between M and X.")
  }
  M <- as.matrix(M)    #Convert the data structure of M into matrix
  
  
  ##Part B: Implement K-means algorithm
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X = X, K = K, M = M, numIter = numIter)
  
  # Return the class assignments
  return(Y)
}

