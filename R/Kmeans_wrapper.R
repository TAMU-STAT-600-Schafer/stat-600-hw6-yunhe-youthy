#' K-means Cluster with Coordinate Descent (E-M)
#'
#' This function takes n samples and compute K cluster centroids after given iterations.
#'
#' @param X A matrix with samples stored in rows (dim: \code{n * p}). 
#' @param K An integer representing the number of cluster.
#' @param M A matrix with initial centroids stored in rows (dim: \code{K * p}, optional).
#' @param numIter The number of iterations. 
#'
#' @return A vector with integers from \code{1} to \code{K}, representing the cluster each sample belongs to.
#' 
#' @examples
#' data1 <- matrix(
#' c(1.0, 2.0,
#'   1.5, 1.8,
#'   5.0, 8.0,
#'   8.0, 8.0,
#'   1.0, 0.5,
#'   9.0, 7.0), 
#' nrow = 6,  
#' byrow = TRUE,  
#' dimnames = list(NULL, c("Feature1", "Feature2")))
#' 
#' kmeans_res <- MyKmeans(X = data1, K = 2)
#' 
#' @export
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  
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
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if (is.null(M)) {
    # Initialization: randomly choose K rows as centers
    M <- RandomSelection(X, K, n)
  } else {
    if (!identical(ncol(X), ncol(M))) stop("ERROR: Incompatible dimensions!")
  }
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}

RandomSelection <- function(X, K, n) {
  rchoice <- sample(n, K, replace=FALSE) 
  M <- X[rchoice, ]
  return(M)
}