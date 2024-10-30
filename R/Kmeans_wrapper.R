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
#' @export
#'
#' @examples
#' data1 <- matrix(
#' c(1.0, 2.0,
#'   1.5, 1.8,
#'   5.0, 8.0,
#'   8.0, 8.0,
#'   1.0, 0.5,
#'   9.0, 7.0), 
#' nrow = 6,  # Number of rows (data points)
#' byrow = TRUE,  # Fill matrix by rows 
#' dimnames = list(NULL, c("Feature1", "Feature2")))
#' 
#' kmeans_res <- MyKmeans(X = data1, K = 2)
#' 
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if (is.null(M)) {
    # Initialization: randomly choose K rows as centers
    M <- RandomSelection(X, K, n)
    flagM <- FALSE # random selection
  } else {
    if (!identical(ncol(X), ncol(M))) stop("ERROR: Not compatible dimensions!")
  }
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}