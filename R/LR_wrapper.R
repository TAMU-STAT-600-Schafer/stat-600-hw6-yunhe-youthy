
#' Title Multi-class logistic regression Algorithm
#'
#' @param X A matrix with samples stored in rows (dim: \code{n * p}), 1st column should be 1s to account for intercept
#' @param y A vector of size n of class labels, from 0 to K-1
#' @param numIter Number of FIXED iterations of the algorithm, default value is 50
#' @param eta Learning rate, default value is 0.1
#' @param lambda Ridge parameter, default value is 1
#' @param beta_init Initial starting values of beta for the algorithm, should be p x K matrix 
#'
#' @return a list includes the: beta - p x K matrix of estimated beta values after numIter iterations
#' and objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
#' 
#' @export
#'
#' @examples
#' # Give example
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  # Compatibility checks from HW3 and initialization of beta_init
  
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  
  # Return the class assignments
  return(out)
}