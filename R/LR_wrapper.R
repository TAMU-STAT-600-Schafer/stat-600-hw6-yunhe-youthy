#' @title Multi-class logistic regression Algorithm
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
#' # Multi-class logistic regression Algorithm Implementation Example.1
#' set.seed(123)
#' X <- cbind(1, matrix(rnorm(500), 100, 5))
#' Y <- sample(0:4, 100, replace = TRUE)
#' LRMultiClass(X = X, y = Y, numIter = 50, eta = 0.1, lambda = 1)
#' 
#' # Multi-class logistic regression Algorithm Implementation Example.2
#' set.seed(456)
#' n <- 100
#' X1 <- cbind(1, matrix(rnorm(n*2, mean = 0, sd = 1), n, 2))
#' X2 <- cbind(1, matrix(rnorm(n*2, mean = 2, sd = 1), n, 2))
#' X <- rbind(X1, X2)
#' Y <- c(rep(0, n), rep(1, n))
#' LRMultiClass(X = X, y = Y, numIter = 50, eta = 0.1, lambda = 1)
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  # Compatibility checks from HW3 and initialization of beta_init
  #Define the variables' dimension
  n = nrow(X)
  p = ncol(X)
  K = length(unique(y)) 
  
  if (!all(X[, 1] == 1)) {
    stop("First column of X should be 1s.") 
  }
  # Check for compatibility of dimensions between X and Y
  if (length(y) != n) {
    stop("The number of rows of X should be the same as the length of y.") 
  }
  # Check eta is positive
  if (eta <= 0) {
    stop("The learning rate eta should be positive.")
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop("The ridge parameter lambda should be non-negative.")
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if (is.null(beta_init)) {
    beta <- matrix(0, p, K)
  } else {
    if (nrow(beta_init) != p | ncol(beta_init) != K) {
      stop("The dimensions of beta_init supplied are not correct.")
    }
    beta <- beta_init
  }
  
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  # out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  out = LRMultiClass_c(X = X, y = y, beta_init = beta, numIter = numIter, eta = eta, lambda = lambda)
  
  # Return the class assignments
  return(out)
}