// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// Helper function to calculate probabilities
arma::mat calc_probs(const arma::mat& X, const arma::mat& beta) {
  arma::mat exp_Xbeta = exp(X * beta);
  arma::rowvec row_sums = sum(exp_Xbeta, 1).t();
  arma::mat probs = exp_Xbeta;
  for(size_t i = 0; i < exp_Xbeta.n_rows; i++) {
    probs.row(i) = exp_Xbeta.row(i) / row_sums(i);
  }
  return probs;
}

// Helper function to calculate objective
double calc_objective(const arma::mat& X, const arma::uvec& y, double lambda, const arma::mat& beta) {
  arma::mat P = calc_probs(X, beta);
  double obj = 0;
  for(size_t i = 0; i < X.n_rows; i++) {
    obj -= log(P(i, y(i)));
  }
  obj += 0.5 * lambda * accu(square(beta));
  return obj;
}

// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X, const arma::uvec& y, const arma::mat& beta_init,
                          int numIter = 50, double eta = 0.1, double lambda = 1){
  // All input is assumed to be correct
  
  // Initialize some parameters
  int K = max(y) + 1; // number of classes
  int p = X.n_cols;
  int n = X.n_rows;
  arma::mat beta = beta_init; // to store betas and be able to change them if needed
  arma::vec objective(numIter + 1); // to store objective values
  
  // Initialize anything else that you may need
  objective(0) = calc_objective(X, y, lambda, beta);
  
  // Newton's method cycle - implement the update EXACTLY numIter iterations
  for(int i = 0; i < numIter; i++) {
    arma::mat P = calc_probs(X, beta);
    
    for(int j = 0; j < K; j++) {
      arma::vec P_k = P.col(j);
      // Create indicator vector for current class
      arma::vec indicator = arma::zeros(n);
      for(int r = 0; r < n; r++) {
        if(y(r) == j) indicator(r) = 1;
      }
      
      // Compute gradient
      arma::vec grad = X.t() * (P_k - indicator) + lambda * beta.col(j);
      
      // Compute Hessian
      arma::vec W = sqrt(P_k % (1 - P_k));
      arma::mat W_X = X.each_col() % W;
      arma::mat H = W_X.t() * W_X + lambda * arma::eye(p, p);
      
      // Update beta using damped Newton's method
      beta.col(j) = beta.col(j) - eta * solve(H, grad);
    }
    
    // Store objective value
    objective(i + 1) = calc_objective(X, y, lambda, beta);
  }
  
  // Create named list with betas and objective values
  return Rcpp::List::create(Rcpp::Named("beta") = beta,
                            Rcpp::Named("objective") = objective);
}
