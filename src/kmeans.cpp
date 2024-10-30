// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    arma::vec n_ones = arma::ones(n);
    arma::rowvec p_ones = arma::ones<arma::rowvec>(p);
    arma::vec X_norms = arma::sum(pow(X, 2), 1);
    arma::vec M_norms = arma::sum(pow(M, 2), 1);
    arma::mat M_iter = M.t(); // copied centers (transpose of the origin)
    
    // For loop with kmeans algorithm
    for (int i = 0; i < numIter; i++){
      /* Iteration Step I: M-Step (Assignment)
       Preparations */
      arma::mat XM = X * M_iter; // ij-th element: x_i^Tc_j
      arma::mat dists = X_norms * p_ones + n_ones * M_iter - 2 * XM; // matrix between samples and centroids
      //Compute and assign vectors to every current cluster
      Y = arma::index_min(dists, 1) + 1;
      
      // Iteration Step II: E-Step (New Centers)
      arma::mat M_temp = arma::zeros(p, K); // new centroids storage (temporary)
      for (int j = 1; j <= K; j++){
        arma::uvec index = arma::find(Y == j);
        if (index.n_elem == 0) {
          throw std::runtime_error("ERROR: Bad Initial Centroids!");
        } else{
          M_temp.col(j-1) = arma::mean(X.rows(index), 0).t();
        }
      }
      if (arma::all(arma::vectorise(M_temp == M_iter))) {
        break;    // exit the for loop
      } else {
        M_iter = M_temp; // next iteration
      }
    }
    
    // Returns the vector of cluster assignments
    return(Y);
}

