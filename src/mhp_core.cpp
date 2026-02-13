// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Build second-difference matrix K
// [[Rcpp::export]]
arma::mat build_K(int T) {
  mat K(T - 2, T, fill::zeros);
  for (int i = 0; i < T - 2; i++) {
    K(i, i) = 1.0;
    K(i, i + 1) = -2.0;
    K(i, i + 2) = 1.0;
  }
  return K;
}

// Core filter computation: trend = solve(I + lambda*A, x)
// [[Rcpp::export]]
arma::vec hp_trend(const arma::vec& x, const arma::mat& A, double lambda) {
  int T = x.n_elem;
  mat I = eye<mat>(T, T);
  mat M = I + lambda * A;
  return solve(M, x, solve_opts::fast);
}

// Compute GCV for single lambda (Eq. 6 from paper)
// [[Rcpp::export]]
double gcv_single(const arma::vec& x, const arma::vec& trend, double lambda, int T) {
  vec resid = x - trend;
  double ss = dot(resid, resid);
  return ss * (1.0 + 2.0 * T / lambda) / T;
}

// Find optimal lambda via grid search
// [[Rcpp::export]]
List mhp_core(const arma::vec& x, int max_lambda) {
  int T = x.n_elem;
  
  // Build A = K'K once
  mat K = build_K(T);
  mat A = K.t() * K;
  
  // Grid search for optimal lambda
  double min_gcv = datum::inf;
  int opt_lambda = 1;
  
  for (int lam = 1; lam <= max_lambda; lam++) {
    vec trend = hp_trend(x, A, (double)lam);
    double gcv = gcv_single(x, trend, (double)lam, T);
    if (gcv < min_gcv) {
      min_gcv = gcv;
      opt_lambda = lam;
    }
  }
  
  // Final decomposition with optimal lambda
  vec trend = hp_trend(x, A, (double)opt_lambda);
  vec cycle = x - trend;
  
  return List::create(
    Named("lambda") = opt_lambda,
    Named("trend") = trend,
    Named("cycle") = cycle,
    Named("gcv") = min_gcv
  );
}

// Standard HP filter with fixed lambda
// [[Rcpp::export]]
List hp_core(const arma::vec& x, double lambda) {
  int T = x.n_elem;
  mat K = build_K(T);
  mat A = K.t() * K;
  vec trend = hp_trend(x, A, lambda);
  vec cycle = x - trend;
  
  return List::create(
    Named("lambda") = lambda,
    Named("trend") = trend,
    Named("cycle") = cycle
  );
}

// Batch processing - multiple series
// [[Rcpp::export]]
List mhp_batch(const arma::mat& X, int max_lambda) {
  int n = X.n_cols;
  int T = X.n_rows;
  
  // Build A = K'K once for all series
  mat K = build_K(T);
  mat A = K.t() * K;
  
  // Pre-allocate output
  mat trends(T, n);
  mat cycles(T, n);
  ivec lambdas(n);
  vec gcvs(n);
  
  // Process each series
  for (int j = 0; j < n; j++) {
    vec x = X.col(j);
    
    double min_gcv = datum::inf;
    int opt_lambda = 1;
    
    for (int lam = 1; lam <= max_lambda; lam++) {
      vec trend = hp_trend(x, A, (double)lam);
      double gcv = gcv_single(x, trend, (double)lam, T);
      if (gcv < min_gcv) {
        min_gcv = gcv;
        opt_lambda = lam;
      }
    }
    
    vec trend = hp_trend(x, A, (double)opt_lambda);
    trends.col(j) = trend;
    cycles.col(j) = x - trend;
    lambdas(j) = opt_lambda;
    gcvs(j) = min_gcv;
  }
  
  return List::create(
    Named("lambdas") = lambdas,
    Named("trends") = trends,
    Named("cycles") = cycles,
    Named("gcvs") = gcvs
  );
}
