// mhpfilter: compiled cores of the HP, modified HP (MHP) and fully modified HP (FMHP) filters.
//
// Every filter of the package solves a symmetric positive definite pentadiagonal system, so each trend costs O(T):
//   constant lambda (HP, MHP)     (I + lambda A) g = x,                  A = K'K
//   "differences" form            (I + K' diag(lambda_r) K) g = x,       lambda_r on the T - 2 second differences
//   "rows" form (Hanif et al.)    (I + diag(lambda_t) A) g = x  <=>  (diag(1 / lambda_t) + A) g = diag(1 / lambda_t) x
// The "rows" form is the one of the published MATLAB code of Hanif, Iqbal and Choudhary (2017); it scales row t of A
// by lambda_t, and is solved through the symmetric matrix diag(1 / lambda_t) + A.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// ---- the pentadiagonal system --------------------------------------------------------------------------------------

struct Penta {
  arma::vec d0, d1, d2;   // diagonal, first and second off-diagonals of the symmetric matrix
  arma::vec scale;        // right-hand side scaling (1 for the symmetric forms, 1 / lambda_t for the rows form)
};

// bands of K' diag(w) K, w of length T - 2
static void kwk_bands(const arma::vec& w, int T, arma::vec& a0, arma::vec& a1, arma::vec& a2) {
  a0.zeros(T); a1.zeros(T - 1); a2.zeros(T - 2);
  const double k[3] = {1.0, -2.0, 1.0};
  for (int r = 0; r < T - 2; ++r) {
    for (int p = 0; p < 3; ++p) {
      a0(r + p) += w(r) * k[p] * k[p];
      if (p < 2) a1(r + p) += w(r) * k[p] * k[p + 1];
    }
    a2(r) += w(r) * k[0] * k[2];
  }
}

// form 0: differences (lam of length T - 2); form 1: rows (lam of length T); constant lambda is either form
static Penta make_system(const arma::vec& lam, int T, int form) {
  Penta P;
  if (form == 0) {
    kwk_bands(lam, T, P.d0, P.d1, P.d2);
    P.d0 += 1.0;
    P.scale.ones(T);
  } else {
    arma::vec one(T - 2, arma::fill::ones);
    kwk_bands(one, T, P.d0, P.d1, P.d2);
    P.scale = 1.0 / lam;
    P.d0 += P.scale;
  }
  return P;
}

// LDL' factor of a symmetric pentadiagonal matrix: unit lower factor with sub-diagonals l1, l2 and diagonal d
struct LDL {
  arma::vec d, l1, l2;
};

static LDL factor(const Penta& P) {
  const int T = P.d0.n_elem;
  LDL F;
  F.d.zeros(T); F.l1.zeros(T); F.l2.zeros(T);
  for (int i = 0; i < T; ++i) {
    if (i >= 2) F.l2(i) = P.d2(i - 2) / F.d(i - 2);
    if (i >= 1) {
      double b = P.d1(i - 1);
      if (i >= 2) b -= F.l2(i) * F.l1(i - 1) * F.d(i - 2);
      F.l1(i) = b / F.d(i - 1);
    }
    double di = P.d0(i);
    if (i >= 1) di -= F.l1(i) * F.l1(i) * F.d(i - 1);
    if (i >= 2) di -= F.l2(i) * F.l2(i) * F.d(i - 2);
    if (di <= 0) Rcpp::stop("the smoothing system is not positive definite (check that every lambda is positive)");
    F.d(i) = di;
  }
  return F;
}

// solve with every column of B (scaled first)
static arma::mat solve_ldl(const LDL& F, const arma::vec& scale, const arma::mat& B) {
  const int T = F.d.n_elem;
  arma::mat X = B.each_col() % scale;
  for (arma::uword c = 0; c < X.n_cols; ++c) {
    double* x = X.colptr(c);
    for (int i = 1; i < T; ++i) {
      x[i] -= F.l1(i) * x[i - 1];
      if (i >= 2) x[i] -= F.l2(i) * x[i - 2];
    }
    for (int i = 0; i < T; ++i) x[i] /= F.d(i);
    for (int i = T - 2; i >= 0; --i) {
      x[i] -= F.l1(i + 1) * x[i + 1];
      if (i + 2 < T) x[i] -= F.l2(i + 2) * x[i + 2];
    }
  }
  return X;
}

// the smoothing parameters of every observation (rows form, length T) or second difference (differences form,
// length T - 2): base lambda, raised at both ends by alpha * j^power for the k end values (j = 1 nearest the middle)
static arma::vec schedule(double base, double alpha, int k, double power, int n) {
  arma::vec lam(n);
  lam.fill(base);
  if (k > 0 && alpha != 0) {
    if (2 * k > n) Rcpp::stop("k end values on each side need at least 2k smoothing parameters");
    for (int j = 1; j <= k; ++j) {
      const double add = alpha * std::pow((double) j, power);
      lam(k - j) = base + add;       // first k: the end value gets the largest increase
      lam(n - k + j - 1) = base + add;
    }
  }
  return lam;
}

// ---- exported cores ------------------------------------------------------------------------------------------------

// trends of the columns of X with the smoothing parameters lam (length T, or T - 2 for the differences form)
// [[Rcpp::export(name = ".mhp_trend_cpp")]]
arma::mat mhp_trend_cpp(const arma::mat& X, const arma::vec& lam, int form) {
  const int T = X.n_rows;
  Penta P = make_system(lam, T, form);
  return solve_ldl(factor(P), P.scale, X);
}

// McDermott's criterion over a grid of base lambdas, with the end-point schedule (alpha, k, power):
// (1 / T) (1 + 2T / lambda) sum_t (x_t - g_t(lambda))^2. The bands of the system are linear in the base lambda,
// so they are prepared once and every lambda costs one in-place factorisation and solve.
// [[Rcpp::export(name = ".mhp_crit_cpp")]]
arma::vec mhp_crit_cpp(const arma::vec& x, const arma::vec& grid, double alpha, int k, double power, int form) {
  const int T = x.n_elem;
  const int n = form == 0 ? T - 2 : T;
  // increments of the schedule over the base value
  arma::vec inc = schedule(0.0, alpha, k, power, n);
  arma::vec one(T - 2, arma::fill::ones);
  arma::vec a0, a1, a2, e0, e1, e2;
  kwk_bands(one, T, a0, a1, a2);                         // A = K'K
  if (form == 0) kwk_bands(inc, T, e0, e1, e2);          // K' diag(inc) K
  std::vector<double> d0(T), d1(T > 1 ? T - 1 : 1), d2(T > 2 ? T - 2 : 1), sc(T), D(T), L1(T), L2(T), y(T);
  arma::vec out(grid.n_elem);
  for (arma::uword m = 0; m < grid.n_elem; ++m) {
    const double lam = grid(m);
    if (form == 0) {
      for (int i = 0; i < T; ++i) { d0[i] = 1.0 + lam * a0(i) + e0(i); sc[i] = 1.0; }
      for (int i = 0; i < T - 1; ++i) d1[i] = lam * a1(i) + e1(i);
      for (int i = 0; i < T - 2; ++i) d2[i] = lam * a2(i) + e2(i);
    } else {
      for (int i = 0; i < T; ++i) { sc[i] = 1.0 / (lam + inc(i)); d0[i] = a0(i) + sc[i]; }
      for (int i = 0; i < T - 1; ++i) d1[i] = a1(i);
      for (int i = 0; i < T - 2; ++i) d2[i] = a2(i);
    }
    // LDL' factorisation and solve, in place
    for (int i = 0; i < T; ++i) {
      L2[i] = i >= 2 ? d2[i - 2] / D[i - 2] : 0.0;
      if (i >= 1) {
        double b = d1[i - 1];
        if (i >= 2) b -= L2[i] * L1[i - 1] * D[i - 2];
        L1[i] = b / D[i - 1];
      } else {
        L1[i] = 0.0;
      }
      double di = d0[i];
      if (i >= 1) di -= L1[i] * L1[i] * D[i - 1];
      if (i >= 2) di -= L2[i] * L2[i] * D[i - 2];
      D[i] = di;
      double v = x(i) * sc[i];
      if (i >= 1) v -= L1[i] * y[i - 1];
      if (i >= 2) v -= L2[i] * y[i - 2];
      y[i] = v;
    }
    double ss = 0.0;
    for (int i = T - 1; i >= 0; --i) {
      double v = y[i] / D[i];
      if (i + 1 < T) v -= L1[i + 1] * y[i + 1];
      if (i + 2 < T) v -= L2[i + 2] * y[i + 2];
      y[i] = v;
      ss += (x(i) - v) * (x(i) - v);
    }
    out(m) = ss * (1.0 + 2.0 * T / lam) / T;
  }
  return out;
}

// gains of every estimate g_t at the frequencies theta: |sum_j b_tj exp(i theta (j - t))| = sqrt((Bc)_t^2 + (Bs)_t^2)
// with c_j = cos(theta j), s_j = sin(theta j); B is never formed
// [[Rcpp::export(name = ".mhp_gain_cpp")]]
arma::mat mhp_gain_cpp(int T, const arma::vec& lam, const arma::vec& theta, int form) {
  Penta P = make_system(lam, T, form);
  LDL F = factor(P);
  const int n = theta.n_elem;
  arma::mat CS(T, 2 * n);
  for (int w = 0; w < n; ++w) {
    for (int j = 0; j < T; ++j) {
      CS(j, w) = std::cos(theta(w) * j);
      CS(j, n + w) = std::sin(theta(w) * j);
    }
  }
  arma::mat S = solve_ldl(F, P.scale, CS);
  return arma::sqrt(arma::square(S.cols(0, n - 1)) + arma::square(S.cols(n, 2 * n - 1)));
}

// the filter weights B (row t: the weights of g_t), for plotting
// [[Rcpp::export(name = ".mhp_weights_cpp")]]
arma::mat mhp_weights_cpp(int T, const arma::vec& lam, int form) {
  Penta P = make_system(lam, T, form);
  arma::mat I(T, T, arma::fill::eye);
  // the columns of the solve are the columns of B; B is not symmetric in the rows form
  return solve_ldl(factor(P), P.scale, I);
}

// loss of every estimate: weight * sum_theta (G_t(theta) - G_c(theta))^2, c the centre (1-based)
static arma::vec loss_of(const arma::mat& G, int centre, double weight) {
  arma::mat D = G.each_row() - G.row(centre - 1);
  return weight * arma::sum(arma::square(D), 1);
}

// [[Rcpp::export(name = ".mhp_loss_cpp")]]
arma::vec mhp_loss_cpp(int T, const arma::vec& lam, const arma::vec& theta, int form, int centre, double weight) {
  return loss_of(mhp_gain_cpp(T, lam, theta, form), centre, weight);
}

// total loss of the end-point schedule for every alpha of a grid, at a fixed base lambda
// [[Rcpp::export(name = ".mhp_scheme_loss_cpp")]]
arma::vec mhp_scheme_loss_cpp(int T, double base, const arma::vec& alphas, int k, double power,
                              const arma::vec& theta, int form, int centre, double weight) {
  const int n = form == 0 ? T - 2 : T;
  arma::vec out(alphas.n_elem);
  for (arma::uword a = 0; a < alphas.n_elem; ++a) {
    arma::vec lam = schedule(base, alphas(a), k, power, n);
    out(a) = arma::accu(loss_of(mhp_gain_cpp(T, lam, theta, form), centre, weight));
  }
  return out;
}

// the schedule itself, for R
// [[Rcpp::export(name = ".mhp_schedule_cpp")]]
arma::vec mhp_schedule_cpp(double base, double alpha, int k, double power, int n) {
  return schedule(base, alpha, k, power, n);
}

// exact generalised cross-validation (Craven and Wahba 1979; Choudhary et al. 2014, eq. 5) from the eigenvalues mu
// of A = K'K and the squared coordinates z2 of the series in its eigenvectors:
// RSS(lambda) = sum_i (lambda mu_i / (1 + lambda mu_i))^2 z2_i,  tr B(lambda) = sum_i 1 / (1 + lambda mu_i),
// GCV(lambda) = (RSS / T) / (1 - tr B / T)^2
// [[Rcpp::export(name = ".mhp_gcv_exact_cpp")]]
arma::vec mhp_gcv_exact_cpp(const arma::vec& mu, const arma::vec& z2, const arma::vec& grid) {
  const int T = mu.n_elem;
  arma::vec out(grid.n_elem);
  for (arma::uword m = 0; m < grid.n_elem; ++m) {
    double rss = 0.0, tr = 0.0;
    for (int i = 0; i < T; ++i) {
      const double s = 1.0 / (1.0 + grid(m) * mu(i));
      tr  += s;
      rss += (1.0 - s) * (1.0 - s) * z2(i);
    }
    const double den = 1.0 - tr / T;
    out(m) = rss / T / (den * den);
  }
  return out;
}
