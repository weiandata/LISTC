# Variance engine: total variance = sampling + measurement (design doc 6).
# All functions are pure vector functions, vectorized for data.table groups.

#' Sampling variance of a weighted mean
#'
#' Linearized estimator: `sum(w^2 * (x - xbar)^2) / sum(w)^2`.
#' @param x Numeric values.
#' @param w Weights.
#' @return Sampling variance of the weighted mean.
#' @keywords internal
var_sampling_mean <- function(x, w) {
  m <- wmean(x, w)
  sum(w^2 * (x - m)^2) / sum(w)^2
}

#' Measurement variance of a weighted mean of theta
#'
#' Delta-method propagation of individual IRT standard errors:
#' `sum(w^2 * se^2) / sum(w)^2`.
#' @param w Weights.
#' @param se Individual IRT standard errors.
#' @return Measurement variance component.
#' @keywords internal
var_measurement_mean <- function(w, se) {
  sum(w^2 * se^2) / sum(w)^2
}

#' Measurement variance of a probabilistic proportion above a cutoff
#'
#' `p_i = 1 - pnorm((c - theta_i)/se_i)`;
#' `(dp/dtheta)^2 * se^2 = dnorm(z)^2`, hence
#' `sum(w^2 * dnorm(z)^2) / sum(w)^2`.
#' @param theta Ability estimates.
#' @param se Individual IRT standard errors.
#' @param cutoff Cut score.
#' @param w Weights.
#' @return Measurement variance component.
#' @keywords internal
var_measurement_prop <- function(theta, se, cutoff, w) {
  z <- (cutoff - theta) / se
  sum(w^2 * stats::dnorm(z)^2) / sum(w)^2
}

#' Measurement variance of a probabilistic level proportion
#'
#' Level k spans (lower, upper];
#' `dp/dtheta * se = dnorm(z_lower) - dnorm(z_upper)`.
#' @param theta Ability estimates.
#' @param se Individual standard errors.
#' @param lower,upper Level boundaries.
#' @param w Weights.
#' @return Measurement variance component.
#' @keywords internal
var_measurement_level <- function(theta, se, lower, upper, w) {
  dl <- if (is.finite(lower)) stats::dnorm((lower - theta) / se) else 0
  du <- if (is.finite(upper)) stats::dnorm((upper - theta) / se) else 0
  sum(w^2 * (dl - du)^2) / sum(w)^2
}

#' Empirical-Bayes posterior transform for WLE/ML estimates
#'
#' `correction = "latent"` (design doc 4.1). Probabilistic classification
#' (`method = "prob"`) is calibrated when `theta` is an EAP estimate and
#' `se` its posterior SD - no correction needed (verified by simulation).
#' For unbiased WLE/ML estimates with sampling SEs, the naive normal
#' probability overstates the spread of the latent distribution; this
#' transform shrinks each estimate to its normal-model posterior first:
#' reliability `rho = 1 - mean_w(se^2)/var_w(theta)`, then
#' `theta* = mu + rho*(theta - mu)` and `se* = sqrt(rho)*se`.
#'
#' @param theta WLE/ML ability estimates.
#' @param se Individual sampling standard errors.
#' @param w Weights.
#' @param rho Optional reliability; estimated from data when `NULL`.
#' @return List with posterior `theta`, `se`, and the `rho` used.
#' @keywords internal
latent_posterior <- function(theta, se, w, rho = NULL) {
  ok <- stats::complete.cases(theta, se, w)
  if (is.null(rho)) {
    vt <- {
      m <- wmean(theta[ok], w[ok])
      sum(w[ok] * (theta[ok] - m)^2) / sum(w[ok])
    }
    err <- sum(w[ok] * se[ok]^2) / sum(w[ok])
    rho <- 1 - err / vt
    if (!is.finite(rho) || rho <= 0.05) {
      rlang::abort(paste0(
        "\u7531\u6570\u636e\u4f30\u8ba1\u7684\u53ef\u9760\u6027 rho = ", round(rho, 3),
        " \u8fc7\u4f4e,\u65e0\u6cd5\u505a latent \u6821\u6b63;\u8bf7\u68c0\u67e5 theta/theta_se,",
        "\u6216\u663e\u5f0f\u63d0\u4f9b rho\u3002"
      ))
    }
    rho <- min(rho, 1)
  }
  mu <- wmean(theta[ok], w[ok])
  list(
    theta = mu + rho * (theta - mu),
    se = sqrt(rho) * se,
    rho = rho
  )
}
