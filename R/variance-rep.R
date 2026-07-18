# Replicate-weights 方差引擎(v0.3,design doc 6)。
# 抽样方差 = factor * sum_r (est_r - est_full)^2;
# 测量方差分量(个体 IRT SE)照旧独立相加。

REP_METHODS <- c("fay", "brr", "jk1", "jk2")

#' Scale factor for replicate-weights variance
#'
#' fay: 1/(R*(1-k)^2) (PISA, k = 0.5); brr: 1/R;
#' jk1: (R-1)/R; jk2: 1 (TIMSS-style paired jackknife).
#' @param method One of "fay", "brr", "jk1", "jk2".
#' @param n_reps Number of replicate weights R.
#' @param fay_k Fay factor (only for `method = "fay"`).
#' @return Scalar variance factor.
#' @keywords internal
rep_factor <- function(method, n_reps, fay_k = 0.5) {
  method <- match.arg(method, REP_METHODS)
  switch(method,
    fay = 1 / (n_reps * (1 - fay_k)^2),
    brr = 1 / n_reps,
    jk1 = (n_reps - 1) / n_reps,
    jk2 = 1
  )
}

# 点估计(仅 estimate,不含方差):replicate 引擎对每个权重列调用一次。
# 返回 data.frame(category, estimate),与 compute_stat 的估计完全一致
# (test 中有一致性断言)。
est_stat <- function(spec, x, w, se = NULL) {
  p <- spec$params
  switch(spec$type,
    mean = data.frame(category = NA_character_, estimate = wmean(x, w),
                      stringsAsFactors = FALSE),
    sd = data.frame(category = NA_character_, estimate = wsd(x, w),
                    stringsAsFactors = FALSE),
    prop_above = {
      est <- if (p$method == "prob") {
        wmean(1 - stats::pnorm((p$cutoff - x) / se), w)
      } else {
        wmean(as.numeric(x >= p$cutoff), w)
      }
      data.frame(category = NA_character_, estimate = est,
                 stringsAsFactors = FALSE)
    },
    level_prop = {
      brk <- p$breaks
      lower <- unname(brk)
      upper <- c(lower[-1], Inf)
      est <- vapply(seq_along(lower), function(k) {
        if (p$method == "prob") {
          pu <- if (is.finite(upper[k])) {
            stats::pnorm((upper[k] - x) / se)
          } else {
            1
          }
          pl <- if (is.finite(lower[k])) {
            stats::pnorm((lower[k] - x) / se)
          } else {
            0
          }
          wmean(pu - pl, w)
        } else {
          wmean(as.numeric(x >= lower[k] & x < upper[k]), w)
        }
      }, numeric(1))
      data.frame(category = names(brk), estimate = est,
                 stringsAsFactors = FALSE)
    },
    quantile = data.frame(
      category = paste0("p", p$probs * 100),
      estimate = wquantile(x, w, p$probs),
      stringsAsFactors = FALSE
    ),
    count = data.frame(category = NA_character_, estimate = length(x),
                       stringsAsFactors = FALSE),
    wcount = data.frame(category = NA_character_, estimate = sum(w),
                        stringsAsFactors = FALSE),
    rlang::abort(paste0("replicate \u5f15\u64ce\u4e0d\u652f\u6301\u7684\u7edf\u8ba1\u91cf\u7c7b\u578b: ", spec$type))
  )
}

# 测量方差分量(与 compute_stat 的公式一致),按 category 对齐。
meas_var <- function(spec, x, w, se) {
  p <- spec$params
  if (is.null(se)) {
    n_out <- if (spec$type == "level_prop") {
      length(p$breaks)
    } else if (spec$type == "quantile") {
      length(p$probs)
    } else {
      1
    }
    return(rep(0, n_out))
  }
  switch(spec$type,
    mean = var_measurement_mean(w, se),
    prop_above = if (p$method == "prob") {
      var_measurement_prop(x, se, p$cutoff, w)
    } else {
      0
    },
    level_prop = {
      brk <- p$breaks
      lower <- unname(brk)
      upper <- c(lower[-1], Inf)
      if (p$method == "prob") {
        vapply(seq_along(lower), function(k) {
          var_measurement_level(x, se, lower[k], upper[k], w)
        }, numeric(1))
      } else {
        rep(0, length(lower))
      }
    },
    quantile = rep(0, length(p$probs)),
    0
  )
}

# replicate 路径的组内计算:wmat 第 1 列为主权重,其余为复制权重。
compute_stat_rep <- function(spec, x, wmat, se = NULL, factor = 1) {
  wmat <- as.matrix(wmat)
  n <- length(x)
  w0 <- wmat[, 1]
  est0 <- est_stat(spec, x, w0, se)
  n_reps <- ncol(wmat) - 1
  if (n_reps < 1) {
    rlang::abort("replicate \u5f15\u64ce\u9700\u8981\u81f3\u5c11 1 \u4e2a\u590d\u5236\u6743\u91cd\u5217\u3002")
  }
  reps <- vapply(seq_len(n_reps), function(r) {
    est_stat(spec, x, wmat[, r + 1], se)$estimate
  }, numeric(nrow(est0)))
  if (nrow(est0) == 1) {
    reps <- matrix(reps, nrow = 1)
  }
  vs <- factor * rowSums((reps - est0$estimate)^2)
  vm <- meas_var(spec, x, w0, se)
  data.frame(
    category = est0$category,
    estimate = est0$estimate,
    se_sampling = sqrt(vs),
    se_measurement = sqrt(vm),
    se_total = sqrt(vs + vm),
    n = n,
    sum_w = sum(w0),
    stringsAsFactors = FALSE
  )
}
