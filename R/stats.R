# Statistic constructors used inside lst_table(values = ...).
# Each returns a "listr_stat" spec; the table engine resolves the variable
# and calls compute_stat() per group. Every statistic yields
# estimate + se_sampling + se_measurement + se_total (design doc 5-6).

new_stat <- function(type, var_quo = NULL, is_prop = FALSE, params = list()) {
  structure(
    list(type = type, var_quo = var_quo, is_prop = is_prop, params = params),
    class = "listr_stat"
  )
}

#' Weighted mean statistic
#' @param var Measure: a theta/score dimension name or a numeric column.
#' @return A statistic spec for [lst_table()].
#' @export
st_mean <- function(var) {
  new_stat("mean", rlang::enquo(var))
}

#' Weighted standard deviation
#' @inheritParams st_mean
#' @return A statistic spec for [lst_table()].
#' @export
st_sd <- function(var) {
  new_stat("sd", rlang::enquo(var))
}

#' Proportion above a cutoff
#' @inheritParams st_mean
#' @param cutoff Numeric threshold.
#' @param method `"hard"` (0/1 classification) or `"prob"`
#'   (probabilistic, uses each person's theta SE; design doc 5.2).
#' @param correction `"none"` (correct for EAP estimates with posterior
#'   SDs) or `"latent"` (empirical-Bayes shrinkage for WLE/ML estimates
#'   with sampling SEs; design doc 4.1). Only relevant when
#'   `method = "prob"`.
#' @param rho Optional reliability for `"latent"`; estimated from the
#'   data when `NULL`.
#' @return A statistic spec for [lst_table()].
#' @export
st_prop_above <- function(var, cutoff, method = c("hard", "prob"),
                          correction = c("none", "latent"), rho = NULL) {
  new_stat("prop_above", rlang::enquo(var), is_prop = TRUE,
           params = list(cutoff = cutoff, method = match.arg(method),
                         correction = match.arg(correction), rho = rho))
}

#' Proportions in proficiency levels
#' @inheritParams st_prop_above
#' @param breaks Named numeric vector: level name -> lower bound,
#'   e.g. `c(L1 = -Inf, L2 = 0.5, L3 = 1.2)`.
#' @return A statistic spec for [lst_table()].
#' @export
st_level_prop <- function(var, breaks, method = c("hard", "prob"),
                          correction = c("none", "latent"), rho = NULL) {
  if (is.null(names(breaks)) || any(names(breaks) == "")) {
    rlang::abort("breaks \u5fc5\u987b\u662f\u547d\u540d\u5411\u91cf:\u540d\u79f0\u662f\u7b49\u7ea7\u540d,\u503c\u662f\u8be5\u7b49\u7ea7\u4e0b\u754c\u3002")
  }
  if (is.unsorted(breaks)) {
    rlang::abort("breaks \u5fc5\u987b\u6309\u4e0b\u754c\u4ece\u4f4e\u5230\u9ad8\u6392\u5217\u3002")
  }
  new_stat("level_prop", rlang::enquo(var), is_prop = TRUE,
           params = list(breaks = breaks, method = match.arg(method),
                         correction = match.arg(correction), rho = rho))
}

#' Weighted quantile statistic
#'
#' Point estimates only in v0.1 (SE reported as NA; planned for v0.2).
#' @inheritParams st_mean
#' @param probs Quantile probabilities.
#' @return A statistic spec for [lst_table()].
#' @export
st_quantile <- function(var, probs = 0.5) {
  new_stat("quantile", rlang::enquo(var), params = list(probs = probs))
}

#' Count and weighted-count statistics
#' @return A statistic spec for [lst_table()].
#' @export
st_count <- function() new_stat("count")

#' @rdname st_count
#' @export
st_wcount <- function() new_stat("wcount")

# 组内计算 --------------------------------------------------------------------
# 输入均为已去除 NA 的向量;返回 data.frame(category, estimate,
# se_sampling, se_measurement, se_total, n, sum_w)。

stat_row <- function(category, est, vs, vm, n, sum_w) {
  data.frame(
    category = category,
    estimate = est,
    se_sampling = sqrt(vs),
    se_measurement = sqrt(vm),
    se_total = sqrt(vs + vm),
    n = n,
    sum_w = sum_w,
    stringsAsFactors = FALSE
  )
}

compute_stat <- function(spec, x, w, se = NULL) {
  n <- length(x)
  sw <- sum(w)
  if (n == 0) {
    return(stat_row(NA_character_, NA_real_, NA_real_, NA_real_, 0, 0))
  }
  p <- spec$params
  switch(spec$type,
    mean = {
      vs <- var_sampling_mean(x, w)
      vm <- if (!is.null(se)) var_measurement_mean(w, se) else 0
      stat_row(NA_character_, wmean(x, w), vs, vm, n, sw)
    },
    sd = {
      s <- wsd(x, w)
      m <- wmean(x, w)
      v_var <- sum(w^2 * ((x - m)^2 - s^2)^2) / sum(w)^2
      vs <- if (s > 0) v_var / (4 * s^2) else 0
      stat_row(NA_character_, s, vs, 0, n, sw)
    },
    prop_above = {
      if (p$method == "prob") {
        if (is.null(se)) {
          rlang::abort("method = \"prob\" \u9700\u8981\u8be5\u53d8\u91cf\u5177\u6709\u914d\u5bf9\u7684\u6807\u51c6\u8bef\u5217\u3002")
        }
        pi <- 1 - stats::pnorm((p$cutoff - x) / se)
        vs <- var_sampling_mean(pi, w)
        vm <- var_measurement_prop(x, se, p$cutoff, w)
        stat_row(NA_character_, wmean(pi, w), vs, vm, n, sw)
      } else {
        ind <- as.numeric(x >= p$cutoff) # 达到分数线计入
        stat_row(NA_character_, wmean(ind, w),
                 var_sampling_mean(ind, w), 0, n, sw)
      }
    },
    level_prop = {
      brk <- p$breaks
      lower <- unname(brk)
      upper <- c(lower[-1], Inf)
      lvls <- names(brk)
      out <- vector("list", length(lvls))
      for (k in seq_along(lvls)) {
        if (p$method == "prob") {
          if (is.null(se)) {
            rlang::abort("method = \"prob\" \u9700\u8981\u8be5\u53d8\u91cf\u5177\u6709\u914d\u5bf9\u7684\u6807\u51c6\u8bef\u5217\u3002")
          }
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
          pik <- pu - pl
          vs <- var_sampling_mean(pik, w)
          vm <- var_measurement_level(x, se, lower[k], upper[k], w)
          out[[k]] <- stat_row(lvls[k], wmean(pik, w), vs, vm, n, sw)
        } else {
          # 达到下界计入本等级: lower <= x < upper
          ind <- as.numeric(x >= lower[k] & x < upper[k])
          out[[k]] <- stat_row(lvls[k], wmean(ind, w),
                               var_sampling_mean(ind, w), 0, n, sw)
        }
      }
      do.call(rbind, out)
    },
    quantile = {
      qs <- wquantile(x, w, p$probs)
      do.call(rbind, lapply(seq_along(p$probs), function(i) {
        stat_row(paste0("p", p$probs[i] * 100), qs[i], NA_real_, NA_real_,
                 n, sw)
      }))
    },
    count = stat_row(NA_character_, n, 0, 0, n, sw),
    wcount = stat_row(NA_character_, sw, 0, 0, n, sw),
    rlang::abort(paste0("\u672a\u77e5\u7684\u7edf\u8ba1\u91cf\u7c7b\u578b: ", spec$type))
  )
}
