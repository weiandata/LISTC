#' Build a pivot-style statistical table
#'
#' Excel-pivot-like interface: declare row variables, column variables and
#' cell statistics; every cell carries estimate and SE components
#' (sampling + measurement, design doc 6).
#'
#' @param x A `listr_data` object.
#' @param rows Row grouping variables (bare names or character vector).
#' @param cols Column grouping variables.
#' @param values Named list of statistic specs (`st_*` functions).
#' @param format Cell display: `"est"`, `"est_se"`, `"est_ci"`, `"percent"`.
#' @param digits Rounding digits (single value, or named per-statistic).
#' @param margins Add "Total" row/column margins.
#' @return A `listr_table` object; see [as_long()] and [as_wide()].
#' @export
lst_table <- function(x, rows = NULL, cols = NULL, values,
                      format = "est_se", digits = 2, margins = FALSE) {
  if (!inherits(x, "listr_data")) {
    rlang::abort("x \u5fc5\u987b\u662f lst_data() \u521b\u5efa\u7684 listr_data \u5bf9\u8c61\u3002")
  }
  if (!is.list(values) || length(values) == 0 ||
      is.null(names(values)) || any(names(values) == "")) {
    rlang::abort("values \u5fc5\u987b\u662f\u547d\u540d\u5217\u8868,\u5982 list(\u5747\u503c = st_mean(math))\u3002")
  }
  for (v in values) {
    if (!inherits(v, "listr_stat")) {
      rlang::abort("values \u7684\u6bcf\u4e2a\u5143\u7d20\u5fc5\u987b\u7531 st_*() \u51fd\u6570\u521b\u5efa\u3002")
    }
  }
  format <- match.arg(format, c("est", "est_se", "est_ci", "percent"))

  row_vars <- resolve_vars(rlang::enquo(rows), x$data, "rows")
  col_vars <- resolve_vars(rlang::enquo(cols), x$data, "cols")
  grp_vars <- c(unname(row_vars), unname(col_vars))

  dt <- data.table::as.data.table(x$data)
  dt[, .lstw := get_weights(x)]
  if (length(grp_vars) > 0) {
    dt[, (grp_vars) := lapply(.SD, as.character), .SDcols = grp_vars]
  }

  # replicate weights 引擎(v0.3):抽样方差改用复制权重法
  repw <- NULL
  if (!is.null(x$roles$rep_weights)) {
    repw <- list(
      cols = x$roles$rep_weights,
      factor = rep_factor(x$rep_method, length(x$roles$rep_weights),
                          x$fay_k %||% 0.5),
      method = x$rep_method
    )
  }

  # grouping sets: 主体 + margins
  sets <- list(grp_vars)
  if (margins && length(grp_vars) > 0) {
    if (length(row_vars) > 0 && length(col_vars) > 0) {
      sets <- c(sets, list(unname(row_vars)), list(unname(col_vars)))
    }
    sets <- c(sets, list(character(0)))
  }

  meta <- list()
  parts <- vector("list", length(values))
  for (i in seq_along(values)) {
    nm <- names(values)[i]
    spec <- values[[i]]
    if (spec$type %in% c("pvalue", "option_dist")) {
      res <- compute_item_stat(x, dt, spec, sets, grp_vars, repw)
      meta[[nm]] <- list(type = spec$type, is_prop = TRUE,
                         variance = if (is.null(repw)) {
                           "linearized"
                         } else {
                           paste0("replicate:", repw$method)
                         })
    } else {
      prep <- prepare_measure(x, dt, spec)
      if (!is.null(prep$pvcols)) {
        res <- compute_grouped_pv(dt, spec, sets, grp_vars, prep$pvcols,
                                  repw, x$pv_sampling %||% "first")
        meta[[nm]] <- list(
          type = spec$type, var = prep$var, is_prop = spec$is_prop,
          method = spec$params$method,
          n_pv = length(prep$pvcols),
          pv_sampling = x$pv_sampling %||% "first",
          variance = paste0(
            "rubin+",
            if (is.null(repw)) "linearized" else paste0("replicate:", repw$method)
          )
        )
      } else {
        res <- compute_grouped(prep$dt, spec, sets, grp_vars,
                               prep$xcol, prep$secol, repw)
        meta[[nm]] <- list(
          type = spec$type, var = prep$var, is_prop = spec$is_prop,
          method = spec$params$method, correction = spec$params$correction,
          rho = prep$rho,
          variance = if (is.null(repw)) {
            "linearized"
          } else {
            paste0("replicate:", repw$method)
          }
        )
      }
    }
    res[, statistic := nm]
    parts[[i]] <- res
  }
  long <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  ord <- c(grp_vars, "statistic", "category", "estimate", "se_sampling",
           "se_measurement", "se_total", "n", "sum_w")
  data.table::setcolorder(long, intersect(ord, names(long)))

  structure(
    list(
      long = tibble::as_tibble(long),
      row_vars = unname(row_vars), col_vars = unname(col_vars),
      values = names(values), meta = meta,
      format = format, digits = digits, margins = margins
    ),
    class = "listr_table"
  )
}

# 解析统计量的变量,并按需做 latent 预变换(全样本 rho,一次完成)
prepare_measure <- function(x, dt, spec) {
  if (is.null(spec$var_quo)) {
    return(list(dt = dt, xcol = NULL, secol = NULL, var = NULL, rho = NULL))
  }
  var <- resolve_stat_var(spec$var_quo, x)
  m <- resolve_measure(x, var)
  if (!is.null(m$pvcols)) {
    # PV 维度:Rubin 合并路径;prob/latent 不适用(PV 已含测量不确定性)
    if (identical(spec$params$method, "prob")) {
      rlang::abort(paste0(
        "PV \u7ef4\u5ea6 \"", var, "\" \u4e0d\u652f\u6301 method = \"prob\":",
        "plausible values \u672c\u8eab\u5df2\u643a\u5e26\u6d4b\u91cf\u4e0d\u786e\u5b9a\u6027,",
        "\u8bf7\u4f7f\u7528 method = \"hard\"(Rubin \u5408\u5e76\u4f1a\u7ed9\u51fa\u6b63\u786e\u7684\u603b\u65b9\u5dee)\u3002"
      ))
    }
    return(list(dt = dt, xcol = NULL, secol = NULL, var = var,
                rho = NULL, pvcols = m$pvcols))
  }
  rho <- NULL
  if (identical(spec$params$correction, "latent") &&
      identical(spec$params$method, "prob")) {
    if (is.null(m$secol)) {
      rlang::abort("latent \u6821\u6b63\u9700\u8981\u8be5\u53d8\u91cf\u5177\u6709\u914d\u5bf9\u7684\u6807\u51c6\u8bef\u5217\u3002")
    }
    un <- latent_posterior(dt[[m$xcol]], dt[[m$secol]], dt[[".lstw"]],
                           rho = spec$params$rho)
    dt <- data.table::copy(dt)
    dt[, c(m$xcol, m$secol) := list(un$theta, un$se)]
    rho <- un$rho
  }
  list(dt = dt, xcol = m$xcol, secol = m$secol, var = var, rho = rho)
}

# 统计量 var 的解析:先按维度名,再退回数据列裸名/字符串
resolve_stat_var <- function(quo, x) {
  dims <- c(names(x$roles$pv), names(x$roles$theta), names(x$roles$score))
  env <- c(
    stats::setNames(as.list(dims), dims),
    col_env(x$data)
  )
  env <- env[!duplicated(names(env))] # 维度名优先,去重以满足数据掩码要求
  out <- tryCatch(
    rlang::eval_tidy(quo, data = env),
    error = function(e) {
      rlang::abort(paste0("\u65e0\u6cd5\u89e3\u6790\u7edf\u8ba1\u91cf\u7684\u53d8\u91cf: ", conditionMessage(e)))
    }
  )
  as.character(out)[1]
}

# data.table 分组计算 + margins 组合
# rep 非 NULL 时走 replicate weights 路径(v0.3):
# list(cols = 复制权重列, factor = 方差因子, method = 方法名)
compute_grouped <- function(dt, spec, sets, grp_vars, xcol, secol,
                            repw = NULL) {
  keep <- c(xcol, secol, ".lstw", repw$cols)
  sub <- dt[stats::complete.cases(dt[, keep, with = FALSE])]
  wcols <- c(".lstw", repw$cols)
  out <- vector("list", length(sets))
  for (s in seq_along(sets)) {
    by <- sets[[s]]
    res <- if (!is.null(repw)) {
      xx <- if (is.null(xcol)) NULL else xcol
      if (length(by) > 0) {
        if (is.null(xx)) {
          sub[, compute_stat_rep(spec, rep(0, .N), .SD, NULL, repw$factor),
              by = by, .SDcols = wcols]
        } else if (is.null(secol)) {
          sub[, compute_stat_rep(spec, get(xx), .SD, NULL, repw$factor),
              by = by, .SDcols = wcols]
        } else {
          sub[, compute_stat_rep(spec, get(xx), .SD, get(secol),
                                 repw$factor),
              by = by, .SDcols = wcols]
        }
      } else {
        if (is.null(xx)) {
          sub[, compute_stat_rep(spec, rep(0, .N), .SD, NULL, repw$factor),
              .SDcols = wcols]
        } else if (is.null(secol)) {
          sub[, compute_stat_rep(spec, get(xx), .SD, NULL, repw$factor),
              .SDcols = wcols]
        } else {
          sub[, compute_stat_rep(spec, get(xx), .SD, get(secol),
                                 repw$factor),
              .SDcols = wcols]
        }
      }
    } else if (is.null(xcol)) {
      if (length(by) > 0) {
        sub[, compute_stat(spec, rep(0, .N), .lstw), by = by]
      } else {
        sub[, compute_stat(spec, rep(0, .N), .lstw)]
      }
    } else if (is.null(secol)) {
      if (length(by) > 0) {
        sub[, compute_stat(spec, get(xcol), .lstw), by = by]
      } else {
        sub[, compute_stat(spec, get(xcol), .lstw)]
      }
    } else {
      if (length(by) > 0) {
        sub[, compute_stat(spec, get(xcol), .lstw, get(secol)), by = by]
      } else {
        sub[, compute_stat(spec, get(xcol), .lstw, get(secol))]
      }
    }
    # 无 by 时 j 返回 data.frame,转回 data.table 再补 margins 标签
    res <- data.table::as.data.table(res)
    for (mv in setdiff(grp_vars, by)) {
      res[, (mv) := TOTAL_LABEL]
    }
    out[[s]] <- res
  }
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

# PV/Rubin 路径的分组计算(v0.4)
compute_grouped_pv <- function(dt, spec, sets, grp_vars, pvcols, repw,
                               pv_sampling) {
  keep <- c(pvcols, ".lstw", repw$cols)
  sub <- dt[stats::complete.cases(dt[, keep, with = FALSE])]
  npv <- length(pvcols)
  factor <- repw$factor
  out <- vector("list", length(sets))
  for (s in seq_along(sets)) {
    by <- sets[[s]]
    res <- if (length(by) > 0) {
      sub[, compute_stat_pv(spec, .SD, npv, factor, pv_sampling),
          by = by, .SDcols = keep]
    } else {
      sub[, compute_stat_pv(spec, .SD, npv, factor, pv_sampling),
          .SDcols = keep]
    }
    res <- data.table::as.data.table(res)
    for (mv in setdiff(grp_vars, by)) {
      res[, (mv) := TOTAL_LABEL]
    }
    out[[s]] <- res
  }
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

# 题目层统计:按题分块,峰值内存与题数解耦(design doc 9.1)
compute_item_stat <- function(x, dt, spec, sets, grp_vars, repw = NULL) {
  items <- spec$params$items %||% unname(x$roles$resp)
  if (is.null(items)) {
    rlang::abort("\u672a\u58f0\u660e resp \u4f5c\u7b54\u5217,\u65e0\u6cd5\u8ba1\u7b97\u9898\u76ee\u5c42\u7edf\u8ba1\u3002")
  }
  key <- x$key
  parts <- list()
  base_cols <- c(grp_vars, ".lstw", repw$cols)
  for (it in items) {
    if (spec$type == "pvalue") {
      tmp <- dt[, base_cols, with = FALSE]
      tmp[, .lstx := score_item(dt[[it]], key[[it]])]
      inner <- new_stat("mean")
      res <- compute_grouped(tmp, inner, sets, grp_vars, ".lstx", NULL, repw)
      res[, category := it]
    } else { # option_dist
      tmp <- dt[, base_cols, with = FALSE]
      opt <- as.character(dt[[it]])
      opt[is.na(opt)] <- spec$params$missing_as
      tmp[, .lstopt := opt]
      res_list <- list()
      for (s in seq_along(sets)) {
        by <- sets[[s]]
        agg <- tmp[, list(sum_w_opt = sum(.lstw), n_opt = .N),
                   by = c(by, ".lstopt")]
        if (length(by) > 0) {
          agg[, `:=`(sum_w = sum(sum_w_opt), n = sum(n_opt)), by = by]
        } else {
          agg[, `:=`(sum_w = sum(sum_w_opt), n = sum(n_opt))]
        }
        agg[, `:=`(
          estimate = sum_w_opt / sum_w,
          se_sampling = NA_real_, se_measurement = NA_real_,
          se_total = NA_real_
        )]
        agg[, category := paste0(it, ":", .lstopt)]
        agg[, c(".lstopt", "sum_w_opt", "n_opt") := NULL]
        for (mv in setdiff(grp_vars, by)) agg[, (mv) := TOTAL_LABEL]
        res_list[[s]] <- agg
      }
      res <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
    }
    parts[[it]] <- res
  }
  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

#' @export
print.listr_table <- function(x, ...) {
  cat("<listr_table> ",
      paste(x$row_vars, collapse = "+"), " x ",
      paste(x$col_vars, collapse = "+"),
      " | \u7edf\u8ba1\u91cf: ", paste(x$values, collapse = ", "),
      " | format = ", x$format, "\n\n", sep = "")
  print(as.data.frame(as_wide(x)), row.names = FALSE)
  invisible(x)
}
