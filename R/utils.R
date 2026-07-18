# Internal helpers -----------------------------------------------------------

# Map每个列名到它自身的字符串,使 c(region, gender) / c(math = th_math)
# 这类裸名表达式可以在 eval_tidy 中直接解析为字符向量。
col_env <- function(data) {
  nms <- names(data)
  as.list(stats::setNames(as.list(nms), nms))
}

# 把 quosure(裸名、c(...)、字符向量)解析为(可命名的)字符向量。
resolve_vars <- function(quo, data, what = "\u53d8\u91cf") {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  out <- tryCatch(
    rlang::eval_tidy(quo, data = col_env(data)),
    error = function(e) {
      rlang::abort(paste0(
        what, " \u4e2d\u5f15\u7528\u4e86\u6570\u636e\u91cc\u4e0d\u5b58\u5728\u7684\u5217: ",
        conditionMessage(e)
      ))
    }
  )
  if (is.null(out)) {
    return(NULL)
  }
  out <- as.character(out) |> stats::setNames(names(out))
  missing <- setdiff(unname(out), names(data))
  if (length(missing) > 0) {
    rlang::abort(paste0(
      what, " \u4e2d\u7684\u5217\u5728\u6570\u636e\u91cc\u4e0d\u5b58\u5728: ",
      paste(missing, collapse = ", ")
    ))
  }
  out
}

# 加权均值/方差 ---------------------------------------------------------------

wmean <- function(x, w) sum(w * x) / sum(w)

# 加权样本标准差(总体式定义)
wsd <- function(x, w) {
  m <- wmean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

# 加权分位数(累计权重线性插值)
wquantile <- function(x, w, probs) {
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) - 0.5 * w
  cw <- cw / sum(w)
  vapply(probs, function(p) {
    stats::approx(cw, x, xout = p, rule = 2, ties = "ordered")$y
  }, numeric(1))
}

# 单元格格式化 ----------------------------------------------------------------

fmt_num <- function(v, digits, percent = FALSE) {
  ifelse(
    is.na(v), "",
    if (percent) {
      sprintf(paste0("%.", digits, "f%%"), 100 * v)
    } else {
      sprintf(paste0("%.", digits, "f"), v)
    }
  )
}

format_cell <- function(est, se, format, digits, percent = FALSE) {
  e <- fmt_num(est, digits, percent)
  s <- fmt_num(se, digits, percent)
  switch(format,
    est = e,
    est_se = ifelse(s == "", e, paste0(e, " (", s, ")")),
    est_ci = {
      lo <- fmt_num(est - 1.96 * se, digits, percent)
      hi <- fmt_num(est + 1.96 * se, digits, percent)
      ifelse(s == "", e, paste0(e, " [", lo, ", ", hi, "]"))
    },
    percent = ifelse(s == "", e, paste0(e, " (", s, ")")),
    rlang::abort(paste0("\u672a\u77e5\u7684 format: ", format))
  )
}

# 中文感知的显示宽度(全角计 2)
display_width <- function(x) {
  nchar(as.character(x), type = "width")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

TOTAL_LABEL <- "\u5408\u8ba1" # "合计"
