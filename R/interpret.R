#' Rule-based plain-language interpretation of a listr_table
#'
#' Generates descriptive conclusions from templated rules (no LLM):
#' highest/lowest groups, significant differences (difference > 2 x
#' combined SE), and small-sample warnings (n < 30). v0.1 covers scalar
#' statistics (mean, sd, prop_above); level/item statistics gain rules
#' in v0.2.
#'
#' @param tab A `listr_table`.
#' @param lang Output language; v0.1 supports `"zh"`.
#' @return Character vector of interpretation sentences.
#' @export
lst_interpret <- function(tab, lang = c("zh", "en")) {
  stopifnot(inherits(tab, "listr_table"))
  lang <- match.arg(lang)
  long <- tab$long
  out <- character(0)
  grp_vars <- c(tab$row_vars, tab$col_vars)

  for (stat in tab$values) {
    m <- tab$meta[[stat]]
    if (!m$type %in% c("mean", "sd", "prop_above")) next
    d <- long[long$statistic == stat, , drop = FALSE]
    if (length(grp_vars) > 0) {
      not_total <- rowSums(sapply(grp_vars, function(g) {
        d[[g]] == TOTAL_LABEL
      }, simplify = "matrix")) == 0
      d <- d[not_total, , drop = FALSE]
    }
    d <- d[!is.na(d$estimate), , drop = FALSE]
    if (nrow(d) < 2 || length(grp_vars) == 0) next

    lab <- apply(d[, grp_vars, drop = FALSE], 1, paste, collapse = "-")
    fmt <- function(v) {
      if (isTRUE(m$is_prop)) {
        paste0(sprintf("%.1f", 100 * v), "%")
      } else {
        sprintf("%.2f", v)
      }
    }
    i_max <- which.max(d$estimate)
    i_min <- which.min(d$estimate)
    out <- c(out, paste0(
      "[", stat, "] \u6700\u9ad8: ", lab[i_max], "(", fmt(d$estimate[i_max]),
      ");\u6700\u4f4e: ", lab[i_min], "(", fmt(d$estimate[i_min]), ")\u3002"
    ))
    diff <- d$estimate[i_max] - d$estimate[i_min]
    se_comb <- sqrt(d$se_total[i_max]^2 + d$se_total[i_min]^2)
    if (is.finite(se_comb) && se_comb > 0) {
      if (diff > 2 * se_comb) {
        out <- c(out, paste0(
          "[", stat, "] ", lab[i_max], " \u663e\u8457\u9ad8\u4e8e ", lab[i_min],
          "(\u5dee\u5f02 ", fmt(diff), ",\u8d85\u8fc7 2 \u500d\u5408\u5e76\u6807\u51c6\u8bef)\u3002"
        ))
      } else {
        out <- c(out, paste0(
          "[", stat, "] ", lab[i_max], " \u4e0e ", lab[i_min],
          " \u7684\u5dee\u5f02(", fmt(diff), ")\u672a\u8d85\u8fc7 2 \u500d\u5408\u5e76\u6807\u51c6\u8bef,",
          "\u4e0d\u80fd\u8ba4\u4e3a\u5b58\u5728\u663e\u8457\u5dee\u5f02\u3002"
        ))
      }
    }
    small <- d$n < 30
    if (any(small)) {
      out <- c(out, paste0(
        "[", stat, "] \u6ce8\u610f: ", paste(lab[small], collapse = "\u3001"),
        " \u7684\u6837\u672c\u91cf\u4e0d\u8db3 30,\u7ed3\u679c\u4e0d\u7a33\u5b9a,\u8bf7\u8c28\u614e\u89e3\u8bfb\u3002"
      ))
    }
  }
  if (length(out) == 0) {
    out <- "(\u672c\u8868\u6682\u65e0\u81ea\u52a8\u89e3\u8bfb\u89c4\u5219\u9002\u7528\u7684\u7edf\u8ba1\u91cf\u3002)"
  }
  out
}
