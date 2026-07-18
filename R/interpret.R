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
    if (m$type %in% c("level_prop", "pvalue")) {
      out <- c(out, interpret_categorical(tab, stat, m, grp_vars))
      next
    }
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

# \u7b49\u7ea7\u5360\u6bd4 / \u9898\u76ee\u6b63\u7b54\u7387\u7684\u89e3\u8bfb\u89c4\u5219(v0.2)
interpret_categorical <- function(tab, stat, m, grp_vars) {
  long <- tab$long
  d <- long[long$statistic == stat & !is.na(long$category), , drop = FALSE]
  if (length(grp_vars) > 0) {
    not_total <- rowSums(sapply(grp_vars, function(g) {
      d[[g]] == TOTAL_LABEL
    }, simplify = "matrix")) == 0
    total_only <- !not_total
  } else {
    not_total <- rep(TRUE, nrow(d))
    total_only <- rep(TRUE, nrow(d))
  }
  d_all <- d[if (any(total_only)) total_only else not_total, , drop = FALSE]
  d_grp <- d[not_total, , drop = FALSE]
  out <- character(0)
  pct <- function(v) paste0(sprintf("%.1f", 100 * v), "%")

  if (m$type == "level_prop") {
    # \u603b\u4f53\u4e3b\u5bfc\u7b49\u7ea7
    agg <- tapply(d_all$estimate, d_all$category, mean, na.rm = TRUE)
    if (length(agg) > 0) {
      top <- names(agg)[which.max(agg)]
      out <- c(out, paste0(
        "[", stat, "] \u603b\u4f53\u4e0a\u5360\u6bd4\u6700\u9ad8\u7684\u7b49\u7ea7\u662f\"", top,
        "\"(", pct(max(agg, na.rm = TRUE)), ")\u3002"
      ))
    }
    # \u6700\u9ad8\u7b49\u7ea7(\u6700\u540e\u4e00\u4e2a category)\u5728\u7ec4\u95f4\u7684\u5dee\u5f02
    if (length(grp_vars) > 0 && nrow(d_grp) > 0) {
      last_lvl <- utils::tail(unique(d$category), 1)
      dg <- d_grp[d_grp$category == last_lvl & !is.na(d_grp$estimate), ,
                  drop = FALSE]
      if (nrow(dg) >= 2) {
        lab <- apply(dg[, grp_vars, drop = FALSE], 1, paste, collapse = "-")
        i_max <- which.max(dg$estimate)
        i_min <- which.min(dg$estimate)
        out <- c(out, paste0(
          "[", stat, "] \"", last_lvl, "\"\u7b49\u7ea7\u5360\u6bd4\u6700\u9ad8\u7684\u662f ", lab[i_max],
          "(", pct(dg$estimate[i_max]), "),\u6700\u4f4e\u7684\u662f ", lab[i_min],
          "(", pct(dg$estimate[i_min]), ")\u3002"
        ))
      }
    }
  } else { # pvalue
    agg <- tapply(d_all$estimate, d_all$category, mean, na.rm = TRUE)
    if (length(agg) >= 2) {
      easiest <- names(agg)[which.max(agg)]
      hardest <- names(agg)[which.min(agg)]
      out <- c(out, paste0(
        "[", stat, "] \u6b63\u7b54\u7387\u6700\u9ad8\u7684\u9898\u76ee\u662f ", easiest,
        "(", pct(max(agg, na.rm = TRUE)), "),\u6700\u4f4e\u7684\u662f ", hardest,
        "(", pct(min(agg, na.rm = TRUE)), ")\u3002"
      ))
      low <- agg[agg < 0.2]
      if (length(low) > 0) {
        out <- c(out, paste0(
          "[", stat, "] \u6ce8\u610f: ", paste(names(low), collapse = "\u3001"),
          " \u7684\u6b63\u7b54\u7387\u4f4e\u4e8e 20%,\u5efa\u8bae\u68c0\u67e5\u9898\u76ee\u8d28\u91cf\u6216\u6559\u5b66\u8986\u76d6\u3002"
        ))
      }
    }
  }
  out
}
