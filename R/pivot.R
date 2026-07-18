#' Extract the tidy long form of a listr_table
#'
#' One row per group combination x statistic x category, with columns
#' estimate, se_sampling, se_measurement, se_total, n, sum_w.
#' @param tab A `listr_table`.
#' @return A tibble.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' tab <- lst_table(x, rows = region, values = list(mean = st_mean(math)))
#' as_long(tab)
#' @export
as_long <- function(tab) {
  stopifnot(inherits(tab, "listr_table"))
  tab$long
}

#' Extract the wide (row x column layout) form of a listr_table
#'
#' Cells are formatted according to the table's `format` and `digits`;
#' proportion-type statistics are shown as percentages.
#' @param tab A `listr_table`.
#' @return A tibble laid out as declared in [lst_table()].
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' tab <- lst_table(x, rows = region, values = list(mean = st_mean(math)))
#' as_wide(tab)
#' @export
as_wide <- function(tab) {
  stopifnot(inherits(tab, "listr_table"))
  df <- as.data.frame(tab$long, stringsAsFactors = FALSE)
  digits <- tab$digits

  dig_for <- function(stat, meta) {
    if (meta$type %in% c("count", "wcount")) {
      return(0L)
    }
    if (length(digits) > 1 && !is.null(names(digits)) &&
        stat %in% names(digits)) {
      return(as.integer(digits[[stat]]))
    }
    if (length(digits) == 1) as.integer(digits) else 2L
  }

  # 逐统计量格式化单元格
  df$.cell <- ""
  for (nm in tab$values) {
    idx <- df$statistic == nm
    m <- tab$meta[[nm]]
    fmt <- if (m$type %in% c("count", "wcount")) "est" else tab$format
    df$.cell[idx] <- format_cell(df$estimate[idx], df$se_total[idx],
                                 fmt, dig_for(nm, m),
                                 percent = isTRUE(m$is_prop))
  }

  # 列键:列分组变量值 + 统计量名 + category
  ckey_parts <- c(tab$col_vars, "statistic",
                  if ("category" %in% names(df) &&
                        any(!is.na(df$category))) "category")
  parts <- lapply(ckey_parts, function(cn) as.character(df[[cn]]))
  keys <- do.call(paste, c(parts, list(sep = "_")))
  keys <- gsub("_NA$", "", keys)
  df$.colkey <- keys

  # 列顺序:统计量声明序 -> 合计列最后 -> 出现序
  is_total_col <- if (length(tab$col_vars) > 0) {
    Reduce(`|`, lapply(tab$col_vars, function(cv) {
      df[[cv]] == TOTAL_LABEL
    }))
  } else {
    rep(FALSE, nrow(df))
  }
  ord <- order(match(df$statistic, tab$values), is_total_col)
  col_levels <- unique(df$.colkey[ord])
  df$.colkey <- factor(df$.colkey, levels = col_levels)

  if (length(tab$row_vars) == 0) {
    df$.lstrow <- "\u603b\u4f53"
    row_lhs <- ".lstrow"
  } else {
    row_lhs <- tab$row_vars
  }
  dt <- data.table::as.data.table(
    df[, c(row_lhs, ".colkey", ".cell"), drop = FALSE]
  )
  f <- stats::as.formula(paste(
    paste(paste0("`", row_lhs, "`"), collapse = " + "),
    "~ .colkey"
  ))
  wide <- data.table::dcast(dt, f, value.var = ".cell",
                            fun.aggregate = function(v) v[1], fill = "")
  if (length(tab$row_vars) == 0) {
    data.table::setnames(wide, ".lstrow", " ")
  } else {
    # 合计行排到最后
    is_total_row <- Reduce(`|`, lapply(tab$row_vars, function(rv) {
      wide[[rv]] == TOTAL_LABEL
    }))
    wide <- wide[order(is_total_row)]
  }
  tibble::as_tibble(wide)
}
