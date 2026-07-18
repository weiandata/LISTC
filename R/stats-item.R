# Item-level statistics over resp columns (design doc section 5).
# Computed by the table engine with per-item chunking (design doc 9.1);
# items appear in the long result as `category`.

#' Weighted item p-value (proportion correct / mean score rate)
#'
#' Requires a scoring key declared in [lst_data()] (`key =`), a named
#' vector item -> correct answer. Numeric responses already scored 0/1
#' can use `key = NULL` items by declaring them directly.
#'
#' @param items Character vector of item (resp) columns; `NULL` means all
#'   declared resp columns.
#' @return A statistic spec for [lst_table()].
#' @examples
#' d <- data.frame(id = 1:50, grp = rep(c("a", "b"), 25),
#'                 q1 = sample(c("A", "B"), 50, TRUE),
#'                 q2 = rbinom(50, 1, 0.7))
#' x <- lst_data(d, id = id, group = grp, resp = c(q1, q2),
#'               key = list(q1 = "A"))
#' lst_table(x, rows = grp, values = list(pv = st_pvalue()))
#' @export
st_pvalue <- function(items = NULL) {
  new_stat("pvalue", is_prop = TRUE, params = list(items = items))
}

#' Weighted item option distribution (including missing rate)
#'
#' Each option share is the weighted mean of a 0/1 indicator for that
#' option, so it carries a sampling standard error from the same engine
#' as the other statistics: linearized by default, or replicate-based
#' when `rep_weights` are declared. Missing responses form their own
#' category, and within an item the shares of one group sum to 1. As for
#' [st_pvalue()], there is no measurement component for raw item
#' responses, so `se_measurement` is 0 and `se_total` equals
#' `se_sampling`.
#'
#' Cost scales with the number of options: an item with k distinct
#' responses costs about k times a single [st_pvalue()] pass.
#'
#' @param items Character vector of item (resp) columns; `NULL` means all
#'   declared resp columns.
#' @param missing_as Label used for missing responses; `NULL` uses a
#'   built-in default label (Chinese for "missing").
#' @return A statistic spec for [lst_table()].
#' @examples
#' d <- data.frame(id = 1:50,
#'                 q1 = sample(c("A", "B", "C", NA), 50, TRUE))
#' x <- lst_data(d, id = id, resp = q1)
#' lst_table(x, values = list(opts = st_option_dist(items = "q1")))
#' @export
st_option_dist <- function(items = NULL, missing_as = NULL) {
  # \u9ed8\u8ba4\u6807\u7b7e"(\u7f3a\u5931)";NULL \u54e8\u5175\u4fdd\u6301 Rd usage \u6bb5\u4e3a ASCII\u3002
  if (is.null(missing_as)) missing_as <- "(\u7f3a\u5931)"
  new_stat("option_dist", is_prop = TRUE,
           params = list(items = items, missing_as = missing_as))
}

# 按题计分:返回 0/1(或 NA)向量
score_item <- function(resp, key_value) {
  if (is.null(key_value)) {
    # 无计分键:要求本身是 0/1 数值
    v <- suppressWarnings(as.numeric(resp))
    if (!all(v %in% c(0, 1) | is.na(v))) {
      rlang::abort("\u65e0\u8ba1\u5206\u952e\u7684\u9898\u76ee\u5217\u5fc5\u987b\u5df2\u662f 0/1 \u8ba1\u5206\u3002")
    }
    v
  } else {
    ifelse(is.na(resp), NA_real_, as.numeric(resp == key_value))
  }
}
