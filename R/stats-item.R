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
#' @export
st_pvalue <- function(items = NULL) {
  new_stat("pvalue", is_prop = TRUE, params = list(items = items))
}

#' Weighted item option distribution (including missing rate)
#'
#' @param items Character vector of item (resp) columns; `NULL` means all
#'   declared resp columns.
#' @param missing_as Label used for missing responses.
#' @return A statistic spec for [lst_table()].
#' @export
st_option_dist <- function(items = NULL, missing_as = "(\u7f3a\u5931)") {
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
