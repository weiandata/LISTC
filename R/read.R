#' Read sample data from common file formats
#'
#' Dispatches on file extension: csv/tsv/txt (`data.table::fread`),
#' xlsx/xls (readxl), sav/zsav/dta/sas7bdat (haven, value labels are
#' converted to factors). Use `col_select` to load only needed columns
#' (design doc 9.1).
#'
#' @param path File path.
#' @param col_select Optional character vector of columns to read.
#' @param ... Passed to the backend reader.
#' @return A tibble.
#' @examples
#' f <- tempfile(fileext = ".csv")
#' write.csv(data.frame(id = 1:3, score = c(10, 12, 9)), f,
#'           row.names = FALSE)
#' read_listc(f)
#' read_listc(f, col_select = "score")
#' @export
read_listc <- function(path, col_select = NULL, ...) {
  if (!file.exists(path)) {
    rlang::abort(paste0("\u627e\u4e0d\u5230\u6570\u636e\u6587\u4ef6: ", path))
  }
  ext <- tolower(tools::file_ext(path))
  out <- switch(ext,
    csv = ,
    tsv = ,
    txt = {
      d <- data.table::fread(path, select = col_select, ...)
      tibble::as_tibble(d)
    },
    xlsx = ,
    xls = {
      d <- readxl::read_excel(path, ...)
      if (nrow(d) > 5e5) {
        rlang::inform(paste0(
          "\u8be5 Excel \u6587\u4ef6\u6709 ", nrow(d),
          " \u884c;\u5927\u6587\u4ef6\u5efa\u8bae\u8f6c\u5b58\u4e3a csv \u4ee5\u83b7\u5f97\u66f4\u5feb\u7684\u8bfb\u53d6\u901f\u5ea6\u3002"
        ))
      }
      if (!is.null(col_select)) d <- d[, intersect(col_select, names(d))]
      d
    },
    sav = ,
    zsav = read_haven(haven::read_sav, path, col_select, ...),
    dta = read_haven(haven::read_dta, path, col_select, ...),
    sas7bdat = read_haven(haven::read_sas, path, col_select, ...),
    rlang::abort(paste0(
      "\u6682\u4e0d\u652f\u6301\u7684\u6587\u4ef6\u683c\u5f0f: .", ext,
      "(\u652f\u6301 csv/tsv/txt/xlsx/xls/sav/zsav/dta/sas7bdat)"
    ))
  )
  tibble::as_tibble(out)
}

read_haven <- function(fn, path, col_select, ...) {
  d <- if (is.null(col_select)) {
    fn(path, ...)
  } else {
    fn(path, col_select = dplyr_all_of(col_select), ...)
  }
  # 值标签 -> factor,保留显示标签
  is_lab <- vapply(d, inherits, logical(1), "haven_labelled")
  d[is_lab] <- lapply(d[is_lab], haven::as_factor, levels = "labels")
  d
}

# haven 的 col_select 走 tidyselect;字符向量直接传入即可
dplyr_all_of <- function(x) x
