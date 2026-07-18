# Excel 配置簿:调查人员按模板填写,与 YAML 配置一一对应(design doc 1.1)。
# 模板: inst/templates/config-template.xlsx,含"说明/数据与角色/能力维度/
# 统计表/输出"五个 sheet。

#' Copy the Excel configuration template to a writable location
#'
#' @param path Target path for the template workbook; `NULL` uses a
#'   built-in default file name (Chinese, ending in ".xlsx").
#' @param overwrite Overwrite an existing file.
#' @return `path`, invisibly.
#' @examples
#' f <- file.path(tempdir(), "listc-config.xlsx")
#' lst_config_template(f, overwrite = TRUE)
#' file.exists(f)
#' @export
lst_config_template <- function(path = NULL, overwrite = FALSE) {
  # 默认文件名"LISTC配置.xlsx";NULL 哨兵保持 Rd usage 段为 ASCII。
  if (is.null(path)) path <- "LISTC\u914d\u7f6e.xlsx"
  src <- system.file("templates", "config-template.xlsx", package = "LISTC")
  if (src == "") {
    rlang::abort("\u627e\u4e0d\u5230\u5185\u7f6e\u6a21\u677f;\u8bf7\u91cd\u65b0\u5b89\u88c5 LISTC\u3002")
  }
  if (file.exists(path) && !overwrite) {
    rlang::abort(paste0("\u6587\u4ef6\u5df2\u5b58\u5728: ", path, "(\u53ef\u7528 overwrite = TRUE \u8986\u76d6)"))
  }
  file.copy(src, path, overwrite = overwrite)
  invisible(path)
}

# 读取配置簿 -> 与 YAML 等价的配置列表
parse_config_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  need_sheet <- function(nm) {
    if (!nm %in% sheets) {
      rlang::abort(paste0(
        "\u914d\u7f6e\u7c3f\u7f3a\u5c11\u5de5\u4f5c\u8868\"", nm, "\";\u8bf7\u4ece lst_config_template() \u751f\u6210\u7684",
        "\u6a21\u677f\u586b\u5199,\u4e0d\u8981\u5220\u9664\u6216\u91cd\u547d\u540d\u5de5\u4f5c\u8868\u3002"
      ))
    }
    suppressMessages(readxl::read_excel(path, sheet = nm))
  }

  kv <- function(df) {
    stats::setNames(as.list(as.character(df[[2]])), as.character(df[[1]]))
  }
  get_kv <- function(m, key) {
    v <- m[[key]]
    if (is.null(v) || is.na(v) || identical(trimws(v), "")) NULL else trimws(v)
  }
  split_csv <- function(v) {
    if (is.null(v)) NULL else trimws(strsplit(v, "[,,\u3001]")[[1]])
  }

  base <- kv(need_sheet("\u6570\u636e\u4e0e\u89d2\u8272"))
  roles <- list(
    id = get_kv(base, "\u6837\u672c\u7f16\u53f7\u5217"),
    weight = get_kv(base, "\u6743\u91cd\u5217"),
    group = split_csv(get_kv(base, "\u5206\u7ec4\u5217")),
    score = split_csv(get_kv(base, "\u5f97\u5206\u5217"))
  )
  # \u590d\u5236\u6743\u91cd(v0.3,\u53ef\u9009):\u524d\u7f00\u6216\u9017\u53f7\u5206\u9694\u5217\u540d
  repw <- get_kv(base, "\u590d\u5236\u6743\u91cd\u5217")
  if (!is.null(repw)) {
    repw_v <- split_csv(repw)
    roles$rep_weights <- if (length(repw_v) == 1) repw_v else as.list(repw_v)
    roles$rep_method <- get_kv(base, "\u590d\u5236\u6743\u91cd\u65b9\u6cd5")
    fayk <- get_kv(base, "Fay\u7cfb\u6570")
    if (!is.null(fayk)) roles$fay_k <- as.numeric(fayk)
  }
  cfg <- list(data = get_kv(base, "\u6570\u636e\u6587\u4ef6"), roles = roles)

  dims <- need_sheet("\u80fd\u529b\u7ef4\u5ea6")
  dims <- dims[!is.na(dims[[1]]), , drop = FALSE]
  if (nrow(dims) > 0) {
    dim_names <- trimws(as.character(dims[[1]]))
    th <- trimws(as.character(dims[[2]]))
    se <- trimws(as.character(dims[[3]]))
    pvcol <- if (ncol(dims) >= 4) as.character(dims[[4]]) else rep(NA, nrow(dims))
    has_theta <- !is.na(th) & th != "" & th != "NA"
    if (any(has_theta)) {
      cfg$roles$theta <- stats::setNames(as.list(th[has_theta]),
                                         dim_names[has_theta])
      cfg$roles$theta_se <- stats::setNames(as.list(se[has_theta]),
                                            dim_names[has_theta])
    }
    has_pv <- !is.na(pvcol) & trimws(pvcol) != ""
    if (any(has_pv)) {
      cfg$roles$pv <- stats::setNames(
        lapply(pvcol[has_pv], function(v) {
          v <- trimws(strsplit(v, "[,,\u3001]")[[1]])
          if (length(v) == 1) v[[1]] else as.list(v)
        }),
        dim_names[has_pv]
      )
    }
  }

  tabdf <- need_sheet("\u7edf\u8ba1\u8868")
  tabdf <- tabdf[!is.na(tabdf[["\u8868\u540d"]]), , drop = FALSE]
  if (nrow(tabdf) == 0) {
    rlang::abort("\u914d\u7f6e\u7c3f\u7684\"\u7edf\u8ba1\u8868\"\u5de5\u4f5c\u8868\u6ca1\u6709\u586b\u5199\u4efb\u4f55\u7edf\u8ba1\u91cf\u884c\u3002")
  }
  cell <- function(df, i, col) {
    if (!col %in% names(df)) {
      return(NULL)
    }
    v <- df[[col]][i]
    if (is.null(v) || is.na(v) || identical(trimws(as.character(v)), "")) {
      NULL
    } else {
      trimws(as.character(v))
    }
  }
  tables <- list()
  for (nm in unique(tabdf[["\u8868\u540d"]])) {
    rows_i <- which(tabdf[["\u8868\u540d"]] == nm)
    first <- rows_i[1]
    values <- list()
    for (i in rows_i) {
      vname <- cell(tabdf, i, "\u7edf\u8ba1\u91cf\u540d\u79f0") %||% paste0("\u7edf\u8ba1\u91cf", i)
      v <- list(stat = cell(tabdf, i, "\u7edf\u8ba1\u91cf"))
      v$var <- cell(tabdf, i, "\u53d8\u91cf")
      cutoff <- cell(tabdf, i, "\u9608\u503c")
      if (!is.null(cutoff)) v$cutoff <- as.numeric(cutoff)
      brk <- cell(tabdf, i, "\u7b49\u7ea7\u5206\u6570\u7ebf")
      if (!is.null(brk)) v$breaks <- parse_breaks_text(brk)
      v$method <- cell(tabdf, i, "\u65b9\u6cd5")
      v$correction <- cell(tabdf, i, "\u6821\u6b63")
      values[[vname]] <- v
    }
    digits <- cell(tabdf, first, "\u5c0f\u6570\u4f4d")
    tables[[length(tables) + 1]] <- list(
      name = nm,
      rows = split_csv(cell(tabdf, first, "\u884c\u53d8\u91cf")),
      cols = split_csv(cell(tabdf, first, "\u5217\u53d8\u91cf")),
      values = values,
      format = cell(tabdf, first, "\u683c\u5f0f"),
      digits = if (is.null(digits)) NULL else as.integer(digits),
      margins = identical(cell(tabdf, first, "\u5408\u8ba1"), "\u662f")
    )
  }
  cfg$tables <- tables

  outm <- kv(need_sheet("\u8f93\u51fa"))
  out <- list(
    xlsx = get_kv(outm, "Excel\u8f93\u51fa\u8def\u5f84"),
    json = get_kv(outm, "JSON\u8f93\u51fa\u8def\u5f84"),
    html = get_kv(outm, "HTML\u8f93\u51fa\u8def\u5f84")
  )
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) > 0) cfg$output <- out
  cfg
}

# "低=-Inf,中=0,高=1" -> 命名数值向量
parse_breaks_text <- function(txt) {
  parts <- trimws(strsplit(txt, "[,,\u3001]")[[1]])
  kvs <- strsplit(parts, "=")
  bad <- vapply(kvs, function(p) length(p) != 2, logical(1))
  if (any(bad)) {
    rlang::abort(paste0(
      "\u7b49\u7ea7\u5206\u6570\u7ebf\u683c\u5f0f\u4e0d\u6b63\u786e: \"", txt,
      "\"\u3002\u8bf7\u5199\u6210 \u7b49\u7ea7\u540d=\u4e0b\u754c \u5e76\u7528\u9017\u53f7\u5206\u9694,\u5982 \u4f4e=-Inf,\u4e2d=0,\u9ad8=1\u3002"
    ))
  }
  vals <- vapply(kvs, function(p) {
    v <- suppressWarnings(as.numeric(trimws(p[2])))
    if (is.na(v) && trimws(p[2]) %in% c("-Inf", "-inf")) v <- -Inf
    v
  }, numeric(1))
  if (any(is.na(vals))) {
    rlang::abort(paste0("\u7b49\u7ea7\u5206\u6570\u7ebf\u5305\u542b\u65e0\u6cd5\u8bc6\u522b\u7684\u6570\u503c: \"", txt, "\"\u3002"))
  }
  stats::setNames(vals, vapply(kvs, function(p) trimws(p[1]), character(1)))
}
