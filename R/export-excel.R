#' Export listc_table(s) to a formatted Excel workbook
#'
#' Chinese-friendly defaults: DengXian base font and column widths
#' estimated from full-width character counts; override via `style`.
#' A final interpretation sheet (titled "conclusions" in Chinese) carries
#' the rule-based interpretation.
#'
#' @param tab A `listc_table` or named list of them (names become sheets).
#' @param path Output .xlsx path.
#' @param style Optional list of overrides: `font`, `font_size`,
#'   `header_fill`.
#' @param overwrite Overwrite existing file.
#' @param interpret Include the interpretation sheet.
#' @return `path`, invisibly.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' tab <- lst_table(x, rows = region, values = list(mean = st_mean(math)))
#' f <- tempfile(fileext = ".xlsx")
#' lst_to_excel(tab, f, overwrite = TRUE)
#' file.exists(f)
#' @export
lst_to_excel <- function(tab, path, style = NULL, overwrite = FALSE,
                         interpret = TRUE) {
  tabs <- normalize_tabs(tab)
  font <- style$font %||% "\u7b49\u7ebf"
  font_size <- style$font_size %||% 11
  header_fill <- style$header_fill %||% "#D9E2F3"

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font)
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = header_fill,
    border = "TopBottomLeftRight", borderColour = "#8496B0",
    halign = "center", valign = "center", wrapText = TRUE
  )
  body_style <- openxlsx::createStyle(
    border = "TopBottomLeftRight", borderColour = "#B4C6E7",
    halign = "right"
  )

  for (nm in names(tabs)) {
    sheet <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    wide <- as.data.frame(as_wide(tabs[[nm]]))
    openxlsx::writeData(wb, sheet, wide, startRow = 1,
                        headerStyle = header_style, borders = "all",
                        borderColour = "#B4C6E7")
    openxlsx::addStyle(
      wb, sheet, body_style,
      rows = seq_len(nrow(wide)) + 1, cols = seq_len(ncol(wide)),
      gridExpand = TRUE, stack = TRUE
    )
    # 中文感知列宽:全角计 2,限制在 8-40
    widths <- vapply(seq_along(wide), function(j) {
      w <- max(display_width(c(names(wide)[j], as.character(wide[[j]]))),
               na.rm = TRUE)
      min(max(w + 2, 8), 40)
    }, numeric(1))
    openxlsx::setColWidths(wb, sheet, cols = seq_along(wide), widths = widths)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  }

  if (interpret) {
    sheet_c <- "\u7ed3\u8bba" # 结论
    openxlsx::addWorksheet(wb, sheet_c)
    lines <- unlist(lapply(names(tabs), function(nm) {
      c(paste0("\u3010", nm, "\u3011"), lst_interpret(tabs[[nm]]), "")
    }))
    col_df <- stats::setNames(data.frame(lines, stringsAsFactors = FALSE),
                              "\u81ea\u52a8\u89e3\u8bfb") # 自动解读
    openxlsx::writeData(wb, sheet_c, col_df)
    openxlsx::setColWidths(wb, sheet_c, cols = 1, widths = 100)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  invisible(path)
}

normalize_tabs <- function(tab) {
  if (inherits(tab, "listc_table")) {
    return(stats::setNames(list(tab), "\u7ed3\u679c")) # 结果
  }
  if (is.list(tab) && length(tab) > 0 &&
      all(vapply(tab, inherits, logical(1), "listc_table"))) {
    if (is.null(names(tab)) || any(names(tab) == "")) {
      names(tab) <- paste0("\u8868", seq_along(tab)) # 表N
    }
    return(tab)
  }
  rlang::abort("tab \u5fc5\u987b\u662f listc_table \u6216 listc_table \u7684\u547d\u540d\u5217\u8868\u3002")
}
