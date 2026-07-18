#' Export listc_table(s) as a standalone styled HTML report
#'
#' Dependency-free HTML rendering (v0.4): Chinese-friendly fonts,
#' zebra-striped pivot tables, an interpretation section per table and a
#' methods footnote describing the variance engine in use. Suitable for
#' emailing or embedding in Rmd/Quarto via `htmltools::HTML`.
#'
#' @param tab A `listc_table` or named list of them.
#' @param path Optional output .html path; when `NULL`, returns the HTML
#'   string invisibly.
#' @param title Report title; `NULL` uses a built-in default title
#'   (Chinese for "statistical results").
#' @param interpret Include the rule-based interpretation section.
#' @return HTML string, invisibly.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' tab <- lst_table(x, rows = region, values = list(mean = st_mean(math)))
#' f <- tempfile(fileext = ".html")
#' lst_to_html(tab, f, title = "Report")
#' file.exists(f)
#' @export
lst_to_html <- function(tab, path = NULL, title = NULL,
                        interpret = TRUE) {
  # \u9ed8\u8ba4\u6807\u9898"\u7edf\u8ba1\u7ed3\u679c";\u7528 NULL \u54e8\u5175\u800c\u975e\u5b57\u9762\u91cf,\u4ee5\u4fdd\u6301 Rd usage \u6bb5\u4e3a ASCII\u3002
  if (is.null(title)) title <- "\u7edf\u8ba1\u7ed3\u679c"
  tabs <- normalize_tabs(tab)
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  render_table <- function(nm, tb) {
    wide <- as.data.frame(as_wide(tb))
    head_cells <- paste0("<th>", esc(names(wide)), "</th>", collapse = "")
    body_rows <- vapply(seq_len(nrow(wide)), function(i) {
      cells <- vapply(seq_along(wide), function(j) {
        v <- esc(as.character(wide[i, j]))
        tag <- if (j <= length(tb$row_vars)) "th" else "td"
        paste0("<", tag, ">", v, "</", tag, ">")
      }, character(1))
      paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
    }, character(1))
    variance <- unique(unlist(lapply(tb$meta, function(m) m$variance)))
    variance <- variance[!is.na(variance)]
    note <- paste0(
      "\u65b9\u5dee\u4f30\u8ba1: ", paste(variance, collapse = ", "),
      ";\u62ec\u53f7\u5185\u4e3a\u603b\u6807\u51c6\u8bef(\u62bd\u6837+\u6d4b\u91cf/\u63d2\u8865)\u3002"
    )
    interp_html <- ""
    if (interpret) {
      lines <- lst_interpret(tb)
      interp_html <- paste0(
        "<div class='interp'><h3>\u81ea\u52a8\u89e3\u8bfb</h3><ul>",
        paste0("<li>", esc(lines), "</li>", collapse = ""),
        "</ul></div>"
      )
    }
    paste0(
      "<section><h2>", esc(nm), "</h2>",
      "<table><thead><tr>", head_cells, "</tr></thead><tbody>",
      paste0(body_rows, collapse = ""), "</tbody></table>",
      "<p class='note'>", esc(note), "</p>",
      interp_html, "</section>"
    )
  }
  css <- paste0(
    "body{font-family:'DengXian','Microsoft YaHei','PingFang SC',",
    "sans-serif;margin:2em auto;max-width:70em;color:#1a1a2e;}",
    "h1{border-bottom:3px solid #2f5496;padding-bottom:.3em;}",
    "h2{color:#2f5496;margin-top:2em;}",
    "table{border-collapse:collapse;width:100%;font-size:.95em;}",
    "th,td{border:1px solid #b4c6e7;padding:.45em .7em;text-align:right;}",
    "thead th{background:#d9e2f3;text-align:center;}",
    "tbody th{background:#eef2fa;text-align:left;font-weight:600;}",
    "tbody tr:nth-child(even) td{background:#f7f9fd;}",
    ".note{color:#666;font-size:.85em;}",
    ".interp{background:#f4f8ee;border-left:4px solid #70ad47;",
    "padding:.5em 1em;margin-top:1em;}",
    ".interp h3{margin:.2em 0;color:#538135;}",
    "footer{margin-top:3em;color:#999;font-size:.8em;}"
  )
  html <- paste0(
    "<!DOCTYPE html><html lang='zh'><head><meta charset='utf-8'>",
    "<title>", esc(title), "</title><style>", css, "</style></head><body>",
    "<h1>", esc(title), "</h1>",
    paste0(vapply(names(tabs), function(nm) {
      render_table(nm, tabs[[nm]])
    }, character(1)), collapse = ""),
    "<footer>LISTC ", as.character(utils::packageVersion("LISTC")),
    " \u00b7 ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</footer>",
    "</body></html>"
  )
  if (!is.null(path)) {
    writeLines(html, path, useBytes = TRUE)
  }
  invisible(html)
}