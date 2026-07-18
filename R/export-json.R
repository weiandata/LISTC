#' Export listr_table(s) as machine-readable JSON (for AI agents)
#'
#' Emits tidy long results plus metadata for every statistic (type,
#' variable, method, correction, rho) and an `interpretation` field from
#' [lst_interpret()].
#'
#' @param tab A `listr_table` or named list of them.
#' @param path Optional output path; when `NULL`, returns the JSON string.
#' @param pretty Pretty-print JSON.
#' @return JSON string (invisibly when written to `path`).
#' @export
lst_to_json <- function(tab, path = NULL, pretty = TRUE) {
  tabs <- normalize_tabs(tab)
  payload <- list(
    package = "LISTR",
    version = as.character(utils::packageVersion("LISTR")),
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    tables = lapply(names(tabs), function(nm) {
      tb <- tabs[[nm]]
      list(
        name = nm,
        layout = list(rows = tb$row_vars, cols = tb$col_vars),
        statistics = tb$meta,
        results = tb$long,
        interpretation = lst_interpret(tb)
      )
    })
  )
  js <- jsonlite::toJSON(payload, dataframe = "rows", na = "null",
                         auto_unbox = TRUE, pretty = pretty, digits = 8)
  if (is.null(path)) {
    return(js)
  }
  writeLines(js, path, useBytes = TRUE)
  invisible(js)
}
