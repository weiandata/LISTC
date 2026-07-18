#' Classify a variable into proficiency levels by cut scores
#'
#' Boundary convention: reaching a cut score places the person in the
#' higher level (`x >= lower & x < upper`).
#'
#' @param x A `listc_data` object.
#' @param var Measure: theta/score dimension name or numeric column.
#' @param breaks Named numeric vector: level name -> lower bound,
#'   e.g. `c(fail = -Inf, pass = 0.5, excellent = 1.2)`.
#' @param labels Optional level labels (defaults to names of `breaks`).
#' @param name Name of the new column (default `<var>_level`).
#' @return `x` with an ordered-factor level column added.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' x <- lst_classify(x, math, c(low = -Inf, mid = -0.5, high = 0.8))
#' table(x$data$math_level)
#' @export
lst_classify <- function(x, var, breaks, labels = NULL, name = NULL) {
  stopifnot(inherits(x, "listc_data"))
  var <- resolve_stat_var(rlang::enquo(var), x)
  m <- resolve_measure(x, var)
  if (is.null(names(breaks)) || any(names(breaks) == "")) {
    rlang::abort("breaks \u5fc5\u987b\u662f\u547d\u540d\u5411\u91cf:\u540d\u79f0\u662f\u7b49\u7ea7\u540d,\u503c\u662f\u8be5\u7b49\u7ea7\u4e0b\u754c\u3002")
  }
  labels <- labels %||% names(breaks)
  name <- name %||% paste0(var, "_level")
  xv <- x$data[[m$xcol]]
  x$data[[name]] <- cut(xv, breaks = c(unname(breaks), Inf),
                        labels = labels, right = FALSE,
                        ordered_result = TRUE)
  x
}

#' Add an above-cutoff indicator variable
#'
#' Uses `x >= cutoff` (reaching the cut score counts as above).
#' @inheritParams lst_classify
#' @param cutoff Numeric threshold.
#' @param name Name of the new column (default `<var>_above`).
#' @return `x` with a 0/1 indicator column added.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' x <- lst_above(x, math, cutoff = 1)
#' mean(x$data$math_above)
#' @export
lst_above <- function(x, var, cutoff, name = NULL) {
  stopifnot(inherits(x, "listc_data"))
  var <- resolve_stat_var(rlang::enquo(var), x)
  m <- resolve_measure(x, var)
  name <- name %||% paste0(var, "_above")
  x$data[[name]] <- as.numeric(x$data[[m$xcol]] >= cutoff)
  x
}

#' Add arbitrary derived variables (mutate-style)
#'
#' @param x A `listc_data` object.
#' @param ... Name-value expressions evaluated in the data.
#' @return `x` with derived columns added.
#' @examples
#' d <- data.frame(id = 1:5, part1 = 1:5, part2 = 6:10)
#' x <- lst_data(d, id = id)
#' x <- lst_derive(x, total = part1 + part2)
#' x$data$total
#' @export
lst_derive <- function(x, ...) {
  stopifnot(inherits(x, "listc_data"))
  exprs <- rlang::enquos(...)
  if (is.null(names(exprs)) || any(names(exprs) == "")) {
    rlang::abort("lst_derive \u7684\u6bcf\u4e2a\u8868\u8fbe\u5f0f\u90fd\u5fc5\u987b\u547d\u540d,\u5982 \u603b\u5206 = a + b\u3002")
  }
  for (nm in names(exprs)) {
    x$data[[nm]] <- rlang::eval_tidy(exprs[[nm]], data = x$data)
  }
  x
}
