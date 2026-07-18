#' Create a listr_data object
#'
#' Attaches variable roles (id, group, weight, score, theta, theta_se, resp)
#' to a data frame. Columns can be given as bare names or character
#' vectors; theta/theta_se accept named vectors defining dimensions,
#' e.g. `theta = c(math = th_math)`.
#'
#' @param data A data.frame.
#' @param id Sample identifier column.
#' @param group Demographic/grouping columns.
#' @param weight Sampling weight column (defaults to 1 for all rows).
#' @param score Observed score column(s).
#' @param theta Ability estimate column(s), named by dimension.
#' @param theta_se IRT standard error column(s), paired with `theta`.
#' @param resp Item response columns.
#' @param key Optional scoring key for `resp` (named vector: item -> correct
#'   answer), required for [st_pvalue()].
#' @return A `listr_data` object.
#' @export
lst_data <- function(data, id = NULL, group = NULL, weight = NULL,
                     score = NULL, theta = NULL, theta_se = NULL,
                     resp = NULL, key = NULL) {
  if (!is.data.frame(data)) {
    rlang::abort("data \u5fc5\u987b\u662f data.frame\u3002")
  }
  roles <- list(
    id       = resolve_vars(rlang::enquo(id), data, "id"),
    group    = resolve_vars(rlang::enquo(group), data, "group"),
    weight   = resolve_vars(rlang::enquo(weight), data, "weight"),
    score    = resolve_vars(rlang::enquo(score), data, "score"),
    theta    = resolve_vars(rlang::enquo(theta), data, "theta"),
    theta_se = resolve_vars(rlang::enquo(theta_se), data, "theta_se"),
    resp     = resolve_vars(rlang::enquo(resp), data, "resp")
  )
  # theta 维度命名:未命名时用列名本身
  for (r in c("theta", "theta_se", "score")) {
    v <- roles[[r]]
    if (!is.null(v)) {
      nm <- names(v)
      if (is.null(nm)) nm <- rep("", length(v))
      nm[nm == ""] <- unname(v)[nm == ""]
      roles[[r]] <- stats::setNames(unname(v), nm)
    }
  }
  x <- structure(
    list(data = data, roles = roles, key = key),
    class = "listr_data"
  )
  lst_validate(x)
}

#' Validate a listr_data object
#'
#' Checks role pairing (theta/theta_se), non-negative weights, unique ids.
#' @param x A `listr_data` object.
#' @return `x`, invisibly on success.
#' @export
lst_validate <- function(x) {
  stopifnot(inherits(x, "listr_data"))
  r <- x$roles
  d <- x$data
  if (!is.null(r$theta) || !is.null(r$theta_se)) {
    nt <- names(r$theta)
    ns <- names(r$theta_se)
    if (length(r$theta) != length(r$theta_se) ||
        !setequal(nt, ns)) {
      rlang::abort(paste0(
        "theta \u4e0e theta_se \u5fc5\u987b\u6309\u7ef4\u5ea6\u4e00\u4e00\u914d\u5bf9\u3002\u5f53\u524d theta \u7ef4\u5ea6: ",
        paste(nt, collapse = ", "), ";theta_se \u7ef4\u5ea6: ",
        paste(ns, collapse = ", ")
      ))
    }
    for (dim in nt) {
      if (!is.numeric(d[[r$theta[[dim]]]]) ||
          !is.numeric(d[[r$theta_se[[dim]]]])) {
        rlang::abort(paste0("\u7ef4\u5ea6 ", dim, " \u7684 theta/theta_se \u5217\u5fc5\u987b\u662f\u6570\u503c\u3002"))
      }
      se <- d[[r$theta_se[[dim]]]]
      if (any(se < 0, na.rm = TRUE)) {
        rlang::abort(paste0("\u7ef4\u5ea6 ", dim, " \u7684\u6807\u51c6\u8bef\u5217\u5305\u542b\u8d1f\u503c\u3002"))
      }
    }
  }
  if (!is.null(r$weight)) {
    w <- d[[r$weight]]
    if (!is.numeric(w)) rlang::abort("\u6743\u91cd\u5217\u5fc5\u987b\u662f\u6570\u503c\u3002")
    if (any(w < 0, na.rm = TRUE)) rlang::abort("\u6743\u91cd\u5217\u5305\u542b\u8d1f\u503c\u3002")
  }
  if (!is.null(r$id)) {
    if (anyDuplicated(d[[r$id]]) > 0) {
      rlang::abort("id \u5217\u5b58\u5728\u91cd\u590d\u503c,\u6837\u672c\u6807\u8bc6\u5fc5\u987b\u552f\u4e00\u3002")
    }
  }
  if (!is.null(x$key) && is.null(r$resp)) {
    rlang::abort("\u63d0\u4f9b\u4e86\u8ba1\u5206\u952e key \u4f46\u672a\u58f0\u660e resp \u4f5c\u7b54\u5217\u3002")
  }
  invisible(x)
}

#' @export
print.listr_data <- function(x, ...) {
  r <- x$roles
  cat("<listr_data> ", nrow(x$data), " \u884c x ", ncol(x$data), " \u5217\n", sep = "")
  show <- function(label, v) {
    if (!is.null(v)) {
      nm <- names(v)
      lab <- if (!is.null(nm) && !all(nm == unname(v))) {
        paste0(nm, "=", unname(v), collapse = ", ")
      } else {
        paste(unname(v), collapse = ", ")
      }
      cat("  ", label, ": ", lab, "\n", sep = "")
    }
  }
  show("id", r$id)
  show("group", r$group)
  show("weight", r$weight)
  show("score", r$score)
  show("theta", r$theta)
  show("theta_se", r$theta_se)
  if (!is.null(r$resp)) {
    cat("  resp: ", length(r$resp), " \u4e2a\u9898\u76ee\u5217",
        if (!is.null(x$key)) "(\u542b\u8ba1\u5206\u952e)" else "", "\n", sep = "")
  }
  invisible(x)
}

# 内部:取权重向量(无权重时全 1)
get_weights <- function(x) {
  if (is.null(x$roles$weight)) {
    rep(1, nrow(x$data))
  } else {
    x$data[[x$roles$weight]]
  }
}

# 内部:把统计量的 var 解析为 (xcol, secol)。
# 解析顺序:theta 维度名 -> score 维度名 -> 数据列名。
resolve_measure <- function(x, var) {
  r <- x$roles
  if (!is.null(r$theta) && var %in% names(r$theta)) {
    return(list(xcol = r$theta[[var]], secol = r$theta_se[[var]]))
  }
  if (!is.null(r$score) && var %in% names(r$score)) {
    return(list(xcol = r$score[[var]], secol = NULL))
  }
  if (var %in% names(x$data)) {
    secol <- NULL
    if (!is.null(r$theta) && var %in% unname(r$theta)) {
      dim <- names(r$theta)[match(var, unname(r$theta))]
      secol <- r$theta_se[[dim]]
    }
    return(list(xcol = var, secol = secol))
  }
  rlang::abort(paste0(
    "\u627e\u4e0d\u5230\u53d8\u91cf \"", var,
    "\":\u5b83\u65e2\u4e0d\u662f theta/score \u7ef4\u5ea6\u540d,\u4e5f\u4e0d\u662f\u6570\u636e\u5217\u540d\u3002"
  ))
}
