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
#' @param rep_weights Replicate weight columns (v0.3): a character vector
#'   of column names, bare names, or a single prefix string such as
#'   `"W_FSTR"` which expands to `W_FSTR1..R` (PISA style). When set,
#'   sampling variance switches to the replicate-weights engine.
#' @param rep_method Replicate method: `"fay"` (PISA BRR-Fay),
#'   `"brr"`, `"jk1"`, or `"jk2"` (TIMSS). Required with `rep_weights`.
#' @param fay_k Fay factor for `rep_method = "fay"` (default 0.5).
#' @param pv Plausible values (v0.4): named list, dimension -> PV columns
#'   or a template with `#` for the PV number, e.g.
#'   `pv = list(math = "PV#MATH")` expands to PV1MATH..PV10MATH.
#'   PV dimensions are analyzed with Rubin combination.
#' @param pv_sampling Sampling-variance convention for PV dimensions:
#'   `"first"` (PISA manual practice: first PV only) or `"average"`
#'   (average across all PVs).
#' @return A `listr_data` object.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' x
#' @export
lst_data <- function(data, id = NULL, group = NULL, weight = NULL,
                     score = NULL, theta = NULL, theta_se = NULL,
                     resp = NULL, key = NULL,
                     rep_weights = NULL, rep_method = NULL, fay_k = 0.5,
                     pv = NULL, pv_sampling = c("first", "average")) {
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
    resp     = resolve_vars(rlang::enquo(resp), data, "resp"),
    rep_weights = resolve_rep_weights(rlang::enquo(rep_weights), data),
    pv       = resolve_pv(pv, data)
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
    list(data = data, roles = roles, key = key,
         rep_method = rep_method, fay_k = fay_k,
         pv_sampling = match.arg(pv_sampling)),
    class = "listr_data"
  )
  lst_validate(x)
}

# rep_weights:列名向量、裸名或前缀字符串(展开为 前缀1..R)
resolve_rep_weights <- function(quo, data) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  v <- tryCatch(
    rlang::eval_tidy(quo, data = col_env(data)),
    error = function(e) rlang::eval_tidy(quo)
  )
  if (is.null(v)) {
    return(NULL)
  }
  v <- as.character(v)
  if (length(v) == 1 && !v %in% names(data)) {
    hits <- grep(paste0("^", v, "[0-9]+$"), names(data), value = TRUE)
    if (length(hits) == 0) {
      rlang::abort(paste0(
        "rep_weights = \"", v, "\" \u65e2\u4e0d\u662f\u5217\u540d,\u4e5f\u5339\u914d\u4e0d\u5230 \"", v,
        "1\", \"", v, "2\" ... \u8fd9\u6837\u7684\u524d\u7f00\u5217\u3002"
      ))
    }
    # 按数字序排列
    v <- hits[order(as.integer(sub(paste0("^", v), "", hits)))]
  }
  missing <- setdiff(v, names(data))
  if (length(missing) > 0) {
    rlang::abort(paste0(
      "rep_weights \u4e2d\u7684\u5217\u5728\u6570\u636e\u91cc\u4e0d\u5b58\u5728: ",
      paste(missing, collapse = ", ")
    ))
  }
  v
}

#' Validate a listr_data object
#'
#' Checks role pairing (theta/theta_se), non-negative weights, unique ids.
#' @param x A `listr_data` object.
#' @return `x`, invisibly on success.
#' @examples
#' d <- data.frame(id = 1:100, region = rep(c("north", "south"), 50),
#'                 w = runif(100, 0.5, 2), theta = rnorm(100),
#'                 se = runif(100, 0.2, 0.4))
#' x <- lst_data(d, id = id, group = region, weight = w,
#'               theta = c(math = theta), theta_se = c(math = se))
#' lst_validate(x)
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
    idv <- d[[r$id]]
    n_na <- sum(is.na(idv))
    if (n_na > 0) {
      rlang::inform(paste0(
        "\u63d0\u793a: id \u5217\u6709 ", n_na, " \u4e2a\u7f3a\u5931\u503c(\u5360 ",
        sprintf("%.1f%%", 100 * n_na / length(idv)),
        ")\u3002\u7f3a\u5931 id \u4e0d\u5f71\u54cd\u7edf\u8ba1,\u4f46\u65e0\u6cd5\u7528\u4e8e lst_join_person \u5408\u5e76\u3002"
      ))
    }
    if (anyDuplicated(idv[!is.na(idv)]) > 0) {
      rlang::abort("id \u5217\u5b58\u5728\u91cd\u590d\u503c,\u6837\u672c\u6807\u8bc6\u5fc5\u987b\u552f\u4e00\u3002")
    }
  }
  if (!is.null(x$key) && is.null(r$resp)) {
    rlang::abort("\u63d0\u4f9b\u4e86\u8ba1\u5206\u952e key \u4f46\u672a\u58f0\u660e resp \u4f5c\u7b54\u5217\u3002")
  }
  if (!is.null(r$pv)) {
    for (dim in names(r$pv)) {
      for (pc in r$pv[[dim]]) {
        if (!is.numeric(d[[pc]])) {
          rlang::abort(paste0("pv \u7ef4\u5ea6 ", dim, " \u7684\u5217 ", pc,
                              " \u5fc5\u987b\u662f\u6570\u503c\u3002"))
        }
      }
      if (!is.null(r$theta) && dim %in% names(r$theta)) {
        rlang::abort(paste0(
          "\u7ef4\u5ea6\u540d ", dim, " \u540c\u65f6\u51fa\u73b0\u5728 pv \u548c theta \u4e2d;",
          "\u8bf7\u4e3a\u4e24\u79cd\u80fd\u529b\u503c\u4f7f\u7528\u4e0d\u540c\u7684\u7ef4\u5ea6\u540d\u3002"
        ))
      }
    }
  }
  if (!is.null(r$rep_weights)) {
    if (is.null(x$rep_method)) {
      rlang::abort(paste0(
        "\u58f0\u660e\u4e86 rep_weights \u5fc5\u987b\u540c\u65f6\u6307\u5b9a rep_method",
        "(fay/brr/jk1/jk2;PISA \u7528 fay,TIMSS \u7528 jk2)\u3002"
      ))
    }
    x$rep_method <- match.arg(x$rep_method, REP_METHODS)
    for (rc in r$rep_weights) {
      if (!is.numeric(d[[rc]])) {
        rlang::abort(paste0("\u590d\u5236\u6743\u91cd\u5217 ", rc, " \u5fc5\u987b\u662f\u6570\u503c\u3002"))
      }
    }
    if (!is.null(x$fay_k) &&
        (!is.numeric(x$fay_k) || x$fay_k <= 0 || x$fay_k >= 1)) {
      rlang::abort("fay_k \u5fc5\u987b\u5728 (0, 1) \u533a\u95f4\u5185,PISA \u60ef\u4f8b\u4e3a 0.5\u3002")
    }
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

# 内部:把统计量的 var 解析为 (xcol, secol) 或 PV 列组。
# 解析顺序:pv 维度名 -> theta 维度名 -> score 维度名 -> 数据列名。
resolve_measure <- function(x, var) {
  r <- x$roles
  if (!is.null(r$pv) && var %in% names(r$pv)) {
    return(list(xcol = NULL, secol = NULL, pvcols = r$pv[[var]]))
  }
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
    "\":\u5b83\u65e2\u4e0d\u662f pv/theta/score \u7ef4\u5ea6\u540d,\u4e5f\u4e0d\u662f\u6570\u636e\u5217\u540d\u3002"
  ))
}
