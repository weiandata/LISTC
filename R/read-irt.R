# IRT 软件人参数文件解析器:计划于 v0.2 实现(design doc 3/11)。

#' Read a Winsteps PFILE (person parameter file)
#'
#' Planned for v0.2.
#' @param path PFILE path.
#' @return Tibble with columns id, theta, theta_se and fit statistics.
#' @export
read_winsteps_pfile <- function(path) {
  rlang::abort("read_winsteps_pfile() \u8ba1\u5212\u4e8e v0.2 \u5b9e\u73b0\u3002")
}

#' Read ConQuest person estimate output
#'
#' Planned for v0.2.
#' @param path ConQuest show/plausible output path.
#' @return Tibble with columns id, theta, theta_se.
#' @export
read_conquest_person <- function(path) {
  rlang::abort("read_conquest_person() \u8ba1\u5212\u4e8e v0.2 \u5b9e\u73b0\u3002")
}

#' Join person parameters onto a listr_data by id
#'
#' Merges a person-parameter table (columns id, theta, theta_se) onto the
#' data by the declared id role and registers the theta/theta_se roles.
#'
#' @param x A `listr_data` object.
#' @param person A data.frame with columns id, theta, theta_se.
#' @param dim Dimension name for the merged theta (default "theta").
#' @return `x` with theta/theta_se roles filled.
#' @export
lst_join_person <- function(x, person, dim = "theta") {
  stopifnot(inherits(x, "listr_data"))
  if (is.null(x$roles$id)) {
    rlang::abort("lst_join_person \u9700\u8981\u5148\u5728 lst_data() \u4e2d\u58f0\u660e id\u3002")
  }
  need <- c("id", "theta", "theta_se")
  if (!all(need %in% names(person))) {
    rlang::abort("person \u5fc5\u987b\u5305\u542b id\u3001theta\u3001theta_se \u4e09\u5217\u3002")
  }
  tcol <- paste0(".", dim)
  scol <- paste0(".", dim, "_se")
  idx <- match(x$data[[x$roles$id]], person$id)
  x$data[[tcol]] <- person$theta[idx]
  x$data[[scol]] <- person$theta_se[idx]
  x$roles$theta <- c(x$roles$theta, stats::setNames(tcol, dim))
  x$roles$theta_se <- c(x$roles$theta_se, stats::setNames(scol, dim))
  lst_validate(x)
  x
}
