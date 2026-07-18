# IRT 软件人参数文件解析器(design doc 3)。
# 容错策略:跳过注释行,按表头/列位识别 theta 与 SE,输出统一列
# (id, theta, theta_se, ...),便于 lst_join_person() 合并。

#' Read a Winsteps PFILE (person parameter file)
#'
#' Handles both fixed/whitespace PFILE output and csv PFILE
#' (`PFILE=xxx.csv`). Comment lines starting with `;` are skipped; the
#' header row is located by the presence of a `MEASURE` column. The SE
#' column is taken from `ERROR` (or `MODLSE`), the id from `NAME`
#' (falling back to `ENTRY`).
#'
#' @param path PFILE path.
#' @param id_col,theta_col,se_col Optional column-name overrides.
#' @return Tibble with columns id, theta, theta_se plus the remaining
#'   PFILE columns.
#' @examples
#' f <- tempfile(fileext = ".txt")
#' writeLines(c("; PERSON FILE",
#'              ";ENTRY MEASURE COUNT SCORE ERROR NAME",
#'              "1 0.52 20 12 0.41 S001",
#'              "2 -1.03 20 6 0.44 S002"), f)
#' read_winsteps_pfile(f)
#' @export
read_winsteps_pfile <- function(path, id_col = NULL, theta_col = NULL,
                                se_col = NULL) {
  if (!file.exists(path)) {
    rlang::abort(paste0("\u627e\u4e0d\u5230 PFILE \u6587\u4ef6: ", path))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  is_comment <- grepl("^\\s*;", lines)
  # 表头行:含 MEASURE(可能以 ; 开头)
  hdr_idx <- which(grepl("MEASURE", lines, ignore.case = TRUE))[1]
  if (is.na(hdr_idx)) {
    rlang::abort("\u65e0\u6cd5\u8bc6\u522b PFILE \u8868\u5934:\u672a\u627e\u5230 MEASURE \u5217\u3002")
  }
  hdr_line <- sub("^\\s*;", "", lines[hdr_idx])
  body <- lines[setdiff(seq_along(lines), c(which(is_comment), hdr_idx))]
  body <- body[!grepl("^\\s*;", body)]
  csv <- grepl(",", hdr_line, fixed = TRUE)
  parse_line <- function(l) {
    if (csv) {
      trimws(strsplit(l, ",")[[1]])
    } else {
      strsplit(trimws(l), "\\s+")[[1]]
    }
  }
  header <- toupper(parse_line(hdr_line))
  ncol_num <- length(header)
  rows <- lapply(body, function(l) {
    p <- parse_line(l)
    if (length(p) < 2) {
      return(NULL)
    }
    # NAME 常在最后且可能含空格:前 ncol-1 列取数,余下合并为 NAME
    if (!csv && length(p) > ncol_num) {
      p <- c(p[seq_len(ncol_num - 1)],
             paste(p[ncol_num:length(p)], collapse = " "))
    }
    length(p) <- ncol_num
    p
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    rlang::abort("PFILE \u4e2d\u6ca1\u6709\u53ef\u89e3\u6790\u7684\u6570\u636e\u884c\u3002")
  }
  m <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(m) <- header
  pick <- function(override, candidates) {
    if (!is.null(override)) {
      return(toupper(override))
    }
    hit <- candidates[candidates %in% header]
    if (length(hit) == 0) NA_character_ else hit[1]
  }
  tc <- pick(theta_col, "MEASURE")
  sc <- pick(se_col, c("ERROR", "MODLSE", "REALSE", "S.E."))
  ic <- pick(id_col, c("NAME", "PERSON", "ENTRY"))
  if (is.na(tc) || is.na(sc)) {
    rlang::abort(paste0(
      "PFILE \u7f3a\u5c11\u80fd\u529b\u503c\u6216\u6807\u51c6\u8bef\u5217(\u9700\u8981 MEASURE \u4e0e ERROR/MODLSE);",
      "\u5b9e\u9645\u8868\u5934: ", paste(header, collapse = ", ")
    ))
  }
  out <- tibble::tibble(
    id = if (is.na(ic)) seq_len(nrow(m)) else m[[ic]],
    theta = as.numeric(m[[tc]]),
    theta_se = as.numeric(m[[sc]])
  )
  extra <- m[setdiff(header, c(tc, sc, ic))]
  extra[] <- lapply(extra, function(v) {
    nv <- suppressWarnings(as.numeric(v))
    if (all(is.na(nv) == is.na(v))) nv else v
  })
  tibble::as_tibble(cbind(out, extra))
}

#' Read ConQuest person estimate output (WLE/EAP tables)
#'
#' Reads whitespace-delimited ConQuest person files (e.g. `show cases`
#' output or `.wle` files). ConQuest files rarely carry headers, so the
#' column positions are declared explicitly and default to the common
#' 6-column WLE layout: seq, id, score, max, theta, se.
#'
#' @param path ConQuest person file path.
#' @param cols Named integer vector giving 1-based column positions for
#'   `id`, `theta`, `theta_se`.
#' @return Tibble with columns id, theta, theta_se.
#' @examples
#' f <- tempfile(fileext = ".wle")
#' writeLines(c("1 S001 12.00 20.00 0.523 0.412",
#'              "2 S002 6.00 20.00 -1.031 0.437"), f)
#' read_conquest_person(f)
#' @export
read_conquest_person <- function(path,
                                 cols = c(id = 2, theta = 5, theta_se = 6)) {
  if (!file.exists(path)) {
    rlang::abort(paste0("\u627e\u4e0d\u5230 ConQuest \u6587\u4ef6: ", path))
  }
  need <- c("id", "theta", "theta_se")
  if (!all(need %in% names(cols))) {
    rlang::abort("cols \u5fc5\u987b\u5305\u542b id\u3001theta\u3001theta_se \u4e09\u4e2a\u4f4d\u7f6e\u3002")
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  # 跳过任何含字母表头/分隔线的行
  toks <- lapply(lines, function(l) strsplit(trimws(l), "\\s+")[[1]])
  keep <- vapply(toks, function(p) {
    length(p) >= max(cols) &&
      !is.na(suppressWarnings(as.numeric(p[cols[["theta"]]])))
  }, logical(1))
  toks <- toks[keep]
  if (length(toks) == 0) {
    rlang::abort(paste0(
      "\u65e0\u6cd5\u4ece ConQuest \u6587\u4ef6\u89e3\u6790\u51fa\u6570\u636e\u884c;\u8bf7\u7528 cols \u53c2\u6570\u6307\u660e",
      " id/theta/theta_se \u7684\u5217\u4f4d\u7f6e\u3002"
    ))
  }
  tibble::tibble(
    id = vapply(toks, function(p) p[cols[["id"]]], character(1)),
    theta = vapply(toks, function(p) {
      as.numeric(p[cols[["theta"]]])
    }, numeric(1)),
    theta_se = vapply(toks, function(p) {
      as.numeric(p[cols[["theta_se"]]])
    }, numeric(1))
  )
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
#' @examples
#' d <- data.frame(id = c("S001", "S002"), grade = c("G4", "G4"))
#' x <- lst_data(d, id = id, group = grade)
#' person <- data.frame(id = c("S001", "S002"),
#'                      theta = c(0.5, -1.0), theta_se = c(0.4, 0.45))
#' x <- lst_join_person(x, person, dim = "math")
#' x$roles$theta
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
  if (all(is.na(idx))) {
    rlang::abort("person \u7684 id \u4e0e\u6570\u636e\u7684 id \u5b8c\u5168\u5bf9\u4e0d\u4e0a,\u8bf7\u68c0\u67e5\u4e24\u8fb9\u7684\u7f16\u53f7\u3002")
  }
  x$data[[tcol]] <- person$theta[idx]
  x$data[[scol]] <- person$theta_se[idx]
  x$roles$theta <- c(x$roles$theta, stats::setNames(tcol, dim))
  x$roles$theta_se <- c(x$roles$theta_se, stats::setNames(scol, dim))
  lst_validate(x)
  x
}
