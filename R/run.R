#' One-shot entry point: run a full LISTR analysis from a configuration
#'
#' Reads the config, imports only the needed columns, applies roles,
#' computes all requested tables and writes all requested outputs
#' (xlsx and/or json). Primary interface for non-R users and AI agents.
#'
#' @param config Anything accepted by [lst_config()].
#' @param quiet Suppress progress messages.
#' @return Invisibly, `list(tables = <named listr_table list>, log = <list>)`.
#' @examples
#' csv <- tempfile(fileext = ".csv")
#' write.csv(data.frame(id = 1:60, g = rep(c("a", "b"), 30),
#'                      score = rnorm(60, 50, 10)), csv,
#'           row.names = FALSE)
#' out <- tempfile(fileext = ".json")
#' res <- lst_run(list(
#'   data = csv,
#'   roles = list(id = "id", group = list("g")),
#'   tables = list(list(name = "t1", rows = list("g"),
#'                      values = list(mean = list(stat = "st_mean",
#'                                                var = "score")))),
#'   output = list(json = out)
#' ), quiet = TRUE)
#' res$log$tables
#' @export
lst_run <- function(config, quiet = FALSE) {
  t0 <- Sys.time()
  cfg <- lst_config(config)
  say <- function(...) if (!quiet) rlang::inform(paste0(...))

  # 只读取实际用到的列(design doc 9.1)
  r <- cfg$roles
  used_vars <- unlist(lapply(cfg$tables, function(tb) {
    c(tb$rows, tb$cols,
      vapply(tb$values, function(v) v$var %||% NA_character_, character(1)))
  }), use.names = FALSE)
  used_vars <- setdiff(stats::na.omit(used_vars),
                       c(names(r$theta), names(r$score), names(r[["pv"]])))
  needs_items <- any(unlist(lapply(cfg$tables, function(tb) {
    vapply(tb$values, function(v) v$stat %in% c("st_pvalue", "st_option_dist"),
           logical(1))
  })))
  cols <- unique(c(
    r$id, r$weight, unlist(r$group), unlist(r$score),
    unlist(r$theta), unlist(r$theta_se),
    if (needs_items) unlist(r$resp), used_vars
  ))

  # \u590d\u5236\u6743\u91cd/PV \u6a21\u677f:\u663e\u5f0f\u5217\u540d\u76f4\u63a5\u52a0\u5165;\u524d\u7f00\u6216 # \u6a21\u677f\u5219\u5148\u7aa5\u89c6\u8868\u5934
  # \u5c55\u5f00(csv/tsv),\u5176\u4ed6\u683c\u5f0f\u9000\u56de\u8bfb\u53d6\u5168\u90e8\u5217
  peek_header <- function() {
    ext <- tolower(tools::file_ext(cfg$data))
    if (ext %in% c("csv", "tsv", "txt")) {
      names(data.table::fread(cfg$data, nrows = 0))
    } else {
      NULL
    }
  }
  rep_spec <- unlist(r$rep_weights)
  if (!is.null(rep_spec)) {
    if (length(rep_spec) > 1) {
      cols <- unique(c(cols, rep_spec))
    } else if (!is.null(cols)) {
      hdr <- peek_header()
      if (is.null(hdr)) {
        cols <- NULL
      } else {
        hits <- grep(paste0("^", rep_spec, "[0-9]+$"), hdr, value = TRUE)
        cols <- unique(c(cols, if (length(hits) > 0) hits else rep_spec))
      }
    }
  }
  if (!is.null(r[["pv"]]) && !is.null(cols)) { # [[ 避免部分匹配 pv_sampling
    for (dim in names(r[["pv"]])) {
      v <- as.character(unlist(r[["pv"]][[dim]]))
      if (length(v) == 1 && grepl("#", v, fixed = TRUE)) {
        hdr <- peek_header()
        if (is.null(hdr)) {
          cols <- NULL
          break
        }
        cols <- unique(c(cols, expand_pv_template(v, hdr)))
      } else {
        cols <- unique(c(cols, v))
      }
    }
  }

  say("\u8bfb\u53d6\u6570\u636e: ", cfg$data)
  data <- read_listr(cfg$data, col_select = cols)
  say("\u8bfb\u5165 ", nrow(data), " \u884c x ", ncol(data), " \u5217")

  roles_id <- r$id
  roles_group <- unlist(r$group)
  roles_weight <- unlist(r$weight)
  roles_score <- unlist(r$score)
  roles_theta <- unlist(r$theta)
  roles_theta_se <- unlist(r$theta_se)
  roles_resp <- unlist(r$resp)
  roles_repw <- unlist(r$rep_weights)
  roles_pv <- r[["pv"]]
  if (!is.null(roles_pv)) {
    roles_pv <- lapply(roles_pv, function(v) as.character(unlist(v)))
  }
  x <- lst_data(
    data,
    id = roles_id, group = roles_group, weight = roles_weight,
    score = roles_score, theta = roles_theta, theta_se = roles_theta_se,
    resp = roles_resp, key = r$key,
    rep_weights = roles_repw,
    rep_method = r$rep_method,
    fay_k = r$fay_k %||% 0.5,
    pv = roles_pv,
    pv_sampling = r[["pv_sampling"]] %||% "first"
  )

  tables <- list()
  for (tb in cfg$tables) {
    say("\u8ba1\u7b97\u8868: ", tb$name)
    values <- lapply(tb$values, build_stat_from_config)
    names(values) <- names(tb$values)
    tb_rows <- unlist(tb$rows)
    tb_cols <- unlist(tb$cols)
    tables[[tb$name]] <- lst_table(
      x,
      rows = tb_rows, cols = tb_cols,
      values = values,
      format = tb$format %||% "est_se",
      digits = tb$digits %||% 2,
      margins = isTRUE(tb$margins)
    )
  }

  outputs <- character(0)
  if (!is.null(cfg$output$xlsx)) {
    lst_to_excel(tables, cfg$output$xlsx, overwrite = TRUE)
    outputs <- c(outputs, cfg$output$xlsx)
    say("\u5df2\u5199\u51fa Excel: ", cfg$output$xlsx)
  }
  if (!is.null(cfg$output$json)) {
    lst_to_json(tables, cfg$output$json)
    outputs <- c(outputs, cfg$output$json)
    say("\u5df2\u5199\u51fa JSON: ", cfg$output$json)
  }
  if (!is.null(cfg$output$html)) {
    lst_to_html(tables, cfg$output$html)
    outputs <- c(outputs, cfg$output$html)
    say("\u5df2\u5199\u51fa HTML: ", cfg$output$html)
  }

  log <- list(
    data_file = cfg$data, n_rows = nrow(data),
    columns_read = cols, tables = names(tables), outputs = outputs,
    elapsed_secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    timestamp = format(t0, "%Y-%m-%d %H:%M:%S")
  )
  invisible(list(tables = tables, log = log))
}

# 配置条目 -> st_* 统计量对象
build_stat_from_config <- function(v) {
  breaks <- v$breaks
  if (is.list(breaks)) {
    breaks <- unlist(breaks)
  }
  vv <- v$var
  switch(v$stat,
    st_mean = st_mean(vv),
    st_sd = st_sd(vv),
    st_prop_above = st_prop_above(
      vv, cutoff = v$cutoff,
      method = v$method %||% "hard",
      correction = v$correction %||% "none", rho = v$rho
    ),
    st_level_prop = st_level_prop(
      vv, breaks = breaks,
      method = v$method %||% "hard",
      correction = v$correction %||% "none", rho = v$rho
    ),
    st_quantile = st_quantile(vv, probs = v$probs %||% 0.5),
    st_count = st_count(),
    st_wcount = st_wcount(),
    st_pvalue = st_pvalue(items = v$items),
    st_option_dist = st_option_dist(items = v$items),
    rlang::abort(paste0("\u672a\u77e5\u7edf\u8ba1\u91cf: ", v$stat))
  )
}
