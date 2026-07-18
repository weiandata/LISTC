# Config layer: one mechanism serving both non-R users and AI agents.
# Schema: inst/schema/config.schema.json. See design doc section 1.1.

VALID_STATS <- c("st_mean", "st_prop_above", "st_level_prop", "st_quantile",
                 "st_sd", "st_count", "st_wcount", "st_pvalue",
                 "st_option_dist")

#' Read and validate a LISTR run configuration
#'
#' Accepts a YAML/JSON file path, a YAML/JSON string, or a named list.
#' Validation errors are reported in plain Chinese pointing to the
#' offending field. (Excel configuration workbooks arrive in v0.2.)
#'
#' @param config Path to .yml/.yaml/.json, a YAML/JSON string, or a list.
#' @return A validated `listr_config` object.
#' @examples
#' cfg <- lst_config(list(
#'   data = "students.csv",
#'   roles = list(id = "id", weight = "w"),
#'   tables = list(list(name = "t1",
#'                      values = list(n = list(stat = "st_count"))))
#' ))
#' class(cfg)
#' @export
lst_config <- function(config) {
  cfg <- parse_config(config)
  # --- 校验,错误信息面向非技术用户 ---
  need <- function(cond, msg) if (!isTRUE(cond)) rlang::abort(msg)
  need(is.list(cfg), "\u914d\u7f6e\u5fc5\u987b\u662f\u4e00\u7ec4\u952e\u503c(YAML/JSON \u5bf9\u8c61)\u3002")
  need(!is.null(cfg$data) && is.character(cfg$data),
       "\u914d\u7f6e\u7f3a\u5c11 data:\u8bf7\u586b\u5199\u6570\u636e\u6587\u4ef6\u8def\u5f84\u3002")
  need(is.list(cfg$roles),
       "\u914d\u7f6e\u7f3a\u5c11 roles:\u8bf7\u8bf4\u660e\u6bcf\u4e2a\u53d8\u91cf\u7684\u89d2\u8272(id\u3001weight\u3001group \u7b49)\u3002")
  need(!is.null(cfg$roles$id),
       "roles \u91cc\u7f3a\u5c11 id:\u8bf7\u586b\u5199\u6837\u672c\u7f16\u53f7\u5217\u7684\u5217\u540d\u3002")
  if (!is.null(cfg$roles$theta) || !is.null(cfg$roles$theta_se)) {
    nt <- names(cfg$roles$theta)
    ns <- names(cfg$roles$theta_se)
    need(!is.null(nt) && !is.null(ns) && setequal(nt, ns),
         paste0("roles \u91cc\u7684 theta \u4e0e theta_se \u5fc5\u987b\u4f7f\u7528\u76f8\u540c\u7684\u7ef4\u5ea6\u540d,",
                "\u4f8b\u5982 theta: {math: th_math} \u914d theta_se: {math: se_math}\u3002"))
  }
  if (!is.null(cfg$roles$rep_weights)) {
    need(!is.null(cfg$roles$rep_method) &&
           cfg$roles$rep_method %in% c("fay", "brr", "jk1", "jk2"),
         paste0("\u4f7f\u7528\u590d\u5236\u6743\u91cd(rep_weights)\u5fc5\u987b\u540c\u65f6\u586b\u5199 rep_method,",
                "\u53ef\u9009: fay(PISA)\u3001brr\u3001jk1\u3001jk2(TIMSS)\u3002"))
  }
  # \u6ce8\u610f\u7528 [[ \u7cbe\u786e\u53d6\u503c:$pv \u4f1a\u90e8\u5206\u5339\u914d\u5230 pv_sampling
  if (!is.null(cfg$roles[["pv"]])) {
    need(is.list(cfg$roles[["pv"]]) && !is.null(names(cfg$roles[["pv"]])),
         paste0("roles \u91cc\u7684 pv \u5fc5\u987b\u662f\u547d\u540d\u6620\u5c04:\u7ef4\u5ea6\u540d -> PV \u5217,",
                "\u5982 pv: {math: \"PV#MATH\"}\u3002"))
  }
  if (!is.null(cfg$roles[["pv_sampling"]])) {
    need(cfg$roles[["pv_sampling"]] %in% c("first", "average"),
         "pv_sampling \u53ea\u80fd\u662f first \u6216 average\u3002")
  }
  need(is.list(cfg$tables) && length(cfg$tables) > 0,
       "\u914d\u7f6e\u7f3a\u5c11 tables:\u8bf7\u81f3\u5c11\u5b9a\u4e49\u4e00\u5f20\u7edf\u8ba1\u8868\u3002")
  for (i in seq_along(cfg$tables)) {
    tb <- cfg$tables[[i]]
    where <- paste0("\u7b2c ", i, " \u5f20\u8868")
    need(!is.null(tb$name), paste0(where, " \u7f3a\u5c11 name(\u8868\u540d)\u3002"))
    need(is.list(tb$values) && length(tb$values) > 0 &&
           !is.null(names(tb$values)),
         paste0(where, "(", tb$name %||% "", ")\u7f3a\u5c11 values:",
                "\u8bf7\u5b9a\u4e49\u8981\u8ba1\u7b97\u7684\u7edf\u8ba1\u91cf,\u5e76\u7ed9\u6bcf\u4e2a\u7edf\u8ba1\u91cf\u547d\u540d\u3002"))
    for (vn in names(tb$values)) {
      v <- tb$values[[vn]]
      stat <- v$stat
      need(!is.null(stat) && stat %in% VALID_STATS,
           paste0(where, " \u7684\u7edf\u8ba1\u91cf \"", vn, "\" \u7684 stat \u65e0\u6548: ",
                  stat %||% "(\u672a\u586b\u5199)", "\u3002\u53ef\u9009: ",
                  paste(VALID_STATS, collapse = ", ")))
      if (stat %in% c("st_mean", "st_sd", "st_prop_above", "st_level_prop",
                      "st_quantile")) {
        need(!is.null(v$var),
             paste0(where, " \u7684\u7edf\u8ba1\u91cf \"", vn, "\" \u7f3a\u5c11 var(\u7b97\u54ea\u4e2a\u53d8\u91cf)\u3002"))
      }
      if (stat == "st_prop_above") {
        need(is.numeric(v$cutoff),
             paste0(where, " \u7684\u7edf\u8ba1\u91cf \"", vn, "\" \u7f3a\u5c11\u6570\u503c\u578b cutoff(\u9608\u503c)\u3002"))
      }
      if (stat == "st_level_prop") {
        need(is.list(v$breaks) || (is.numeric(v$breaks) &&
                                     !is.null(names(v$breaks))),
             paste0(where, " \u7684\u7edf\u8ba1\u91cf \"", vn,
                    "\" \u7f3a\u5c11 breaks(\u7b49\u7ea7\u540d: \u4e0b\u754c)\u3002"))
      }
    }
    if (!is.null(tb$format)) {
      need(tb$format %in% c("est", "est_se", "est_ci", "percent"),
           paste0(where, " \u7684 format \u65e0\u6548: ", tb$format,
                  "\u3002\u53ef\u9009: est, est_se, est_ci, percent\u3002"))
    }
  }
  structure(cfg, class = "listr_config")
}

parse_config <- function(config) {
  if (inherits(config, "listr_config")) {
    return(unclass(config))
  }
  if (is.list(config)) {
    return(config)
  }
  if (is.character(config) && length(config) == 1) {
    if (file.exists(config)) {
      ext <- tolower(tools::file_ext(config))
      return(switch(ext,
        yml = ,
        yaml = yaml::read_yaml(config),
        json = jsonlite::fromJSON(config, simplifyVector = TRUE,
                                  simplifyDataFrame = FALSE),
        xlsx = parse_config_xlsx(config),
        rlang::abort(paste0("\u65e0\u6cd5\u8bc6\u522b\u7684\u914d\u7f6e\u6587\u4ef6\u683c\u5f0f: .", ext))
      ))
    }
    if (grepl("^\\s*\\{", config)) {
      return(jsonlite::fromJSON(config, simplifyVector = TRUE,
                                simplifyDataFrame = FALSE))
    }
    return(yaml::yaml.load(config))
  }
  rlang::abort("config \u5fc5\u987b\u662f\u914d\u7f6e\u6587\u4ef6\u8def\u5f84\u3001YAML/JSON \u6587\u672c\u6216\u5217\u8868\u3002")
}
