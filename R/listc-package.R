#' @keywords internal
"_PACKAGE"

#' @importFrom stats pnorm dnorm weighted.mean quantile complete.cases
#' @importFrom rlang abort enquo enquos
#' @importFrom data.table := .N .SD .BY
NULL

# data.table 感知声明
.datatable.aware <- TRUE

# data.table NSE 列名(避免 R CMD check 的未定义全局变量提示)
utils::globalVariables(c(
  ".lstw", ".lstx", ".lstopt", ".cell", ".colkey", ".lstrow",
  "statistic", "category", "estimate", "se_sampling", "se_measurement",
  "se_total", "n_opt", "sum_w", "sum_w_opt"
))
