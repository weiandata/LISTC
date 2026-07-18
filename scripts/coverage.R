# 测试覆盖率检查(公司要求:总体 > 90%,核心文件 > 95%)
# 运行: Rscript scripts/coverage.R
# 退出码非 0 表示未达标,可直接接入 CI。

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("请先安装 covr: install.packages(\"covr\")")
}

# 核心文件清单(统计引擎与透视层)
key_files <- c(
  "R/utils.R", "R/spec.R", "R/stats.R", "R/stats-item.R",
  "R/variance.R", "R/variance-rep.R", "R/variance-pv.R",
  "R/table.R", "R/pivot.R"
)
threshold_total <- 90
threshold_key <- 95

cov <- covr::package_coverage(type = "tests", quiet = FALSE)
res <- covr::coverage_to_list(cov)
total <- res$totalcoverage
files <- res$filecoverage

cat(sprintf("\n== LISTC coverage ==\n总体: %.1f%% (要求 > %d%%)\n\n",
            total, threshold_total))
df <- data.frame(
  file = names(files),
  coverage = sprintf("%.1f%%", as.numeric(files)),
  key = ifelse(names(files) %in% key_files, "核心", ""),
  stringsAsFactors = FALSE
)
df <- df[order(as.numeric(files)), ]
print(df, row.names = FALSE, right = FALSE)

fail <- character(0)
if (total < threshold_total) {
  fail <- c(fail, sprintf("总体覆盖率 %.1f%% < %d%%", total, threshold_total))
}
for (f in key_files) {
  v <- files[[f]]
  if (is.null(v) || is.na(v)) {
    fail <- c(fail, paste0(f, " 无覆盖率数据"))
  } else if (v < threshold_key) {
    fail <- c(fail, sprintf("%s %.1f%% < %d%%", f, v, threshold_key))
  }
}
if (length(fail) > 0) {
  cat("\n未达标:\n")
  cat(paste0("  - ", fail, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("\n覆盖率达标。\n")
