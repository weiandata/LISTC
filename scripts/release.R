# v1.0 发布流水线:文档 -> 测试 -> as-cran 检查 -> 覆盖率 -> 打包
# 运行: Rscript scripts/release.R
# 全部通过后在包根目录生成 LISTC_<version>.tar.gz,可直接上传 CRAN。

stopifnot(requireNamespace("devtools", quietly = TRUE))

message("== 1/5 devtools::document() ==")
devtools::document(quiet = TRUE)

message("== 2/5 devtools::test() ==")
res <- devtools::test(stop_on_failure = TRUE)

message("== 3/5 devtools::check(--as-cran) ==")
chk <- devtools::check(args = c("--as-cran"), quiet = TRUE)
if (length(chk$errors) > 0 || length(chk$warnings) > 0) {
  print(chk)
  stop("check 未通过(存在 error/warning),终止发布。")
}
if (length(chk$notes) > 0) {
  message("check NOTES(请人工确认是否可接受):")
  print(chk$notes)
}

message("== 4/5 覆盖率阈值 ==")
source("scripts/coverage.R") # 未达标会以非零状态退出

message("== 5/5 devtools::build() ==")
tarball <- devtools::build(path = ".")
message("\n发布包已生成: ", tarball)
message("上传 CRAN: https://cran.r-project.org/submit.html")
message("随附说明: cran-comments.md")
