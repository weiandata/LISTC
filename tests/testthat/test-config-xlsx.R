write_config_workbook <- function(path, data_file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "说明")
  openxlsx::addWorksheet(wb, "数据与角色")
  openxlsx::writeData(wb, "数据与角色", data.frame(
    设置项 = c("数据文件", "样本编号列", "权重列", "分组列", "得分列"),
    填写值 = c(data_file, "sid", "w", "region", NA),
    说明 = NA
  ))
  openxlsx::addWorksheet(wb, "能力维度")
  openxlsx::writeData(wb, "能力维度", data.frame(
    维度名 = "math", 能力值列 = "th", 标准误列 = "se"
  ))
  openxlsx::addWorksheet(wb, "统计表")
  openxlsx::writeData(wb, "统计表", data.frame(
    表名 = c("t1", "t1"), 行变量 = c("region", NA), 列变量 = NA,
    统计量名称 = c("均值", "等级"),
    统计量 = c("st_mean", "st_level_prop"),
    变量 = c("math", "math"), 阈值 = NA,
    等级分数线 = c(NA, "低=-Inf,中=0,高=1"),
    方法 = c(NA, "prob"), 校正 = NA, 格式 = c("est_se", NA),
    小数位 = c(2, NA), 合计 = c("是", NA)
  ))
  openxlsx::addWorksheet(wb, "输出")
  openxlsx::writeData(wb, "输出", data.frame(
    设置项 = c("Excel输出路径", "JSON输出路径"),
    填写值 = NA, 说明 = NA
  ))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

test_that("Excel 配置簿端到端解析并运行", {
  skip_if_not_installed("openxlsx")
  set.seed(21)
  n <- 200
  d <- data.frame(sid = seq_len(n),
                  region = sample(c("甲", "乙"), n, TRUE),
                  w = runif(n, 0.5, 2), th = rnorm(n),
                  se = runif(n, 0.2, 0.4))
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  xlsx <- tempfile(fileext = ".xlsx")
  write_config_workbook(xlsx, csv)

  cfg <- lst_config(xlsx)
  expect_s3_class(cfg, "listr_config")
  expect_equal(cfg$roles$theta$math, "th")
  expect_equal(cfg$tables[[1]]$values$等级$breaks,
               c(低 = -Inf, 中 = 0, 高 = 1))
  expect_true(cfg$tables[[1]]$margins)

  res <- lst_run(xlsx, quiet = TRUE)
  expect_s3_class(res$tables$t1, "listr_table")
  long <- as_long(res$tables$t1)
  expect_true("合计" %in% long$region)
})

test_that("配置簿缺工作表与坏分数线的报错可读", {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "说明")
  bad <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, bad, overwrite = TRUE)
  expect_error(lst_config(bad), "缺少工作表")
  expect_error(parse_breaks_text("低-Inf"), "格式不正确")
  expect_error(parse_breaks_text("低=abc"), "无法识别")
})

test_that("st_quantile 的 Woodruff SE 接近蒙特卡洛", {
  skip_on_cran()
  set.seed(31)
  n <- 2000
  meds <- replicate(600, {
    x <- rnorm(n)
    wquantile(x, rep(1, n), 0.5)
  })
  x <- rnorm(n)
  r <- compute_stat(
    structure(list(type = "quantile", var_quo = NULL, is_prop = FALSE,
                   params = list(probs = 0.5)), class = "listr_stat"),
    x, rep(1, n)
  )
  ratio <- r$se_sampling / stats::sd(meds)
  expect_gt(ratio, 0.7)
  expect_lt(ratio, 1.4)
})

test_that("lst_interpret 覆盖等级与题目统计", {
  set.seed(41)
  n <- 400
  d <- data.frame(sid = seq_len(n),
                  region = sample(c("甲", "乙"), n, TRUE),
                  w = 1, th = rnorm(n), se = runif(n, .2, .4),
                  q1 = rbinom(n, 1, 0.9), q2 = rbinom(n, 1, 0.1))
  x <- lst_data(d, id = sid, group = region, weight = w,
                theta = c(math = th), theta_se = c(math = se),
                resp = c(q1, q2))
  tab <- lst_table(x, rows = region, values = list(
    等级 = st_level_prop(math, c(低 = -Inf, 高 = 0.8), method = "prob"),
    正答率 = st_pvalue()
  ))
  txt <- lst_interpret(tab)
  expect_true(any(grepl("占比最高的等级", txt)))
  expect_true(any(grepl("正答率最高的题目", txt)))
  expect_true(any(grepl("低于 20%", txt)))
})
