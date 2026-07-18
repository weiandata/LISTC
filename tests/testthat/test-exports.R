mk_tab <- function(n = 200, seed = 51, values = NULL) {
  set.seed(seed)
  d <- data.frame(sid = seq_len(n),
                  region = sample(c("甲", "乙"), n, TRUE),
                  gender = sample(c("男", "女"), n, TRUE),
                  w = runif(n, 0.5, 2), th = rnorm(n),
                  se = runif(n, .2, .4))
  x <- lst_data(d, id = sid, group = c(region, gender), weight = w,
                theta = c(math = th), theta_se = c(math = se))
  values <- values %||% list(均值 = st_mean(math))
  lst_table(x, rows = region, cols = gender, values = values)
}

test_that("lst_to_excel:多表、样式覆盖、interpret 开关", {
  tabs <- list(表一 = mk_tab(), 表二 = mk_tab(seed = 52))
  f <- tempfile(fileext = ".xlsx")
  lst_to_excel(tabs, f, style = list(font = "宋体", font_size = 10,
                                     header_fill = "#EEEEEE"),
               overwrite = TRUE)
  expect_true(file.exists(f))
  sheets <- readxl::excel_sheets(f)
  expect_true(all(c("表一", "表二", "结论") %in% sheets))
  # interpret = FALSE 时无结论 sheet
  f2 <- tempfile(fileext = ".xlsx")
  lst_to_excel(mk_tab(), f2, interpret = FALSE, overwrite = TRUE)
  expect_false("结论" %in% readxl::excel_sheets(f2))
  # 单表默认 sheet 名"结果"
  expect_true("结果" %in% readxl::excel_sheets(f2))
  # overwrite = FALSE 报错由 openxlsx 触发
  expect_error(lst_to_excel(mk_tab(), f2, overwrite = FALSE))
})

test_that("normalize_tabs 的输入校验与自动命名", {
  expect_error(lst_to_excel(list(1, 2), tempfile()), "listc_table")
  expect_error(lst_to_excel("x", tempfile()), "listc_table")
  tabs <- list(mk_tab(), mk_tab(seed = 53)) # 未命名列表
  f <- tempfile(fileext = ".xlsx")
  lst_to_excel(tabs, f, overwrite = TRUE)
  expect_true(all(c("表1", "表2") %in% readxl::excel_sheets(f)))
})

test_that("lst_to_json:字符串返回与写盘、元数据完整", {
  tab <- mk_tab()
  js <- lst_to_json(tab, pretty = FALSE)
  parsed <- jsonlite::fromJSON(js)
  expect_equal(parsed$package, "LISTC")
  expect_equal(parsed$tables$name, "结果")
  f <- tempfile(fileext = ".json")
  lst_to_json(tab, f)
  expect_true(file.exists(f))
  parsed2 <- jsonlite::fromJSON(f)
  expect_true("interpretation" %in% names(parsed2$tables))
})

test_that("lst_to_html:interpret 开关与转义", {
  tab <- mk_tab()
  h1 <- lst_to_html(tab, interpret = FALSE)
  expect_false(grepl("自动解读", h1, fixed = TRUE))
  h2 <- lst_to_html(tab, title = "A<B&C")
  expect_true(grepl("A&lt;B&amp;C", h2, fixed = TRUE))
})

test_that("lst_interpret 分支:不显著、空规则、小样本", {
  # 只有 count 的表 -> 无适用规则
  tab_count <- mk_tab(values = list(人数 = st_count()))
  expect_true(any(grepl("暂无", lst_interpret(tab_count))))
  # 组间差异确定性地小于 2 倍合并 SE -> 不显著分支
  n <- 400
  d <- data.frame(
    sid = seq_len(n), g = rep(c("A", "B"), each = n / 2),
    y = c(rep(c(-1, 1), n / 4), rep(c(-1, 1), n / 4) + 0.001)
  )
  x <- lst_data(d, id = sid, group = g)
  tab_ns <- lst_table(x, rows = g, values = list(m = st_mean(y)))
  expect_true(any(grepl("不能认为存在显著差异", lst_interpret(tab_ns))))
  # 小样本警告
  d2 <- data.frame(sid = 1:40, g = c(rep("A", 35), rep("B", 5)),
                   y = rnorm(40))
  x2 <- lst_data(d2, id = sid, group = g)
  tab_small <- lst_table(x2, rows = g, values = list(m = st_mean(y)))
  expect_true(any(grepl("不足 30", lst_interpret(tab_small))))
})

test_that("print 方法:listc_data 与 listc_table", {
  set.seed(55)
  d <- data.frame(sid = 1:50, region = "甲", w = 1, th = rnorm(50),
                  se = runif(50, .2, .4), q1 = rbinom(50, 1, .5),
                  raw = rpois(50, 30))
  x <- lst_data(d, id = sid, group = region, weight = w,
                theta = c(math = th), theta_se = c(math = se),
                score = raw, resp = q1, key = list(q1 = 1))
  out <- capture.output(print(x))
  expect_true(any(grepl("listc_data", out)))
  expect_true(any(grepl("math=th", out)))
  expect_true(any(grepl("题目列", out)))
  tab <- lst_table(x, rows = region, values = list(m = st_mean(math)))
  out2 <- capture.output(print(tab))
  expect_true(any(grepl("listc_table", out2)))
})

test_that("as_wide 形态:无行变量、est/est_ci/percent 格式、命名 digits", {
  set.seed(56)
  d <- data.frame(sid = 1:100, g = sample(c("A", "B"), 100, TRUE),
                  y = rnorm(100), w = 1)
  x <- lst_data(d, id = sid, group = g, weight = w)
  # 无行变量 -> "总体"行
  tab0 <- lst_table(x, values = list(m = st_mean(y)))
  w0 <- as_wide(tab0)
  expect_equal(nrow(w0), 1)
  expect_equal(w0[[1]][1], "总体")
  # est_ci 格式
  tab_ci <- lst_table(x, rows = g, values = list(m = st_mean(y)),
                      format = "est_ci")
  expect_true(any(grepl("\\[", as_wide(tab_ci)$m)))
  # est 格式(无括号)
  tab_e <- lst_table(x, rows = g, values = list(m = st_mean(y)),
                     format = "est")
  expect_false(any(grepl("\\(", as_wide(tab_e)$m)))
  # 占比 percent 显示 + 命名 digits + count 取整
  tab_p <- lst_table(x, rows = g, values = list(
    p = st_prop_above(y, cutoff = 0),
    n = st_count()
  ), format = "percent", digits = c(p = 1))
  wp <- as_wide(tab_p)
  expect_true(any(grepl("%", wp$p)))
  expect_false(any(grepl("\\.", wp$n))) # count 0 位小数
})

test_that("format_cell 与 fmt_num 边界", {
  expect_equal(format_cell(1.5, 0.2, "est", 1), "1.5")
  expect_true(grepl("\\[", format_cell(1.5, 0.2, "est_ci", 1)))
  expect_error(format_cell(1, 1, "nope", 2), "format")
  expect_equal(fmt_num(NA_real_, 2), "")
  expect_equal(display_width("中文ab"), 6)
})
