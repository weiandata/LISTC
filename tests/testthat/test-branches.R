# 分支与错误路径覆盖(覆盖率目标:总体>90%,核心文件>95%)

test_that("lst_data 校验错误分支", {
  d <- data.frame(sid = 1:10, w = runif(10), th = rnorm(10),
                  se = runif(10, .1, .3), g = "A",
                  ch = letters[1:10])
  expect_error(lst_data("nope"), "data.frame")
  expect_error(lst_data(d, id = sid, weight = ch), "数值")
  d_neg <- transform(d, w = -w)
  expect_error(lst_data(d_neg, id = sid, weight = w), "负值")
  d_sena <- transform(d, se = -se)
  expect_error(lst_data(d_sena, id = sid, theta = c(m = th),
                        theta_se = c(m = se)), "负值")
  expect_error(lst_data(d, id = sid, theta = c(m = ch),
                        theta_se = c(m = se)), "数值")
  expect_error(lst_data(d, id = sid, key = list(q = 1)), "resp")
  expect_error(lst_data(d, id = sid, rep_weights = c(w),
                        rep_method = "fay", fay_k = 2), "fay_k")
  expect_error(lst_data(d, id = sid, rep_weights = ch,
                        rep_method = "fay"), "数值")
  expect_error(lst_data(d, id = nope_col), "不存在")
  # id 缺失 -> 提示而非报错
  d_na <- d
  d_na$sid[1] <- NA
  expect_message(lst_data(d_na, id = sid), "缺失")
})

test_that("pv 声明错误分支", {
  d <- data.frame(sid = 1:10, PV1M = rnorm(10), PV2M = rnorm(10),
                  th = rnorm(10), se = runif(10, .1, .3),
                  chr1 = letters[1:10], chr2 = letters[1:10])
  expect_error(lst_data(d, id = sid, pv = c("PV1M", "PV2M")), "命名")
  expect_error(lst_data(d, id = sid, pv = list(m = "PV1M")), "至少需要 2")
  expect_error(lst_data(d, id = sid, pv = list(m = c("PV1M", "NOPE"))),
               "不存在")
  expect_error(lst_data(d, id = sid, pv = list(m = c("chr1", "chr2"))),
               "数值")
  expect_error(
    lst_data(d, id = sid, theta = c(m = th), theta_se = c(m = se),
             pv = list(m = c("PV1M", "PV2M"))),
    "不同的维度名"
  )
})

test_that("read_listr 各格式与标签转换", {
  d <- data.frame(a = 1:5, b = letters[1:5])
  # xlsx 往返
  fx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, fx)
  r1 <- read_listr(fx)
  expect_equal(nrow(r1), 5)
  r1b <- read_listr(fx, col_select = "a")
  expect_equal(names(r1b), "a")
  # sav 往返 + 值标签转 factor + col_select
  fs <- tempfile(fileext = ".sav")
  ds <- data.frame(x = 1:4)
  ds$lab <- haven::labelled(c(1, 2, 1, 2), labels = c(男 = 1, 女 = 2))
  haven::write_sav(ds, fs)
  r2 <- read_listr(fs)
  expect_true(is.factor(r2$lab))
  expect_equal(levels(r2$lab), c("男", "女"))
  r2b <- read_listr(fs, col_select = "x")
  expect_equal(names(r2b), "x")
  # dta 往返
  fd <- tempfile(fileext = ".dta")
  haven::write_dta(data.frame(y = c(1.5, 2.5)), fd)
  expect_equal(nrow(read_listr(fd)), 2)
})

test_that("read-irt 错误与回退分支", {
  expect_error(read_winsteps_pfile("no-such-file"), "找不到")
  f1 <- tempfile()
  writeLines(c("; comment only", "no header here"), f1)
  expect_error(read_winsteps_pfile(f1), "MEASURE")
  f2 <- tempfile()
  writeLines(";ENTRY MEASURE ERROR NAME", f2) # 只有表头
  expect_error(read_winsteps_pfile(f2), "数据行")
  f3 <- tempfile()
  writeLines(c(";ENTRY MEASURE COUNT", "1 0.5 20"), f3) # 缺 SE 列
  expect_error(read_winsteps_pfile(f3), "MEASURE 与 ERROR")
  # 无 NAME 列 -> ENTRY 作 id;列覆盖参数
  f4 <- tempfile()
  writeLines(c(";ENTRY MEASURE ERROR", "7 0.5 0.4"), f4)
  r <- read_winsteps_pfile(f4)
  expect_equal(r$id[1], "7")
  r2 <- read_winsteps_pfile(f4, id_col = "ENTRY", theta_col = "MEASURE",
                            se_col = "ERROR")
  expect_equal(r2$theta, 0.5)
  # ConQuest 错误分支
  expect_error(read_conquest_person("no-such"), "找不到")
  expect_error(read_conquest_person(f4, cols = c(id = 2)), "三个位置")
  f5 <- tempfile()
  writeLines("header only line", f5)
  expect_error(read_conquest_person(f5), "解析")
  # join 错误分支
  d <- data.frame(sid = 1:3, g = "A")
  x <- lst_data(d, id = sid, group = g)
  expect_error(lst_join_person(x, data.frame(id = 1)), "三列")
  bad <- data.frame(id = 99:101, theta = 1:3, theta_se = rep(.3, 3))
  expect_error(lst_join_person(x, bad), "对不上")
  x2 <- lst_data(d, group = g)
  ok <- data.frame(id = 1:3, theta = 1:3, theta_se = rep(.3, 3))
  expect_error(lst_join_person(x2, ok), "声明 id")
})

test_that("lst_table 输入校验与 rep 特殊统计量", {
  d <- data.frame(sid = 1:60, g = rep(c("A", "B"), 30),
                  y = rnorm(60), w = 1,
                  rw1 = runif(60, .5, 1.5), rw2 = runif(60, .5, 1.5))
  x <- lst_data(d, id = sid, group = g, weight = w,
                rep_weights = c(rw1, rw2), rep_method = "jk2")
  expect_error(lst_table("x", values = list(m = st_mean(y))), "listr_data")
  expect_error(lst_table(x, values = list(st_mean(y))), "命名")
  expect_error(lst_table(x, values = list(m = 1)), "st_")
  expect_error(lst_table(x, values = list(m = st_mean(nope))), "解析")
  # rep 路径下 count/sd/quantile/wcount 全部可跑
  tab <- lst_table(x, rows = g, values = list(
    n = st_count(), wn = st_wcount(), s = st_sd(y),
    q = st_quantile(y, probs = c(.25, .75))
  ))
  long <- as_long(tab)
  expect_equal(sort(unique(long$statistic)), sort(c("n", "wn", "s", "q")))
  expect_true(all(long$se_sampling[long$statistic == "q"] >= 0))
  # 引擎内部错误分支
  expect_error(
    compute_stat_rep(structure(list(type = "mean", params = list()),
                               class = "listr_stat"),
                     1:5, matrix(1, 5, 1), NULL, 1),
    "至少 1 个"
  )
  expect_error(
    est_stat(structure(list(type = "nope", params = list()),
                       class = "listr_stat"), 1:5, rep(1, 5)),
    "不支持"
  )
})

test_that("prob 统计缺 se 的报错与 latent 显式 rho", {
  d <- data.frame(sid = 1:50, g = "A", y = rnorm(50), w = 1,
                  th = rnorm(50), se = runif(50, .2, .4))
  x <- lst_data(d, id = sid, group = g, weight = w,
                theta = c(m = th), theta_se = c(m = se))
  # y 无配对 se -> prob 报错
  expect_error(
    lst_table(x, rows = g, values = list(
      p = st_prop_above(y, cutoff = 0, method = "prob")
    )),
    "标准误"
  )
  expect_error(
    lst_table(x, rows = g, values = list(
      p = st_prop_above(y, cutoff = 0, method = "prob",
                        correction = "latent")
    )),
    "标准误"
  )
  # latent 显式 rho
  tab <- lst_table(x, rows = g, values = list(
    p = st_prop_above(m, cutoff = 1, method = "prob",
                      correction = "latent", rho = 0.8)
  ))
  expect_equal(tab$meta$p$rho, 0.8)
})

test_that("lst_classify/lst_above 自定义参数与错误", {
  d <- data.frame(sid = 1:20, y = seq(-2, 2, length.out = 20))
  x <- lst_data(d, id = sid)
  expect_error(lst_classify(x, y, breaks = c(1, 2)), "命名")
  x2 <- lst_classify(x, y, c(低 = -Inf, 高 = 0),
                     labels = c("L", "H"), name = "lv")
  expect_equal(levels(x2$data$lv), c("L", "H"))
  x3 <- lst_above(x, y, 0, name = "above0")
  expect_true("above0" %in% names(x3$data))
  expect_error(lst_derive(x, y + 1), "命名")
})

test_that("config 解析分支:passthrough/json/yaml 文件/坏扩展名", {
  cfg_list <- list(
    data = "a.csv", roles = list(id = "sid"),
    tables = list(list(name = "t",
                       values = list(m = list(stat = "st_count"))))
  )
  c1 <- lst_config(cfg_list)
  expect_s3_class(c1, "listr_config")
  expect_identical(unclass(lst_config(c1)), unclass(c1)) # passthrough
  # json 字符串与文件
  js <- jsonlite::toJSON(cfg_list, auto_unbox = TRUE)
  expect_s3_class(lst_config(as.character(js)), "listr_config")
  fj <- tempfile(fileext = ".json")
  writeLines(js, fj)
  expect_s3_class(lst_config(fj), "listr_config")
  # yaml 文件
  fy <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg_list, fy)
  expect_s3_class(lst_config(fy), "listr_config")
  # 坏扩展名与坏类型
  fb <- tempfile(fileext = ".docx")
  file.create(fb)
  expect_error(lst_config(fb), "无法识别")
  expect_error(lst_config(42), "必须是")
  # 更多校验分支
  bad <- cfg_list
  bad$tables[[1]]$values$m <- list(stat = "st_mean") # 缺 var
  expect_error(lst_config(bad), "var")
  bad2 <- cfg_list
  bad2$tables[[1]]$values$m <- list(stat = "st_prop_above", var = "y")
  expect_error(lst_config(bad2), "cutoff")
  bad3 <- cfg_list
  bad3$tables[[1]]$values$m <- list(stat = "st_level_prop", var = "y")
  expect_error(lst_config(bad3), "breaks")
  bad4 <- cfg_list
  bad4$tables[[1]]$format <- "fancy"
  expect_error(lst_config(bad4), "format 无效")
  bad5 <- cfg_list
  bad5$tables[[1]]$name <- NULL
  expect_error(lst_config(bad5), "name")
  bad6 <- cfg_list
  bad6$roles$rep_weights <- "W"
  expect_error(lst_config(bad6), "rep_method")
  bad7 <- cfg_list
  bad7$roles$pv <- c("PV1", "PV2")
  expect_error(lst_config(bad7), "命名")
  bad8 <- cfg_list
  bad8$roles$pv_sampling <- "middle"
  expect_error(lst_config(bad8), "pv_sampling")
})

test_that("lst_run 非 quiet 消息与 sav 输入的列回退", {
  set.seed(61)
  n <- 80
  d <- data.frame(sid = 1:n, g = sample(c("A", "B"), n, TRUE),
                  w = runif(n, .5, 2), th = rnorm(n))
  for (r in 1:4) d[[paste0("RW", r)]] <- d$w * runif(n, .5, 1.5)
  fs <- tempfile(fileext = ".sav")
  haven::write_sav(d, fs)
  cfg <- list(
    data = fs,
    roles = list(id = "sid", weight = "w", group = list("g"),
                 rep_weights = "RW", rep_method = "brr"),
    tables = list(list(name = "t", rows = list("g"),
                       values = list(m = list(stat = "st_mean",
                                              var = "th"))))
  )
  msgs <- capture.output(res <- lst_run(cfg, quiet = FALSE),
                         type = "message")
  expect_true(any(grepl("读取数据", msgs)))
  expect_equal(res$tables$t$meta$m$variance, "replicate:brr")
  expect_true(res$log$n_rows == n)
})

test_that("配置模板可复制(安装后可用)", {
  src <- system.file("templates", "config-template.xlsx",
                     package = "LISTR")
  skip_if(src == "", "模板未随包安装(load_all 环境)")
  f <- file.path(tempdir(), "模板副本.xlsx")
  if (file.exists(f)) file.remove(f)
  lst_config_template(f)
  expect_true(file.exists(f))
  expect_error(lst_config_template(f), "已存在")
  lst_config_template(f, overwrite = TRUE)
})

test_that("score_item:无计分键时要求 0/1 计分", {
  d <- data.frame(sid = 1:20, g = "A", w = 1,
                  q_ok = rbinom(20, 1, .5),
                  q_bad = sample(1:5, 20, TRUE))
  x <- lst_data(d, id = sid, group = g, weight = w,
                resp = c(q_ok, q_bad))
  # q_ok 无键但已是 0/1 -> 可算
  tab <- lst_table(x, values = list(pv = st_pvalue(items = "q_ok")))
  expect_true(is.finite(as_long(tab)$estimate[1]))
  # q_bad 无键且非 0/1 -> 报错
  expect_error(
    lst_table(x, values = list(pv = st_pvalue(items = "q_bad"))),
    "0/1"
  )
})

test_that("resolve_vars 对空向量与 NULL 表达式的处理", {
  d <- data.frame(sid = 1:5, y = rnorm(5))
  # group = c() 求值为 NULL -> 角色为空而非报错
  x <- lst_data(d, id = sid, group = c())
  expect_null(x$roles$group)
  tab <- lst_table(x, values = list(m = st_mean(y)))
  expect_equal(nrow(as_wide(tab)), 1)
})

test_that("resolve_vars:字符串列名不存在走第二报错分支", {
  d <- data.frame(sid = 1:5, y = rnorm(5))
  # 字符串能正常求值,但列不在数据里 -> "中的列在数据里不存在"
  expect_error(lst_data(d, id = sid, group = "nope"),
               "中的列在数据里不存在")
  expect_error(lst_data(d, id = sid, group = c("y", "also_nope")),
               "also_nope")
})

test_that("resolve_vars/resolve_measure 错误信息", {
  d <- data.frame(a = 1, b = 2)
  x <- lst_data(d)
  expect_error(resolve_measure(x, "nope"), "找不到变量")
  expect_error(
    lst_table(x, rows = c(a, nope), values = list(n = st_count())),
    "不存在"
  )
})
