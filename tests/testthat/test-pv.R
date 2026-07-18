mk_pv_data <- function(n = 500, m_pv = 5, rho = 0.735, seed = 21) {
  # rho = 1/(1+0.36): x = theta + e, e ~ N(0, 0.6^2)
  set.seed(seed)
  theta <- rnorm(n)
  x_obs <- theta + rnorm(n, 0, 0.6)
  post_mean <- rho * x_obs
  post_sd <- sqrt(1 - rho)
  d <- data.frame(sid = seq_len(n),
                  region = sample(c("甲", "乙"), n, TRUE),
                  w = runif(n, 0.5, 2))
  for (m in seq_len(m_pv)) {
    d[[paste0("PV", m, "MATH")]] <- post_mean + rnorm(n, 0, post_sd)
  }
  attr(d, "theta") <- theta
  d
}

test_that("PV 模板展开", {
  nms <- c("PV1MATH", "PV2MATH", "PV10MATH", "PV1READ", "other")
  expect_equal(expand_pv_template("PV#MATH", nms),
               c("PV1MATH", "PV2MATH", "PV10MATH"))
  expect_equal(expand_pv_template("PV#READ", nms), "PV1READ")
  expect_null(expand_pv_template("PVMATH", nms))
})

test_that("compute_stat_pv 手算 Rubin 合并", {
  spec <- structure(list(type = "mean", var_quo = NULL, is_prop = FALSE,
                         params = list()), class = "listr_stat")
  x1 <- c(1, 2, 3, 4)
  x2 <- c(2, 3, 4, 5)
  w <- rep(1, 4)
  sd_df <- data.frame(p1 = x1, p2 = x2, w = w)
  r <- compute_stat_pv(spec, sd_df, npv = 2, NULL, "first")
  e1 <- mean(x1)
  e2 <- mean(x2)
  expect_equal(r$estimate, mean(c(e1, e2)))
  b <- stats::var(c(e1, e2))
  expect_equal(r$se_measurement^2, (1 + 1 / 2) * b)
  u1 <- compute_stat(spec, x1, w)$se_sampling^2
  expect_equal(r$se_sampling^2, u1) # pv_sampling = "first"
  ra <- compute_stat_pv(spec, sd_df, npv = 2, NULL, "average")
  u2 <- compute_stat(spec, x2, w)$se_sampling^2
  expect_equal(ra$se_sampling^2, mean(c(u1, u2)))
})

test_that("lst_data 声明 pv:模板、校验与 prob 拒绝", {
  d <- mk_pv_data()
  x <- lst_data(d, id = sid, group = region, weight = w,
                pv = list(math = "PV#MATH"))
  expect_equal(x$roles$pv$math, paste0("PV", 1:5, "MATH"))
  expect_error(
    lst_data(d, id = sid, pv = list(math = "PV#SCI")),
    "不足 2 个"
  )
  expect_error(
    lst_table(x, rows = region, values = list(
      p = st_prop_above(math, cutoff = 1, method = "prob")
    )),
    "prob"
  )
})

test_that("lst_table PV 路径:均值/等级/margins 与 meta", {
  d <- mk_pv_data()
  x <- lst_data(d, id = sid, group = region, weight = w,
                pv = list(math = "PV#MATH"))
  brk <- c(低 = -Inf, 中 = -0.5, 高 = 0.8)
  tab <- lst_table(x, rows = region, values = list(
    均值 = st_mean(math),
    等级 = st_level_prop(math, breaks = brk),
    人数 = st_count()
  ), margins = TRUE)
  long <- as_long(tab)
  expect_equal(tab$meta$均值$variance, "rubin+linearized")
  expect_equal(tab$meta$均值$n_pv, 5)
  m_rows <- long$statistic == "均值"
  expect_true(all(long$se_measurement[m_rows] > 0)) # 插补分量
  expect_true(all(long$se_sampling[m_rows] > 0))
  lv <- long[long$statistic == "等级" & long$region == "甲", ]
  expect_equal(sum(lv$estimate), 1, tolerance = 1e-9)
  expect_true("合计" %in% long$region)
})

test_that("PV + replicate weights 组合(PISA 完整方差)", {
  d <- mk_pv_data(n = 300)
  for (r in 1:8) d[[paste0("W_FSTR", r)]] <- d$w * runif(300, 0.5, 1.5)
  x <- lst_data(d, id = sid, group = region, weight = w,
                pv = list(math = "PV#MATH"),
                rep_weights = "W_FSTR", rep_method = "fay")
  tab <- lst_table(x, rows = region, values = list(m = st_mean(math)))
  expect_equal(tab$meta$m$variance, "rubin+replicate:fay")
  long <- as_long(tab)
  expect_true(all(long$se_sampling > 0))
  expect_true(all(long$se_measurement > 0))
})

test_that("Rubin 总方差蒙特卡洛覆盖率", {
  skip_on_cran()
  spec <- structure(list(type = "mean", var_quo = NULL, is_prop = FALSE,
                         params = list()), class = "listr_stat")
  set.seed(31)
  n <- 400
  m_pv <- 5
  rho <- 1 / (1 + 0.36)
  sims <- replicate(300, {
    theta <- rnorm(n)
    x_obs <- theta + rnorm(n, 0, 0.6)
    pvs <- sapply(seq_len(m_pv), function(m) {
      rho * x_obs + rnorm(n, 0, sqrt(1 - rho))
    })
    r <- compute_stat_pv(spec, data.frame(pvs, w = rep(1, n)), m_pv,
                         NULL, "first")
    c(r$estimate, r$se_total)
  })
  covered <- mean(abs(sims[1, ]) < 1.96 * sims[2, ])
  ratio <- mean(sims[2, ]) / stats::sd(sims[1, ])
  expect_gt(covered, 0.90)
  expect_gt(ratio, 0.8)
  expect_lt(ratio, 1.35)
})

test_that("lst_to_html 生成完整报告并可写盘", {
  d <- mk_pv_data(n = 200)
  x <- lst_data(d, id = sid, group = region, weight = w,
                pv = list(math = "PV#MATH"))
  tab <- lst_table(x, rows = region, values = list(均值 = st_mean(math)))
  html <- lst_to_html(tab, title = "测试报告")
  expect_true(grepl("<table>", html, fixed = TRUE))
  expect_true(grepl("测试报告", html, fixed = TRUE))
  expect_true(grepl("rubin", html, fixed = TRUE))
  f <- tempfile(fileext = ".html")
  lst_to_html(tab, f)
  expect_true(file.exists(f))
})

test_that("配置层 PV + HTML 输出端到端", {
  d <- mk_pv_data(n = 200)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  html <- tempfile(fileext = ".html")
  cfg <- list(
    data = csv,
    roles = list(id = "sid", weight = "w", group = list("region"),
                 pv = list(math = "PV#MATH")),
    tables = list(list(name = "t", rows = list("region"),
                       values = list(m = list(stat = "st_mean",
                                              var = "math")))),
    output = list(html = html)
  )
  res <- lst_run(cfg, quiet = TRUE)
  expect_true(file.exists(html))
  expect_equal(res$tables$t$meta$m$variance, "rubin+linearized")
})
