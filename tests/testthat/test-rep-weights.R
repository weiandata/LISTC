mk_spec <- function(type, is_prop = FALSE, params = list()) {
  structure(list(type = type, var_quo = NULL, is_prop = is_prop,
                 params = params), class = "listr_stat")
}

test_that("replicate 方差因子公式", {
  expect_equal(rep_factor("fay", 80, 0.5), 0.05)
  expect_equal(rep_factor("brr", 80), 1 / 80)
  expect_equal(rep_factor("jk1", 10), 0.9)
  expect_equal(rep_factor("jk2", 75), 1)
})

test_that("compute_stat_rep 手算小例", {
  x <- c(1, 2, 3, 4)
  w0 <- rep(1, 4)
  w1 <- c(2, 0, 2, 0)
  w2 <- c(0, 2, 0, 2)
  r <- compute_stat_rep(mk_spec("mean"), x, cbind(w0, w1, w2), NULL,
                        factor = 1)
  e0 <- mean(x)
  e1 <- stats::weighted.mean(x, w1)
  e2 <- stats::weighted.mean(x, w2)
  expect_equal(r$estimate, e0)
  expect_equal(r$se_sampling^2, (e1 - e0)^2 + (e2 - e0)^2)
})

test_that("est_stat 与 compute_stat 的估计完全一致", {
  set.seed(5)
  n <- 300
  x <- rnorm(n)
  w <- runif(n, 0.5, 2)
  se <- runif(n, 0.1, 0.4)
  brk <- c(L1 = -Inf, L2 = -0.5, L3 = 0.5)
  specs <- list(
    mk_spec("mean"), mk_spec("sd"),
    mk_spec("prop_above", TRUE, list(cutoff = .3, method = "hard")),
    mk_spec("prop_above", TRUE, list(cutoff = .3, method = "prob")),
    mk_spec("level_prop", TRUE, list(breaks = brk, method = "hard")),
    mk_spec("level_prop", TRUE, list(breaks = brk, method = "prob")),
    mk_spec("quantile", params = list(probs = c(.25, .5))),
    mk_spec("count"), mk_spec("wcount")
  )
  for (sp in specs) {
    expect_equal(est_stat(sp, x, w, se)$estimate,
                 compute_stat(sp, x, w, se)$estimate,
                 tolerance = 1e-12, info = sp$type)
  }
})

test_that("JK1 捕捉整群抽样设计效应,线性化会低估", {
  skip_on_cran()
  set.seed(9)
  g_n <- 50
  m <- 20
  n <- g_n * m
  emp_sd <- stats::sd(replicate(400, {
    cl <- rep(rnorm(g_n, 0, 0.5), each = m)
    mean(cl + rnorm(n, 0, 1))
  }))
  cl_id <- rep(seq_len(g_n), each = m)
  y <- rep(rnorm(g_n, 0, 0.5), each = m) + rnorm(n, 0, 1)
  wmat <- matrix(1, n, 1 + g_n)
  for (g in seq_len(g_n)) {
    wmat[, 1 + g] <- ifelse(cl_id == g, 0, g_n / (g_n - 1))
  }
  r <- compute_stat_rep(mk_spec("mean"), y, wmat, NULL,
                        factor = rep_factor("jk1", g_n))
  lin <- sqrt(var_sampling_mean(y, rep(1, n)))
  expect_gt(r$se_sampling / emp_sd, 0.7)
  expect_lt(r$se_sampling / emp_sd, 1.4)
  expect_lt(lin, r$se_sampling)
})

test_that("lst_data 声明复制权重:前缀展开与校验", {
  set.seed(11)
  n <- 100
  d <- data.frame(sid = seq_len(n), region = sample(c("A", "B"), n, TRUE),
                  w = runif(n, 0.5, 2), th = rnorm(n),
                  se = runif(n, .2, .4))
  for (r in 1:8) d[[paste0("W_FSTR", r)]] <- d$w * runif(n, 0.5, 1.5)

  x <- lst_data(d, id = sid, group = region, weight = w,
                theta = c(math = th), theta_se = c(math = se),
                rep_weights = "W_FSTR", rep_method = "fay")
  expect_equal(x$roles$rep_weights, paste0("W_FSTR", 1:8))
  expect_error(
    lst_data(d, id = sid, weight = w, rep_weights = "W_FSTR"),
    "rep_method"
  )
  expect_error(
    lst_data(d, id = sid, weight = w, rep_weights = "NOPE",
             rep_method = "fay"),
    "前缀"
  )
})

test_that("lst_table 走 replicate 引擎并记录方法", {
  set.seed(12)
  n <- 400
  d <- data.frame(sid = seq_len(n), region = sample(c("A", "B"), n, TRUE),
                  w = runif(n, 0.5, 2), th = rnorm(n),
                  se = runif(n, .2, .4),
                  q1 = rbinom(n, 1, 0.6))
  for (r in 1:8) d[[paste0("rw", r)]] <- d$w * runif(n, 0.5, 1.5)
  x <- lst_data(d, id = sid, group = region, weight = w,
                theta = c(math = th), theta_se = c(math = se),
                resp = q1,
                rep_weights = "rw", rep_method = "fay")
  tab <- lst_table(x, rows = region, values = list(
    均值 = st_mean(math),
    优秀率 = st_prop_above(math, cutoff = 1, method = "prob"),
    正答率 = st_pvalue(items = "q1")
  ), margins = TRUE)
  long <- as_long(tab)
  expect_true(all(long$se_sampling[long$statistic == "均值"] > 0))
  expect_equal(tab$meta$均值$variance, "replicate:fay")
  # prob 统计的测量分量仍然存在
  expect_true(all(long$se_measurement[long$statistic == "优秀率"] > 0))
  # 合计行存在且题目统计也有 SE
  expect_true("合计" %in% long$region)
  expect_true(all(long$se_sampling[long$statistic == "正答率"] >= 0))
})

test_that("配置层支持复制权重(YAML)", {
  set.seed(13)
  n <- 200
  d <- data.frame(sid = seq_len(n), region = sample(c("A", "B"), n, TRUE),
                  w = runif(n, 0.5, 2), th = rnorm(n),
                  se = runif(n, .2, .4))
  for (r in 1:6) d[[paste0("W_FSTR", r)]] <- d$w * runif(n, 0.5, 1.5)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  cfg <- list(
    data = csv,
    roles = list(id = "sid", weight = "w", group = list("region"),
                 theta = list(math = "th"), theta_se = list(math = "se"),
                 rep_weights = "W_FSTR", rep_method = "fay"),
    tables = list(list(name = "t", rows = list("region"),
                       values = list(m = list(stat = "st_mean",
                                              var = "math"))))
  )
  res <- lst_run(cfg, quiet = TRUE)
  expect_equal(res$tables$t$meta$m$variance, "replicate:fay")
  # 缺 rep_method 时给出友好错误
  cfg2 <- cfg
  cfg2$roles$rep_method <- NULL
  expect_error(lst_run(cfg2, quiet = TRUE), "rep_method")
})
