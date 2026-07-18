make_spec <- function(type, is_prop = FALSE, params = list()) {
  structure(list(type = type, var_quo = NULL, is_prop = is_prop,
                 params = params), class = "listr_stat")
}

test_that("compute_stat mean 返回正确结构", {
  r <- compute_stat(make_spec("mean"), c(1, 2, 3), c(1, 1, 1))
  expect_equal(r$estimate, 2)
  expect_equal(r$n, 3)
  expect_true(all(c("se_sampling", "se_measurement", "se_total")
                  %in% names(r)))
  expect_equal(r$se_measurement, 0)
})

test_that("prop_above hard: 达到阈值计入", {
  r <- compute_stat(
    make_spec("prop_above", TRUE, list(cutoff = 2, method = "hard")),
    c(1, 2, 3, 4), rep(1, 4)
  )
  expect_equal(r$estimate, 0.75)
})

test_that("prob 版占比在 se->0 时收敛于硬分类", {
  set.seed(3)
  x <- rnorm(500)
  w <- runif(500, 0.5, 2)
  hard <- compute_stat(
    make_spec("prop_above", TRUE, list(cutoff = 0.5, method = "hard")),
    x, w
  )
  prob <- compute_stat(
    make_spec("prop_above", TRUE, list(cutoff = 0.5, method = "prob")),
    x, w, rep(1e-8, 500)
  )
  expect_equal(prob$estimate, hard$estimate, tolerance = 1e-6)
  expect_equal(prob$se_measurement, 0, tolerance = 1e-6)
})

test_that("等级占比之和恒为 1(hard 与 prob)", {
  set.seed(4)
  x <- rnorm(300)
  w <- runif(300, 0.5, 2)
  se <- runif(300, 0.1, 0.4)
  brk <- c(L1 = -Inf, L2 = -0.5, L3 = 0.5, L4 = 1.5)
  for (m in c("hard", "prob")) {
    r <- compute_stat(
      make_spec("level_prop", TRUE, list(breaks = brk, method = m)),
      x, w, se
    )
    expect_equal(sum(r$estimate), 1, tolerance = 1e-9)
    expect_equal(r$category, names(brk))
  }
})

test_that("prob 等级占比在 se->0 时收敛于硬分类", {
  set.seed(5)
  x <- rnorm(400)
  w <- rep(1, 400)
  brk <- c(A = -Inf, B = 0, C = 1)
  hard <- compute_stat(
    make_spec("level_prop", TRUE, list(breaks = brk, method = "hard")),
    x, w
  )
  prob <- compute_stat(
    make_spec("level_prop", TRUE, list(breaks = brk, method = "prob")),
    x, w, rep(1e-9, 400)
  )
  expect_equal(prob$estimate, hard$estimate, tolerance = 1e-6)
})

test_that("加权分位数正确", {
  x <- c(1, 2, 3, 4, 5)
  w <- rep(1, 5)
  r <- compute_stat(make_spec("quantile", params = list(probs = 0.5)), x, w)
  expect_equal(r$estimate, 3)
})

test_that("st_* 构造器捕获变量名", {
  s <- st_mean(math)
  expect_s3_class(s, "listr_stat")
  expect_equal(s$type, "mean")
  expect_error(st_level_prop(x, breaks = c(1, 2)), "命名")
  expect_error(st_level_prop(x, breaks = c(b = 2, a = 1)), "从低到高")
})
