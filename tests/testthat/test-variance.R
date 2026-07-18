# 公式对照 + 蒙特卡洛验证(design doc 10)

test_that("加权均值与 stats::weighted.mean 一致", {
  set.seed(1)
  x <- rnorm(100)
  w <- runif(100, 0.5, 2)
  expect_equal(wmean(x, w), stats::weighted.mean(x, w))
})

test_that("抽样方差公式与手算一致", {
  x <- c(1, 2, 3, 4)
  w <- c(1, 1, 2, 2)
  m <- sum(w * x) / sum(w)
  manual <- sum(w^2 * (x - m)^2) / sum(w)^2
  expect_equal(var_sampling_mean(x, w), manual)
})

test_that("等权重时抽样 SE 接近经典 SE 公式", {
  set.seed(2)
  x <- rnorm(5000)
  w <- rep(1, 5000)
  classic <- stats::sd(x) / sqrt(5000)
  expect_equal(sqrt(var_sampling_mean(x, w)), classic,
               tolerance = 0.02)
})

test_that("测量方差公式与手算一致", {
  w <- c(1, 2)
  se <- c(0.3, 0.4)
  expect_equal(var_measurement_mean(w, se),
               (1 * 0.09 + 4 * 0.16) / 9)
})

test_that("均值总 SE 的蒙特卡洛验证(抽样+测量两层)", {
  skip_on_cran()
  set.seed(42)
  n <- 400
  se_i <- runif(n, 0.2, 0.5)
  w <- runif(n, 0.5, 2)
  # 多次重复:每次重抽真值和测量误差
  reps <- 800
  means <- replicate(reps, {
    theta_true <- rnorm(n, 0, 1)
    theta_obs <- theta_true + rnorm(n, 0, se_i)
    wmean(theta_obs, w)
  })
  # 解析 SE(用一次实现计算,真值方差=1)
  theta_true <- rnorm(n, 0, 1)
  theta_obs <- theta_true + rnorm(n, 0, se_i)
  analytic <- sqrt(var_sampling_mean(theta_obs, w) +
                     var_measurement_mean(w, se_i))
  # 注意:抽样方差以观测方差为基础,本身已含测量成分,
  # 此处仅检验量级一致(比值在 0.8-1.35 之间)
  expect_gt(analytic / stats::sd(means), 0.8)
  expect_lt(analytic / stats::sd(means), 1.35)
})

test_that("latent 校正:WLE 型估计的等级占比更接近真值", {
  set.seed(7)
  n <- 4000
  err_sd <- 0.5
  theta_true <- rnorm(n, 0, 1)
  wle <- theta_true + rnorm(n, 0, err_sd) # 无偏估计 + 抽样误差
  se_i <- rep(err_sd, n)
  w <- rep(1, n)
  cutoff <- 1.2
  true_prop <- mean(theta_true >= cutoff)
  naive <- mean(1 - stats::pnorm((cutoff - wle) / se_i))
  un <- latent_posterior(wle, se_i, w)
  corrected <- mean(1 - stats::pnorm((cutoff - un$theta) / un$se))
  expect_equal(un$rho, 1 / (1 + err_sd^2), tolerance = 0.08)
  expect_lt(abs(corrected - true_prop), abs(naive - true_prop))
})

test_that("latent 对 rho 过低报错", {
  expect_error(
    latent_posterior(rnorm(100), rep(10, 100), rep(1, 100)),
    "rho"
  )
})
