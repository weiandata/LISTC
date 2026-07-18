sim_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    sid = seq_len(n),
    region = sample(c("东部", "西部"), n, replace = TRUE),
    gender = sample(c("男", "女"), n, replace = TRUE),
    w = runif(n, 0.5, 2),
    th = rnorm(n),
    se = runif(n, 0.2, 0.5),
    raw = rpois(n, 40),
    q1 = sample(c("A", "B", "C", NA), n, replace = TRUE),
    q2 = sample(c(0, 1), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

make_ld <- function(d = sim_data()) {
  lst_data(d,
    id = sid, group = c(region, gender), weight = w,
    theta = c(math = th), theta_se = c(math = se),
    score = raw, resp = c(q1, q2), key = list(q1 = "B")
  )
}

test_that("lst_data 角色解析与校验", {
  x <- make_ld()
  expect_s3_class(x, "listc_data")
  expect_equal(x$roles$theta, c(math = "th"))
  expect_equal(x$roles$weight, "w")
  d <- sim_data()
  d$sid[2] <- d$sid[1]
  expect_error(lst_data(d, id = sid), "唯一")
  d2 <- sim_data()
  expect_error(lst_data(d2, theta = c(math = th)), "配对")
})

test_that("lst_table 基本形状与加权均值正确", {
  d <- sim_data()
  x <- make_ld(d)
  tab <- lst_table(x, rows = region, values = list(均值 = st_mean(math)))
  long <- as_long(tab)
  expect_equal(sort(unique(long$region)), sort(unique(d$region)))
  east <- d[d$region == "东部", ]
  expect_equal(
    long$estimate[long$region == "东部"],
    stats::weighted.mean(east$th, east$w)
  )
  wide <- as_wide(tab)
  expect_equal(nrow(wide), 2)
})

test_that("margins 生成合计行列", {
  x <- make_ld()
  tab <- lst_table(x, rows = region, cols = gender,
                   values = list(m = st_mean(math)), margins = TRUE)
  long <- as_long(tab)
  expect_true("合计" %in% long$region)
  expect_true("合计" %in% long$gender)
  grand <- long[long$region == "合计" & long$gender == "合计", ]
  expect_equal(nrow(grand), 1)
})

test_that("prob 统计经引擎运行且 SE 分量齐全", {
  x <- make_ld()
  tab <- lst_table(x, rows = region, values = list(
    优秀率 = st_prop_above(math, cutoff = 1, method = "prob")
  ))
  long <- as_long(tab)
  expect_true(all(long$se_measurement > 0))
  expect_true(all(long$se_total >= long$se_sampling))
})

test_that("latent 校正经引擎运行并记录 rho", {
  x <- make_ld(sim_data(n = 2000))
  tab <- lst_table(x, rows = region, values = list(
    p = st_prop_above(math, cutoff = 1, method = "prob",
                      correction = "latent")
  ))
  expect_true(is.numeric(tab$meta$p$rho))
})

test_that("题目层统计:pvalue 与选项分布", {
  d <- sim_data()
  x <- make_ld(d)
  tab <- lst_table(x, rows = region, values = list(
    正答率 = st_pvalue(items = c("q1", "q2"))
  ))
  long <- as_long(tab)
  expect_setequal(unique(long$category), c("q1", "q2"))
  tab2 <- lst_table(x, values = list(选项 = st_option_dist(items = "q1")))
  long2 <- as_long(tab2)
  expect_true(any(grepl("^q1:", long2$category)))
  # 每组选项占比之和为 1
  expect_equal(sum(long2$estimate), 1, tolerance = 1e-9)
})

test_that("等级占比 + 多统计量 + 宽表", {
  x <- make_ld()
  brk <- c(低 = -Inf, 中 = -0.5, 高 = 0.8)
  tab <- lst_table(x, rows = region, cols = gender, values = list(
    均值 = st_mean(math),
    等级 = st_level_prop(math, breaks = brk, method = "prob"),
    人数 = st_count()
  ), margins = TRUE)
  wide <- as_wide(tab)
  expect_true(nrow(wide) == 3) # 两个 region + 合计
  expect_true(any(grepl("等级", names(wide))))
  # 合计行在最后
  expect_equal(wide$region[nrow(wide)], "合计")
})

test_that("lst_classify / lst_above / lst_derive", {
  x <- make_ld()
  x <- lst_classify(x, math, c(低 = -Inf, 高 = 0))
  # 默认列名基于维度名(math),不是底层列名(th)
  expect_true("math_level" %in% names(x$data))
  expect_true(is.ordered(x$data$math_level))
  # 达到分数线进入高等级
  expect_equal(as.character(x$data$math_level[x$data$th >= 0][1]), "高")
  x <- lst_above(x, math, 0)
  expect_true(all(x$data$th_above %in% c(0, 1)))
  x <- lst_derive(x, 双倍 = raw * 2)
  expect_equal(x$data$双倍, x$data$raw * 2)
})

test_that("lst_interpret 产生结论文字", {
  x <- make_ld()
  tab <- lst_table(x, rows = region, values = list(均值 = st_mean(math)))
  txt <- lst_interpret(tab)
  expect_true(any(grepl("最高", txt)))
})

test_that("st_option_dist 的抽样 SE 等同于 0/1 指示变量的加权均值 SE", {
  d <- sim_data()
  x <- make_ld(d)
  long <- as_long(lst_table(x, rows = region,
                            values = list(opt = st_option_dist(items = "q1"))))
  long <- as.data.frame(long)

  # 逐个选项手工构造指示变量,用 st_mean 走同一套线性化方差
  opt <- as.character(d$q1)
  opt[is.na(opt)] <- "(缺失)"
  for (ct in sort(unique(opt))) {
    d2 <- d
    d2$ind <- as.numeric(opt == ct)
    x2 <- lst_data(d2, id = sid, group = region, weight = w, score = ind)
    ref <- as.data.frame(as_long(
      lst_table(x2, rows = region, values = list(m = st_mean(ind)))
    ))
    got <- long[long$category == paste0("q1:", ct), ]
    got <- got[order(got$region), ]
    ref <- ref[order(ref$region), ]
    expect_equal(got$estimate, ref$estimate, tolerance = 1e-10)
    expect_equal(got$se_sampling, ref$se_sampling, tolerance = 1e-10)
  }
})

test_that("st_option_dist 在复制权重下给出 replicate SE", {
  d <- sim_data()
  set.seed(99)
  for (i in 1:8) d[[paste0("rw", i)]] <- runif(nrow(d), 0.5, 2)
  rw <- paste0("rw", 1:8)
  x <- lst_data(d, id = sid, group = region, weight = w, resp = q1,
                rep_weights = rw, rep_method = "jk1")
  long <- as.data.frame(as_long(
    lst_table(x, rows = region, values = list(opt = st_option_dist(items = "q1")))
  ))
  expect_true(all(is.finite(long$se_sampling)))
  expect_true(all(long$se_sampling > 0))

  opt <- as.character(d$q1)
  opt[is.na(opt)] <- "(缺失)"
  ct <- sort(unique(opt))[1]
  d2 <- d
  d2$ind <- as.numeric(opt == ct)
  x2 <- lst_data(d2, id = sid, group = region, weight = w, score = ind,
                 rep_weights = rw, rep_method = "jk1")
  ref <- as.data.frame(as_long(
    lst_table(x2, rows = region, values = list(m = st_mean(ind)))
  ))
  got <- long[long$category == paste0("q1:", ct), ]
  expect_equal(got$se_sampling[order(got$region)],
               ref$se_sampling[order(ref$region)], tolerance = 1e-10)
})

test_that("st_option_dist 的 SE 分量约定与 st_pvalue 一致", {
  x <- make_ld()
  long <- as.data.frame(as_long(
    lst_table(x, rows = region, values = list(opt = st_option_dist(items = "q1")))
  ))
  expect_true(all(long$se_measurement == 0))
  expect_equal(long$se_total, long$se_sampling, tolerance = 1e-12)
})

test_that("每个统计量的每个单元格都带抽样 SE(宣传口径回归测试)", {
  x <- make_ld()
  brk <- c(低 = -Inf, 中 = -0.5, 高 = 0.8)
  stats <- list(
    st_mean = st_mean(math),
    st_sd = st_sd(math),
    st_prop_above = st_prop_above(math, cutoff = 0.5, method = "prob"),
    st_level_prop = st_level_prop(math, breaks = brk, method = "prob"),
    st_quantile = st_quantile(math, probs = c(0.25, 0.5)),
    st_count = st_count(),
    st_wcount = st_wcount(),
    st_pvalue = st_pvalue(items = "q1"),
    st_option_dist = st_option_dist(items = "q1")
  )
  for (nm in names(stats)) {
    long <- as_long(lst_table(
      x, rows = region, values = stats::setNames(list(stats[[nm]]), "v")
    ))
    expect_true(all(is.finite(long$se_sampling)),
                info = paste("se_sampling 非有限值:", nm))
  }
})
