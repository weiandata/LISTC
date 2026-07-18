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
  expect_s3_class(x, "listr_data")
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
