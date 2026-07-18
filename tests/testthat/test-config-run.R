test_that("配置校验给出中文友好错误", {
  expect_error(lst_config(list()), "data")
  expect_error(lst_config(list(data = "a.csv")), "roles")
  expect_error(lst_config(list(data = "a.csv", roles = list())), "id")
  expect_error(
    lst_config(list(
      data = "a.csv", roles = list(id = "sid"),
      tables = list(list(name = "t", values = list(
        x = list(stat = "st_nope")
      )))
    )),
    "stat 无效"
  )
})

test_that("lst_run 端到端:csv -> xlsx + json", {
  skip_if_not_installed("openxlsx")
  set.seed(9)
  n <- 300
  d <- data.frame(
    sid = seq_len(n),
    region = sample(c("甲", "乙"), n, replace = TRUE),
    w = runif(n, 0.5, 2),
    th = rnorm(n),
    se = runif(n, 0.2, 0.4)
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  xlsx <- tempfile(fileext = ".xlsx")
  json <- tempfile(fileext = ".json")

  cfg <- list(
    data = csv,
    roles = list(
      id = "sid", weight = "w", group = list("region"),
      theta = list(math = "th"), theta_se = list(math = "se")
    ),
    tables = list(list(
      name = "区域均值",
      rows = list("region"),
      values = list(
        均值 = list(stat = "st_mean", var = "math"),
        优秀率 = list(stat = "st_prop_above", var = "math",
                    cutoff = 1, method = "prob")
      ),
      margins = TRUE
    )),
    output = list(xlsx = xlsx, json = json)
  )

  res <- lst_run(cfg, quiet = TRUE)
  expect_true(file.exists(xlsx))
  expect_true(file.exists(json))
  expect_s3_class(res$tables$区域均值, "listr_table")
  js <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
  expect_equal(js$package, "LISTR")
  expect_true(length(js$tables$interpretation[[1]]) > 0)

  # YAML 字符串同样可用
  yml <- paste0(
    "data: ", csv, "\n",
    "roles:\n",
    "  id: sid\n",
    "  weight: w\n",
    "  group: [region]\n",
    "  theta: {math: th}\n",
    "  theta_se: {math: se}\n",
    "tables:\n",
    "  - name: t1\n",
    "    rows: [region]\n",
    "    values:\n",
    "      m: {stat: st_mean, var: math}\n"
  )
  res2 <- lst_run(yml, quiet = TRUE)
  expect_s3_class(res2$tables$t1, "listr_table")
})

test_that("read_listr 分派与列选择", {
  d <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  r <- read_listr(csv, col_select = c("a", "c"))
  expect_equal(names(r), c("a", "c"))
  expect_error(read_listr("no-such.file.csv"), "找不到")
  bad <- tempfile(fileext = ".foo")
  file.create(bad)
  expect_error(read_listr(bad), "暂不支持")
})
