test_that("read_winsteps_pfile 解析空白分隔 PFILE", {
  pf <- tempfile(fileext = ".txt")
  writeLines(c(
    "; PERSON FILE FOR demo",
    ";ENTRY MEASURE COUNT SCORE ERROR NAME",
    "1 0.52 20 12 0.41 S001",
    "2 -1.03 20 6 0.44 S002",
    "3 1.87 20 18 0.52 S003 extra name"
  ), pf)
  r <- read_winsteps_pfile(pf)
  expect_equal(nrow(r), 3)
  expect_equal(r$theta, c(0.52, -1.03, 1.87))
  expect_equal(r$theta_se, c(0.41, 0.44, 0.52))
  expect_equal(r$id[1], "S001")
  expect_equal(r$id[3], "S003 extra name") # NAME 含空格
})

test_that("read_winsteps_pfile 解析 csv PFILE 并支持列覆盖", {
  pf <- tempfile(fileext = ".csv")
  writeLines(c(
    "ENTRY,MEASURE,MODLSE,PERSON",
    "1,0.5,0.4,A01",
    "2,-0.5,0.45,A02"
  ), pf)
  r <- read_winsteps_pfile(pf)
  expect_equal(r$theta_se, c(0.4, 0.45))
  expect_equal(r$id, c("A01", "A02"))
})

test_that("read_conquest_person 默认 6 列布局", {
  cf <- tempfile(fileext = ".wle")
  writeLines(c(
    "1 S001 12.00 20.00 0.523 0.412",
    "2 S002 6.00 20.00 -1.031 0.437"
  ), cf)
  r <- read_conquest_person(cf)
  expect_equal(r$id, c("S001", "S002"))
  expect_equal(r$theta, c(0.523, -1.031))
  expect_equal(r$theta_se, c(0.412, 0.437))
})

test_that("lst_join_person 合并并注册角色", {
  d <- data.frame(sid = c("S001", "S002", "S003"),
                  region = c("A", "A", "B"))
  x <- lst_data(d, id = sid, group = region)
  person <- data.frame(id = c("S001", "S003"),
                       theta = c(0.5, 1.2), theta_se = c(0.4, 0.5))
  x <- lst_join_person(x, person, dim = "math")
  expect_equal(names(x$roles$theta), "math")
  expect_equal(x$data$.math, c(0.5, NA, 1.2))
  tab <- lst_table(x, rows = region, values = list(m = st_mean(math)))
  expect_true(all(is.finite(as_long(tab)$estimate)))
})
