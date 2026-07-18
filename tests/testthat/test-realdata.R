# 真实数据冒烟测试:realdata/ 存在时运行(该目录不进 git/不进包,
# R CMD check 与 CI 环境会自动跳过)。完整试跑用 scripts/realdata-run.R。

realdata_dir <- function() {
  cand <- c(
    file.path(testthat::test_path(), "..", "..", "realdata"),
    "realdata"
  )
  for (p in cand) {
    if (dir.exists(p)) {
      return(normalizePath(p))
    }
  }
  NULL
}

test_that("真实监测数据冒烟:读入/角色/透视/解读", {
  rd <- realdata_dir()
  skip_if(is.null(rd), "realdata/ 不存在")
  sav <- list.files(rd, pattern = "sav", full.names = TRUE)
  sav <- sav[!grepl("output", sav)]
  skip_if(length(sav) == 0, "无 sav 文件")
  sav <- sav[1]
  if (!grepl("\\.sav$", sav)) {
    fixed <- file.path(tempdir(), "realdata-smoke.sav")
    if (!file.exists(fixed)) {
      ok <- suppressWarnings(file.symlink(sav, fixed))
      if (!ok) skip("无法创建符号链接")
    }
    sav <- fixed
  }
  d <- read_listc(sav, col_select = c(
    "ID", "WEIGHT", "SCORE", "LEVEL", "PROVINCE", "AREA",
    "SCHOOLTYPE", "GENDER"
  ), n_max = 20000)
  expect_gt(nrow(d), 1000)
  expect_true(all(c("SCORE", "WEIGHT") %in% names(d)))
  expect_true(is.numeric(d$SCORE))
  # id 有缺失 -> 应给出中文提示而非报错(真实数据 ID 缺失约 4.85%)
  expect_message(
    x <- lst_data(d, id = ID, weight = WEIGHT,
                  group = c(PROVINCE, AREA, GENDER),
                  score = c(总分 = SCORE))
  )
  lv <- c(待提高 = -Inf, 合格 = 40, 良好 = 60, 优秀 = 80)
  tab <- lst_table(x, rows = AREA, cols = GENDER, values = list(
    平均分 = st_mean(总分),
    达标率 = st_prop_above(总分, cutoff = 60),
    等级 = st_level_prop(总分, breaks = lv),
    人数 = st_count()
  ), margins = TRUE)
  long <- as_long(tab)
  expect_true(all(is.finite(long$estimate[long$statistic == "平均分"])))
  expect_true(all(long$se_sampling[long$statistic == "平均分"] > 0))
  # 等级占比和为 1(每个非合计组)
  lvl <- long[long$statistic == "等级" & long$AREA != "合计" &
                long$GENDER != "合计", ]
  sums <- tapply(lvl$estimate, paste(lvl$AREA, lvl$GENDER), sum)
  expect_true(all(abs(sums - 1) < 1e-9))
  expect_true(length(lst_interpret(tab)) > 0)
  expect_true(nchar(lst_to_html(tab)) > 1000)
})
