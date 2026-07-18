# 真实数据端到端试跑(不进 git/不进包;realdata/ 已被忽略)
# 运行: Rscript scripts/realdata-run.R
# 输出: realdata/output/ 下的 xlsx/json/html + 控制台耗时

if (!requireNamespace("LISTR", quietly = TRUE)) {
  suppressPackageStartupMessages(devtools::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(LISTR))
}

rd <- "realdata"
stopifnot(dir.exists(rd))
out_dir <- file.path(rd, "output")
dir.create(out_dir, showWarnings = FALSE)

# --- 定位 sav 文件;扩展名被"_副本"破坏时做临时符号链接修复 ---
sav <- list.files(rd, pattern = "sav", full.names = TRUE)
sav <- sav[!grepl("output", sav)][1]
if (!grepl("\\.sav$", sav)) {
  fixed <- file.path(tempdir(), "realdata-fixed.sav")
  if (file.exists(fixed)) file.remove(fixed)
  ok <- suppressWarnings(file.symlink(normalizePath(sav), fixed))
  if (!ok) file.copy(sav, fixed) # 文件系统不支持符号链接时退回复制
  message("注意: 文件名缺少 .sav 扩展名(被'_副本'后缀破坏),已用临时链接修复。",
          "建议把原文件重命名为规范的 .sav。")
  sav <- fixed
}

t0 <- Sys.time()
message("读取: ", sav)
d <- read_listr(sav, col_select = c(
  "ID", "WEIGHT", "SCORE", "LEVEL", "PROVINCE", "CITY", "AREA",
  "SCHOOLTYPE", "GENDER", "IMPORTANT",
  "IMAGINATIONANDCOURIOUS", "IMAGINATION", "COURIOUS"
))
message(sprintf("读入 %s 行 x %d 列, %.1fs", format(nrow(d), big.mark = ","),
                ncol(d), as.numeric(Sys.time() - t0, units = "secs")))

# GENDER 是 1/2 数值,转成可读标签
d$GENDER <- factor(d$GENDER, levels = c(1, 2), labels = c("男", "女"))
d$LEVEL <- factor(d$LEVEL, levels = c(4, 6, 8, 11),
                  labels = c("四年级", "六年级", "八年级", "十一年级"))

x <- lst_data(d,
  id = ID, weight = WEIGHT,
  group = c(PROVINCE, AREA, SCHOOLTYPE, GENDER, LEVEL, IMPORTANT),
  score = c(总分 = SCORE, 想象好奇 = IMAGINATIONANDCOURIOUS)
)
print(x)

lv <- c(待提高 = -Inf, 合格 = 40, 良好 = 60, 优秀 = 80)
t1 <- Sys.time()
tab_grade <- lst_table(x,
  rows = LEVEL, cols = GENDER,
  values = list(
    平均分 = st_mean(总分),
    达标率 = st_prop_above(总分, cutoff = 60),
    等级分布 = st_level_prop(总分, breaks = lv),
    人数 = st_count(),
    加权人数 = st_wcount()
  ),
  margins = TRUE
)
tab_area <- lst_table(x,
  rows = AREA, cols = LEVEL,
  values = list(
    平均分 = st_mean(总分),
    中位数 = st_quantile(总分, probs = 0.5),
    达标率 = st_prop_above(总分, cutoff = 60)
  ),
  margins = TRUE
)
tab_prov <- lst_table(x,
  rows = PROVINCE,
  values = list(
    平均分 = st_mean(总分),
    达标率 = st_prop_above(总分, cutoff = 60),
    人数 = st_count()
  )
)
message(sprintf("三张透视表计算完成: %.1fs",
                as.numeric(Sys.time() - t1, units = "secs")))

tabs <- list(学段x性别 = tab_grade, 城乡x学段 = tab_area, 分省 = tab_prov)
lst_to_excel(tabs, file.path(out_dir, "真实数据试跑.xlsx"), overwrite = TRUE)
lst_to_json(tabs, file.path(out_dir, "真实数据试跑.json"))
lst_to_html(tabs, file.path(out_dir, "真实数据试跑.html"),
            title = "义务教育质量监测数据试跑报告")
message("输出已写入 ", out_dir)
print(tab_grade)
writeLines(lst_interpret(tab_grade))
message(sprintf("总耗时 %.1fs", as.numeric(Sys.time() - t0, units = "secs")))
