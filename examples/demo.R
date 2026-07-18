# LISTC 使用示例:函数 API(统计人员)与配置 API(lst_run)
# 运行前: devtools::load_all() 或 library(LISTC)

set.seed(2026)
n <- 5000
demo <- data.frame(
  student_id = sprintf("S%05d", seq_len(n)),
  region = sample(c("东部", "中部", "西部"), n, TRUE, c(.4, .35, .25)),
  gender = sample(c("男", "女"), n, TRUE),
  w_final = runif(n, 0.5, 2),
  th_math = rnorm(n),
  se_math = runif(n, 0.25, 0.45),
  raw_total = rpois(n, 40)
)

# ---- 函数 API ----
x <- lst_data(demo,
  id = student_id, group = c(region, gender), weight = w_final,
  theta = c(math = th_math), theta_se = c(math = se_math),
  score = raw_total
)

lv <- c(待提高 = -Inf, 合格 = -0.5, 良好 = 0.5, 优秀 = 1.2)
tab <- lst_table(x,
  rows = region, cols = gender,
  values = list(
    平均能力 = st_mean(math),
    优秀率 = st_prop_above(math, cutoff = 1.2, method = "prob"),
    等级 = st_level_prop(math, breaks = lv, method = "prob"),
    人数 = st_count()
  ),
  margins = TRUE
)
print(tab)
as_long(tab)     # tidy 长表(含 SE 三分量)
lst_interpret(tab)

lst_to_excel(tab, "demo-results.xlsx", overwrite = TRUE)
cat(lst_to_json(tab), file = "demo-results.json")

# ---- 配置 API(新手/AI agent 同一入口)----
utils::write.csv(demo, "students.csv", row.names = FALSE)
res <- lst_run("examples/config-example.yml")
str(res$log)
