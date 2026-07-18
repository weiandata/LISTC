# LISTR 性能基准(design doc 9.1 验收)
# 运行: Rscript scripts/benchmark.R [n]  (默认 1e6;验收线用 5e6)
# 记录典型任务耗时与峰值内存;16GB 环境验收: 5e6 全套表 < 5 分钟

suppressPackageStartupMessages(library(LISTR))

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) >= 1) as.numeric(args[1]) else 1e6
n_items <- 50
cat(sprintf("== LISTR benchmark: n = %s, items = %d ==\n",
            format(n, big.mark = ","), n_items))

set.seed(1)
t0 <- Sys.time()
demo <- data.frame(
  student_id = seq_len(n),
  region = sample(paste0("R", 1:10), n, TRUE),
  gender = sample(c("M", "F"), n, TRUE),
  grade = sample(paste0("G", 4:9), n, TRUE),
  w = runif(n, 0.5, 2),
  th = rnorm(n),
  se = runif(n, 0.2, 0.5)
)
for (j in seq_len(n_items)) {
  demo[[paste0("q", j)]] <- sample(c(0L, 1L), n, TRUE)
}
cat(sprintf("data simulated: %.1fs, ~%.1f GB in memory\n",
            as.numeric(Sys.time() - t0, units = "secs"),
            as.numeric(object.size(demo)) / 1e9))

bench <- function(label, expr) {
  gc(reset = TRUE)
  t <- system.time(expr)
  g <- gc()
  cat(sprintf("%-38s %8.2fs  peak %6.0f MB\n", label, t[["elapsed"]],
              sum(g[, 6])))
}

x <- NULL
bench("lst_data (roles + validation)", {
  x <<- lst_data(demo,
    id = student_id, group = c(region, gender, grade), weight = w,
    theta = c(math = th), theta_se = c(math = se),
    resp = paste0("q", seq_len(n_items))
  )
})

lv <- c(L1 = -Inf, L2 = -0.5, L3 = 0.5, L4 = 1.5)
bench("mean+prob prop+levels, 10x2 margins", {
  lst_table(x, rows = region, cols = gender, values = list(
    m = st_mean(math),
    p = st_prop_above(math, cutoff = 1.2, method = "prob"),
    lv = st_level_prop(math, breaks = lv, method = "prob"),
    n = st_count()
  ), margins = TRUE)
})

bench("3-way grouping (10x2x6)", {
  lst_table(x, rows = c(region, grade), cols = gender,
            values = list(m = st_mean(math)))
})

bench(sprintf("item pvalues (%d items x 10 groups)", n_items), {
  lst_table(x, rows = region, values = list(pv = st_pvalue()))
})

bench("latent correction (rho estimation)", {
  lst_table(x, rows = region, values = list(
    p = st_prop_above(math, cutoff = 1.2, method = "prob",
                      correction = "latent")
  ))
})

cat("== done ==\n")
