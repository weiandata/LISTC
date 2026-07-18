# Plausible values / Rubin 合并引擎(v0.4,design doc 6.2)。
# est = mean_m(est_m);插补方差 B = var_m(est_m);
# 抽样方差 U = 各 PV 的抽样方差(线性化或 replicate)按口径合并;
# 总方差 = U + (1 + 1/M) B。
# 分量映射:se_sampling = sqrt(U),se_measurement = sqrt((1+1/M)B)。

# "PV#MATH" 模板 -> PV1MATH..PVmMATH(按 # 位置的数字排序)
expand_pv_template <- function(template, data_names) {
  if (!grepl("#", template, fixed = TRUE)) {
    return(NULL)
  }
  # 先转义所有非字母数字字符(# 除外),再把 # 换成数字捕获组
  esc <- gsub("([^A-Za-z0-9#])", "\\\\\\1", template)
  pat <- paste0("^", gsub("#", "([0-9]+)", esc, fixed = TRUE), "$")
  hits <- grep(pat, data_names, value = TRUE)
  if (length(hits) == 0) {
    return(character(0))
  }
  ord <- order(as.integer(sub(pat, "\\1", hits)))
  hits[ord]
}

# PV 声明解析:命名列表 dim -> 列名向量 或 含 # 的模板字符串
resolve_pv <- function(pv, data) {
  if (is.null(pv)) {
    return(NULL)
  }
  if (!is.list(pv) || is.null(names(pv)) || any(names(pv) == "")) {
    rlang::abort(paste0(
      "pv \u5fc5\u987b\u662f\u547d\u540d\u5217\u8868:\u7ef4\u5ea6\u540d -> PV \u5217,",
      "\u5982 pv = list(math = \"PV#MATH\") \u6216 ",
      "list(math = c(\"PV1MATH\", \"PV2MATH\"))\u3002"
    ))
  }
  out <- list()
  for (dim in names(pv)) {
    v <- as.character(unlist(pv[[dim]]))
    if (length(v) == 1 && grepl("#", v, fixed = TRUE)) {
      hits <- expand_pv_template(v, names(data))
      if (length(hits) < 2) {
        rlang::abort(paste0(
          "pv \u7ef4\u5ea6 ", dim, " \u7684\u6a21\u677f \"", v,
          "\" \u5339\u914d\u5230\u7684\u5217\u4e0d\u8db3 2 \u4e2a;PV \u5206\u6790\u81f3\u5c11\u9700\u8981 2 \u4e2a plausible value\u3002"
        ))
      }
      v <- hits
    }
    missing <- setdiff(v, names(data))
    if (length(missing) > 0) {
      rlang::abort(paste0(
        "pv \u7ef4\u5ea6 ", dim, " \u4e2d\u7684\u5217\u5728\u6570\u636e\u91cc\u4e0d\u5b58\u5728: ",
        paste(missing, collapse = ", ")
      ))
    }
    if (length(v) < 2) {
      rlang::abort(paste0("pv \u7ef4\u5ea6 ", dim, " \u81f3\u5c11\u9700\u8981 2 \u4e2a PV \u5217\u3002"))
    }
    out[[dim]] <- v
  }
  out
}

# PV 路径的组内计算。
# sd_df 前 npv 列为 PV,随后 1 列主权重,再往后为复制权重(可无)。
# repw_factor 为 NULL 时,U 用线性化公式;否则用 replicate 法。
# pv_sampling: "first"(PISA 手册常用,仅第 1 个 PV 算抽样方差)
#              或 "average"(对全部 PV 求平均,更稳健更慢)。
compute_stat_pv <- function(spec, sd_df, npv, repw_factor = NULL,
                            pv_sampling = "first") {
  m_all <- as.matrix(sd_df)
  xmat <- m_all[, seq_len(npv), drop = FALSE]
  w0 <- m_all[, npv + 1]
  repmat <- if (ncol(m_all) > npv + 1) {
    m_all[, (npv + 2):ncol(m_all), drop = FALSE]
  } else {
    NULL
  }
  n <- nrow(xmat)

  # 各 PV 的点估计
  ests <- vapply(seq_len(npv), function(m) {
    est_stat(spec, xmat[, m], w0)$estimate
  }, numeric(nrow(est_stat(spec, xmat[, 1], w0))))
  if (is.null(dim(ests))) {
    ests <- matrix(ests, nrow = 1)
  }
  cat_names <- est_stat(spec, xmat[, 1], w0)$category
  est <- rowMeans(ests)
  # 插补方差 B(跨 PV)
  b_var <- apply(ests, 1, stats::var)
  vm <- (1 + 1 / npv) * b_var

  # 抽样方差 U
  pv_idx <- if (identical(pv_sampling, "average")) seq_len(npv) else 1L
  u_m <- vapply(pv_idx, function(m) {
    if (is.null(repmat)) {
      compute_stat(spec, xmat[, m], w0)$se_sampling^2
    } else {
      est_m <- ests[, m]
      reps <- vapply(seq_len(ncol(repmat)), function(r) {
        est_stat(spec, xmat[, m], repmat[, r])$estimate
      }, numeric(nrow(ests)))
      if (is.null(dim(reps))) {
        reps <- matrix(reps, nrow = 1)
      }
      repw_factor * rowSums((reps - est_m)^2)
    }
  }, numeric(nrow(ests)))
  if (is.null(dim(u_m))) {
    u_m <- matrix(u_m, nrow = nrow(ests))
  }
  vs <- rowMeans(u_m)

  data.frame(
    category = cat_names,
    estimate = est,
    se_sampling = sqrt(vs),
    se_measurement = sqrt(vm),
    se_total = sqrt(vs + vm),
    n = n,
    sum_w = sum(w0),
    stringsAsFactors = FALSE
  )
}
