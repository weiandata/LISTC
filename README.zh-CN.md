# LISTR

> **大规模测评数据的透视式统计表——每个单元格都带正确的标准误。**

[![R-CMD-check](https://github.com/weiandata/LISTR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/weiandata/LISTR/actions/workflows/R-CMD-check.yaml)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2%2B-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
[![覆盖率: 95.9%](https://img.shields.io/badge/coverage-95.9%25-brightgreen.svg)](scripts/coverage.R)

**[English →](README.md)** ·
[非编程人员快速上手](docs/guides/quickstart-zh.md) ·
[进阶操作指南](docs/guides/advanced-zh.md) ·
[设计文档](docs/design-v1.md)

LISTR 把测评/调查的个体数据——人口学变量、得分、抽样权重、带个体
标准误的 IRT 能力值、复制权重、plausible values——变成 **Excel
数据透视表式的统计表**:行变量、列变量、单元格统计量自由组合,
且每个单元格都带有**与抽样设计和测量模型匹配的标准误**。

现有 R 生态没有同时做到这两端:调查统计包(EdSurvey、intsvy、
Rrepest、BIFIEsurvey)算得对但没有自由布局;透视表包(pivottabler)
能布局但不懂权重、复制设计和测量误差。LISTR 两者兼得,并且够快:
**500 万样本全套统计表,笔记本约 10 秒**。

## 安装

```r
# install.packages("LISTR")                      # 上架 CRAN 后
# install.packages("remotes")
remotes::install_github("weiandata/LISTR")       # 开发版
```

## 快速示例

```r
library(LISTR)

# 1. 声明每一列的角色
x <- lst_data(students,
  id       = student_id,
  group    = c(region, gender),
  weight   = w_final,
  theta    = c(math = th_math),      # IRT 能力值
  theta_se = c(math = se_math)       # 对应的个体标准误
)

# 2. 定义透视表:行、列、单元格统计量
lv <- c(待提高 = -Inf, 合格 = -0.5, 良好 = 0.5, 优秀 = 1.2)
tab <- lst_table(x,
  rows = region, cols = gender,
  values = list(
    平均能力 = st_mean(math),
    优秀率   = st_prop_above(math, cutoff = 1.2, method = "prob"),
    等级分布 = st_level_prop(math, breaks = lv, method = "prob"),
    人数     = st_count()
  ),
  margins = TRUE                     # 追加"合计"行列
)

# 3. 使用结果
tab                          # 宽表:每格形如 "0.52 (0.03)"
as_long(tab)                 # tidy 长表:se_sampling / se_measurement 分量
lst_to_excel(tab, "结果.xlsx")
lst_to_html(tab, "报告.html")
lst_interpret(tab)           # 规则化中文自动解读
```

## 一套引擎,三类用户

| 用户 | 交互方式 | 输出 |
| --- | --- | --- |
| 不写代码的调查人员 | 填 **Excel 配置模板**(`lst_config_template()` 生成),一行运行:`lst_run("配置.xlsx")` | 中文样式 Excel(含"结论"自动解读 sheet)、HTML 报告 |
| 统计人员 | 完整函数 API(`lst_data → st_* → lst_table`) | `listr_table` 对象、tidy 长表、SE 全分量 |
| AI agent / 自动化管线 | YAML/JSON 配置驱动 `lst_run()`;包内附 JSON Schema 与 `llms.txt` | 带逐统计量元数据的机器可读 JSON |

## 方差引擎

总方差 = 抽样 + 测量,三个分量(`se_sampling`、`se_measurement`、
`se_total`)始终分开报告。

| 数据情形 | 声明 | 抽样方差 | 测量方差 |
| --- | --- | --- | --- |
| 普通加权调查 | `weight` | 线性化 | — |
| 运营性 IRT 测评 | `theta` + `theta_se` | 线性化 | 个体 SE 的 delta 法传递;概率化分类(`method = "prob"`)对分数线附近误分类稳健;WLE/ML 用 `correction = "latent"` 经验贝叶斯校正 |
| PISA/TIMSS 复制设计 | `rep_weights = "W_FSTR"`、`rep_method = "fay"` | BRR/Fay/JK1/JK2 | 同上 |
| Plausible values | `pv = list(math = "PV#MATH")` | 线性化或复制权重 | Rubin(1987)合并 |

全部公式经测试套件中的蒙特卡洛模拟验证(覆盖率 95.9%,核心引擎
文件均 >95%)。一个写进设计的重要结论:EAP + 后验标准差做概率化
分类本身就是校准的,而 WLE/ML 估计需要 `"latent"` 校正——详见
[设计文档 §4.1/§6](docs/design-v1.md)。

## 数据进,结果出

**输入:** csv/tsv(data.table::fread)、Excel、SPSS/SAS/Stata
(haven,保留值标签)、Winsteps PFILE、ConQuest 人参数文件。
**输出:** 中文样式 Excel(等线字体、全角列宽)、含计算元数据的
tidy JSON、单文件 HTML 报告——每种都附规则化自动解读,防止
非专业读者误读标准误。

## 性能

data.table 后端、按需读列、按题分块。Apple Silicon 实测
(`scripts/benchmark.R`):

| 任务(含 50 个作答列) | 100 万人 | 500 万人 |
| --- | --- | --- |
| 均值+概率化占比+等级,10×2 含合计 | 2.0 秒 | 10.5 秒 |
| 50 题正答率 × 10 组 | 1.6 秒 | 7.7 秒 |

## 文档

* Vignette:`vignette("LISTR-intro")`
* 非编程人员指南(中/英):[docs/guides/](docs/guides/)
* 设计文档(全部方法与蒙特卡洛证据):[docs/design-v1.md](docs/design-v1.md)
* 面向 LLM agent:`system.file("llms.txt", package = "LISTR")`

## 许可

GPL (>= 2)。Copyright (c) 2026 惟安数据科技(北京)有限公司。
详见 [inst/COPYRIGHTS](inst/COPYRIGHTS)。
