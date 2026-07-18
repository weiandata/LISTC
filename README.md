# LISTC

> **Pivot-style statistical tables for large-scale assessment data —
> every cell with the right standard error.**

[![R-CMD-check](https://github.com/weiandata/LISTC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/weiandata/LISTC/actions/workflows/R-CMD-check.yaml)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2%2B-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0)
[![coverage: 96.3%](https://img.shields.io/badge/coverage-96.3%25-brightgreen.svg)](https://github.com/weiandata/LISTC/blob/main/scripts/coverage.R)

**[中文文档 →](https://github.com/weiandata/LISTC/blob/main/README.zh-CN.md)** ·
[Quick start for non-programmers](https://github.com/weiandata/LISTC/blob/main/docs/guides/quickstart-en.md) ·
[Advanced guide](https://github.com/weiandata/LISTC/blob/main/docs/guides/advanced-en.md) ·
[Design document](https://github.com/weiandata/LISTC/blob/main/docs/design-v1.md)

LISTC turns assessment and survey microdata — demographics, scores,
sampling weights, IRT ability estimates with individual standard errors,
replicate weights, plausible values — into **Excel-pivot-style tables**
where you freely choose the row variables, column variables and cell
statistics, and every cell carries a **design-appropriate standard
error**.

No existing R package combines both halves: survey packages
(EdSurvey, intsvy, Rrepest, BIFIEsurvey) compute correct statistics but
offer no free-form table layout; pivot packages (pivottabler) lay out
tables but know nothing about weights, replicate designs or measurement
error. LISTC does both, at scale: **5 million respondents, full table
set, ~10 seconds on a laptop**.

## Installation

```r
# install.packages("LISTC")                      # once on CRAN
# install.packages("remotes")
remotes::install_github("weiandata/LISTC")       # development version
```

## Quick example

```r
library(LISTC)

# 1. Declare what each column means
x <- lst_data(students,
  id       = student_id,
  group    = c(region, gender),
  weight   = w_final,
  theta    = c(math = th_math),      # IRT ability estimate
  theta_se = c(math = se_math)       # its individual standard error
)

# 2. Lay out the pivot table: rows, columns, cell statistics
lv <- c(below = -Inf, basic = -0.5, good = 0.5, excellent = 1.2)
tab <- lst_table(x,
  rows = region, cols = gender,
  values = list(
    mean      = st_mean(math),
    excellent = st_prop_above(math, cutoff = 1.2, method = "prob"),
    levels    = st_level_prop(math, breaks = lv, method = "prob"),
    n         = st_count()
  ),
  margins = TRUE
)

# 3. Use the results
tab                          # wide layout: "0.52 (0.03)" per cell
as_long(tab)                 # tidy data with se_sampling / se_measurement
lst_to_excel(tab, "results.xlsx")
lst_to_html(tab, "report.html")
lst_interpret(tab)           # rule-based plain-language conclusions
```

## One engine, three audiences

| Audience | Interface | Output |
| --- | --- | --- |
| Survey staff (no R) | Fill an **Excel configuration workbook** (`lst_config_template()`), run one line: `lst_run("config.xlsx")` | Styled Excel + auto interpretation sheet, HTML report |
| Statisticians | Full function API (`lst_data → st_* → lst_table`) | `listc_table` object, tidy long data, all SE components |
| AI agents / pipelines | YAML/JSON config for `lst_run()`; JSON Schema + `llms.txt` ship in `inst/` | Machine-readable JSON with per-statistic metadata |

## Variance engines

Total variance = sampling + measurement, always reported as separate
components (`se_sampling`, `se_measurement`, `se_total`).

| Data situation | Declare | Sampling variance | Measurement variance |
| --- | --- | --- | --- |
| Simple weighted survey | `weight` | linearized | — |
| Operational IRT scores | `theta` + `theta_se` | linearized | delta-method propagation of individual SEs; probabilistic classification (`method = "prob"`); EB `correction = "latent"` for WLE/ML |
| PISA/TIMSS replicate designs | `rep_weights = "W_FSTR"`, `rep_method = "fay"` | BRR/Fay/JK1/JK2 | as above |
| Plausible values | `pv = list(math = "PV#MATH")` | linearized or replicate | Rubin (1987) combination |

All formulas are validated against Monte Carlo simulations in the test
suite (96.3% coverage; core engine files > 95%). A notable result baked
into the design: for EAP estimates with posterior SDs, probabilistic
classification is already calibrated — while WLE/ML estimates need the
`"latent"` empirical-Bayes correction. See
[design doc §4.1/§6](https://github.com/weiandata/LISTC/blob/main/docs/design-v1.md).

## Data in, results out

**In:** csv/tsv (data.table::fread), Excel, SPSS/SAS/Stata with value
labels (haven), Winsteps PFILE, ConQuest person files.
**Out:** styled Excel workbooks (Chinese-friendly fonts and widths),
tidy JSON with computation metadata, standalone HTML reports — each with
rule-based plain-language interpretation that guards non-experts against
misreading standard errors.

## Performance

data.table backend, column pruning on import, per-item chunking.
Measured on Apple Silicon (see `scripts/benchmark.R`):

| Task (50 item columns) | 1M persons | 5M persons |
| --- | --- | --- |
| mean + prob proportion + levels, 10×2 with margins | 2.0 s | 10.5 s |
| 50 item p-values × 10 groups | 1.6 s | 7.7 s |

## Documentation

* Vignette: `vignette("LISTC-intro")`
* Non-programmer guides (EN/中文): [docs/guides/](https://github.com/weiandata/LISTC/tree/main/docs/guides/)
* Design document with all methods and Monte Carlo evidence:
  [docs/design-v1.md](https://github.com/weiandata/LISTC/blob/main/docs/design-v1.md)
* For LLM agents: `system.file("llms.txt", package = "LISTC")`

## License

GPL (>= 2). Copyright (c) 2026 WEIAN DATA TECH (Beijing) Co., Ltd.
See [inst/COPYRIGHTS](inst/COPYRIGHTS).
