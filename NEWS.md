# LISTC 1.0.0

First public release.

## Core

* `lst_data()` declares variable roles (id, groups, sampling weight,
  scores, ability estimates paired with individual IRT standard errors,
  item responses with scoring keys, replicate weights, plausible values).
* `lst_table()` builds Excel-pivot-style tables: free combination of row
  variables, column variables and cell statistics, with optional "Total"
  margins; results available as tidy long data (`as_long()`) or a
  formatted wide layout (`as_wide()`).
* Statistics: `st_mean()`, `st_sd()`, `st_prop_above()`,
  `st_level_prop()`, `st_quantile()`, `st_count()`/`st_wcount()`, item
  p-values `st_pvalue()` and option distributions `st_option_dist()`.

## Variance engines

* Linearized sampling variance by default; Woodruff intervals for
  quantiles.
* Measurement variance propagated from individual IRT standard errors;
  probabilistic classification (`method = "prob"`) robust to
  misclassification near cut scores; empirical-Bayes
  `correction = "latent"` for WLE/ML estimates.
* Replicate weights (`rep_weights`, methods fay/brr/jk1/jk2) for
  PISA/TIMSS-style designs, with prefix expansion such as `"W_FSTR"`.
* Plausible values (`pv = list(math = "PV#MATH")`) with Rubin
  combination, composing with replicate weights.

## Interfaces and output

* One-shot `lst_run()` driven by YAML/JSON text or an Excel
  configuration workbook (`lst_config_template()`); JSON Schema and an
  `llms.txt` API digest ship in `inst/` for automated agents.
* Importers for csv/tsv, Excel, SPSS/SAS/Stata (value labels preserved)
  and Winsteps/ConQuest person files.
* Export to styled Excel workbooks, machine-readable JSON and
  standalone HTML reports, each with rule-based plain-language
  interpretation.
