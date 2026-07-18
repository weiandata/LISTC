# Changelog

All notable changes to this repository are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- v0.1 implementation of the LISTR package: `lst_data()` roles/validation,
  weighted statistics (`st_mean`, `st_sd`, `st_prop_above`,
  `st_level_prop`, `st_quantile`, `st_count`/`st_wcount`, `st_pvalue`,
  `st_option_dist`) with sampling + measurement SE components,
  probabilistic classification (`method = "prob"`), empirical-Bayes
  `correction = "latent"` for WLE/ML estimates, data.table pivot engine
  (`lst_table()`, `as_long()`/`as_wide()`, margins), import layer
  (`read_listr()` with `col_select`), config layer (`lst_config()`,
  `lst_run()`), Excel/JSON export with rule-based interpretation
  (`lst_to_excel()`, `lst_to_json()`, `lst_interpret()`), testthat
  suite with Monte Carlo checks, `inst/llms.txt`, config JSON Schema,
  example config and demo script, and an R CMD check workflow.
- Design document `docs/design-v1.md` (v1, including audience tiers,
  performance targets, and the simulation-revised correction semantics).
- Add profile-specific GPL R-package and proprietary-project licensing assets.
- Apply the canonical proprietary notice to the template repository itself.

### Changed

- Replace the generic license placeholder with deterministic company profile
  selection guidance.

## [1.0.0] - 2026-07-10

### Added

- Establish the language-independent WeianData repository template.
- Add governance, contribution, security, ownership, and versioning documents.
- Add issue and pull request templates.
- Add Markdown and link validation workflow.
- Add documentation, examples, and scripts guidance.
