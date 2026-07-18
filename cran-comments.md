# CRAN comments — LISTC 1.0.0

## Submission

First submission of LISTC to CRAN.

## Test environments

* Local: macOS (aarch64-apple-darwin23), R 4.6.0
* GitHub Actions: ubuntu-latest, R release
* win-builder: R devel and R release

## R CMD check results

0 errors | 0 warnings | 1 note (R CMD check --as-cran, local).

The one note is the expected

```text
* checking CRAN incoming feasibility ... NOTE
New submission
```

## Notes for reviewers

* The package targets Chinese-language survey practitioners: runtime
  messages and some output labels are in Chinese. All such strings in
  R sources use \uxxxx escapes; the package Encoding is UTF-8 and the
  Language field is set to zh-CN. All Rd documentation is ASCII-only
  English, so the PDF reference manual builds without CJK LaTeX support.
* Test coverage is 96.1% overall (core variance/pivot engine files
  > 95%); statistical formulas are additionally validated against Monte
  Carlo simulations in the test suite (marked skip_on_cran where
  long-running).
* Examples and tests write only to tempdir()/tempfile().
* The two DOIs in Description are cited as evidence for the implemented
  methods: Woodruff (1952) for quantile confidence intervals and
  Rubin (1987) for combination across plausible values.
