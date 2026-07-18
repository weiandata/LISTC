# CRAN comments — LISTR 1.0.0

## Submission

First submission of LISTR to CRAN.

## Test environments

* Local: macOS (aarch64-apple-darwin23), R 4.6.0
* GitHub Actions: ubuntu-latest, R release

## R CMD check results

0 errors | 0 warnings | 0 notes (R CMD check --as-cran, local).

Expected on incoming checks: the "New submission" NOTE.

## Notes for reviewers

* The package targets Chinese-language survey practitioners: runtime
  messages and some output labels are in Chinese. All such strings in
  R sources use \uxxxx escapes; the package Encoding is UTF-8 and the
  Language field is set to zh-CN. Function documentation (Rd) is in
  English.
* Test coverage is 95.9% overall; statistical formulas are additionally
  validated against Monte Carlo simulations in the test suite (marked
  skip_on_cran where long-running).
* Examples write only to tempdir()/tempfile().
