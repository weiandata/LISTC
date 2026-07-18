# CRAN submission — LISTC 1.0.0

Submit at <https://xmpalantir.wu.ac.at/cransubmit/>.

## Artifact

| Field | Value |
| --- | --- |
| File | `LISTC_1.0.0.tar.gz` |
| Size | 83,210 bytes |
| SHA-256 | `eb433259898c1b03b1a810ae8ab7006a6c07e4166fe1bc8c55ce70e9a6b4add2` |
| Built with | R 4.6.0, `R CMD build .` |

## Step 1 — form fields

| Form field | Paste this |
| --- | --- |
| Name | `Kunxiang Ma` |
| Email | `makunxiang@weiandata.com` |
| Package file | `LISTC_1.0.0.tar.gz` |

The email must match the `cre` maintainer address in `DESCRIPTION`, because
CRAN sends the confirmation link there.

## Step 2 — "Optional comment" box

Paste everything between the rules.

---

First submission of LISTC to CRAN.

Test environments:

* Local: macOS (aarch64-apple-darwin23), R 4.6.0
* GitHub Actions: ubuntu-latest, R release
* win-builder: R devel and R release

R CMD check results: 0 errors | 0 warnings | 1 note. The one note is the
expected "New submission".

Notes for reviewers:

* The package targets Chinese-language survey practitioners: runtime
  messages and some output labels are in Chinese. All such strings in R
  sources use \uxxxx escapes; the package Encoding is UTF-8 and the
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
* The package name is LISTC, not LISTR. CRAN already hosts `listr`
  (Tools for Lists); this package was renamed before submission
  specifically to avoid a name differing only in case.

---

## Before you submit

1. Rename the GitHub repository `weiandata/LISTR` -> `weiandata/LISTC`
   and make it public. Until then the three URLs in `DESCRIPTION` and
   `man/LISTC-package.Rd` return 404 and CRAN's incoming check flags
   them as possibly invalid.
2. Check the win-builder results emailed to `makunxiang@weiandata.com`
   (both R-devel and R-release were submitted).
3. Confirm the `Repository checks` workflow is green — its only
   remaining failure is the link check hitting those same two GitHub
   URLs.

## After you submit

CRAN emails a confirmation link to the maintainer address; the
submission is not queued until that link is clicked. Keep
`LISTC_1.0.0.tar.gz` and the `v1.0.0` tag unchanged until CRAN responds,
so that any requested revision starts from exactly what was reviewed.
