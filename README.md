# LISTR

Large-scale Item-response Statistics Tables in R.

Status: In development (design phase)

Owner: WEIAN DATA Engineering

## Project Overview

LISTR is an R package that turns assessment and survey sample data
(demographics, IDs, item responses, scores, sampling weights, ability
estimates and their IRT standard errors) into fully customizable,
Excel-pivot-style statistical tables. Users declare row variables, column
variables and cell statistics; every cell carries a measurement-sound
standard error.

Design document: [docs/design-v1.md](docs/design-v1.md)

## Features (planned, v1)

- Import from CSV/Excel, SPSS/SAS/Stata (with value labels), and IRT
  software person files (Winsteps PFILE, ConQuest).
- Weighted means, proportions above cut scores, proficiency-level
  percentages, quantiles, counts, and item-level statistics
  (p-values, option distributions).
- Measurement-error propagation from individual IRT standard errors
  (sampling + measurement variance components reported separately).
- Probabilistic level classification robust to measurement error near
  cut scores.
- Pivot-table interface (`lst_table()`) producing tidy long and wide
  layouts, with formatted Excel export.
- Three audiences, one engine: config-template + `lst_run()` one-shot
  entry for non-R survey staff, the full function API for statisticians,
  and JSON output plus `inst/llms.txt` and a config JSON Schema for AI
  agents. Rule-based plain-language interpretation guards non-experts
  against misreading standard errors.
- Million-scale performance: data.table aggregation backend, column
  pruning on import and per-item chunking, targeting 5M persons on a
  16GB office laptop.

## Repository Structure

```text
.
├── .github/
│   ├── ISSUE_TEMPLATE/
│   ├── workflows/
│   └── PULL_REQUEST_TEMPLATE.md
├── docs/
├── examples/
├── scripts/
├── templates/licensing/
├── .editorconfig
├── .gitignore
├── CHANGELOG.md
├── CODEOWNERS
├── CONTRIBUTING.md
├── PROPRIETARY.md
├── README.md
└── SECURITY.md
```

Language-native source and test directories belong in the generated
repository, not in this common template.

## Getting Started

1. On GitHub, open this repository and select **Use this template**.
2. Choose **Create a new repository**.
3. Enter a lowercase, hyphen-separated repository name and select the correct
   owner and visibility.
4. Clone the generated repository and create a short-lived topic branch from
   `main`.
5. Select the required files from [licensing profiles](templates/licensing/README.md):
   R packages use GPL version 2 or later; static websites and WAEF-style
   frameworks use the proprietary profile.
6. Add reproducible setup, validation, security, and release automation before
   the repository is relied on.
7. Configure branch protection, required checks, and appropriate reviewers in
   GitHub.

Repository names use lowercase kebab case, for example `irt-engine` or
`knowledge-base`.

## Development

Changes use short-lived branches named `<category>/<kebab-case-topic>`, such as
`feature/add-score-export` or `docs/clarify-setup`. Submit changes through a
pull request and keep `main` releasable. See [CONTRIBUTING.md](CONTRIBUTING.md)
for commit, review, testing, and evidence requirements.

The shared workflow checks Markdown style and links. Generated repositories
must add the language-specific build, test, dependency, security, and release
checks required by their project.

## Documentation

- [Documentation index](docs/README.md)
- [Repository standard summary](docs/repository-standard.md)
- [Template development guide](docs/Repository_Template_Development_Guide.md)
- [WeianData Engineering Handbook](https://github.com/weiandata/.github/blob/main/handbook/README.md)

The Engineering Handbook is the normative source for engineering rules. This
template implements safe defaults and does not replace or redefine those
standards.

## Repository Lifecycle

Every repository moves through planning, development, testing, release,
maintenance, and eventual archival. The owner must keep purpose, status,
validation evidence, supported versions, and maintenance expectations current
throughout that lifecycle.

## Data and Security

Do not commit credentials, secrets, personal information, restricted client
data, or unapproved datasets. Use synthetic, public, or explicitly authorized
fixtures and follow [SECURITY.md](SECURITY.md) for private vulnerability
reporting.

## Support

Use the repository's issue templates for non-sensitive defects, features, and
documentation work. Report security concerns only through the private channels
listed in [SECURITY.md](SECURITY.md).

## Copyright and licensing

Copyright (c) 2026 WEIAN DATA TECH (Beijing) Co., Ltd. All rights reserved.
This template repository is proprietary; see
[PROPRIETARY.md](PROPRIETARY.md). Generated repositories must apply the
[profile-specific assets](templates/licensing/README.md) before publication.
