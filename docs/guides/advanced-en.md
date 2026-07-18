# LISTC Advanced Guide (non-programmer edition)

[中文版](advanced-zh.md) · [← Quick start](quickstart-en.md)

Everything here is done through the **configuration workbook** — still
no code. Each section ends with the equivalent YAML so you can hand the
configuration to a colleague or an AI assistant.

## 1. Several tables in one run

In the tables sheet, **rows with different table names become different
tables**. Each table gets its own worksheet in the Excel output.

## 2. The nine statistics

| Statistic | Meaning | Also fill |
| --- | --- | --- |
| st_mean | weighted mean | variable |
| st_sd | weighted standard deviation | variable |
| st_prop_above | share of people at/above a cutoff | variable, cutoff |
| st_level_prop | share in each proficiency level | variable, cut scores |
| st_quantile | quantiles (median by default) | variable |
| st_count / st_wcount | count / weighted count | — |
| st_pvalue | item proportion correct | (declare response columns in roles) |
| st_option_dist | item option distribution incl. missing | same |

"Variable" is an ability dimension name (e.g. `math`) or any numeric
column in the data.

## 3. Hard vs probabilistic classification (the method column)

- Empty (hard): cut at the threshold. Simple, but people near the cut
  score get misclassified by measurement error, biasing the shares.
- `prob` (recommended): uses each person's ability ± SE to compute the
  **probability** of falling in each level, then aggregates. Robust to
  misclassification — a LISTC speciality.
- Correction column: leave **empty** for **EAP**-type ability estimates
  (flexMIRT/mirt); fill `latent` for **WLE/ML** estimates
  (Winsteps/ConQuest), otherwise extreme-level shares are
  overestimated. If unsure, ask whoever produced the ability estimates.

## 4. PISA/TIMSS-style international data

Cluster samples need **replicate weights** for correct standard errors.
In the data & roles sheet:

| Setting | Value |
| --- | --- |
| Replicate weight columns | `W_FSTR` (prefix, expands to W_FSTR1..80) |
| Replicate method | `fay` (PISA) or `jk2` (TIMSS) |

If the data carries **plausible values** (PV1MATH..PV10MATH), put
`PV#MATH` in the PV column of the ability-dimensions sheet (`#` stands
for the number) and leave ability/SE columns empty. LISTC performs the
Rubin combination automatically; declaring both PVs and replicate
weights reproduces the official PISA variance recipe. Do **not** set
method `prob` on a PV dimension — PVs already carry the measurement
uncertainty (the software will remind you).

## 5. Three outputs, three uses

| Output | Sheet entry | Best for |
| --- | --- | --- |
| Excel | Excel output path | day-to-day delivery, further editing |
| HTML | HTML output path | a polished report you can forward as-is |
| JSON | JSON output path | data teams and AI systems |

Fill all three to get all three.

## 6. The three standard errors

Every number carries three SE components (visible in JSON/long output):

- **se_sampling** — uncertainty from surveying only a sample;
- **se_measurement** — uncertainty from imperfect measurement (or
  variation across plausible values);
- **se_total** — the combination; **quote this one**.

The parenthesised value in Excel cells is se_total.

## 7. Reading rules of thumb

- **Significant difference**: claim "A higher than B" only when
  |A − B| > 2 × sqrt(SE_A² + SE_B²). The conclusions sheet applies this
  rule automatically.
- **Small cells**: results with n < 30 are unstable; the conclusions
  sheet flags them — quote with care.

## 8. Handing the configuration to an AI assistant

The workbook and YAML are equivalent. An assistant can generate a
`config.yml` like this, which you run with `lst_run("config.yml")`:

```yaml
data: students.csv
roles:
  id: student_id
  weight: w_final
  group: [region, gender]
  pv: {math: "PV#MATH"}        # or theta / theta_se
  rep_weights: W_FSTR
  rep_method: fay
tables:
  - name: overview
    rows: [region]
    margins: true
    values:
      mean: {stat: st_mean, var: math}
      top_rate: {stat: st_prop_above, var: math, cutoff: 1.2}
output:
  xlsx: results.xlsx
  html: report.html
```

The package ships an API digest for LLMs (`llms.txt`) and a JSON Schema
for configurations — hand those two files to the assistant and it can
write valid configurations for you.
