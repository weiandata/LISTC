# LISTR Quick Start (for non-programmers)

[中文版](quickstart-zh.md) · [Advanced guide →](advanced-en.md)

No programming required. The whole workflow is three steps: **set up
once → fill in one Excel configuration workbook → run one line**. For
every new dataset you only edit the workbook.

## Step 0: One-time setup (~10 minutes)

1. Install R from <https://cran.r-project.org> (pick your OS, click
   through the installer).
2. Install RStudio from <https://posit.co/download/rstudio-desktop/>.
3. Open RStudio, paste these two lines into the **Console**
   (bottom-left) and press Enter:

   ```r
   install.packages("remotes")
   remotes::install_github("weiandata/LISTR")
   ```

## Step 1: Generate and fill the configuration workbook

Paste into the Console and press Enter:

```r
LISTR::lst_config_template("my-config.xlsx")
```

Open **my-config.xlsx** in Excel. Read the instructions sheet first,
then fill the other four sheets following the grey example rows:

| Sheet | What to fill | Example |
| --- | --- | --- |
| Data & roles | where the data file is; which columns are id / weight / groups | data file `students.csv`, groups `region,gender` |
| Ability dimensions | ability and standard-error columns (leave empty if none) | math / th_math / se_math |
| Tables | the tables you want: rows, columns, statistics | rows `region`, cols `gender`, statistic "mean" |
| Output | where results go | `results.xlsx` |

Tips:

- Column names must match the data file **exactly** (case-sensitive).
- One row = one statistic; rows sharing the same table name merge into
  one pivot table.
- Level cut scores are written as `below=-Inf,basic=40,good=60,top=80`
  (name=lower bound, comma-separated; `-Inf` means no lower limit).
- Put `prob` in the method column to use each person's standard error
  probabilistically (recommended when SE columns exist); leave empty
  for hard classification.

## Step 2: Run

```r
LISTR::lst_run("my-config.xlsx")
```

Your output files appear within seconds.

## Step 3: Read the results

Open the generated **results.xlsx**:

- One worksheet per table. Cells look like **`52.62 (0.02)`** — the
  estimate, with its **standard error in parentheses** expressing
  uncertainty. Rule of thumb: treat a group difference as real only if
  it exceeds about twice the combined standard errors.
- The final **conclusions** sheet contains auto-generated plain-language
  findings (highest/lowest groups, significance, small-sample warnings)
  ready to quote in a report.

## When something goes wrong

Error messages state exactly which configuration cell to fix, e.g.
"table 1, statistic 'top rate' is missing a numeric cutoff". Common
cases:

| Error mentions | Cause | Fix |
| --- | --- | --- |
| data file not found | wrong path | put data and config in one folder, or use a full path |
| column does not exist | header spelling mismatch | copy-paste column names from the data file |
| missing worksheet | a sheet was renamed/deleted | regenerate the template, move your entries over |
| unsupported file format | broken extension (e.g. `.sav_copy`) | rename to a proper `.sav`/`.csv` |

Ready for more (multiple tables, replicate weights, PISA data, HTML
reports)? See the [advanced guide](advanced-en.md).
