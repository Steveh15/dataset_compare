
<!-- ### Contact -->

<!-- Stephen Mackie [smackie@its.jnj.com](mailto:test) -->

<!-- ### App version  -->

<!-- App version: 1 -->
<!-- App version release date: Today -->

<!-- ### Objectives of why the App was built and what it does -->

<!-- ### Who the target users of the app are?  -->

<!-- ### What the key inputs/outputs of the app are? -->

<!-- ### An Acknowledgments Section -->

<!-- ### Confidentiality Statement -->



# User Guide: Dataset Comparison Tool

### Contact

Stephen Mackie [smackie@its.jnj.com](mailto:test)

### App version

App version: 1
App version release date: Today

## Purpose

This application has been built to compare ADPP and ADPP-like datasets and produce a comparison report of the two datasets. An interactive report detailing the differences is generated which can be downloaded as HTML and saved for reference if needed. The report can assist is demonstrating that the ADPP and ADPP-like datasets are equivalent and the same statistical outputs would be generated from either dataset.

## How to use

Simply upload the datasets and press **Compare Datasets** to generate a comparison report. There are two additional options that can be selected before generating the report.

1. **Include folder paths for the report?** - For traceability, it can be useful to include the original folder paths of the uploaded datasets in the report. However, due to technical limitations, the app cannot detect the folder paths automatically after file upload. If you’d like to include this information, tick this box — two text fields will appear for you to enter the folder paths manually.

2. **Define unique keys for row-level checks?** To enable detailed row-level comparisons, you must define **unique key variables** — a combination of variables that uniquely identifies each row in the dataset. These must be consistent across both datasets. Common examples include USUBJID combined with a `--SEQ` variable (e.g., `PCSEQ`, `ASEQ`). If your datasets use parameter codes like `PARAMCD` or `PPTESTCD`, you may also include these to improve clarity in the comparison report. The app will validate the keys, and an error will be shown if they do not uniquely identify rows.

You can add comments to the report once it is generated which will be displayed in the app and included in the downloadable HTML report. This allows you to annotate findings or record decisions made during review.

The following conditions must be met before a report can be generated.

* **Unique Key Validation** If unique keys are defined then they must exist in each dataset and uniquely define each row in both datasets.
* **Dataset Validation** Each dataset must contain a `PARAMCD` and `AVAL` variable.

## Details of checks performed

The application compares two uploaded datasets by performing a series of automated checks, grouped into two categories:

- **Structure and Content Checks** — Basic dataset shape and format comparisons  
- **Row-Level Checks** — Detailed value comparisons (enabled only when unique keys are defined)

---

### Structure and Content Checks

These checks run on all datasets, even without key variables.

#### ✅ Differences in Number of Rows

Compares total row counts between the two datasets.

#### ✅ Differences in Columns

Identifies columns that are present in only one dataset.

#### ✅ Differences in Column Types

Lists shared variables that have different underlying data types (e.g., numeric vs character).

#### ✅ Differences in Rounding of AVAL

This check compares the **maximum number of decimal places** found in `AVAL` within each `PARAMCD` group. It is useful for identifying cases where `AVAL` values may have been rounded in one dataset but not the other.

To avoid false positives caused by floating-point precision differences, the check only considers the **first 10 decimal places** of each value.

For example, if `AVAL` is unrounded in one dataset and rounded to 3 decimal places in the other, the difference will be detected and tabulated here.

---

### Row-Level Checks

These checks are only available when **unique keys** have been defined and validated.

#### ✅ Unmatched Records

Shows which records appear in one dataset but not the other, based on the selected key variables.

#### ✅ AVAL Comparison

Compares the `AVAL` field between matched rows in both datasets. Includes two components:

**AVAL Differences Summary**

- Shows the total number of differing `AVAL` values
- Breaks them down by magnitude of difference:
  - ∆ ≥ 1e-3
  - 1e-3 > ∆ ≥ 1e-6
  - 1e-6 > ∆ ≥ 1e-9
  - ∆ ≤ 1e-9 (effectively negligible)
- Also reports the number of cases where `AVAL` is missing in one dataset

**AVAL Differences Table**

Displays every individual row-level `AVAL` mismatch for detailed inspection.

#### ✅ All Other Variables Comparison

Compares the values in all variables except `AVA`L and displays the results in two views:
- Lists every individual difference found across matched rows.
- Distinct Differences: Summarizes the unique combinations of variable name and differing values. Repeated occurrences of the same difference are shown only once.

---
