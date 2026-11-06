# Data and code: religiosity, political ideology, and civic pro-environmental behaviour

Data and analysis code for the manuscript *Meaning systems under secularisation:
Political ideology, but not self-assessed religiosity, is associated with civic
pro-environmental behaviour in eight European coastal case studies*.

## Supplementary materials

### [⬇ Download the supplementary materials (Word document)](https://github.com/amorphia78/pro-coast_religion_release_1/raw/refs/heads/main/output/religion_supplementary.docx)

Sample description, descriptive statistics, prior robustness checks, full model
tables, the suppression check, model diagnostics and descriptive charts. Word asks to
update fields when the document opens; accept this to build its table of contents.

## About this package

Running this package reproduces every table, figure and generated number in the
main text, and the whole of the supplementary materials document.

The analysis was preregistered: <https://doi.org/10.5281/zenodo.20234640>.
Deviations are listed in [docs/deviations_from_preregistration.md](docs/deviations_from_preregistration.md).

**Licence:** [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).
If you use these data or this code, please cite the article.

## Contents

| Path | What it is |
|---|---|
| `data/procoast_wave2_religion_cpeb.tsv` | Survey responses: only the variables used in the article |
| `data/codebook.md` | Every variable, its wording and its values |
| `data/recruitment_methods.csv` | Case-study recruitment descriptions, for Table S1 |
| `src/data_prep.R` | Loads the data and builds the z-scored model variables |
| `src/religion_analysis.R` | The preregistered models and everything reported from them |
| `src/religion_descriptives.R` | Sample description, correlations and descriptive charts |
| `src/religion_table_output.R` | Shared code for writing the tables |
| `src/religion_supplementary.py` | Assembles the supplementary `.docx` |
| `docs/deviations_from_preregistration.md` | Departures from the preregistered plan |
| `output/` | The results as reported: tables, figures and supplementary materials |

## Running it

Run these from the root of this package, in this order:

```bash
Rscript src/religion_analysis.R
```

```bash
Rscript src/religion_descriptives.R
```

```bash
python src/religion_supplementary.py
```

The first command fits ten Bayesian models, which took about 15 minutes on a desktop
PC. Fits are saved to `output/models/` and read back on later runs; to recompute one,
name it in `REFIT` at the top of `src/religion_analysis.R`.

## Where each result ends up

Everything is written to `output/`, which as supplied holds the results reported in
the article. Rerunning overwrites those files, so keep a copy if you want to compare.

| In the article | File |
|---|---|
| Table 1 | `descriptives_summary.xlsx` |
| Table 2 | `correlations.xlsx` |
| Table 3 | `model_table_antidiversity.xlsx` |
| Table 4 | `model_table_leftright.xlsx` |
| Figure 1 | `religion_analysis.png` |
| Figure 2 | `sigma_relig_prior_posterior.png` |
| Other numbers in the text | `main_text_analysis.md`, `main_text_descriptives.md` |
| Supplementary materials, complete | `religion_supplementary.docx` |

The supplementary document's own tables and figures are in `output/supplementary/`
and `output/descriptive_charts/`.

## How closely a rerun matches the article

**Descriptive results** match exactly, because they involve no random numbers. The
one difference is cosmetic: categories with equal counts in the Participants text may
be listed in a different order.

**Model results** match to within Monte Carlo error rather than exactly. The models
are fitted by MCMC, without a fixed seed, so each fit is slightly different. In a test
refit from this package, estimates and interval bounds in Tables 3 and 4 differed from
the published values by at most 0.01, and probabilities by at most 0.03. In the
supplementary model tables, interval bounds for intercepts differed by up to 0.09, and
those for all other parameters by at most 0.02.

A refit may occasionally give divergent-transition or low effective-sample-size
warnings; the published fits had one divergent transition each in two of the ten
models. If a fit gives such warnings, recompute it by naming it in `REFIT`.

The points in the diagnostic
plots also shift slightly on every run, even from the same fit, because `residuals()`
in brms draws from the posterior predictive distribution by default.

## Requirements

Tested with the versions below on Windows 11. `rstan` needs a working C++ toolchain
(on Windows, Rtools).

- **R 4.5.0:** brms 2.22.0, rstan 2.32.7, posterior 1.6.1, ggplot2 4.0.1,
  patchwork 1.3.2, scales 1.4.0, psych 2.5.3, openxlsx 4.2.9, zip 2.3.3
- **Python 3.13.3:** python-docx 1.2.0, openpyxl 3.1.5

## About the data

The data come from the Wave 2 PRO-COAST stakeholder questionnaire. This file contains
13 of the 74 variables in the full survey file, unchanged apart from row order. Left
out are all response identifiers and dates, survey language, detailed stakeholder
roles, and every item the article does not use.

The file has 821 respondents from nine case studies. Montenegro's 102 respondents
enter no model because their questionnaire did not include the religiosity item; see
the [codebook](data/codebook.md) for why they are nevertheless in the file.

## Funding

EU Horizon grant PRO-COAST (Project ID 101082327), with additional support from UK
Research and Innovation for the same project (grant number 10115053).
