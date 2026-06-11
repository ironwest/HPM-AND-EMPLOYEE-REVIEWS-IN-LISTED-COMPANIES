# HPM and employee reviews — analysis code

Code accompanying the manuscript "Association between Health and Productivity Management Survey scores and independent employee review ratings" (submitted to Industrial Health).

# HPM Survey Scores and Independent Employee Review Ratings

Data-processing and analysis code for:

> Nishida N. *Association between Health and Productivity Management Survey scores and independent employee review ratings: a cross-sectional study of listed companies using public data.* (submitted to *Industrial Health*)

This repository contains the R scripts used to construct the analytical dataset linking three sources — METI's Health and Productivity Management (HPM) Survey feedback sheets (fiscal 2023–2026), employee review data from Tenshoku-kaigi (Livesense Inc.), and annual securities reports from the FSA's EDINET — and to produce all tables and figures in the manuscript.

## Important notes on reproducibility

These are the **working scripts as actually used**, published for transparency and auditability rather than as a one-command pipeline:

- Scripts were executed **interactively** (RStudio), section by section. Some sections exist purely for inspection (`View()`-style checks, `clipr::write_clip()` calls that copy intermediate tables to the clipboard for manual verification, a helper that opens Google searches in a browser during manual company matching).
- Manual-resolution decisions (duplicate company codes, name changes, ambiguous matches) are recorded **in full as inline comments**, including the evidence consulted (feedback-sheet URLs, deviation-score agreement, insurer names, corporate-history pages). These comments are the audit trail for every hand-coded correspondence in `replace_code`, `matchbysearch`, `extrac_matching`, and the tkid assignments.
- Raw input data are **not** redistributed in this repository (see Data availability below). To re-run the pipeline you must obtain the inputs yourself and place them in the directory layout described below.
- The scripts are provided **as-is**. They may contain exploratory sections, superseded code fragments, and comments from earlier iterations of the analysis; they are not guaranteed to run top-to-bottom without modification. The published manuscript is the authoritative record of the final analytical specification and sample sizes.

## Pipeline

Scripts are numbered in execution order. Each script reads from `data/` (raw inputs) and/or `middledata/` (intermediate `.rds` files written by earlier scripts).

| Script | Purpose | Key outputs |
|---|---|---|
| `01_question_map.R` | Reads the four HPM Survey result Excel files (FY2023–2026), reconciles question numbering across years using the cross-year correspondence table extracted from the questionnaire PDFs (`pdftools::pdf_data`), and assigns a year-invariant question ID. | `middledata/question_map.rds` |
| `02_resolve_companycode_kenkoukeiei.R` | Standardizes company-code formats across years (leading `0` vs `A`), detects duplicate codes/names, and resolves every case by manual review (feedback sheets, deviation-score agreement, insurer, industry). All decisions documented inline. | `middledata/d23.rds` … `d26.rds` |
| `03_build_panel_kenkoukeiei.R` | Builds the four-year HPM company panel: harmonizes deviation-score column names across years, normalizes company names, verifies all 138 code-level name conflicts are explained by name changes, and pivots to one row per company. | `middledata/panel_kenkoukeiei.rds` |
| `04_scrape_and_extract_edinet.R` | (a) Crawls the EDINET document-list API (2017-01-01 to 2026-06-01) for annual securities reports (docType 120) with resumable progress logging; (b) matches HPM-panel listed companies to EDINET codes (exact match on normalized names plus documented manual resolution of ~96 cases, mostly delistings); (c) bulk-downloads the CSV-converted XBRL archives. Requires `EDINET_API_KEY` (see below). | `output/edinet_index.csv`, `middledata/kktoedinetcode.rds`, `data/edinet/docs/*.zip` |
| `05_tougou_process_extradata.R` | Extracts the target XBRL elements (net income, total assets, employees, average age/tenure/salary, etc.) from the downloaded CSV archives in parallel (`furrr`), pivots to one row per company-year, and computes employee-count-based auxiliary variables. | `middledata/edinetlong.rds`, `middledata/wideedinet.rds` |
| `06_match_tenshokukaigi_to_kk.R` | Links the Tenshoku-kaigi review data (3,091 companies, provided under agreement with Livesense Inc.) to the HPM panel via normalized-name exact matching; resolves the four residual cases and three split-review name-change cases manually (documented inline). | `middledata/kktotk.rds`, `middledata/tk.rds` |
| `07_make_panel_master_tantai.R` | Assembles the cross-sectional master table (one row per company): HPM aggregates (latest / mean / OLS slope over FY2022–2025 evaluation years), firm attributes, review scores, and non-consolidated EDINET variables. | `middledata/master_cross_v3.rds` |
| `08_figure1_flow.R` | Computes the sample-selection counts for the flow diagram (Fig. 1) and constructs the analysis dataset, including the holding-company indicators (non-consolidated/consolidated employee ratio, affiliate-shares/total-assets ratio). | `middledata/datafor_analysis.rds` |
| `09_table1_tertile.R` | Table 1: descriptive statistics by tertile of the latest HPM deviation score (`gtsummary`), with variable construction (winsorized ROA, industry lumping/recoding, English labels). | `middledata/analye_this.rds` |
| `10_table2_m0m5.R` | Table 2: hierarchical OLS models M0–M5 (industry fixed effects, industry-clustered SEs, `fixest`), fully standardized coefficients, E-values (`EValue`), CR2 small-sample robustness check (`clubSandwich`), and model diagnostics (VIF, splines, DFBETAS, residual checks). | `middledata/data_for_sensitivityanalysis.rds` |
| `11_table3_sensitivity.R` | Table 3: sensitivity analyses — (A) ≥100 reviews; (B) four-consecutive-year participants; (C) log(1+reviews) weighting; (D) mean HPM score as exposure; (E1/E2) excluding suspected holding companies (employee ratio ≤2% / ≤10%). | `middledata/datforgraph2.rds` |
| `12_table4_low.R` | Figure 2: Models 1–6 for each of the 12 review axes, Holm correction across axes for the fully adjusted model, and the forest-plot figure (`ggplot2` + `ragg`). | `paper1/TOUKOUYOU/figure2.png` |

## Expected directory layout

```
.
├── 01_question_map.R … 12_table4_low.R
├── data/
│   ├── kenko_keiei/
│   │   ├── raw/            # METI HPM Survey result Excel files (FY2023–2026)
│   │   └── questionnaire/  # HPM Survey questionnaire sample PDFs (FY2023–2026)
│   ├── edinet/
│   │   ├── edinet_codes.csv  # FSA "EDINET code list" CSV
│   │   ├── docs/             # bulk-downloaded XBRL-CSV zip archives (script 04)
│   │   └── extradocs/        # manually downloaded archives for delisted firms
│   └── kuchikomi/tenshokukaigi/  # Tenshoku-kaigi snapshot (NOT redistributable)
├── middledata/   # intermediate .rds files (created by the scripts)
└── output/       # EDINET crawl index and progress logs
```

## Data availability

| Source | Status | How to obtain |
|---|---|---|
| METI HPM Survey feedback sheets & questionnaires (FY2023–2026) | Public | METI's HPM (健康経営) website. Place the result Excel files in `data/kenko_keiei/raw/` and the questionnaire sample PDFs in `data/kenko_keiei/questionnaire/` with the file names referenced in scripts 01–02. |
| EDINET annual securities reports & code list | Public | FSA EDINET. The code list CSV goes to `data/edinet/edinet_codes.csv`; the report archives are downloaded by script 04 via the EDINET API v2. |
| Tenshoku-kaigi review data | **Not redistributable** | Provided to the author under a data-provision agreement with Livesense Inc. (snapshot of publicly displayed ratings as of 2026-05-25; no personal information). Researchers wishing to use equivalent data should contact Livesense Inc. directly. |

Because the review data cannot be redistributed, scripts 06 onward cannot be re-executed end-to-end by third parties. They are published so that every processing and modeling decision can be inspected.

## Requirements

- R ≥ 4.5 (analyses in the paper used R 4.5.2)
- Packages: `tidyverse`, `readxl`, `openxlsx`, `pdftools`, `httr2`, `furrr`, `parallelly`, `lubridate`, `Hmisc`, `gtsummary`, `fixest` (0.14.1), `broom`, `EValue`, `clubSandwich` (0.7.0), `splines`, `scales`, `knitr`, `ragg`, `clipr`

### EDINET API key

Script 04 uses the EDINET API v2, which requires a subscription key. Register at the EDINET API portal, then set the key as an environment variable before running:

```r
# ~/.Renviron
EDINET_API_KEY=your_key_here
```

The crawl and download loops are resumable: progress is logged to `output/edinet_index_progress.csv` and `output/edinet_downloads_progress.csv`, and already-downloaded non-empty archives are skipped.

## Citation

If you use this code, please cite the paper above. The manuscript's Methods section ("Data processing") provides the narrative overview corresponding to scripts 01–08.

## License

Code: MIT License (see `LICENSE`). The license covers the code only; it does not extend to any third-party data described above.

## Contact

Norimitsu Nishida — FactoryHealth Inc. (norimitsu-nishida@factory-health.com)
