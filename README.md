# HPM and employee reviews — analysis code

Code accompanying the manuscript "Association between Health and Productivity Management Survey scores and independent employee review ratings" (submitted to Industrial Health).

## Files
- `01_build_hpm_panel.R` — merge METI HPM feedback sheets (FY2023–2026)
- `02_link_reviews_edinet.R` — link review and EDINET data
- `03_analysis.R` — main models and sensitivity analyses (fixest, clubSandwich)
- `04_figures.R` — Figures 1–2

## Data availability
- METI HPM Survey and EDINET data are publicly available (see manuscript).
- Review data were provided by Livesense Inc. under agreement and 
  **cannot be redistributed**; only the processing code is provided here.

## Environment
R 4.5.2. See manuscript for package versions.
