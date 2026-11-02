# Analysis code

Reproduces every table and figure in the manuscript and supplement from `data/data1.csv` (602 respondents, no missing values).

Run from the project folder (the one containing `data/`, `analysis/`, `outputs/`):

| Step | Script | What it does | Time |
|---|---|---|---|
| 0 | `Rscript analysis/00_prepare_data.R` | Recodes variables, verifies each composite score equals the sum of its items | seconds |
| 1 | `Rscript analysis/01_main_models.R` | Table 1, pooled logistic / Firth / modified Poisson / AME, GVIF, bootstrap validation, nested blocks, E-values, KR-20 | ~30 s |
| 2 | `Rscript analysis/02_heterogeneity_models.R` | District interaction tests, stratified Firth models, g-computation with 1,000 bootstrap resamples | ~10 min |
| 3 | `Rscript analysis/03_sensitivity.R` | Nine specifications, item-level models, reasons and problems after moving | ~30 s |
| 4 | `python analysis/04_ml_validation.py` | Repeated cross-validation (logistic, random forest, gradient boosting), grouped permutation importance | ~60 min |
| 4b | `python analysis/04b_cv_interactions.py` | Ridge with and without district interactions vs random forest | ~10 min |
| 5 | `Rscript analysis/05_figures_models.R` | Figures 4, 5, S1, S2 | seconds |
| 6 | `python analysis/06_figures_descriptive.py` | Figures 2, 3, S3, S4 | seconds |
| 7 | `python analysis/07_export_excel.py` | All tables into `outputs/All_results_tables.xlsx` | seconds |
| 8 | `python manuscript/build_docx.py` | Builds `Manuscript.docx` and `Supplementary_Material.docx`, audits citations | seconds |

Software: R 4.6.1 with dplyr, tidyr, ggplot2, patchwork, logistf, rms, pROC, ResourceSelection, sandwich, lmtest, car, marginaleffects, EValue; Python 3.12 with pandas, numpy, scikit-learn 1.9, matplotlib, python-docx, openpyxl.

Seeds are fixed in every script (fieldwork start and end dates), so bootstrap and cross-validation results reproduce exactly.

Coding notes
- Occupation: manual labour = day labourer, farmer, fisher; other = housewife, shopkeeper, other (as in the submitted Table 1).
- Impact severity enters the pooled model in its original five levels and the district comparison in three levels (none or minor, recoverable, irrecoverable) because the five-level reference cell holds only 3 and 2 Kurigram respondents.

## Version 4: rebuilt on the raw KoboToolbox export

The raw exports are in `data/raw_kobo/` (copied unchanged from the the project project folder). Version 4 links `data1.csv` back to the raw records, defines the analytic sample from the raw answers (adults with a yes or no answer to item 5.13, n = 571), and adds field-site, interviewer and transportability analyses. Outputs go to `outputs/v4/` and `outputs/figures_v4/`; version 1 to 3 outputs are untouched.

| Step | Script | What it does |
|---|---|---|
| 12 | `python analysis/12_link_raw_kobo.py` | Links the 602 data1 rows to the 603 raw interviews (optimal assignment over 35 fields), audits every recode, assigns field site, upazila and anonymised interviewer (E1 to E7) |
| 12b | `python analysis/12b_sunamganj.py` | Extracts and codes the 2024 Sunamganj round for the transport check |
| 13 | `Rscript analysis/13_prepare_v4.R` | Adds raw fields to the analysis file; defines the v4 analytic sample |
| 14 | `Rscript analysis/14_v4_models.R` | Revision analysis (script 08) re-run on n = 571, plus outcome-coding sensitivity (~20 min) |
| 14b | `Rscript analysis/14b_v4_items.R` | Item-level models, GVIF |
| 14c, 14d | `python analysis/14c_v4_ml_validation.py`, `14d_v4_cv_interactions.py` | Cross-validated learners and permutation importance (~60 min) |
| 15 | `Rscript analysis/15_design_robustness.R` | Site and interviewer prevalence, crossed random effects, CR2 / fixed-effect re-estimation, heterogeneity within sites and interviewers, leave-one-interviewer-out, convergent validity, time since arrival, transportability |
| 15b | `Rscript analysis/15b_interviewer_item_dependence.R` | Cramér's V of every item with interviewer |
| 16 | `Rscript analysis/16_figures_v4.R` | Figures 3 to 5 and S1 to S3, S6 |
| 17 | `Rscript analysis/17_descriptives_v4.R` | Table 2 (sample characteristics), district item profiles, destination choice |
| 06b, 09b, 11b | `python analysis/06b_figures_descriptive_v4.py`, `09b_study_area_map_v4.py`, `11b_framework_v4.py` | Figures S4, S5, S7; Fig. 1 map with surveyed upazilas; Fig. 2 framework |
| 18 | `python analysis/18_export_excel_v4.py` | All v4 tables in `outputs/v4/the project_v4_tables.xlsx` |
| docs | `python manuscript/build_docx_v4.py`, `build_supplement_v4.py`, `build_note_v4.py` | Manuscript (highlighted and clean), supplement, revision note |
| pdf | `powershell -File manuscript/render_v4.ps1` | Word-to-PDF for the v4 documents; run only when Word is closed |

Additional R package: clubSandwich (CR2 cluster-robust variance). Correction to the note above: the seed 20240531 is an arbitrary fixed value; it is not a fieldwork date (fieldwork took place in September and October 2023).

## Version 5: language revision

Same data, models and estimates as version 4. `manuscript/content_v5.py` is a full rewrite of the text (main text 7,247 words, abstract 249, 87 references, short captions); `build_docx_v5.py`, `build_supplement_v5.py` and `build_journals_v5.py` produce `Manuscript_v5_highlighted.docx` (changes since v4 in yellow), `Manuscript_v5_clean.docx`, `Supplementary_Material_v5.docx` and `Journal_Shortlist_v5.docx`. PDFs: `powershell -ExecutionPolicy Bypass -File manuscript/render_v5.ps1` with Word closed.

## Version 6: submission files for Population and Environment

`manuscript/content_v6.py` restructures the paper around four stated hypotheses (H1 to H4), builds the Results on the main tables and figures, and organises the Discussion around the hypotheses. New or changed figures: Fig. 1 framework with hypothesis tags (`11c_framework_v6.py`), Fig. 2 map with OpenStreetMap river channels replacing the generalised Natural Earth centre-lines (`09c_osm_water.py`, `09d_study_area_map_v6.py`; OSM extracts in `gis/osm/`), Fig. 4 standardised probabilities (`16b_fig4_v6.R`). Builders: `build_docx_v6.py`, `build_supplement_v6.py`, `build_titlepage_v6.py`; outputs in `manuscript/submission_v6/`. PDFs: `powershell -ExecutionPolicy Bypass -File manuscript/render_v6.ps1` with Word closed.


### Final figure numbers (v7)

Script and output file names keep the numbering of earlier drafts. In the submitted paper:

- Fig. 1 framework: `11c_framework_v6.py`. Fig. 2 map: `09d_study_area_map_v6.py`.
- Fig. 3 destination choice and problems: `06b_figures_descriptive_v4.py`.
- Fig. 4 settlements and interviewers, Fig. 6 design-based estimates: `16_figures_v4.R`. Fig. 5 standardised probabilities: `16b_fig4_v6.R`.
- Online Resource 1: Fig. S1 file FigS1, Fig. S2 file FigS2, Fig. S3 file Fig4_prevalence_ratios, Fig. S4 file FigS5, Fig. S5 file FigS3, Fig. S6 file FigS7.
