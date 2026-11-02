"""07_export_excel.py: collect every results table into one formatted Excel workbook."""
from pathlib import Path

import pandas as pd
from openpyxl import load_workbook
from openpyxl.styles import Alignment, Border, Font, Side

ROOT = Path(__file__).resolve().parents[1]
T = ROOT / "outputs" / "tables"
SHEETS = [
    ("T1_by_displacement", "Table1_characteristics_by_displacement.csv", "Table 1. Respondent characteristics by displacement status"),
    ("T2_pooled_models", "Table3_pooled_models_raw.csv", "Table 2. Crude (COR), adjusted (AOR), Firth and modified-Poisson (aPR) estimates"),
    ("T2b_AME", "TableS_AME_pooled.csv", "Table 2 (cont.). Average marginal effects, percentage points"),
    ("T3_district_models", "Table4_stratified_firth.csv", "Table 3. District-stratified Firth logistic models"),
    ("T3b_heterogeneity", "Table_heterogeneity_by_predictor.csv", "Table 3 (cont.). Covariate-by-district interaction tests"),
    ("T3c_forest_data", "FigData_forest.csv", "Fig. 4 data. Pooled (comparative specification) and district Firth estimates"),
    ("T4_performance", "Table_performance_pooled.csv", "Table 4. Pooled model performance"),
    ("T4b_global_het", "Table_global_heterogeneity.csv", "Table 4 (cont.). Global heterogeneity test"),
    ("S1_reliability", "TableS_index_reliability.csv", "Table S1. KR-20 of count indices"),
    ("S2_items_by_district", "Table2_hazard_profiles_by_district.csv", "Table S2. Item prevalence by district"),
    ("S3_GVIF", "TableS_GVIF.csv", "Table S3. Generalised VIF"),
    ("S4_nested_blocks", "Table_nested_blocks.csv", "Table S4. Domain blocks entered in sequence"),
    ("S5_sensitivity", "TableS_sensitivity_key_estimates.csv", "Table S5. Robustness across specifications"),
    ("S6_item_models", "TableS_item_level_models.csv", "Table S6. Item-level Firth models"),
    ("S7_Evalues", "TableS_Evalues.csv", "Table S7. E-values"),
    ("S8_crossvalidation", "TableS_ml_crossvalidation.csv", "Table S8. Repeated cross-validation"),
    ("S8b_interaction_cv", "TableS_cv_interaction_check.csv", "Table S8 (cont.). Ridge with district interactions vs random forest"),
    ("S9_gcomputation", "FigData_gcomp_by_district.csv", "Table S9. Standardised probabilities by district"),
    ("S_bootstrap_valid", "TableS_bootstrap_validation.csv", "Bootstrap optimism (rms::validate, B = 1000)"),
    ("S_scores_by_district", "TableS_scores_by_district.csv", "Mean scores by district (Wilcoxon p)"),
    ("S_permutation_imp", "FigData_permutation_importance.csv", "Grouped permutation importance (random forest)"),
    ("S_reasons_problems", "FigData_reasons_issues.csv", "Reasons for moving and problems after moving (displaced only)"),
]


def numeric(s):
    try:
        return pd.to_numeric(s)
    except (ValueError, TypeError):
        return s


if __name__ == "__main__":
    out = ROOT / "outputs" / "All_results_tables.xlsx"
    with pd.ExcelWriter(out, engine="openpyxl") as xw:
        for name, f, _ in SHEETS:
            df = pd.read_csv(T / f, keep_default_na=False).apply(numeric)
            df.to_excel(xw, sheet_name=name, index=False, startrow=2)
    wb = load_workbook(out)
    thin, thick = Side(style="thin"), Side(style="medium")
    for name, _, title in SHEETS:
        ws = wb[name]
        ws["A1"] = title
        ws["A1"].font = Font(name="Times New Roman", bold=True, size=11)
        for cell in ws[3]:
            cell.font = Font(name="Times New Roman", bold=True, size=10)
            cell.border = Border(top=thick, bottom=thin)
            cell.alignment = Alignment(wrap_text=True, vertical="center")
        for row in ws.iter_rows(min_row=4):
            for cell in row:
                cell.font = Font(name="Times New Roman", size=10)
                if isinstance(cell.value, float):
                    cell.number_format = "0.000"
        for cell in ws[ws.max_row]:
            cell.border = Border(bottom=thick)
        for col in ws.columns:
            w = max(len(str(c.value)) if c.value is not None else 0 for c in col[2:])
            ws.column_dimensions[col[0].column_letter].width = min(max(10, w + 2), 55)
        ws.freeze_panes = "A4"
    wb.save(out)
    print("saved", out.name, len(SHEETS), "sheets")
