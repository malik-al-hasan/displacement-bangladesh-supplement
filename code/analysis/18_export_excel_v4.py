"""18_export_excel_v4.py  -  every v4 results table in one workbook (outputs/v4/the project_v4_tables.xlsx), with an index."""
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
V4 = ROOT / "outputs" / "v4"
files = sorted([p for p in V4.rglob("*.csv") if not p.name.startswith("analysis_data")],
               key=lambda p: (not p.name.startswith("Table_"), p.name))
out = V4 / "the project_v4_tables.xlsx"
with pd.ExcelWriter(out, engine="openpyxl") as xw:
    index = []
    for i, f in enumerate(files, start=1):
        sheet = f"{i:02d}_{f.stem}"[:31]
        df = pd.read_csv(f, keep_default_na=False)
        num = df.select_dtypes("number").columns
        df[num] = df[num].round(4)
        df.to_excel(xw, sheet_name=sheet, index=False)
        index.append({"sheet": sheet, "file": str(f.relative_to(ROOT)), "rows": len(df)})
    pd.DataFrame(index).to_excel(xw, sheet_name="00_index", index=False)
print(out, len(files), "tables")
