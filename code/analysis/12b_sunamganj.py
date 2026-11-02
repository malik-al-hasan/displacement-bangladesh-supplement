"""
12b_sunamganj.py

Extracts the 2024 Sunamganj round (Tahirpur and Bishwambarpur upazilas,
31 Oct - 10 Nov 2024; same Kobo form version as the 2023 rounds) from the
all-versions export and codes it exactly like the 2023 analysis file, so it
can serve as an out-of-region transport check.

Output: outputs/raw/sunamganj_2024_coded.csv
"""
import io
import re
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
raw = (ROOT / "data" / "raw_kobo" / "kobo_all_versions_export_2024-11-14_with_Sunamganj.csv").read_bytes()
df = pd.read_csv(io.StringIO(raw.decode("cp1252", errors="replace")), dtype=str, keep_default_na=False)
df = df[df["-"].str.startswith("2024")].copy()


def col(prefix):
    return [c for c in df.columns if c.startswith(prefix)][0]


def has(series, label):
    return series.str.contains(re.escape(label), case=False).astype(int)


rap, slo = df[col("3.2 Which kind")], df[col("4.1 What kinds of slow")]
imp = lambda s: pd.to_numeric(s.str[0], errors="coerce")
edu = df[col("3. What is your educational")]
s = pd.DataFrame({
    "district": "Sunamganj",
    "upazila": np.where(df["B. Upazilla name"].str.lower().str.startswith("tahir"), "Tahirpur", "Bishwambarpur"),
    "date": df["-"].str[:10],
    "gender": df[col("1. What is your gender?")],
    "age": df[col("2. What is your age?")],
    "edu3": np.select([edu == "No Education", edu == "Below Primary"], ["No education", "Below primary"], "Secondary or above"),
    "rapid": has(rap, "Floods") + has(rap, "Cyclones") + has(rap, "Drought") + has(rap, "Tidal Surge") + has(rap, "Heatwave"),
    "slow": has(slo, "Sea level rise") + has(slo, "Riverbank erosion") + has(slo, "Salinity intrusion") + has(slo, "Increase in temparature"),
    "imp_rapid_n": imp(df[col("3.3 Impacts")]),
    "imp_slow_n": imp(df[col("4.2 Impacts")]),
    "raw_513": df[col("5.13 Have you been displaced")].replace({"": "None"}),
    "residence": df[col("2.1 How long")],
})
cut3 = lambda x: np.select([x <= 2, x <= 4], ["None or minor", "Recoverable"], "Irrecoverable")
s["imp_rapid3"], s["imp_slow3"] = cut3(s.imp_rapid_n), cut3(s.imp_slow_n)
s["displaced_valid"] = s.raw_513.map({"Yes": 1, "No": 0})
s["in_primary"] = (s.age != "Under 18") & s.displaced_valid.notna() & s.gender.isin(["Male", "Female"])
s.to_csv(ROOT / "outputs" / "raw" / "sunamganj_2024_coded.csv", index=False)
print(len(s), s.in_primary.sum())
print(s.groupby("upazila").raw_513.value_counts().unstack(fill_value=0))
print(s[["rapid", "slow"]].describe().T)
print(s.edu3.value_counts().to_dict(), s.imp_rapid3.value_counts().to_dict(), s.imp_slow3.value_counts().to_dict())
