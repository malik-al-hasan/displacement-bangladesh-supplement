"""
12_link_raw_kobo.py

Links the cleaned analysis file (data/data1.csv, n = 602) back to the raw
KoboToolbox export (n = 603) so that every analysed respondent recovers
interview date, enumerator, upazila, union/village and the raw answer to the
displacement question (5.13) before any recoding.

Inputs
  data/data1.csv
  data/raw_kobo/kobo_labels_export_2023-11-14.csv   (Barishal + Kurigram, 2023)

Outputs (outputs/raw/)
  linked_raw_data1.csv          data1 rows + raw metadata and raw item values
  recode_audit.csv              raw level -> data1 level counts for every item
  linkage_report.txt            match diagnostics
"""
import io
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs" / "raw"
OUT.mkdir(parents=True, exist_ok=True)


def read_kobo(path):
    raw = Path(path).read_bytes()
    try:
        txt = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        txt = raw.decode("cp1252", errors="replace")
    return pd.read_csv(io.StringIO(txt), dtype=str, keep_default_na=False)


k = read_kobo(ROOT / "data" / "raw_kobo" / "kobo_labels_export_2023-11-14.csv")
k = k.replace({"": np.nan})
k.columns = [c.strip() for c in k.columns]
k["raw_row"] = np.arange(1, len(k) + 1)


def col(prefix):
    hits = [c for c in k.columns if c.startswith(prefix)]
    if not hits:
        raise KeyError(prefix)
    return hits[0]


def has(series, label):
    return series.fillna("").str.contains(re.escape(label), case=False).astype(int)


# ---- district: interview month separates the two field rounds ---------------
k["date"] = pd.to_datetime(k["Start Time"].str[:10])
k["district"] = np.where(k["date"].dt.month == 9, "Barishal", "Kurigram")

# ---- data1-style recodes of the raw answers ---------------------------------
imp_map = {"1": "No impacts", "2": "Some bad impacts", "3": "Recoverable impacts",
           "4": "A lot of recoverable impacts", "5": "A lot of irrecoverable impacts"}
rap = k[col("3.2 Which kind")]
slo = k[col("4.1 What kinds of slow")]
vul = k[col("2.5 If yes, what are the causes?")]
neld = "5.2 Have you or someone you know experienced non-economic losses"

r = pd.DataFrame({
    "District": k["district"],
    "Vulnerability_flood_area": has(vul, "Flood prone area"),
    "Vulnerability_Low_lying_land": has(vul, "Low lying land"),
    "Vulnerability_Erosion": has(vul, "Erosion prone area"),
    "Vulnerability_Salinity_intrusion": has(vul, "Salinity intrusion"),
    "Vulnerability_Drought": has(vul, "Drought prone area"),
    "Hazard_flood": has(rap, "Floods"),
    "Hazard_Cyclones": has(rap, "Cyclones"),
    "Hazard_Drought": has(rap, "Drought"),
    "Hazard_Tidal_Surge": has(rap, "Tidal Surge"),
    "Hazard_Heatwave": has(rap, "Heatwave"),
    "Hazard_Sea_level_rise": has(slo, "Sea level rise"),
    "Hazard_Riverbank_erosion": has(slo, "Riverbank erosion"),
    "Hazard_Salinity_intrusion": has(slo, "Salinity intrusion"),
    "Hazard_Increase_temperature": has(slo, "Increase in temparature"),
    "Impacts_rapid_disasters": k[col("3.3 Impacts")].str[0].map(imp_map),
    "Impacts_slow_disasters": k[col("4.2 Impacts")].str[0].map(imp_map),
    "NELD_cultural_heritage": k[f"{[c for c in k.columns if c.startswith(neld) and c.endswith('/Loss of cultural heritage')][0]}"].astype(int),
    "NELD_Health_impacts": k[[c for c in k.columns if c.startswith(neld) and c.endswith("/Health impacts")][0]].astype(int),
    "NELD_biodiversity": k[[c for c in k.columns if c.startswith(neld) and c.endswith("/Loss of biodiversity and ecosystems")][0]].astype(int),
    "NELD_social_cohesion": k[[c for c in k.columns if c.startswith(neld) and c.endswith("/Loss of social cohesion")][0]].astype(int),
})
r["raw_row"] = k["raw_row"].values

# ---- raw metadata and raw values retained for the analysis -------------------
meta = pd.DataFrame({
    "raw_row": k["raw_row"],
    "interview_date": k["date"].dt.date.astype(str),
    "code": k["Code"].str.strip(),
    "union_village_raw": k["A. Union/Village Name"].str.strip(),
    "upazila_raw": k["B. Upazilla name"].str.strip(),
    "raw_gender": k[col("1. What is your gender?")],
    "raw_age": k[col("2. What is your age?")],
    "raw_edu": k[col("3. What is your educational")],
    "raw_occ": k[col("4. What is you occupation")],
    "raw_occ_other": k[col("4.1 Others please")],
    "raw_residence": k[col("2.1 How long")],
    "raw_vulnerable": k[col("2.4 Do you consider")],
    "raw_displaced_5_13": k[col("5.13 Have you been displaced")],
    "raw_reason_5_14": k[col("5.14 Reasons")],
    "raw_reason_other": k[col("5.14.1")],
    "raw_issue_5_15": k[col("5.15 What kinds of issues")],
    "raw_women_dm_5_9": k[col("5.9 Do women")],
    "raw_autonomy_7_5": k[col("7.5 Do you have autonomy")],
    "raw_support_7_1": k[col("7.1 In the past")],
    "raw_provider_7_2": k[col("7.2 Who provides")],
    "raw_neld_displacement_migration": k[[c for c in k.columns if c.startswith(neld) and c.endswith("/Displacement or migration")][0]].astype(int),
    "raw_anxious_5_5": k[col("5.5 Do you feel anxious")],
    "raw_sleep_5_6": k[col("5.6 Have you ever had trouble")],
    "raw_marginalised_5_8": k[col("5.8 Who do you")],
    "raw_informed_5_1": k[col("5.1 How well")],
    "raw_fund_delay_7_4": k[col("7.4 How long")],
    "raw_cash_pref_7_3": k[col("7.3 Do you prefer")],
    "raw_temp_change_2_2": k[col("2.2 Have you observed")],
    "raw_disaster_3_1": k[col("3.1 Do you experience")],
})

# ---- link to data1 ------------------------------------------------------------
# data1 carries no respondent ID, so records are linked by global optimal
# assignment (Hungarian algorithm) on the number of disagreeing fields across
# every item that data1 shares with the raw export, after applying the
# recodes that data1 used. Most pairs agree on all fields.
from scipy.optimize import linear_sum_assignment

d1 = pd.read_csv(ROOT / "data" / "data1.csv", keep_default_na=False)
d1.columns = [c.strip() for c in d1.columns]
d1["d1_row"] = np.arange(1, len(d1) + 1)

KEY = [c for c in r.columns if c != "raw_row"]
blank = lambda s: s.replace({"None": np.nan})
occ_other = meta["raw_occ_other"].fillna("").str.strip().str.lower()
occ = meta["raw_occ"].copy()
occ[(occ == "Other") & occ_other.str.contains("house|home")] = "Housewife "
occ[(occ == "Other") & occ_other.str.contains("student")] = "Student"
SEC = {
    "Gender": meta["raw_gender"],
    "Age_group": meta["raw_age"].replace({"Under 18": "18-35"}),
    "Educational_status": meta["raw_edu"].replace({"HSC": "HSC or above", "Bachelor's degree": "HSC or above", "Master's degree": "HSC or above"}),
    "Occupation": occ,
    "Residence_duration": meta["raw_residence"].replace({"1-5 years": "Up to 5 years", "Less than a year": "Up to 5 years"}),
    "Women_decision": meta["raw_women_dm_5_9"].replace({"None": "No"}),
    "Displacement": meta["raw_displaced_5_13"].replace({"None": "No", "Not sure": "Yes"}),
    "Reasons_displacement": meta["raw_reason_5_14"].replace({"Proximity/Near to a relative": "Near to a relative"}),
    "Issues_face_displacement": meta["raw_issue_5_15"].replace({"Others": "Livelihood disruption"}),
    "Areas_vulnerability": np.where(meta["raw_vulnerable"] == "Yes", "Yes", "No"),
    "Disaster_rapid_event_risk": meta["raw_disaster_3_1"],
    "Get_financial_support": meta["raw_support_7_1"].replace({"Housing program": "Others", "Educational opportunities": "Others"}),
    "Support_provider": meta["raw_provider_7_2"].replace({"International organizations": "Others", "Private sector": "Others"}),
    "Autonomy_use_finance": np.where(meta["raw_autonomy_7_5"] == "Yes", "Yes", "No"),
}
R = r[KEY].copy()
for c, v in SEC.items():
    R[c] = np.asarray(v)
FIELDS = list(R.columns)
A = d1[FIELDS].astype(str).apply(lambda s: s.str.strip()).to_numpy()
B = R.astype(str).apply(lambda s: s.str.strip()).to_numpy()
cost = (A[:, None, :] != B[None, :, :]).sum(axis=2)
ri, ci = linear_sum_assignment(cost)
link = pd.DataFrame({"d1_row": d1["d1_row"].to_numpy()[ri], "raw_row": r["raw_row"].to_numpy()[ci],
                     "n_disagree": cost[ri, ci]})
# second-best distance: how distinctive each match is
srt = np.sort(cost, axis=1)
link["second_best"] = srt[ri, 1]
fieldwise = pd.Series((A[ri] != B[ci]).sum(axis=0), index=FIELDS)

lines = []
lines.append(f"data1 rows: {len(d1)}; raw rows: {len(k)}")
lines.append(f"linked one-to-one: {len(link)} across {len(FIELDS)} comparable fields")
lines.append("number of disagreeing fields per linked pair:")
lines.append(link["n_disagree"].value_counts().sort_index().to_string())
lines.append("pairs whose match is unique (second-best candidate differs on more fields): %d" % (link.second_best > link.n_disagree).sum())
lines.append("disagreements by field:")
lines.append(fieldwise[fieldwise > 0].to_string())
unlinked_raw = sorted(set(k["raw_row"]) - set(link["raw_row"]))
lines.append(f"raw rows not in data1: {unlinked_raw}")
lines.append(meta[meta.raw_row.isin(unlinked_raw)][["raw_row", "code", "raw_gender", "raw_age", "raw_displaced_5_13"]].to_string())

out = d1.merge(link, on="d1_row", how="left").merge(meta, on="raw_row", how="left")
out["district_check"] = (out["District"] == np.where(pd.to_datetime(out["interview_date"]).dt.month == 9, "Barishal", "Kurigram"))
lines.append(f"district agreement: {out['district_check'].mean():.4f}")

# Interviewer code aliases (one interviewer used two code prefixes) are held with the data and are
# not distributed, to keep interviewers anonymous. Supply them as a dict, e.g. {"<alias>": "<prefix>"}.
INTERVIEWER_ALIASES = {}

# ---- field site, upazila and enumerator ----------------------------------------
# Village names were typed free-hand; the interview code (enumerator/sex-number/
# site abbreviation) gives a second, more consistent site marker. Sites are
# assigned from the village text first and the code suffix second.
v = out["union_village_raw"].fillna("").str.lower().str.replace(r"[^a-z]", "", regex=True)
suf = out["code"].fillna("").str.lower().str.replace(" ", "").str.extract(r"[/\-]([a-z]+)$")[0].fillna("")
site = pd.Series("", index=out.index)
rules = [("manta", "MC"), ("bang|bong|bung|^bb$|^bv$", "BB"), ("pala|pola|palu|^pp$", "PP"),
         ("namar|nakhar|nanar|^nc$", "NC"), ("lalganj|^lg$|^ll$|mehendiganj", "LG"),
         ("ulania|ulaniya|hasanpur|^ul$|mehedigong|mehendigong", "UL"), ("tung", "LH"),
         ("baghm|bagm|rowmari$", "BG"), ("fal|fol|flo", "FC"), ("mod|mad|mid|mohod", "MD")]
for pat, s in rules:
    m = (site == "") & v.str.contains(pat)
    site[m] = s
lah = v.str.contains("lahar") | (v == "mc")
site[lah & (suf == "mc")] = "MC"
site[lah & (suf == "lh")] = "LH"
site[(v == "mc")] = "MC"
# residual codes: one 'bs' record (Barishal Sadar, Palashpur team) and one blank village
site[(site == "") & (suf == "bs")] = "PP"
site[(site == "") & (suf == "nc")] = "NC"
assert (site != "").all(), out.loc[site == "", ["code", "union_village_raw"]]
SITES = {
    "LH": ("Barishal", "Barishal Sadar", "Laharhat (Tungibaria)", "Peri-urban riverbank"),
    "MC": ("Barishal", "Barishal Sadar", "Manta boat community, Laharhat", "Boat-dwelling fishing community"),
    "PP": ("Barishal", "Barishal Sadar", "Palashpur", "Urban riverside informal settlement"),
    "BB": ("Barishal", "Barishal Sadar", "Bangabandhu Colony", "Urban riverside informal settlement"),
    "NC": ("Barishal", "Barishal Sadar", "Namar Char", "River island (char)"),
    "LG": ("Barishal", "Mehendiganj", "Lalganj", "River island (char)"),
    "UL": ("Barishal", "Mehendiganj", "Ulania (incl. Hasanpur)", "River island (char)"),
    "BG": ("Kurigram", "Rowmari", "Baghmara", "Mainland floodplain village"),
    "FC": ("Kurigram", "Rowmari", "Faluar Char", "River island (char)"),
    "MD": ("Kurigram", "Char Rajibpur", "Modoner Char", "River island (char)"),
}
out["site"] = site.values
out["site_name"] = out["site"].map(lambda s: SITES[s][2])
out["upazila"] = out["site"].map(lambda s: SITES[s][1])
out["settlement_type"] = out["site"].map(lambda s: SITES[s][3])
assert (out["site"].map(lambda s: SITES[s][0]) == out["District"]).all()
out["enumerator"] = (out["code"].str.split(r"[/\- ]").str[0].str.lower().str.strip()
                     .replace(INTERVIEWER_ALIASES).str.capitalize())
# enumerators are anonymised in all outputs
enum_ids = {e: f"E{i + 1}" for i, e in enumerate(sorted(out["enumerator"].unique()))}
out["enum_id"] = out["enumerator"].map(enum_ids)
lines.append("sites:\n" + out.groupby(["District", "upazila", "site", "site_name"]).size().to_string())
lines.append("enumerators: " + str(out["enum_id"].value_counts().sort_index().to_dict()))

# ---- item-by-item audit of recodes (raw level -> data1 level) -----------------
RAWCOL = {"Gender": "raw_gender", "Age_group": "raw_age", "Educational_status": "raw_edu",
          "Occupation": "raw_occ", "Residence_duration": "raw_residence",
          "Women_decision": "raw_women_dm_5_9", "Displacement": "raw_displaced_5_13",
          "Reasons_displacement": "raw_reason_5_14", "Issues_face_displacement": "raw_issue_5_15",
          "Areas_vulnerability": "raw_vulnerable", "Get_financial_support": "raw_support_7_1",
          "Support_provider": "raw_provider_7_2", "Autonomy_use_finance": "raw_autonomy_7_5"}
audit = []
for d1col, rawcol in RAWCOL.items():
    t = out.groupby([out[rawcol].fillna("<blank>"), out[d1col]]).size().reset_index(name="n")
    t.columns = ["raw_level", "data1_level", "n"]
    t.insert(0, "item", d1col)
    audit.append(t)
audit = pd.concat(audit, ignore_index=True)
audit.to_csv(OUT / "recode_audit.csv", index=False)

out.to_csv(OUT / "linked_raw_data1.csv", index=False)
(OUT / "linkage_report.txt").write_text("\n".join(lines), encoding="utf-8")
print("\n".join(lines))
print(audit.to_string())
