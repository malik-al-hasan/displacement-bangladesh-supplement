"""
06b_figures_descriptive_v4.py  (v4 analytic sample, n = 571)
  Fig 3   Reason for choosing the destination (item 5.14) and main problem after displacement (5.15), displaced respondents
  Fig S5  Exposure, hazard and non-economic loss profiles of the two districts
  Fig S7  Grouped permutation importance by district (random forest), if available
Brown and black with shape coding; 600 dpi PNG plus vector PDF.
"""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import fisher_exact
from scipy.stats.contingency import association

ROOT = Path(__file__).resolve().parents[1]
V4 = ROOT / "outputs" / "v4"
FIG = ROOT / "outputs" / "figures_v4"
FIG.mkdir(parents=True, exist_ok=True)
BRN, BLK, GREY, INK = "#8C510A", "#1A1A1A", "#6E6E6E", "#1A1A1A"
MM = 1 / 25.4
plt.rcParams.update({"font.family": "Arial", "font.size": 8, "axes.linewidth": 0.6, "axes.edgecolor": "#404040",
                     "xtick.major.width": 0.6, "ytick.major.width": 0.6, "axes.spines.top": False,
                     "axes.spines.right": False, "pdf.fonttype": 42})


def save(fig, name):
    fig.savefig(FIG / f"{name}.png", dpi=600, bbox_inches="tight", facecolor="white")
    fig.savefig(FIG / f"{name}.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def wilson(k, n, z=1.96):
    p = k / n; den = 1 + z**2 / n; c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return c - h, c + h


d = pd.read_csv(V4 / "analysis_data_v4_primary.csv", keep_default_na=False)
nB, nK = (d.district == "Barishal").sum(), (d.district == "Kurigram").sum()

# ---- Fig 3 -------------------------------------------------------------------------------------
dd = d[d.displaced == 1]
nBd, nKd = (dd.district == "Barishal").sum(), (dd.district == "Kurigram").sum()
fig, axes = plt.subplots(1, 2, figsize=(174 * MM, 80 * MM))
fig.subplots_adjust(left=0.185, right=0.985, top=0.925, bottom=0.125, wspace=0.70)
for ax, (col, title, tag) in zip(axes, [("reason", "Reason for choosing the destination", "a"),
                                        ("issue", "Main problem after displacement", "b")]):
    tab = pd.crosstab(dd[col], dd.district)
    pct = 100 * tab / tab.sum()
    pct = pct.loc[pct.mean(axis=1).sort_values().index]
    yy = np.arange(len(pct))
    ax.barh(yy + 0.19, pct["Barishal"], height=0.36, color=BRN, label=f"Barishal (n = {nBd})")
    ax.barh(yy - 0.19, pct["Kurigram"], height=0.36, color=BLK, label=f"Kurigram (n = {nKd})")
    for i, (b, k) in enumerate(zip(pct["Barishal"], pct["Kurigram"])):
        ax.text(b + 1, i + 0.19, f"{b:.0f}", va="center", fontsize=8, color=INK)
        ax.text(k + 1, i - 0.19, f"{k:.0f}", va="center", fontsize=8, color=INK)
    labels = [c.replace("Lack of essential services (food, water etc)", "Lack of essential services")
               .replace("Lack of proper housing/accommodation", "Lack of proper housing")
               .replace("None", "None given").replace("Job Opportunities", "Job opportunities") for c in pct.index]
    ax.set_yticks(yy); ax.set_yticklabels(labels); ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 95); ax.set_xlabel("Displaced respondents (%)")
    ax.set_title(f"{tag}  {title}", loc="left", fontsize=8, fontweight="bold")
axes[0].legend(loc="lower right", frameon=False)
fig.savefig(FIG / "Fig_destination_choice_problems.png", dpi=600, facecolor="white")
fig.savefig(FIG / "Fig_destination_choice_problems.pdf", facecolor="white")
plt.close(fig)

# ---- Fig S5 ------------------------------------------------------------------------------------
items = [("Exposure of current location", [("v_flood", "Flood-prone"), ("v_lowland", "Low-lying"), ("v_erosion", "Erosion-prone"),
                                          ("v_salinity", "Salinity-affected"), ("v_drought", "Drought-prone")]),
         ("Rapid-onset hazards faced", [("h_flood", "Flood"), ("h_cyclone", "Cyclone"), ("h_drought", "Drought"),
                                        ("h_tidal", "Tidal surge"), ("h_heat", "Heatwave")]),
         ("Slow-onset hazards faced", [("h_slr", "Sea-level rise"), ("h_erosion", "Riverbank erosion"),
                                       ("h_salinity", "Salinity intrusion"), ("h_temp", "Rising temperature")]),
         ("Non-economic loss (self or someone known)", [("n_culture", "Cultural heritage"), ("n_health", "Health"),
                                                        ("n_biodiv", "Biodiversity"), ("n_social", "Social cohesion")])]
rows, ypos, heads, y = [], [], [], 0
for dom, its in items:
    heads.append((y, dom)); y += 1
    its = sorted(its, key=lambda t: -d.loc[d.district == "Kurigram", t[0]].mean())
    for v, lab in its:
        B = d.loc[d.district == "Barishal", v]; K = d.loc[d.district == "Kurigram", v]
        tb = pd.crosstab(d[v], d.district)
        V = association(tb.to_numpy(), method="cramer") if tb.shape[0] > 1 else np.nan
        p = fisher_exact(tb.to_numpy())[1] if tb.shape == (2, 2) else np.nan
        rows.append((lab, B.sum(), K.sum(), V, p)); ypos.append(y); y += 1
    y += 0.4
fig, ax = plt.subplots(figsize=(150 * MM, 120 * MM))
for yy, (lab, b, k, V, p) in zip(ypos, rows):
    bp, kp = 100 * b / nB, 100 * k / nK
    bl, bh = wilson(b, nB); kl, kh = wilson(k, nK)
    ax.plot([bp, kp], [yy, yy], color="#C8C8C8", lw=1.6, zorder=1, solid_capstyle="round")
    ax.errorbar(bp, yy + 0.16, xerr=[[bp - 100 * bl], [100 * bh - bp]], fmt="^", ms=4.6, color=BRN, elinewidth=0.7, capsize=0, zorder=3)
    ax.errorbar(kp, yy - 0.16, xerr=[[kp - 100 * kl], [100 * kh - kp]], fmt="s", ms=4.0, color=BLK, elinewidth=0.7, capsize=0, zorder=3)
    pv = "<0.001" if p < 0.001 else f"= {p:.3f}"
    ax.text(103, yy, f"V = {V:.2f}   p {pv}", va="center", fontsize=6.3, color=GREY)
ax.set_yticks(ypos + [h[0] for h in heads]); ax.set_yticklabels([r[0] for r in rows] + [h[1] for h in heads])
for t in ax.get_yticklabels():
    if t.get_text() in [h[1] for h in heads]:
        t.set_fontweight("bold")
ax.set_xlim(0, 100); ax.set_xticks(range(0, 101, 20))
ax.set_xlabel("Respondents reporting the item (%), with Wilson 95% CI")
ax.grid(axis="x", color="#E6E6E6", lw=0.5); ax.set_axisbelow(True); ax.tick_params(axis="y", length=0)
ax.set_ylim(-0.7, y - 0.3); ax.invert_yaxis()
ax.plot([], [], "^", color=BRN, ms=4.6, label=f"Barishal (delta), n = {nB}")
ax.plot([], [], "s", color=BLK, ms=4.0, label=f"Kurigram (floodplain), n = {nK}")
ax.legend(loc="lower center", bbox_to_anchor=(0.5, 1.0), ncol=2, frameon=False)
ax.text(103, 0, "Cramér's V, Fisher p", fontsize=6.3, fontweight="bold", color=GREY, va="center")
fig.subplots_adjust(right=0.78)
save(fig, "FigS5_district_profiles")

# ---- Fig S7 permutation importance --------------------------------------------------------------
imp_path = V4 / "ml" / "FigData_permutation_importance.csv"
if imp_path.exists():
    imp = pd.read_csv(imp_path)
    piv = imp.pivot(index="label", columns="sample", values="importance_auc_drop")
    sd = imp.pivot(index="label", columns="sample", values="sd_across_folds")
    piv = piv.sort_values("Pooled")
    fig, ax = plt.subplots(figsize=(150 * MM, 105 * MM))
    yy = np.arange(len(piv))
    for col, mk, off, c in [("Pooled", "o", 0.25, GREY), ("Barishal", "^", 0.0, BRN), ("Kurigram", "s", -0.25, BLK)]:
        if col not in piv:
            continue
        v = piv[col].to_numpy() * 100
        e = sd[col].reindex(piv.index).to_numpy() * 100 / np.sqrt(25)
        ax.errorbar(v, yy + off, xerr=1.96 * e, fmt=mk, ms=4, color=c, elinewidth=0.7, capsize=0, label=col)
    ax.axvline(0, color="#808080", lw=0.6, ls="--")
    ax.set_yticks(yy); ax.set_yticklabels(piv.index)
    ax.set_xlabel("Drop in held-out AUC when the variable is permuted (x 100)")
    ax.grid(axis="x", color="#E6E6E6", lw=0.5); ax.set_axisbelow(True); ax.tick_params(axis="y", length=0)
    ax.legend(loc="lower right", frameon=False)
    save(fig, "FigS7_permutation_importance")
print("v4 descriptive figures written")
