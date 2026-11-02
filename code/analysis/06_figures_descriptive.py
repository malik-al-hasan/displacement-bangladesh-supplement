"""
06_figures_descriptive.py
  Fig 2  Conceptual framework
  Fig 3  Exposure, hazard and non-economic loss profiles of the two districts
  Fig S3 Grouped permutation importance by district (random forest)
  Fig S4 Reasons for moving and main problem after moving, displaced respondents
Okabe-Ito colours with shape coding; 600 dpi PNG plus vector PDF.
"""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

ROOT = Path(__file__).resolve().parents[1]
TAB = ROOT / "outputs" / "tables"
FIG = ROOT / "outputs" / "figures"
FIG.mkdir(parents=True, exist_ok=True)

BLUE, VERM, GREY, INK = "#0072B2", "#D55E00", "#4D4D4D", "#1A1A1A"
MM = 1 / 25.4
plt.rcParams.update({
    "font.family": "Arial", "font.size": 7.5, "axes.linewidth": 0.6,
    "axes.edgecolor": "#404040", "xtick.major.width": 0.6, "ytick.major.width": 0.6,
    "axes.spines.top": False, "axes.spines.right": False, "pdf.fonttype": 42,
})


def save(fig, name):
    fig.savefig(FIG / f"{name}.png", dpi=600, bbox_inches="tight", facecolor="white")
    fig.savefig(FIG / f"{name}.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def wilson(k, n, z=1.96):
    p = k / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return c - h, c + h


# ---------------------------------------------------------------------------
# Fig 2  Conceptual framework
# ---------------------------------------------------------------------------
def box(ax, x, y, w, h, title, body, fc="white", ec=GREY, lw=0.8, title_col=INK):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.004,rounding_size=0.012",
                                fc=fc, ec=ec, lw=lw))
    ax.text(x + 0.012, y + h - 0.022, title, ha="left", va="top", fontsize=7.4,
            fontweight="bold", color=title_col)
    if body:
        ax.text(x + 0.012, y + h - 0.075, body, ha="left", va="top", fontsize=6.6,
                color=INK, linespacing=1.35)


def arrow(ax, p0, p1, style="-|>", ls="-", col=GREY, lw=0.9, rad=0.0):
    ax.add_patch(FancyArrowPatch(p0, p1, arrowstyle=style, mutation_scale=9, lw=lw, color=col,
                                 linestyle=ls, connectionstyle=f"arc3,rad={rad}",
                                 shrinkA=0, shrinkB=0))


fig = plt.figure(figsize=(174 * MM, 96 * MM))
ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.axis("off")

for x, t in [(0.02, "Place: hazard regime"), (0.28, "Exposure and loss"), (0.585, "Capabilities and constraints"),
             (0.815, "Outcome")]:
    ax.text(x, 0.975, t, fontsize=7.8, fontweight="bold", color=GREY, va="top")

# hazard regimes
box(ax, 0.02, 0.57, 0.20, 0.30, "Delta (Barishal)",
    "Cyclone, tidal surge\nSalinity in soil and water\nTidal riverbank erosion\nFishing, coastal farming",
    ec=BLUE, lw=1.1, title_col=BLUE)
box(ax, 0.02, 0.13, 0.20, 0.30, "Floodplain (Kurigram)",
    "Monsoon and flash flood\nDry-season drought\nChar and bank erosion\nSmallholder cultivation",
    ec=VERM, lw=1.1, title_col=VERM)

# exposure and loss column
ys = [0.715, 0.505, 0.295, 0.085]
heads = [("Hazard experience", "Rapid-onset and slow-onset events"),
         ("Exposure of current location", "Flood-prone, low-lying or eroding land"),
         ("Impact severity", "Minor, recoverable, irrecoverable"),
         ("Non-economic loss (NELD)", "Heritage, health, biodiversity, cohesion")]
for yb, (t, bdy) in zip(ys, heads):
    box(ax, 0.28, yb, 0.25, 0.17, t, bdy)

# regimes feed the exposure column through a bracket
ax.plot([0.245, 0.245], [0.17, 0.83], color="#9A9A9A", lw=0.9)
for yb in (0.715, 0.285):
    ax.plot([0.22, 0.245], [yb, yb], color="#9A9A9A", lw=0.9)
for yb in ys:
    arrow(ax, (0.245, yb + 0.085), (0.28, yb + 0.085), lw=0.9, col="#9A9A9A")

# exposure column converges on the main pathway
ax.plot([0.555, 0.555], [0.17, 0.80], color=GREY, lw=0.9)
for yb in ys:
    ax.plot([0.53, 0.555], [yb + 0.085, yb + 0.085], color=GREY, lw=0.9)
arrow(ax, (0.555, 0.485), (0.815, 0.485), lw=1.8, col=INK)

# capabilities condition the pathway (moderation arrow onto the main arrow)
box(ax, 0.585, 0.64, 0.205, 0.27, "",
    "")
ax.text(0.597, 0.885, "Rootedness", fontsize=7.2, fontweight="bold", va="top", color=INK)
ax.text(0.597, 0.85, "years at current residence", fontsize=6.5, va="top", color=INK)
ax.text(0.597, 0.80, "Household agency", fontsize=7.2, fontweight="bold", va="top", color=INK)
ax.text(0.597, 0.765, "women in household decisions", fontsize=6.5, va="top", color=INK)
ax.text(0.597, 0.715, "Means to move", fontsize=7.2, fontweight="bold", va="top", color=INK)
ax.text(0.597, 0.68, "relief, food aid, credit, training", fontsize=6.5, va="top", color=INK)
arrow(ax, (0.6875, 0.64), (0.6875, 0.50), lw=1.2, col=INK)
ax.text(0.695, 0.565, "conditions who\nconverts loss\ninto a move", fontsize=6.2, color=GREY,
        va="center", style="italic", linespacing=1.25)

# outcome
ax.add_patch(FancyBboxPatch((0.815, 0.375), 0.165, 0.22, boxstyle="round,pad=0.004,rounding_size=0.012",
                            fc="#F2F2F2", ec=INK, lw=1.1))
ax.text(0.8975, 0.485, "Climate-induced\ndisplacement", ha="center", va="center",
        fontsize=7.8, fontweight="bold", color=INK, linespacing=1.3)

# reverse pathway in cross-sectional data (dashed)
arrow(ax, (0.88, 0.375), (0.53, 0.53), ls=(0, (3, 2)), col=VERM, lw=0.9, rad=-0.35)
arrow(ax, (0.94, 0.595), (0.79, 0.86), ls=(0, (3, 2)), col=VERM, lw=0.9, rad=0.35)
ax.text(0.80, 0.27, "Dashed arrows: reverse pathway.\nA move changes the exposure of\n"
        "where people now live and resets\ntheir years at residence.",
        fontsize=6.1, color=VERM, va="top", linespacing=1.3)
save(fig, "Fig2_conceptual_framework")

# ---------------------------------------------------------------------------
# Fig 3  District profiles (item prevalence with Wilson 95% CI)
# ---------------------------------------------------------------------------
t2 = pd.read_csv(TAB / "Table2_hazard_profiles_by_district.csv")
nB, nK = 302, 300
order = list(t2["Domain"].unique())
rows, ypos, y = [], [], 0
head_rows = []
for dom in order:
    head_rows.append((y, dom)); y += 1
    s_ = t2[t2["Domain"] == dom].sort_values("Kurigram_pct", ascending=False)
    for _, r in s_.iterrows():
        rows.append(r); ypos.append(y); y += 1
    y += 0.4
fig, ax = plt.subplots(figsize=(150 * MM, 120 * MM))
for yy, r in zip(ypos, rows):
    bl, bh = wilson(r.Barishal_n, nB); kl, kh = wilson(r.Kurigram_n, nK)
    ax.plot([r.Barishal_pct, r.Kurigram_pct], [yy, yy], color="#C8C8C8", lw=1.6, zorder=1,
            solid_capstyle="round")
    ax.errorbar(r.Barishal_pct, yy + 0.16, xerr=[[r.Barishal_pct - 100 * bl], [100 * bh - r.Barishal_pct]],
                fmt="^", ms=4.6, color=BLUE, elinewidth=0.7, capsize=0, zorder=3)
    ax.errorbar(r.Kurigram_pct, yy - 0.16, xerr=[[r.Kurigram_pct - 100 * kl], [100 * kh - r.Kurigram_pct]],
                fmt="s", ms=4.0, color=VERM, elinewidth=0.7, capsize=0, zorder=3)
    pv = "<0.001" if r.p < 0.001 else f"{r.p:.3f}"
    ax.text(103, yy, f"V = {r.CramersV:.2f}   p {('= ' + pv) if not pv.startswith('<') else pv}",
            va="center", fontsize=6.3, color=GREY)
ticks = ypos + [h[0] for h in head_rows]
labs = [r.Item for r in rows] + [h[1] for h in head_rows]
ax.set_yticks(ticks); ax.set_yticklabels(labs)
for t in ax.get_yticklabels():
    if t.get_text() in order:
        t.set_fontweight("bold")
ax.set_xlim(0, 100); ax.set_xticks(range(0, 101, 20))
ax.set_xlabel("Respondents reporting the item (%), with Wilson 95% CI")
ax.grid(axis="x", color="#E6E6E6", lw=0.5); ax.set_axisbelow(True)
ax.tick_params(axis="y", length=0)
ax.set_ylim(-0.7, y - 0.3)
ax.invert_yaxis()
ax.plot([], [], "^", color=BLUE, ms=4.6, label="Barishal (delta), n = 302")
ax.plot([], [], "s", color=VERM, ms=4.0, label="Kurigram (floodplain), n = 300")
ax.legend(loc="lower center", bbox_to_anchor=(0.5, 1.0), ncol=2, frameon=False)
ax.text(103, 0, "Cramér's V, p", fontsize=6.3, fontweight="bold", color=GREY, va="center")
fig.subplots_adjust(right=0.78)
save(fig, "Fig3_district_profiles")

# ---------------------------------------------------------------------------
# Fig S3  Permutation importance by district
# ---------------------------------------------------------------------------
imp_path = TAB / "FigData_permutation_importance.csv"
if imp_path.exists():
    imp = pd.read_csv(imp_path)
    piv = imp.pivot(index="label", columns="sample", values="importance_auc_drop")
    sd = imp.pivot(index="label", columns="sample", values="sd_across_folds")
    piv = piv.sort_values("Pooled")
    fig, ax = plt.subplots(figsize=(150 * MM, 105 * MM))
    yy = np.arange(len(piv))
    for k, (col, mk, off, c) in enumerate([("Pooled", "o", 0.25, GREY), ("Barishal", "^", 0.0, BLUE),
                                            ("Kurigram", "s", -0.25, VERM)]):
        if col not in piv:
            continue
        v = piv[col].to_numpy() * 100
        e = sd[col].reindex(piv.index).to_numpy() * 100 / np.sqrt(25)
        ax.errorbar(v, yy + off, xerr=1.96 * e, fmt=mk, ms=4, color=c, elinewidth=0.7,
                    capsize=0, label=col)
    ax.axvline(0, color="#808080", lw=0.6, ls="--")
    ax.set_yticks(yy); ax.set_yticklabels(piv.index)
    ax.set_xlabel("Drop in held-out AUC when the variable is permuted (x 100)")
    ax.grid(axis="x", color="#E6E6E6", lw=0.5); ax.set_axisbelow(True)
    ax.tick_params(axis="y", length=0)
    ax.legend(loc="lower right", frameon=False)
    save(fig, "FigS3_permutation_importance")

# ---------------------------------------------------------------------------
# Fig S4  Reasons and problems among the displaced
# ---------------------------------------------------------------------------
ri = pd.read_csv(TAB / "FigData_reasons_issues.csv", keep_default_na=False)
fig, axes = plt.subplots(1, 2, figsize=(174 * MM, 72 * MM), gridspec_kw={"wspace": 0.95})
for ax, q in zip(axes, ["Primary reason for moving", "Main problem after moving"]):
    s = ri[ri["question"] == q]
    piv = s.pivot(index="category", columns="district", values="pct")
    piv = piv.loc[piv.mean(axis=1).sort_values().index]
    yy = np.arange(len(piv))
    ax.barh(yy + 0.19, piv["Barishal"], height=0.36, color=BLUE, label="Barishal (n = 141)")
    ax.barh(yy - 0.19, piv["Kurigram"], height=0.36, color=VERM, label="Kurigram (n = 222)")
    for i, (b, k) in enumerate(zip(piv["Barishal"], piv["Kurigram"])):
        ax.text(b + 1, i + 0.19, f"{b:.0f}", va="center", fontsize=6, color=INK)
        ax.text(k + 1, i - 0.19, f"{k:.0f}", va="center", fontsize=6, color=INK)
    labels = [c.replace("Lack of essential services (food, water etc)", "Lack of essential services")
               .replace("Lack of proper housing/accommodation", "Lack of proper housing") for c in piv.index]
    ax.set_yticks(yy); ax.set_yticklabels(labels); ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 95); ax.set_xlabel("Displaced respondents (%)")
    p = s["p_district"].iloc[0]
    ax.set_title(f"{'a' if q.startswith('Primary') else 'b'}  {q}  (Fisher p {'< 0.001' if p < 0.001 else '= %.3f' % p})",
                 loc="left", fontsize=7.6, fontweight="bold")
axes[0].legend(loc="lower right", frameon=False)
save(fig, "FigS4_reasons_problems")
print("Descriptive figures written.")
