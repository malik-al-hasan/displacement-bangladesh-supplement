"""11b_framework_v4.py  -  Fig. 2 (v4), temporal and spatial framework; item wording from the 2023 Kobo form."""
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

OUT = Path(__file__).resolve().parents[1] / "outputs" / "figures_v4"
INK, GREY, BLUE, VERM = "#1A1A1A", "#4D4D4D", "#0072B2", "#D55E00"
plt.rcParams.update({"font.family": "Arial"})
MM = 1 / 25.4


def box(ax, x, y, w, h, title, lines, ec=GREY, fc="white", lw=0.8, tcol=INK):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.004,rounding_size=0.012", fc=fc, ec=ec, lw=lw))
    ax.text(x + 0.012, y + h - 0.02, title, fontsize=7.2, fontweight="bold", color=tcol, va="top")
    ax.text(x + 0.012, y + h - 0.066, lines, fontsize=6.4, color=INK, va="top", linespacing=1.35)


def arrow(ax, a, b, ls="-", col=GREY, lw=1.0, rad=0.0, style="-|>"):
    ax.add_patch(FancyArrowPatch(a, b, arrowstyle=style, mutation_scale=9, lw=lw, color=col, linestyle=ls,
                                 connectionstyle=f"arc3,rad={rad}", shrinkA=0, shrinkB=0))


fig = plt.figure(figsize=(174 * MM, 100 * MM))
ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.axis("off")

# time axis
arrow(ax, (0.03, 0.955), (0.97, 0.955), col="#9E9E9E", lw=1.2)
for x, t in [(0.03, "Before any move (origin)"), (0.43, "Move"), (0.66, "At interview (current place)")]:
    ax.text(x, 0.975, t, fontsize=7.4, fontweight="bold", color=GREY, va="bottom")

# origin column
box(ax, 0.03, 0.72, 0.33, 0.17, "Background characteristics", "Sex, age, education\n(occupation may change with a move)")
box(ax, 0.03, 0.43, 0.33, 0.24, "Hazards faced and impacts (time-ambiguous)",
    "Asked as hazards 'you face' in 'your area';\nfor movers mainly the destination, where\n90% had lived for six years or more\nImpact severity: minor, recoverable, irrecoverable")
ax.text(0.04, 0.395, "Origin, date and distance of moves were not recorded", fontsize=6.2, style="italic", color=GREY, va="top")

# outcome
ax.add_patch(FancyBboxPatch((0.42, 0.50), 0.18, 0.20, boxstyle="round,pad=0.004,rounding_size=0.012", fc="#F2F2F2", ec=INK, lw=1.1))
ax.text(0.51, 0.60, "Reported\ndisplacement\n(item 5.13: 'displaced\nfrom another area')", ha="center", va="center", fontsize=7.0, fontweight="bold", linespacing=1.2)

# current column
items = [("Exposure of current location", "flood-prone, low-lying, eroding, dry"),
         ("Residence duration", "resets when a household moves"),
         ("Support received", "relief, credit, training; timing unknown"),
         ("Women's part in response decisions", "perceived, at interview"),
         ("Non-economic loss and damage", "self or someone known; place ambiguous")]
y = 0.81
for t, s in items:
    box(ax, 0.66, y, 0.31, 0.10, t, s, ec="#9E9E9E")
    y -= 0.115

# arrows: origin -> displacement (estimated in primary model)
arrow(ax, (0.36, 0.80), (0.42, 0.65), col=INK, lw=1.3)
arrow(ax, (0.36, 0.55), (0.42, 0.57), col=INK, lw=1.3)
ax.text(0.39, 0.505, "primary\nmodel", fontsize=6.0, color=INK, ha="center", style="italic")
# displacement -> current conditions, and dashed reverse pathways
for yy in (0.86, 0.745, 0.63, 0.515, 0.40):
    arrow(ax, (0.60, 0.60), (0.66, yy), col=GREY, lw=0.8)
for yy, rad in ((0.84, 0.28), (0.42, -0.28)):
    arrow(ax, (0.66, yy), (0.59, 0.70 if yy > 0.6 else 0.50), ls=(0, (3, 2)), col=VERM, lw=0.9, rad=rad)
ax.text(0.405, 0.455, "Solid: may change after a move.\nDashed: may also have shaped the\nmove; direction cannot be resolved\nwith cross-sectional data. These are\nanalysed as correlates, added singly.",
        fontsize=6.0, color=VERM, va="top", linespacing=1.3)

# district band
ax.add_patch(FancyBboxPatch((0.03, 0.02), 0.94, 0.15, boxstyle="round,pad=0.004,rounding_size=0.012", fc="#FAFAFA", ec=GREY, lw=0.8, ls=(0, (4, 2))))
ax.text(0.045, 0.155, "Place at two scales, and the interview", fontsize=7.2, fontweight="bold", color=INK, va="top")
ax.text(0.045, 0.118, "District: Barishal (delta, seven field sites) and Kurigram (floodplain, three sites) differ at once in hazards, livelihoods and institutions.\n"
        "Field site: settlements within each district differ in form and history. Interviewer: practice in asking item 5.13 adds measurement\n"
        "variance. District heterogeneity is tested without and with site and interviewer fixed effects.",
        fontsize=6.2, color=INK, va="top", linespacing=1.3)
for x, ytop in ((0.19, 0.38), (0.81, 0.34)):
    arrow(ax, (x, 0.17), (x, ytop), col="#9E9E9E", lw=0.7, ls=(0, (2, 2)))
fig.savefig(OUT / "Fig2_conceptual_framework.png", dpi=600, bbox_inches="tight", facecolor="white")
fig.savefig(OUT / "Fig2_conceptual_framework.pdf", bbox_inches="tight", facecolor="white")
print("framework written")
