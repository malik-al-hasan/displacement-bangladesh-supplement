"""11c_framework_v6.py  -  Fig. 1, temporal and spatial framework; item wording from the 2023 Kobo form.
v7: straight orthogonal connectors (bus lines), legend in place of the explanatory note, brown and black palette, lettering at 8 pt or more."""
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

OUT = Path(__file__).resolve().parents[1] / "outputs" / "figures_v6"
OUT.mkdir(exist_ok=True)
INK, GREY, LGREY, BRN = "#1A1A1A", "#4D4D4D", "#8A8A8A", "#8C510A"
DASH = (0, (3, 2))
plt.rcParams.update({"font.family": "Arial"})
MM = 1 / 25.4


def box(ax, x, y, w, h, title, lines, ec=GREY, fc="white", lw=0.8, tcol=INK):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.004,rounding_size=0.012", fc=fc, ec=ec, lw=lw))
    ax.text(x + 0.012, y + h - 0.016, title, fontsize=8.5, fontweight="bold", color=tcol, va="top")
    ax.text(x + 0.012, y + h - 0.047, lines, fontsize=8, color=INK, va="top", linespacing=1.3)


def arrow(ax, a, b, ls="-", col=GREY, lw=1.0):
    ax.add_patch(FancyArrowPatch(a, b, arrowstyle="-|>", mutation_scale=9, lw=lw, color=col, linestyle=ls,
                                 shrinkA=0, shrinkB=0, joinstyle="miter", capstyle="butt"))


def line(ax, xs, ys, ls="-", col=GREY, lw=1.0):
    ax.add_line(Line2D(xs, ys, lw=lw, color=col, linestyle=ls, solid_capstyle="butt", dash_capstyle="butt"))


def dashed_arrow(ax, a, b, ls=DASH, col=GREY, lw=1.0):
    """Dashed shaft with a solid head, so the head stays clean."""
    dx, dy = b[0] - a[0], b[1] - a[1]
    n = (dx ** 2 + dy ** 2) ** 0.5
    c = (b[0] - 0.014 * dx / n, b[1] - 0.014 * dy / n)
    line(ax, [a[0], c[0]], [a[1], c[1]], ls=ls, col=col, lw=lw)
    arrow(ax, c, b, col=col, lw=lw)


fig = plt.figure(figsize=(174 * MM, 142 * MM))
ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.axis("off")
LX0, LX1, OX0, OX1, BX, RX0, RX1 = 0.02, 0.345, 0.395, 0.565, 0.595, 0.625, 0.98

# time axis
arrow(ax, (LX0, 0.963), (RX1, 0.963), col=LGREY, lw=1.2)
for x, t in [(LX0, "Before any move (origin)"), (OX0, "Move"), (RX0, "At interview (current place)")]:
    ax.text(x, 0.971, t, fontsize=8.5, fontweight="bold", color=GREY, va="bottom")

# origin column
box(ax, LX0, 0.81, LX1 - LX0, 0.13, "Background characteristics", "Sex, age, education\n(occupation may change with a move)")
box(ax, LX0, 0.50, LX1 - LX0, 0.285, "Hazards and impacts",
    "Time-ambiguous: asked as hazards\n'you face' in 'your area', for movers\nmainly the destination, where 90%\nhad lived for six years or more\nImpact severity: minor, recoverable,\nirrecoverable")
ax.text(LX0 + 0.012, 0.486, "Origin, date and distance of moves\nwere not recorded", fontsize=8, style="italic", color=GREY, va="top", linespacing=1.25)

# outcome, tall enough to take both origin arrows as straight horizontals
OY0, OY1, OMID = 0.56, 0.90, 0.73
ax.add_patch(FancyBboxPatch((OX0, OY0), OX1 - OX0, OY1 - OY0, boxstyle="round,pad=0.004,rounding_size=0.012",
                            fc="#F2F2F2", ec=INK, lw=1.1))
ax.text((OX0 + OX1) / 2, OMID, "Reported\ndisplacement", ha="center", va="bottom", fontsize=9, fontweight="bold", linespacing=1.2)
ax.text((OX0 + OX1) / 2, OMID - 0.012, "item 5.13: 'displaced\nfrom another area'", ha="center", va="top", fontsize=8, linespacing=1.25)

# origin -> outcome (primary model): straight horizontal arrows
arrow(ax, (LX1, 0.875), (OX0, 0.875), col=INK, lw=1.3)
arrow(ax, (LX1, 0.6425), (OX0, 0.6425), col=INK, lw=1.3)

# current column
items = [("Exposure of current location", "flood-prone, low-lying, eroding, dry"),
         ("Residence duration", "resets when a household moves"),
         ("Support received", "relief, credit, training; timing unknown"),
         ("Women's part in response decisions", "perceived, at interview"),
         ("Non-economic loss and damage", "self or someone known; place ambiguous")]
y, mids = 0.857, []
for t, s in items:
    box(ax, RX0, y, RX1 - RX0, 0.083, t, s, ec=LGREY)
    mids.append(y + 0.0415); y -= 0.1

# outcome <-> current conditions: one vertical bus with straight branches, dashed brown (direction unresolved)
line(ax, [BX, BX], [min(mids), max(mids)], ls=DASH, col=BRN, lw=1.0)
dashed_arrow(ax, (BX, OMID), (OX1, OMID), col=BRN, lw=1.0)
for m in mids:
    dashed_arrow(ax, (BX, m), (RX0, m), col=BRN, lw=1.0)

# legend
GX, GY = 0.27, 0.39
arrow(ax, (GX, GY), (GX + 0.05, GY), col=INK, lw=1.3)
ax.text(GX + 0.06, GY, "Estimated in the primary model", fontsize=8, color=INK, va="center")
line(ax, [GX, GX + 0.05], [GY - 0.04, GY - 0.04], ls=DASH, col=BRN, lw=1.0)
ax.text(GX + 0.06, GY - 0.04, "Direction unresolved; each correlate added singly", fontsize=8, color=INK, va="center")
line(ax, [GX, GX + 0.05], [GY - 0.08, GY - 0.08], ls=(0, (2, 2)), col=LGREY, lw=0.9)
ax.text(GX + 0.06, GY - 0.08, "Place and interviewer as context", fontsize=8, color=INK, va="center")

# district band
ax.add_patch(FancyBboxPatch((LX0, 0.03), RX1 - LX0, 0.19, boxstyle="round,pad=0.004,rounding_size=0.012", fc="#FAFAFA", ec=GREY, lw=0.8, ls=(0, (4, 2))))
ax.text(LX0 + 0.015, 0.208, "Place at two scales, and the interview", fontsize=8.5, fontweight="bold", color=INK, va="top")
ax.text(LX0 + 0.015, 0.174, "District: Barishal (delta, seven field sites) and Kurigram (floodplain, three sites) differ at once in hazards,\n"
        "livelihoods and institutions. Field site: settlements within each district differ in form and history.\n"
        "Interviewer: practice in asking item 5.13 adds measurement variance. District heterogeneity is tested\n"
        "without and with site and interviewer fixed effects.",
        fontsize=8, color=INK, va="top", linespacing=1.3)
for x, ytop in ((0.10, 0.43), (0.84, 0.45)):
    dashed_arrow(ax, (x, 0.224), (x, ytop), ls=(0, (2, 2)), col=LGREY, lw=0.9)


def tag(x, y, t, col=BRN):
    ax.text(x, y, t, fontsize=8, fontweight="bold", color="white", ha="center", va="center", zorder=12,
            bbox=dict(boxstyle="round,pad=0.25", fc=col, ec="none"))


tag(0.285, 0.525, "H2a / H2b")
tag(0.948, 0.918, "H3")
tag(0.935, 0.2, "H1, H4")
fig.savefig(OUT / "Fig2_conceptual_framework.png", dpi=600, facecolor="white")
fig.savefig(OUT / "Fig2_conceptual_framework.pdf", facecolor="white")
print("framework written")
