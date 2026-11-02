"""
09_study_area_map.py  -  Figure 1, study area.
Boundaries: geoBoundaries (BBS/OCHA ROAP 2020, CC BY 3.0 IGO); rivers, coastline and
neighbouring countries: Natural Earth (public domain). Projection: WGS 84 / UTM zone 46N.
"""
from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrow, Rectangle

ROOT = Path(__file__).resolve().parents[1]
G = ROOT / "gis"
OUT = ROOT / "outputs" / "figures"
CRS = "EPSG:32646"
BLUE, VERM = "#0072B2", "#D55E00"
plt.rcParams.update({"font.family": "Arial", "font.size": 7})


def add_scalebar(ax, km, loc=(0.05, 0.05)):
    """Two-segment scale bar of `km` kilometres (map units are metres), anchored in axes fraction."""
    x0, x1 = ax.get_xlim(); y0, y1 = ax.get_ylim()
    x = x0 + loc[0] * (x1 - x0); y = y0 + loc[1] * (y1 - y0)
    h = 0.012 * (y1 - y0); seg = km * 1000 / 2
    for i, fc in enumerate(["black", "white"]):
        ax.add_patch(Rectangle((x + i * seg, y), seg, h, facecolor=fc, edgecolor="black", lw=0.5, zorder=9))
    for i, lab in enumerate(["0", str(km // 2), f"{km} km"]):
        ax.text(x + i * seg, y + 1.6 * h, lab, ha="center", va="bottom", fontsize=5.5, zorder=9)

adm2 = gpd.read_file(G / "geoBoundaries_BGD_ADM2.geojson").to_crs(CRS)
adm2["name"] = adm2.shapeName.replace({"Barisal": "Barishal"})
upa = gpd.read_file(G / "upazilas_study_districts.geojson")
upa = upa[upa.district.isin(["Barisal", "Kurigram"])].to_crs(CRS)
upa["district"] = upa.district.replace({"Barisal": "Barishal"})
upa["label"] = (upa.shapeName.str.replace("Barisal", "Barishal").str.replace(r"\s*\(.*\)", "", regex=True)
                .str.replace("Banari Para", "Banaripara"))
bgd = adm2.dissolve()
world_ll = gpd.read_file(G / "ne_50m_admin_0_countries")
world = world_ll.clip((84, 18, 96, 29)).to_crs(CRS)
ocean = gpd.read_file(G / "ne_10m_ocean").clip((84, 18, 96, 29)).to_crs(CRS)
rivers = gpd.read_file(G / "ne_10m_rivers_lake_centerlines").clip((87.5, 20.5, 93, 27.5)).to_crs(CRS)

fig = plt.figure(figsize=(174 / 25.4, 120 / 25.4))
axA = fig.add_axes([0.0, 0.0, 0.56, 1.0])
axK = fig.add_axes([0.585, 0.53, 0.40, 0.45])
axB = fig.add_axes([0.585, 0.02, 0.40, 0.47])

# ---- panel a: Bangladesh -----------------------------------------------------------
xmin, ymin, xmax, ymax = bgd.total_bounds
pad = 40000
axA.set_xlim(xmin - pad, xmax + pad); axA.set_ylim(ymin - pad, ymax + pad)
ocean.plot(ax=axA, color="#DCEBF5", zorder=0)
world.plot(ax=axA, color="#F2F2F2", edgecolor="#BDBDBD", linewidth=0.4, zorder=1)
adm2.plot(ax=axA, color="#FAFAFA", edgecolor="#9E9E9E", linewidth=0.3, zorder=2)
adm2[adm2.name == "Barishal"].plot(ax=axA, color=BLUE, alpha=0.85, edgecolor="black", linewidth=0.5, zorder=3)
adm2[adm2.name == "Kurigram"].plot(ax=axA, color=VERM, alpha=0.85, edgecolor="black", linewidth=0.5, zorder=3)
bgd.boundary.plot(ax=axA, color="#424242", linewidth=0.6, zorder=4)
rivers.plot(ax=axA, color="#4A90C2", linewidth=0.9, zorder=5)
for nm, dx, dy in [("Kurigram", 30000, 10000), ("Barishal", -95000, -30000)]:
    c = adm2[adm2.name == nm].representative_point().iloc[0]
    axA.annotate(nm, (c.x, c.y), (c.x + dx, c.y + dy), fontsize=8, fontweight="bold",
                 color=VERM if nm == "Kurigram" else BLUE,
                 arrowprops=dict(arrowstyle="-", lw=0.5, color="#424242"), zorder=8)
for txt, lon, lat, rot in [("Brahmaputra", 89.62, 24.95, -80), ("Teesta", 88.85, 26.20, -45),
                           ("Ganges", 88.70, 24.30, -25)]:
    p = gpd.GeoSeries(gpd.points_from_xy([lon], [lat]), crs="EPSG:4326").to_crs(CRS).iloc[0]
    axA.text(p.x, p.y, txt, fontsize=6, style="italic", color="#2C6E9B", rotation=rot, zorder=8, ha="center")
p = gpd.GeoSeries(gpd.points_from_xy([90.0], [21.25]), crs="EPSG:4326").to_crs(CRS).iloc[0]
axA.text(p.x, p.y, "Bay of Bengal", fontsize=7.5, style="italic", color="#2C6E9B", ha="center", zorder=8)
for txt, lon, lat in [("INDIA", 88.25, 25.6), ("INDIA", 91.9, 25.75), ("MYANMAR", 93.0, 21.4)]:
    p = gpd.GeoSeries(gpd.points_from_xy([lon], [lat]), crs="EPSG:4326").to_crs(CRS).iloc[0]
    axA.text(p.x, p.y, txt, fontsize=6, color="#8A8A8A", ha="center", zorder=8)
add_scalebar(axA, 100, loc=(0.06, 0.04))
axA.add_patch(FancyArrow(0.92, 0.86, 0, 0.045, width=0.006, head_width=0.025, head_length=0.022,
                         transform=axA.transAxes, color="black", zorder=9))
axA.text(0.92, 0.94, "N", transform=axA.transAxes, ha="center", va="bottom", fontsize=8, fontweight="bold")
axA.text(0.02, 0.98, "a", transform=axA.transAxes, fontsize=10, fontweight="bold", va="top")
# locator inset: South Asia
ins = axA.inset_axes([0.70, 0.04, 0.28, 0.27])
world_ll.clip((60, 0, 105, 40)).plot(ax=ins, color="#E0E0E0", edgecolor="white", linewidth=0.2, aspect="equal")
bgd.to_crs("EPSG:4326").plot(ax=ins, color="#424242", aspect="equal")
ins.set_xlim(66, 100); ins.set_ylim(5, 37); ins.set_xticks([]); ins.set_yticks([]); ins.set_xlabel(""); ins.set_ylabel("")
for s in ins.spines.values():
    s.set_linewidth(0.5)
ins.text(0.5, 0.03, "South Asia", transform=ins.transAxes, ha="center", fontsize=5.5, color="#616161")
axA.set_axis_off()


# ---- panels b and c: upazilas -----------------------------------------------------------
OFFSET = {"Phulbari": (-7000, 2500), "Nageshwari": (5500, 1000), "Chilmari": (-6500, 1500), "Raumari": (6000, 2500),
          "Bhurungamari": (0, -3500), "Char Rajibpur": (2500, -1500), "Rajarhat": (-1500, 0), "Kurigram Sadar": (1000, 0),
          "Agailjhara": (-4000, -4000), "Gaurnadi": (3500, 4500), "Wazirpur": (-5000, 1500), "Babuganj": (4500, -2500),
          "Mehendiganj": (-4000, -1000), "Muladi": (-2500, 1500), "Hizla": (1500, 2000)}


def district_panel(ax, name, col, tag, km, sb=(0.62, 0.05)):
    u = upa[upa.district == name]
    dist = adm2[adm2.name == name]
    x0, y0, x1, y1 = u.total_bounds; p = 8000
    ax.set_xlim(x0 - p, x1 + p); ax.set_ylim(y0 - p, y1 + p)
    adm2.plot(ax=ax, color="#F5F5F5", edgecolor="#BDBDBD", linewidth=0.4, zorder=1)
    u.plot(ax=ax, color=col, alpha=0.22, edgecolor=col, linewidth=0.5, zorder=2)
    dist.boundary.plot(ax=ax, color="black", linewidth=0.8, zorder=3)
    rivers.plot(ax=ax, color="#4A90C2", linewidth=1.1, zorder=4)
    for _, r in u.iterrows():
        c = r.geometry.representative_point(); dx, dy = OFFSET.get(r.label, (0, 0))
        ax.text(c.x + dx, c.y + dy, r.label, fontsize=5.0, ha="center", va="center", color="#212121", zorder=6,
                bbox=dict(boxstyle="round,pad=0.12", fc="white", ec="none", alpha=0.6))
    ax.text(0.02, 0.98, f"{tag}  {name} district", transform=ax.transAxes, fontsize=8, fontweight="bold",
            va="top", color=col)
    add_scalebar(ax, km, loc=sb)
    ax.set_axis_off()
    ax.add_patch(Rectangle((0, 0), 1, 1, transform=ax.transAxes, fill=False, lw=0.5, ec="#9E9E9E"))


district_panel(axK, "Kurigram", VERM, "b", 20, sb=(0.06, 0.04))
district_panel(axB, "Barishal", BLUE, "c", 20)
fig.savefig(OUT / "Fig1_study_area.png", dpi=600, bbox_inches="tight", facecolor="white")
fig.savefig(OUT / "Fig1_study_area.pdf", bbox_inches="tight", facecolor="white")
print("map written")
