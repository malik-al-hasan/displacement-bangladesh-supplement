"""
09d_study_area_map_v6.py  -  Figure 1 (v6), study area.

Changes from v4: the Natural Earth river centre-lines (a generalised world-scale layer that in Bangladesh
draws schematic 'Ganges' and 'Brahmaputra' courses, including a line across dry land west of Barishal city)
are removed. District panels show river channels from OpenStreetMap (water polygons and river centre-lines,
(c) OpenStreetMap contributors, ODbL; built by 09c_osm_water.py). The national panel shows no rivers,
because no country-wide river layer of comparable accuracy was available.
Boundaries: geoBoundaries (BBS/OCHA ROAP 2020, CC BY 3.0 IGO); coastline and neighbouring countries:
Natural Earth (public domain). Projection: WGS 84 / UTM zone 46N (EPSG:32646).
"""
from pathlib import Path

import geopandas as gpd
import matplotlib.patheffects as pe
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrow, Patch, Rectangle
from shapely.geometry import box

ROOT = Path(__file__).resolve().parents[1]
G = ROOT / "gis"
OUT = ROOT / "outputs" / "figures_v6"
OUT.mkdir(exist_ok=True)
CRS = "EPSG:32646"
BRN, BLK = "#8C510A", "#1A1A1A"
WATER, WATER_EDGE, RIVER_TXT = "#9ECAE1", "#6BAED6", "#08519C"
plt.rcParams.update({"font.family": "Arial", "font.size": 8})
halo = [pe.withStroke(linewidth=1.6, foreground="white")]


def add_scalebar(ax, km, loc=(0.05, 0.05)):
    x0, x1 = ax.get_xlim(); y0, y1 = ax.get_ylim()
    x = x0 + loc[0] * (x1 - x0); y = y0 + loc[1] * (y1 - y0)
    h = 0.012 * (y1 - y0); seg = km * 1000 / 2
    for i, fc in enumerate(["black", "white"]):
        ax.add_patch(Rectangle((x + i * seg, y), seg, h, facecolor=fc, edgecolor="black", lw=0.5, zorder=9))
    for i, lab in enumerate(["0", str(km // 2), f"{km} km"]):
        ax.text(x + i * seg, y + 1.6 * h, lab, ha="center", va="bottom", fontsize=7, zorder=9)


def pt(lon, lat):
    return gpd.GeoSeries(gpd.points_from_xy([lon], [lat]), crs="EPSG:4326").to_crs(CRS).iloc[0]


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
WATERP = {d: gpd.read_file(G / "osm" / f"{d}_water_polygons.geojson") for d in ("kurigram", "barishal")}
WATERL = {d: gpd.read_file(G / "osm" / f"{d}_river_lines.geojson") for d in ("kurigram", "barishal")}
# river labels placed on the OSM centre-lines (lon, lat, rotation)
RIVER_LABELS = {"kurigram": [("Brahmaputra", 89.738, 25.69, -80), ("Teesta", 89.50, 25.655, -32),
                             ("Dharla", 89.575, 25.955, -50), ("Jinjiram", 89.885, 25.585, -82)],
                "barishal": [("Meghna", 90.705, 22.80, -72), ("Kirtankhola", 90.375, 22.745, -62),
                             ("Arial Khan", 90.33, 23.075, -12)]}
SURVEYED = ["Barishal Sadar", "Mehendiganj", "Raumari", "Char Rajibpur"]
OFFSET = {"Phulbari": (-6000, 8500), "Nageshwari": (5500, 1000), "Chilmari": (-6500, 1500), "Raumari": (1000, 12500),
          "Bhurungamari": (0, -3500), "Char Rajibpur": (7500, -4500), "Rajarhat": (-1500, 0), "Kurigram Sadar": (1000, 0),
          "Ulipur": (-3000, 0), "Agailjhara": (-4000, -4000), "Gaurnadi": (2500, -6500), "Wazirpur": (-5000, 1500),
          "Babuganj": (4500, -2500), "Mehendiganj": (-2000, 6500), "Muladi": (-2500, 1500), "Hizla": (1500, 2000),
          "Barishal Sadar": (-9500, -5500), "Bakerganj": (0, -2000), "Banaripara": (0, 0)}

fig = plt.figure(figsize=(174 / 25.4, 172 / 25.4))
axA = fig.add_axes([0.0, 0.10, 0.45, 0.88])
axK = fig.add_axes([0.46, 0.505, 0.535, 0.485])
axB = fig.add_axes([0.46, 0.01, 0.535, 0.485])

# ---- panel a: Bangladesh ------------------------------------------------------------------------
xmin, ymin, xmax, ymax = bgd.total_bounds
pad = 40000
axA.set_xlim(xmin - pad, xmax + pad); axA.set_ylim(ymin - pad, ymax + pad)
ocean.plot(ax=axA, color="#DCEBF5", zorder=0)
world.plot(ax=axA, color="#F2F2F2", edgecolor="#BDBDBD", linewidth=0.4, zorder=1)
adm2.plot(ax=axA, color="#FAFAFA", edgecolor="#9E9E9E", linewidth=0.3, zorder=2)
adm2[adm2.name == "Barishal"].plot(ax=axA, color=BRN, alpha=0.85, edgecolor="black", linewidth=0.5, zorder=3)
adm2[adm2.name == "Kurigram"].plot(ax=axA, color=BLK, alpha=0.85, edgecolor="black", linewidth=0.5, zorder=3)
bgd.boundary.plot(ax=axA, color="#424242", linewidth=0.6, zorder=4)
for nm, dx, dy, ha in [("Kurigram", 30000, 10000, "left"), ("Barishal", -38000, -30000, "right")]:
    c = adm2[adm2.name == nm].representative_point().iloc[0]
    axA.annotate(nm, (c.x, c.y), (c.x + dx, c.y + dy), ha=ha, fontsize=9, fontweight="bold", color=BLK if nm == "Kurigram" else BRN,
                 arrowprops=dict(arrowstyle="-", lw=0.5, color="#424242"), zorder=8)
p = pt(90.35, 21.25); axA.text(p.x, p.y, "Bay of Bengal", fontsize=8, style="italic", color="#2C6E9B", ha="center", zorder=8)
for txt, lon, lat in [("INDIA", 88.1, 23.95), ("INDIA", 91.9, 25.75)]:
    p = pt(lon, lat); axA.text(p.x, p.y, txt, fontsize=7, color="#6E6E6E", ha="center", zorder=8)
add_scalebar(axA, 100, loc=(0.06, 0.04))
axA.add_patch(FancyArrow(0.92, 0.86, 0, 0.045, width=0.006, head_width=0.025, head_length=0.022,
                         transform=axA.transAxes, color="black", zorder=9))
axA.text(0.92, 0.94, "N", transform=axA.transAxes, ha="center", va="bottom", fontsize=8, fontweight="bold")
axA.text(0.02, 0.98, "a", transform=axA.transAxes, fontsize=10, fontweight="bold", va="top")
ins = axA.inset_axes([0.70, 0.04, 0.28, 0.27])
world_ll.clip((60, 0, 105, 40)).plot(ax=ins, color="#E0E0E0", edgecolor="white", linewidth=0.2, aspect="equal")
bgd.to_crs("EPSG:4326").plot(ax=ins, color="#424242", aspect="equal")
ins.set_xlim(66, 100); ins.set_ylim(5, 37); ins.set_xticks([]); ins.set_yticks([]); ins.set_xlabel(""); ins.set_ylabel("")
for s in ins.spines.values():
    s.set_linewidth(0.5)
ins.text(0.5, 0.03, "South Asia", transform=ins.transAxes, ha="center", fontsize=7, color="#424242")
axA.set_axis_off(); axA.set_anchor("N")


# ---- panels b and c: districts with OSM river channels --------------------------------------------
def district_panel(ax, name, key, col, tag, km, sb):
    u = upa[upa.district == name]
    dist = adm2[adm2.name == name]
    x0, y0, x1, y1 = u.total_bounds; p = 8000
    ax.set_xlim(x0 - p, x1 + p); ax.set_ylim(y0 - p, y1 + p)
    view = gpd.GeoSeries([box(x0 - p, y0 - p, x1 + p, y1 + p)], crs=CRS)
    adm2.plot(ax=ax, color="#F5F5F5", edgecolor="#BDBDBD", linewidth=0.4, zorder=1)
    u.plot(ax=ax, color=col, alpha=0.08, edgecolor=col, linewidth=0.4, zorder=2)
    u[u.label.isin(SURVEYED)].plot(ax=ax, facecolor="none", edgecolor=col, linewidth=0.0, hatch="////", alpha=0.6, zorder=2.4)
    wp = WATERP[key].to_crs(CRS).clip(view)
    wp.plot(ax=ax, color=WATER, edgecolor=WATER_EDGE, linewidth=0.3, zorder=3)
    WATERL[key].to_crs(CRS).clip(view).plot(ax=ax, color=WATER_EDGE, linewidth=0.35, zorder=3.1)
    u[u.label.isin(SURVEYED)].boundary.plot(ax=ax, color="black", linewidth=0.7, zorder=3.5)
    dist.boundary.plot(ax=ax, color="black", linewidth=0.9, zorder=3.6)
    for _, r in u.iterrows():
        c = r.geometry.representative_point(); dx, dy = OFFSET.get(r.label, (0, 0))
        bold = r.label in SURVEYED
        ax.text(c.x + dx, c.y + dy, r.label, fontsize=7.5 if bold else 6.8, fontweight="bold" if bold else "normal",
                ha="center", va="center", color="#212121", zorder=6, path_effects=halo)
    for lab, lon, lat, rot in RIVER_LABELS[key]:
        q = pt(lon, lat)
        ax.text(q.x, q.y, lab, fontsize=7.5, style="italic", color=RIVER_TXT, rotation=rot, ha="center", va="center",
                zorder=7, path_effects=halo)
    ax.text(0.02, 0.98, f"{tag}  {name} district", transform=ax.transAxes, fontsize=9, fontweight="bold", va="top", color=col)
    add_scalebar(ax, km, loc=sb)
    ax.set_axis_off()
    ax.add_patch(Rectangle((0, 0), 1, 1, transform=ax.transAxes, fill=False, lw=0.5, ec="#9E9E9E"))


district_panel(axK, "Kurigram", "kurigram", BLK, "b", 20, sb=(0.06, 0.03))
district_panel(axB, "Barishal", "barishal", BRN, "c", 20, sb=(0.72, 0.04))
fig.legend(handles=[Patch(facecolor=WATER, edgecolor=WATER_EDGE, linewidth=0.4, label="River channel and water body"),
                    Patch(facecolor="white", edgecolor="black", hatch="////", linewidth=0.6, label="Upazila with field sites")],
           loc="lower left", bbox_to_anchor=(0.03, 0.27), frameon=True, fontsize=8, framealpha=0.95, edgecolor="#BDBDBD")
fig.savefig(OUT / "Fig1_study_area.png", dpi=600, facecolor="white")
fig.savefig(OUT / "Fig1_study_area.pdf", facecolor="white")
print("map written")
