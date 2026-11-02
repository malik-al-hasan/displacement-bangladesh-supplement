"""
09c_osm_water.py
Builds river-channel polygons and named river centre-lines for the map from OpenStreetMap data
(Overpass API extracts in gis/osm/, (c) OpenStreetMap contributors, ODbL).

Outputs (gis/osm/):
  {district}_water_polygons.geojson   wide river channels and water bodies (natural=water, waterway=riverbank)
  {district}_river_lines.geojson      waterway=river centre-lines with English names where tagged
  bangladesh_major_river_lines.geojson  named major rivers for the national panel (if the extract exists)
"""
import json
from pathlib import Path

import geopandas as gpd
from shapely.geometry import LineString, Polygon
from shapely.ops import linemerge, polygonize, unary_union

G = Path(__file__).resolve().parents[1] / "gis" / "osm"


def way_coords(w):
    return [(p["lon"], p["lat"]) for p in w.get("geometry", [])]


def relation_polygons(rel):
    outers, inners = [], []
    for m in rel.get("members", []):
        if m.get("type") != "way" or not m.get("geometry"):
            continue
        ls = LineString([(p["lon"], p["lat"]) for p in m["geometry"]])
        (inners if m.get("role") == "inner" else outers).append(ls)
    if not outers:
        return None
    outer = unary_union(list(polygonize(linemerge(outers))))
    if inners:
        inner = unary_union(list(polygonize(linemerge(inners))))
        outer = outer.difference(inner)
    return outer if not outer.is_empty else None


def name_of(tags):
    return tags.get("name:en") or tags.get("name") or ""


def build(district):
    d = json.load(open(G / f"{district}_water.json", encoding="utf-8"))
    polys, lines = [], []
    for el in d["elements"]:
        t = el.get("tags", {})
        if el["type"] == "way":
            c = way_coords(el)
            if len(c) < 2:
                continue
            if t.get("waterway") == "river":
                lines.append({"name": name_of(t), "geometry": LineString(c)})
            elif len(c) >= 4 and c[0] == c[-1]:
                polys.append({"kind": t.get("water") or t.get("waterway"), "name": name_of(t), "geometry": Polygon(c).buffer(0)})
        elif el["type"] == "relation":
            g = relation_polygons(el)
            if g is not None:
                polys.append({"kind": t.get("water") or t.get("waterway"), "name": name_of(t), "geometry": g.buffer(0)})
    P = gpd.GeoDataFrame(polys, crs="EPSG:4326")
    L = gpd.GeoDataFrame(lines, crs="EPSG:4326")
    P = P[P.geometry.is_valid & ~P.geometry.is_empty]
    P.to_file(G / f"{district}_water_polygons.geojson", driver="GeoJSON")
    L.to_file(G / f"{district}_river_lines.geojson", driver="GeoJSON")
    area = P.to_crs("EPSG:32646").area.sum() / 1e6
    print(district, "polygons", len(P), f"({area:.0f} km2)", "lines", len(L))


def national():
    f = G / "bangladesh_major_rivers.json"
    if not f.exists():
        return
    d = json.load(open(f, encoding="utf-8"))
    rows = [{"name": name_of(el.get("tags", {})), "geometry": LineString(way_coords(el))}
            for el in d["elements"] if el["type"] == "way" and len(way_coords(el)) >= 2]
    gdf = gpd.GeoDataFrame(rows, crs="EPSG:4326").drop_duplicates(subset=["geometry"])
    gdf.to_file(G / "bangladesh_major_river_lines.geojson", driver="GeoJSON")
    print("national lines", len(gdf), gdf.name.value_counts().head(15).to_dict())


if __name__ == "__main__":
    for dist in ("kurigram", "barishal"):
        build(dist)
    national()
