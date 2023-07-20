import matplotlib.pyplot as plt
import geopandas
import pandas as pd
import pathlib

import contextily as cx


# Read the background
# bathymetry_path = "//fs/shared/datac/Geo/Layers/Belgium/elevation/bathymetry/bathymetry_bmz_emodnet_2020.tif"
# bathymetry_xr = rioxarray.open_rasterio(bathymetry_path)

# Read the station coordinates
output_folder = pathlib.Path('../figures')
locations_path = pathlib.Path('../recordings_locations.csv')

df = pd.read_csv(locations_path)
geodf = geopandas.GeoDataFrame(df,
                               geometry=geopandas.points_from_xy(x=df['lon'], y=df['lat']),
                               crs='epsg:4326')

# Distribution of all the collected data
fig, ax = plt.subplots(figsize=(8, 10))
geodf_crs = geodf.to_crs('epsg:3857')
ax = geodf_crs.plot('country', categorical=True, legend=True, ax=ax, markersize=25,  marker='x', zorder=3)
for x, y, label in zip(geodf_crs.geometry.x, geodf_crs.geometry.y, geodf_crs.station_name):
    ax.annotate(label, xy=(x, y), xytext=(3, 3), textcoords="offset points", zorder=2, fontsize=14)

cx.add_basemap(ax, source=cx.providers.Stamen.TerrainBackground)
ax.set_axis_off()
plt.xlabel('Longitude [degrees East]')
plt.ylabel('Latitude [degrees North]')
plt.title(None)
plt.savefig(output_folder.joinpath('recordings_locations.png'), dpi=350, bbox_inches="tight",
            transparent=True)
plt.show()
