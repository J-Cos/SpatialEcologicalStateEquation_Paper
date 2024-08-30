#########################
#create dummy rasters
######################

# Import modules
import numpy as np
import rasterio
from rasterio.transform import Affine


# Generate mesh grid for rasters
x = np.linspace(-90, 90, 100)
y = np.linspace(90, -90, 50)
X, Y = np.meshgrid(x, y)

# Generate values for mesh grid
Z1 = np.abs(((X - 10) ** 2 + (Y - 10) ** 2) / 1 ** 2)
Z2 = np.abs(((X + 10) ** 2 + (Y + 10) ** 2) / 2.5 ** 2)
Z3 = np.abs(((X + 3) + (Y - 8) ** 2) / 3 ** 2)

# Generate raster values for two rasters
Z_a = (Z1 - Z2)
Z_b = (Z2 - Z3)
Z_c = (Z1 - Z3)


# Set transform
xres = (x[-1] - x[0]) / len(x)
yres = (y[-1] - y[0]) / len(y)
transform = Affine.translation(x[0] - xres / 2, y[0] - yres / 2) * Affine.scale(xres, yres)

# Save first raster
with rasterio.open(
        "math_raster_a.tif",
        mode="w",
        driver="GTiff",
        height=Z_a.shape[0],
        width=Z_a.shape[1],
        count=1,
        dtype=Z_a.dtype,
        crs="+proj=latlong",
        transform=transform,
) as new_dataset:
        new_dataset.write(Z_a, 1)

# Save second raster
with rasterio.open(
        "math_raster_b.tif",
        mode="w",
        driver="GTiff",
        height=Z_b.shape[0],
        width=Z_b.shape[1],
        count=1,
        dtype=Z_b.dtype,
        crs="+proj=latlong",
        transform=transform,
) as new_dataset:
        new_dataset.write(Z_b, 1)

# Save second raster
with rasterio.open(
        "math_raster_c.tif",
        mode="w",
        driver="GTiff",
        height=Z_b.shape[0],
        width=Z_b.shape[1],
        count=1,
        dtype=Z_b.dtype,
        crs="+proj=latlong",
        transform=transform,
) as new_dataset:
        new_dataset.write(Z_c, 1)