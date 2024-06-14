import rasterio
import time

# Open raster and plot
S = rasterio.open("math_raster_a.tif").read(1)
#B = rasterio.open("math_raster_a.tif").read(1)
N = rasterio.open("math_raster_b.tif").read(1)
E = rasterio.open("math_raster_c.tif").read(1)

B_predicted=np.full_like(S, fill_value=np.nan)

start_time = time.time()
for row in range(S.shape[0]):
    for col in range(S.shape[1]):
        s = {'S':S[row,col], 'E':E[row,col], 'N':N[row,col]}
        B_predicted[row, col]=biomass_approx(s)

end_time = time.time()
print("loop will take this many hours per million cells: ", round((end_time-start_time)*200/60/60, 2))


