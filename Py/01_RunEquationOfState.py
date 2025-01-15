# Import modules
import numpy as np
import rasterio
import time
import imageio
from rasterio.transform import Affine
from rasterio.plot import show
from rasterio.plot import show_hist

#specify dataset
adir = rasterio.open("Data/Vars_Adirondacks_GPP.tif")
yell = rasterio.open("Data/Vars_Yellowstone_GPP.tif")
redw = rasterio.open("Data/Vars_Redwoods_GPP.tif")



# adirondacks
dataset=adir


dataset.descriptions
S=dataset.read(1)##[10:200, 10:200]#[2700:3000, 600:900]
B=dataset.read(2)#[10:200, 10:200]#[2700:3000, 600:900]
N=dataset.read(3)#[10:200, 10:200]#[2700:3000, 600:900]
E=dataset.read(4)#[10:200, 10:200]#[2700:3000, 600:900]


# Plot raster


B_predicted=np.full_like(B, fill_value=np.nan)

start_time = time.time()
for row in range(S.shape[0]):
    for col in range(S.shape[1]):
        if not(np.isnan(S[row,col])):
            s = {'S':S[row,col], 'E':E[row,col], 'N':N[row,col]}
            B_predicted[row, col]=biomass(s)
            print("row ", row, " col:", col)

end_time = time.time()
print("loop took this many hours: ", round((end_time-start_time)/60/60, 2))

np.save('Outputs/B_predicted_adirondacks.npy', B_predicted)    # .npy extension is added if not given
d = np.load('Outputs/B_predicted_adirondacks.npy')
np.array_equal(B_predicted, d, equal_nan=True)

imageio.imwrite('Outputs/B_predicted_adirondacks.tif', d)

# yellowstone
dataset=yell


dataset.descriptions
S=dataset.read(1)##[10:200, 10:200]#[2700:3000, 600:900]
B=dataset.read(2)#[10:200, 10:200]#[2700:3000, 600:900]
N=dataset.read(3)#[10:200, 10:200]#[2700:3000, 600:900]
E=dataset.read(4)#[10:200, 10:200]#[2700:3000, 600:900]


# Plot raster


B_predicted=np.full_like(B, fill_value=np.nan)

start_time = time.time()
for row in range(S.shape[0]):
    for col in range(S.shape[1]):
        if not(np.isnan(S[row,col])):
            s = {'S':S[row,col], 'E':E[row,col], 'N':N[row,col]}
            B_predicted[row, col]=biomass(s)
            print("row ", row, " col:", col)

end_time = time.time()
print("loop took this many hours: ", round((end_time-start_time)/60/60, 2))

np.save('Outputs/B_predicted_yellowstone.npy', B_predicted)    # .npy extension is added if not given
d = np.load('Outputs/B_predicted_yellowstone.npy')
np.array_equal(B_predicted, d, equal_nan=True)

imageio.imwrite('Outputs/B_predicted_yellowstone.tif', d)


# redwoods
dataset=redw


dataset.descriptions
S=dataset.read(1)##[10:200, 10:200]#[2700:3000, 600:900]
B=dataset.read(2)#[10:200, 10:200]#[2700:3000, 600:900]
N=dataset.read(3)#[10:200, 10:200]#[2700:3000, 600:900]
E=dataset.read(4)#[10:200, 10:200]#[2700:3000, 600:900]


# Plot raster


B_predicted=np.full_like(B, fill_value=np.nan)

start_time = time.time()
for row in range(S.shape[0]):
    for col in range(S.shape[1]):
        if not(np.isnan(S[row,col])):
            s = {'S':S[row,col], 'E':E[row,col], 'N':N[row,col]}
            B_predicted[row, col]=biomass(s)
            print("row ", row, " col:", col)

end_time = time.time()
print("loop took this many hours: ", round((end_time-start_time)/60/60, 2))

np.save('Outputs/B_predicted_redwoods.npy', B_predicted)    # .npy extension is added if not given
d = np.load('Outputs/B_predicted_redwoods.npy')
np.array_equal(B_predicted, d, equal_nan=True)

imageio.imwrite('Outputs/B_predicted_redwoods.tif', d)


# conus
dataset=rasterio.open("Data/CONUSvariables.tif")


dataset.descriptions
S=dataset.read(1)##[10:200, 10:200]#[2700:3000, 600:900]
B=dataset.read(2)#[10:200, 10:200]#[2700:3000, 600:900]
N=dataset.read(3)#[10:200, 10:200]#[2700:3000, 600:900]
E=dataset.read(4)#[10:200, 10:200]#[2700:3000, 600:900]


# Plot raster


B_predicted=np.full_like(B, fill_value=np.nan)

start_time = time.time()
for row in range(S.shape[0]):
    for col in range(S.shape[1]):
        if not(np.isnan(S[row,col])):
            s = {'S':S[row,col], 'E':E[row,col], 'N':N[row,col]}
            B_predicted[row, col]=biomass(s)
            print("row ", row, " col:", col)

end_time = time.time()
print("loop took this many hours: ", round((end_time-start_time)/60/60, 2))

np.save('Outputs/B_predicted_CONUS.npy', B_predicted)    # .npy extension is added if not given
d = np.load('Outputs/B_predicted_CONUS.npy')
np.array_equal(B_predicted, d, equal_nan=True)

imageio.imwrite('Outputs/B_predicted_CONUS.tif', d)
