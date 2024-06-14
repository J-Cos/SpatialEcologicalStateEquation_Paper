library(terra)
library(tidyverse)

##############
# Adirondacks
##############

# load
bp<-rast('Outputs/B_predicted.tif')
var<-rast("Data/Adirondacks_variables.tif")
adir<-vect('Data/apaLandClass202306')
boundary<-vect('Data/AdirondackParkBoundary2017')
pop<-rast('Data/Adirondacks_population.tif')


# combine rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)
vars<-c(var, bp)

# align vectors
adir<-project(adir, vars)
boundary<-project(boundary, vars) %>% 
    aggregate %>% 
    as.polygons

# mask raster to boundary
vars<-mask(vars, boundary) %>% crop(., boundary)
pop<-mask(pop, boundary) %>% crop(., boundary)



# save
terra::writeVector(adir, filename="Outputs/LandClasses", overwrite=TRUE)
terra::writeVector(boundary, filename="Outputs/AdirondacksBoundary", overwrite=TRUE)
terra::writeRaster(vars, filename="Outputs/AdirondacksEquationVariables.tif", overwrite=TRUE)
terra::writeRaster(pop, filename="Outputs/AdirondacksPopulation.tif", overwrite=TRUE)

##############
# Redwoods
##############

# load
bp<-rast('Outputs/B_predicted_redwoods.tif')
var<-rast("Data/Redwoods_variables.tif")
boundary<-vect('Data/Redwoods_boundary.kml')

# combine rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)
vars<-c(var, bp) %>%
    tidyterra::rename("B_predicted" = "B_predicted_redwoods")

# align vectors
boundary<-project(boundary, vars)
buffered<-buffer(boundary, 10000)
expanse(boundary)/1000000
expanse(buffered)/1000000


# mask raster to boundary
vars<-mask(vars, buffered)
vars<-crop(vars, ext(buffered))
# save
terra::writeVector(boundary, filename="Outputs/RedwoodsBoundary", overwrite=TRUE)
terra::writeRaster(vars, filename="Outputs/RedwoodsEquationVariables.tif", overwrite=TRUE)