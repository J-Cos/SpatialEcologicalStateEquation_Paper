library(terra)
library(tidyverse)

##############
# Adirondacks
##############

# load
bp<-rast('Outputs/B_predicted_adirondacks.tif')
var<-rast("Data/Vars_Adirondacks_GPP.tif")
adir<-vect('Data/apaLandClass202306')
boundary<-vect('Data/AdirondackParkBoundary2017')


# combine rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)
vars<-c(var, bp) %>%
    tidyterra::rename("B_predicted" = "B_predicted_adirondacks")

# align vectors
adir<-project(adir, vars)
boundary<-project(boundary, vars) %>% 
    aggregate %>% 
    as.polygons

#create degradation polys
wilderness<-adir[adir$LCCode %in% c(7)] %>% aggregate(dissolve=TRUE)
semi<-adir[adir$LCCode %in% c(8, 9, 10)] %>% aggregate(dissolve=TRUE)
deg<-adir[adir$LCCode %in% c(1,2,3,4,5,6,11,12,13)]  %>% aggregate(dissolve=TRUE)
lakes<-adir[adir$LCCode %in% c(15)]  %>% aggregate(dissolve=TRUE)
cats<-vect(c(wilderness, semi, deg, lakes))
values(cats)<-data.frame(names=c("wild", "semi", "deg", "lake"))

# mask raster to boundary
vars<-mask(vars, boundary) %>% crop(., boundary)

# save
terra::writeVector(cats, filename="Outputs/Adirondacks/LandClasses", overwrite=TRUE)
terra::writeVector(boundary, filename="Outputs/Adirondacks/Boundary", overwrite=TRUE)
terra::writeRaster(vars, filename="Outputs/Adirondacks/EquationVariables.tif", overwrite=TRUE)

##############
# Redwoods
##############

# load
bp<-rast('Outputs/B_predicted_redwoods.tif')
var<-rast("Data/Vars_Redwoods_GPP.tif")
boundary<-vect('Data/Redwoods_boundary.kml')

# combine rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)
vars<-c(var, bp) %>%
    tidyterra::rename("B_predicted" = "B_predicted_redwoods")

# align vectors
boundary<-project(boundary, vars)
buffered<-buffer(boundary, (1000*100))
expanse(boundary)/1000
expanse(buffered)/1000

cats<-vect(c( boundary, erase(buffered, boundary)))
values(cats)<-data.frame(names=c("wild", "deg"))


# mask raster to boundary
vars<-mask(vars, buffered)
vars<-crop(vars, ext(buffered))

# save
terra::writeVector(cats, filename="Outputs/Redwoods/LandClasses", overwrite=TRUE)
terra::writeVector(buffered, filename="Outputs/Redwoods/Boundary", overwrite=TRUE)
terra::writeRaster(vars, filename="Outputs/Redwoods/EquationVariables.tif", overwrite=TRUE)




##############
# Yellowstone
##############

# load
bp<-rast('Outputs/B_predicted_yellowstone.tif')
var<-rast("Data/Vars_Yellowstone_GPP.tif")
boundary<-vect("Data/YELL_boundary")
v<-vect("Data/WildernessAreas2024")

# combine rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)
vars<-c(var, bp) %>%
    tidyterra::rename("B_predicted" = "B_predicted_yellowstone")

# align vectors
boundary<-project(boundary, vars)
buffered<-buffer(boundary, (1000*30))
expanse(boundary)/1000
expanse(buffered)/1000

# mask raster to boundary
vars<-mask(vars, buffered)
vars<-crop(vars, ext(buffered))

# mask wilderness polygons to boundary
v<-mask(v, buffered)
wilderness<-crop(v, ext(buffered)) %>% aggregate(dissolve=TRUE)

cats<-vect(c(
    boundary, 
    wilderness,
    erase(buffered, aggregate(vect(c(boundary, wilderness))))
    ))
values(cats)<-data.frame(names=c("wild", "semi", "deg"))

# save
terra::writeVector(cats, filename="Outputs/Yellowstone/LandClasses", overwrite=TRUE)
terra::writeVector(buffered, filename="Outputs/Yellowstone/Boundary", overwrite=TRUE)
terra::writeRaster(vars, filename="Outputs/Yellowstone/EquationVariables.tif", overwrite=TRUE)