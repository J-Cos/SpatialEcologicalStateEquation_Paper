#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
bp<-rast('Outputs/B_predicted_CONUS.tif')
var<-rast("Data/CONUSvariables.tif")
pas<-vect(
        c(
            vect("Data/WDPA_WDOECM_Dec2024_Public_USA_shp_0", layer="WDPA_WDOECM_Dec2024_Public_USA_shp-polygons"),
            vect("Data/WDPA_WDOECM_Dec2024_Public_USA_shp_1", layer="WDPA_WDOECM_Dec2024_Public_USA_shp-polygons"),
            vect("Data/WDPA_WDOECM_Dec2024_Public_USA_shp_2", layer="WDPA_WDOECM_Dec2024_Public_USA_shp-polygons")
        ))
usa <- vect("Data/us-state-boundaries")

# 2) spatially align
# match projections of rasters
crs(bp)<-crs(var)
ext(bp)<-ext(var)

# match projections of vectors to rasters
pas<-project(pas, var) %>% crop(., ext(var))
usa<-project(usa, var) %>% crop(., ext(var))

# 3) process spatial data
#combine rasters
vars<-c(var, bp) %>%
    tidyterra::rename("B_predicted" = "B_predicted_CONUS")
vars[["dev"]]<-(log(vars[["b"]])-log(vars[["B_predicted"]]))

vars[["states"]]<-rasterize(usa, vars, "name")
vars[["states_cover"]]<-rasterize(usa, vars, cover=TRUE)

vars[["IUCN_CAT"]]<-rasterize(pas[pas$MARINE==0] , vars, "IUCN_CAT")
vars[["PA_NAME"]]<-rasterize(pas[pas$MARINE==0] , vars, "NAME")
vars[["WDPAID"]]<-rasterize(pas[pas$MARINE==0] , vars, "WDPAID")
vars[["PA_cover"]]<-rasterize(pas, vars, cover=TRUE)

vars<-mask(x=vars, mask=vars[["mask"]]>0.01, maskvalues=1, inverse=TRUE)

# 4) save spatial data
writeVector(pas, "Outputs/PAs")
writeVector(usa, "Outputs/States")
writeRaster(vars, "Outputs/Variables_EeosOnly.tif", overwrite=TRUE)