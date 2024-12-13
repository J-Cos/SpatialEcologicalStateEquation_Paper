library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(rnaturalearth)

loadData<-function(region){
    l<-list(
        cats=terra::vect(file.path("Outputs", region, "LandClasses")),
        boundary=terra::vect( file.path("Outputs", region, "Boundary")),
        vars=terra::rast(file.path("Outputs", region, "EquationVariables.tif")))
    return(l)
}

getCategoryRaster<-function(data){

    cat_rast<-data[["vars"]][[1]] %>%
        mask(x=., mask=data[["cats"]][data[["cats"]]$names =="wild"], inverse=TRUE, updatevalue=1) %>%
        mask(x=., mask=data[["cats"]][data[["cats"]]$names =="semi"], inverse=TRUE, updatevalue=2) %>%
        mask(x=., mask=data[["cats"]][data[["cats"]]$names =="deg"], inverse=TRUE, updatevalue=3) %>%
        mask(x=., mask=data[["cats"]][!data[["cats"]]$names %in% c("wild", "semi", "deg")], inverse=TRUE, updatevalue=1000) %>%
        mask(x=., mask= data[["vars"]][[1]] ) %>%
        tidyterra::rename("Ecosystem category" = "s")
    return(cat_rast)
}


############################
# Adirondacks
############################
# load
a<-loadData("Adirondacks")
r<-loadData("Redwoods")
y<-loadData("Yellowstone")


getCategoryRaster(a) %>% plot




cat_rast<-a[["vars"]][[1]] %>%

    mask(x=., mask= a[["vars"]][[1]] ) %>%
    tidyterra::rename("Ecosystem category" = "s")

datavispanel<-ggplot() +
    geom_spatraster(data = cat_rast)+
    scale_fill_manual(values=c("#3C5488FF", "#00A087FF", "#7E6148FF"), na.translate = FALSE)+
    #geom_spatvector(data=boundary, linetype=2, color="black", linewidth=1, fill=NA)+ 
    geom_spatvector(data=lakes, alpha=1, fill="black", color="black")+
    theme_minimal() +
    theme(
        legend.position = c(0.15, 0.10),
        legend.title=element_blank(),
        legend.key.size = unit(10, "pt"))








vars<-rast('Outputs/AdirondacksEquationVariables.tif')
adir<-vect('Outputs/LandClasses')
boundary<-vect('Outputs/AdirondacksBoundary')

# create a raster of land categories
wilderness<-adir[adir$LCCode %in% c(7)] %>% aggregate(dissolve=TRUE)
semi<-adir[adir$LCCode %in% c(8, 9, 10)] %>% aggregate(dissolve=TRUE)
deg<-adir[adir$LCCode %in% c(1,2,3,4,5,6,11,12,13)]  %>% aggregate(dissolve=TRUE)
lakes<-adir[adir$LCCode %in% c(15)]  %>% aggregate(dissolve=TRUE)
cat_rast<-vars[[1]] %>%
    mask(x=., mask=wilderness, inverse=TRUE, updatevalue=1) %>%
    mask(x=., mask=semi, inverse=TRUE, updatevalue=2) %>%
    mask(x=., mask=deg, inverse=TRUE, updatevalue=3) %>%
    mask(x=., mask=vars[[1]]) %>%
    tidyterra::rename("Ecosystem category" = "s")
cls <- data.frame(id=1:3, cover=c("Wilderness", "Semi-wilderness", "Resource management"))
levels(cat_rast) <- cls

# plot land category raster
datavispanel<-ggplot() +
    geom_spatraster(data = cat_rast)+
    scale_fill_manual(values=c("#3C5488FF", "#00A087FF", "#7E6148FF"), na.translate = FALSE)+
    #geom_spatvector(data=boundary, linetype=2, color="black", linewidth=1, fill=NA)+ 
    geom_spatvector(data=lakes, alpha=1, fill="black", color="black")+
    theme_minimal() +
    theme(
        legend.position = c(0.15, 0.10),
        legend.title=element_blank(),
        legend.key.size = unit(10, "pt"))
# plot locality on continent map
map<-rnaturalearth::ne_countries(returnclass = "sf") %>% vect
na<-map[map$admin %in% c("United States of America")]#,"Canada", "Mexico")]
localepanel<-ggplot() +
    geom_spatvector(data=na, color="dark grey", fill="white", linewidth=1)+ 
    geom_spatvector(data=boundary, color="black", fill="black", linewidth=1)+ 
    coord_sf(ylim = c(20, 50), xlim = c(-130, -70))+
    theme_void()

# combine plots and save
png(file.path("Figures", "Figure1.png"), height = 8, width = 8, units = 'in', res = 300)
cowplot::ggdraw() +
  cowplot::draw_plot(datavispanel) +
  cowplot::draw_plot(localepanel, x = 0.15, y = 0.78, width = 0.2, height =0.2)
dev.off()

############################
# Redwoods
############################
# load
vars<-rast('Outputs/RedwoodsEquationVariables.tif')
boundary<-vect('Outputs/RedwoodsBoundary')

#do
#the 
#rest
