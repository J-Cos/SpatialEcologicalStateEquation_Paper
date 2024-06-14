library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)

############################
# Adirondacks
############################
# load
vars<-rast('Outputs/AdirondacksEquationVariables.tif')
adir<-vect('Outputs/LandClasses')
boundary<-vect('Outputs/AdirondacksBoundary')

# create a raster of land categories
wild<-adir[adir$LCCode==7 ] %>% aggregate()
resman<-adir[adir$LCCode==5 ] %>% aggregate()
other<-adir[adir$LCCode!=7 & adir$LCCode!=5 ]
cat_rast<-vars[[1]] %>%
    mask(x=., mask=wild, inverse=TRUE, updatevalue=1) %>%
    mask(x=., mask=resman, inverse=TRUE, updatevalue=2) %>%
    mask(x=., mask=other, inverse=TRUE, updatevalue=3) %>%
    mask(x=., mask=vars[[1]]) %>%
    tidyterra::rename("Ecosystem category" = "s")
cls <- data.frame(id=1:3, cover=c("Wilderness", "Resource management", "Other forest"))
levels(cat_rast) <- cls

# plot land category raster
datavispanel<-ggplot() +
    geom_spatraster(data = cat_rast)+
    scale_fill_manual(values=c("green", "red", "gray"), na.translate = FALSE)+
    geom_spatvector(data=boundary, linetype=2, color="black", linewidth=1, fill=NA)+ 
    theme_classic()+
    theme( legend.title=element_blank())

# plot locality on continent map
map<-rnaturalearth::ne_countries(returnclass = "sf") %>% vect
na<-map[map$admin %in% c("United States of America","Canada", "Mexico")]
localepanel<-ggplot() +
    geom_spatvector(data=na, color="dark grey", linewidth=1)+ 
    geom_spatvector(data=boundary, color="black", fill="black", linewidth=1)+ 
    coord_sf(ylim = c(20, 50), xlim = c(-130, -70))+
    theme_void()

# combine plots and save
png(file.path("Figures", "Figure1.png"), height = 8.3, width = 11.7, units = 'in', res = 300)
cowplot::ggdraw() +
  cowplot::draw_plot(datavispanel) +
  cowplot::draw_plot(localepanel, x = 0.15, y = 0.8, width = 0.2, height =0.2)
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
