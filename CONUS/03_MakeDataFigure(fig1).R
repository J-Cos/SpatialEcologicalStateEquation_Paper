#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")

# 4) make fig1 - Data

fig1panel_l<-list()
for (layer in names(vars[[1:6]])){

    fig1panel_l[[layer]]<-ggplot() +
        geom_spatraster(data=vars[[layer]], na.rm = TRUE)+
        geom_spatvector(data=usa,color="grey", fill=NA)+
        viridis::scale_fill_viridis( na.value = "transparent")+
        ggtitle(layer)+
        mapTheme
}

cowplot::plot_grid(
  plotlist=fig1panel_l,
  labels = c('A', 'B', 'C', 'D', 'E', 'F'),
  align="hv",
  ncol=2
)

ggsave("Figures/Figure1.png", height=10, width=12.5)
