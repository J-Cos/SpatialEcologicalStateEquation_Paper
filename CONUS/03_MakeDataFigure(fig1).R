#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")
options(scipen=-1)
# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")

# 4) make fig1 - Data

plot_subtitles<-c("Richness", "Biomass (dry Kg)", "Abundance", "Productivity (10 MgC)", "Forest cover (%)", "Predicted biomass (unitless)")
fig1panel_l<-list()
for (layer in names(vars[[1:6]])){

    fig1panel_l[[layer]]<-ggplot() +
        geom_spatraster(data=vars[[layer]], na.rm = TRUE)+
        geom_spatvector(data=usa,color="grey", fill=NA)+
        viridis::scale_fill_viridis(na.value = "transparent", name="")+
        ggtitle(plot_subtitles[which(names(vars[[1:6]])==layer)])+
        theme(legend.title=element_blank())+
        mapTheme
}

cowplot::plot_grid(
  plotlist=fig1panel_l,
  labels = c('A', 'B', 'C', 'D', 'E', 'F'),
  align="hv",
  ncol=2
)

ggsave("Figures/Figure1.png", height=10, width=12.5)
