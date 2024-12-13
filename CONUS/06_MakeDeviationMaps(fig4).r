#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")

#panel A
#get high grade PAs - only include those larger than 100km2 for visual clarity
majorPAs<-pas[pas$IUCN_CAT %in% c("Ia", "Ib", "II") & pas$GIS_AREA>100] %>%
    aggregate(dissolve=TRUE)
#make panel
fig4a<- ggplot() +
    geom_spatraster(data=vars, aes(fill=dev),  na.rm = TRUE)+
    scale_fill_gradient2(limits=c(-2.3,2.3), na.value = "transparent", mid="light grey", name="Log\ndeviation")+
    geom_spatvector(data=usa,color="dark grey", fill=NA)+
    geom_spatvector(data=majorPAs,color="black", fill=NA)+
    mapTheme

# 7) make fig 4
patches<-patches(vars[["dev"]]>0.1, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
bigpatches<-which(table(values(patches))>100)
deviatingPatches<-patches %in% bigpatches

patches2<-patches(vars[["dev"]] < (-0.1), directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
bigpatches2<-which(table(values(patches2))>100)
deviatingPatches2<-patches2 %in% bigpatches2

deviatingPatches<-as.factor(sum(deviatingPatches,deviatingPatches2*-1))

cls <- data.frame(id=c(1,0, -1), cover=c("High", "", "Low"))
levels(deviatingPatches) <- cls

fig4b<-ggplot() +
    geom_spatraster(data=deviatingPatches,  na.rm = TRUE, alpha=0.75)+
    geom_spatvector(data=usa,color="dark grey", fill=NA)+
    geom_spatvector(data=majorPAs,color="black", fill=NA)+
    scale_fill_manual(values=c(scales::muted("blue"),"transparent", scales::muted("red")), name="Predicted\nforest\ncondition")+
    #scale_fill_gradient2(limits=c(-1,1), na.value = "transparent")+
    #ggtitle("patches of consistent deviation can be used to identify major areas of intact forest 
    #(e.g. adirondack, yellowstone and olympic national parks )
    #and major areas of more degradation
    #(e.g. unprotected forests in oregon, arkansas and georgia, three of the largest timber producing states)")+
    mapTheme

cowplot::plot_grid(
  fig4a, fig4b,
  labels = c('A', 'B'),
  align="hv",
  ncol=1
)

ggsave("Figures/Figure4.png", width=11, height=12, dpi=600)

