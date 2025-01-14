#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")

#Figure S2
#get Good grade PAs - only include those larger than 100km2 for visual clarity
majorPAs<-pas[pas$IUCN_CAT %in% c("Ia", "Ib", "II") & pas$GIS_AREA>100] #%>%
    #aggregate(dissolve=TRUE)

# get polygon of tree absence
p<-as.polygons(!is.na(vars$mask))
treeAbsence<-p[p$mask==0]

majorPAs_wTrees<-erase(majorPAs,treeAbsence)

#make figure
dev_zscore<-(vars$dev-mean(values(vars$dev), na.rm=TRUE))/sd(values(vars$dev), na.rm=TRUE)
dev_zscore[dev_zscore>3]<-3
dev_zscore[dev_zscore<(-3)]<-(-3)

fig4a<- ggplot() +
    geom_spatraster(data=dev_zscore, aes(fill=dev),  na.rm = TRUE)+
    scale_fill_gradient2(na.value = "transparent", mid="light grey", name="Standardised\ndeviation\nfrom EEOS\nprediction")+
    geom_spatvector(data=usa,color="dark grey", fill=NA)+
    geom_spatvector(data=majorPAs_wTrees,color="black", fill=NA)+
    mapTheme

fig4a
ggsave("Figures/FigureS1.png", width=11, height=12, dpi=600)

#make figure
dev_mte_zscore<-(vars$dev_mte-mean(values(vars$dev_mte), na.rm=TRUE))/sd(values(vars$dev_mte), na.rm=TRUE)
dev_mte_zscore[dev_mte_zscore>3]<-3
dev_mte_zscore[dev_mte_zscore<(-3)]<-(-3)
figS2<- ggplot() +
    geom_spatraster(data=dev_mte_zscore, aes(fill=dev_mte),  na.rm = TRUE)+
    scale_fill_gradient2(na.value = "transparent", mid="light grey", name="Standardised\ndeviation\nfrom GPP\nprediction")+
    geom_spatvector(data=usa,color="dark grey", fill=NA)+
    geom_spatvector(data=majorPAs_wTrees,color="black", fill=NA)+
    mapTheme

figS2
ggsave("Figures/FigureS2.png", width=11, height=12, dpi=600)


# 7) make fig 3

#get patches 
patches<-patches(vars[["dev"]]>0.1, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
bigpatches<-which(table(values(patches))>100)
deviatingPatches<-patches %in% bigpatches

patches2<-patches(vars[["dev"]] < (-0.1), directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
bigpatches2<-which(table(values(patches2))>100)
deviatingPatches2<-patches2 %in% bigpatches2

deviatingPatches<-as.factor(sum(deviatingPatches,deviatingPatches2*-1))

cls <- data.frame(id=c(1,0, -1), cover=c("Good", "", "Poor"))
levels(deviatingPatches) <- cls

# get coverage stats
pp<-as.polygons(deviatingPatches)
coverage<-list( "H"=erase(majorPAs, pp[pp$cover!="Poor"]) ,
    "L" = erase(majorPAs, pp[pp$cover!="Good"]) )
lapply(coverage, length)
lapply(coverage, function(x){expanse(x, unit="km") %>% sum})

# label dataframe
annotation <- data.frame(
   x = c(-1.2,-0.85, -1.39, -0.98, -1.3, -1.03)*10^7,
   y = c(0.55,0.56, 0.62, 0.34, 0.538, 0.64)*10^7,
   label = c("Yellowstone", "Adirondacks", "Olympic", "Southeastern 'Wood-Basket'", "Oregon timber region", "Wisconsin-Minnesota\ntimber region")
)



# make figure
fig4b<-ggplot() +
    geom_spatraster(data=deviatingPatches,  na.rm = TRUE, alpha=0.75)+
    geom_spatvector(data=usa,color="dark grey", fill=NA)+
    geom_spatvector(data=majorPAs_wTrees,color="black", fill=NA)+
    scale_fill_manual(values=c(scales::muted("blue"),"transparent", scales::muted("red")), name="Predicted\necosystem\ncondition")+
    #scale_fill_gradient2(limits=c(-1,1), na.value = "transparent")+
    #ggtitle("patches of consistent deviation can be used to identify major areas of intact forest 
    #(e.g. adirondack, yellowstone and olympic national parks )
    #and major areas of more degradation
    #(e.g. unprotected forests in oregon, arkansas and georgia, three of the largest timber producing states)")+
    geom_label(data=annotation, aes( x=x, y=y, label=label),   size=2  ,     color="black", fontface="bold" )+
    mapTheme+
    theme(  
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank())


fig4b
ggsave("Figures/Figure4.png", width=15, height=10, dpi=600)
