# can you reverse engineer disturbed areas from the results?
library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridis)
library(ggnewscale)

#functions
scale<-function(lyr) {
    ( lyr-mean(values(lyr), na.rm=TRUE) ) / sd(values(lyr), na.rm=TRUE)
}

getPatchRaster<-function(dev=dev, sd, sdlow, patchSize, moreThan=FALSE, lessThan=FALSE, between=FALSE) {

    if (moreThan) {patches<-patches(dev>sd, directions=4, zeroAsNA=TRUE, allowGaps=FALSE) }
    else if (lessThan) {patches<-patches(dev<sd, directions=4, zeroAsNA=TRUE, allowGaps=FALSE) }
    else if (between) {patches<-patches((dev<sd & dev>sdlow), directions=4, zeroAsNA=TRUE, allowGaps=FALSE) }
    else (stop("do you want more than or less than?"))

    bigpatches<-which(table(values(patches))>patchSize)
    deviatingPatches<-patches %in% bigpatches
    return(deviatingPatches)
}

getProportionPatchesInsideCategory <- function(patches, LCcategory) {sum(terra::extract(patches,LCcategory)$patches) / sum(values(patches))}

# load
vars<-rast('Outputs/AdirondacksEquationVariables.tif') # skipping rf and lm for now
adir<-vect('Outputs/LandClasses')
boundary<-vect('Outputs/AdirondacksBoundary')
pop<-rast("Outputs/AdirondacksPopulation.tif")

#calculate scaled biomasses s and deviation
vars[['b_scaled']]<-scale(vars[['b']])
vars[['B_predicted_scaled']]<-scale(vars[['B_predicted']])
dev<-vars[['b_scaled']]-vars[['B_predicted_scaled']]


#get rasters showing patches deviating by more than certain standard deviation
mt1<-getPatchRaster(dev=dev, sd=1, patchSize=50, moreThan=TRUE)
mt2<-getPatchRaster(dev=dev, sd=2, patchSize=50, moreThan=TRUE)
lt1<-getPatchRaster(dev=dev, sd=-1, patchSize=50, lessThan=TRUE)
lt2<-getPatchRaster(dev=dev, sd=-2, patchSize=50, lessThan=TRUE)
near0<-getPatchRaster(dev=dev, sd=1, sdlow=-1, patchSize=50, between=TRUE)

groups<-sum(c( mt1, mt2, lt1*-1, lt2*-1))
groups[groups==0]<-NA
groups[near0]<-0
groups<-as.factor(groups)

levels(groups) <- data.frame("ID"=levels(groups)[[1]]$ID, "sum"=c("<-2", "<-1", "within1", ">1", ">2"))

#get polygons on land uses to produce inofrmative map
wilderness<-adir[adir$LCCode %in% c(7)] %>% aggregate(dissolve=TRUE)
semi<-adir[adir$LCCode %in% c(8, 9, 10)] %>% aggregate(dissolve=TRUE)
deg<-adir[adir$LCCode %in% c(1,2,3,4,5,6,11,12,13)]  %>% aggregate(dissolve=TRUE)
lakes<-adir[adir$LCCode %in% c(15)]  %>% aggregate(dissolve=TRUE)

#get number of patches in total
patches(mt1, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
patches(mt2, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
patches(lt1, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
patches(lt2, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)
patches(near0, directions=4, zeroAsNA=TRUE, allowGaps=FALSE)

#get proportion of patches per category
cats=list("deg"=deg, "semi"=semi, "wilderness"=wilderness)
SDs<-list(lt2, lt1, near0, mt1, mt2)
df<-data.frame("sd"=c("lt2", "lt1", "near0", "mt1", "mt2"), "deg"=NA, "semi"=NA, "wilderness"=NA)
for (cat in names(cats)) {
    for (sd in 1:length(SDs)){
        df[sd, cat]<-getProportionPatchesInsideCategory(SDs[[sd]], cats[[cat]])
        print(paste0(cat, " ", sd, " complete"))
    }
}

#make plot
p<-ggplot()+
    geom_spatvector(data=lakes, alpha=1, fill="white", color="white")+
    geom_spatvector(data=wilderness,fill="black", color=NA, alpha=0.6)+
    geom_spatvector(data=semi, fill="black" , color=NA,  alpha=0.3)+
    geom_spatvector(data=deg, alpha=0.1,color=NA,  fill="black")+
    #geom_spatraster(data=is.na(dev), aes(fill=b_scaled))+
    scale_fill_manual(labels=c("TRUE", "FALSE"), values=c("black", "transparent"))+
    new_scale_fill() +
    geom_spatraster(data=groups, aes(fill=sum))+
    scale_fill_manual(name="Biomass deviation\nfrom prediction\n(z-score)" , labels=c(" > 2 less", "> 1 less", "Within 1", "> 1 greater", "> 2 greater"), values=c("red", "#FA7F2E", "#42B540FF", "#5BA2CC", "blue"), na.translate = FALSE)+
    #new_scale_fill() +
    #geom_spatraster(data=near0, aes(fill=patches))+
    #scale_fill_manual(name=element_blank(), values=c("transparent", "#42B540FF"), labels = c("", "Within 1 SD") )+
    theme_minimal()+
    theme(legend.position = c(0.1, 0.9))

png(file.path("Figures", paste0("Figure4.png")), height = 10, width = 10, units = 'in', res = 300)
p
dev.off()



###########################
# make supplementary deviation map - truncated at deviations greater or less than 2 sds
################################
dev_trunc<-dev
dev_trunc[dev_trunc>2]<-2
dev_trunc[dev_trunc< -2]<- -2


pa<-ggplot()+
    geom_spatvector(data=lakes, alpha=1, fill="white", color="white")+
    geom_spatvector(data=wilderness,fill="black", color=NA, alpha=0.5)+
    geom_spatraster(data=mask(dev_trunc, wilderness), aes(fill=b_scaled))+
    scale_fill_viridis(option="turbo", na.value = "transparent", direction=-1)+
    geom_spatvector(data=boundary, color="black", fill=NA)+
    theme_minimal()+
    theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=16))+
    ggtitle("Wilderness")
pb<-ggplot()+
    geom_spatvector(data=lakes, alpha=1, fill="white", color="white")+
    geom_spatvector(data=semi,fill="black", color=NA, alpha=0.5)+
    geom_spatraster(data=mask(dev_trunc, semi), aes(fill=b_scaled))+
    scale_fill_viridis(name="Biomass deviation\nfrom prediction\n(z-score)", option="turbo", na.value = "transparent", direction=-1)+
    geom_spatvector(data=boundary, color="black", fill=NA)+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size=16), legend.title.align=0.5)+
    ggtitle("Semi-wilderness")
pc<-ggplot()+
    geom_spatvector(data=lakes, alpha=1, fill="white", color="white")+
    geom_spatvector(data=deg,fill="black", color=NA, alpha=0.5)+
    geom_spatraster(data=mask(dev_trunc, deg), aes(fill=b_scaled))+
    scale_fill_viridis(option="turbo", na.value = "transparent", direction=-1)+
    geom_spatvector(data=boundary, color="black", fill=NA)+
    theme_minimal()+
    theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=16))+
    ggtitle("Resource management")
pd<-ggplot()+
    geom_spatvector(data=boundary, alpha=0.5, color="black", fill="black")+
    geom_spatvector(data=lakes, fill="white", color="white")+
    geom_spatraster(data=dev_trunc, aes(fill=b_scaled))+
    scale_fill_viridis(option="turbo", na.value = "transparent", direction=-1)+
    theme_minimal()+
    theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=16))+
    ggtitle("Adirondack Park")

png(file.path("Figures", paste0("FigureS1.png")), height = 10, width = 10, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(pd, y=0.5, x=0, height=0.5, width=0.5)+
    cowplot::draw_plot(pa, y=0.5, x=0.5, height=0.5, width=0.5)+
    cowplot::draw_plot(pb+ theme(legend.position="none"), y=0, x=0, height=0.5, width=0.5)+
    cowplot::draw_plot(pc, y=0, x=0.5, height=0.5, width=0.5)+
    cowplot::draw_plot(cowplot::get_legend(pb), y=0.4, x=0, height=1, width=1)
dev.off()