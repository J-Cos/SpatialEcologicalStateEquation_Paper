# can you reverse engineer disturbed areas from the results?
library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridis)

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

getProportionPatchesOutsideCategory <- function(patches, LCcategory) {1- sum(terra::extract(patches,LCcategory)$patches) / sum(values(patches))}

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

groups<-as.factor(sum(c( mt1, mt2, lt1*-1, lt2*-1)))
groups[groups==0]<-NA
levels(groups) <- data.frame("ID"=levels(groups)[[1]]$ID, "sum"=c("<-2", "<-1", "NA", ">1", ">2"))

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
getProportionPatchesOutsideCategory(mt1, deg)
getProportionPatchesOutsideCategory(mt2, deg)
getProportionPatchesOutsideCategory(lt1, wilderness)
getProportionPatchesOutsideCategory(lt2, wilderness)
getProportionPatchesOutsideCategory(near0, deg)



#make plot
p<-ggplot()+
    geom_spatvector(data=wilderness,fill="blue", alpha=0.1)+
    geom_spatvector(data=semi, fill="grey" , alpha=0.1)+
    geom_spatvector(data=deg, alpha=0.1, fill="red")+
    geom_spatvector(data=boundary, linewidth=2, linetype=2, color="black", fill=NA)+
    geom_spatraster(data=groups, aes(fill=sum))+
    scale_fill_manual(name="Deviation from\nEquation of State\nPrediction (SDs)" , values=c("red", "orange",  "cyan", "blue"), na.translate = FALSE)+
    geom_spatvector(data=lakes, alpha=1, fill="black")+
    theme_minimal()

p2<-ggplot()+
    geom_spatvector(data=wilderness,fill="blue", alpha=0.1)+
    geom_spatvector(data=semi, fill="grey" , alpha=0.1)+
    geom_spatvector(data=deg, alpha=0.1, fill="red")+
    geom_spatvector(data=boundary, linewidth=2, linetype=2, color="black", fill=NA)+
    geom_spatraster(data=near0, aes(fill=patches))+
    scale_fill_manual(name="Deviation from\nEquation of State\nPrediction (SDs)" , values=c("transparent", "green"), labels = c("", "Within 1 SD"))+
    geom_spatvector(data=lakes, alpha=1, fill="black")+
    theme_minimal()
    
#save plot
png(file.path("Figures", paste0("Figure4.png")), height = 10, width = 10, units = 'in', res = 300)
p
dev.off()

#alternative
png(file.path("Figures", paste0("Figure4alt.png")), height = 10, width = 20, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(p, x=0, y=0, width=0.5, height=1)+
    cowplot::draw_plot(p2, x=0.5, y=0, width=0.5, height=1)+
    cowplot::draw_plot_label(   label = c("A", "B"), 
                        size = 15, 
                        x = c(0, 0.5), 
                        y = c(1, 1))
dev.off()