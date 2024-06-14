library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridis)
library(ranger)

region<-"Adirondacks"

# load
vars<-rast(paste0("Outputs/AllPredictedVariables_", region, ".tif"))
adir<-vect('Outputs/LandClasses')
boundary<-vect('Outputs/AdirondacksBoundary')
pop<-rast("Outputs/AdirondacksPopulation.tif")
palette<-c("Wilderness"="blue", "Semi-wilderness"="grey", "Resource Management"="red")

#calculate scaled biomasses
scale<-function(lyr) {
    ( lyr-mean(values(lyr), na.rm=TRUE) ) / sd(values(lyr), na.rm=TRUE)
}
rsq <- function (x, y) cor(x, y) ^ 2

vars[['b_scaled']]<-scale(vars[['b']])
vars[['B_predicted_scaled']]<-scale(vars[['B_predicted']])

#calculate deviation
vars[['dev']]<-vars[['B_predicted_scaled']]-vars[['b_scaled']]

#get polygons on land uses to produce inofrmative map
wilderness<-adir[adir$LCCode %in% c(7)] %>% aggregate(dissolve=TRUE)
semi<-adir[adir$LCCode %in% c(8, 9, 10)] %>% aggregate(dissolve=TRUE)
deg<-adir[adir$LCCode %in% c(1,2,3,4,5,6,11,12,13)]  %>% aggregate(dissolve=TRUE)
lakes<-adir[adir$LCCode %in% c(15)]  %>% aggregate(dissolve=TRUE)
cats<-vect(c(wilderness, semi, deg, lakes))
values(cats)<-data.frame(names=c("w", "s", "d", "l"))
land_cats<-cats[cats$names!="l"]

# create dataframe of pixels categorised by land category
cat_vals<-terra::extract(vars, land_cats, weights=TRUE) %>%
    filter(weight>0.99) %>%
    filter(!is.na(s)) %>%
    mutate(ID=as.factor(ID)) %>%
    mutate(ID = fct_recode(ID, "Wilderness"="1", "Semi-wilderness"="2", "Resource Management"="3"))

# check average biomass across land use categories
#biomass
cat_vals %>%
    group_by(ID) %>%
    summarise(
        mean(b), sd(b)
    )
lm(b~ID, data=cat_vals) %>% summary
lm(b~ID, data=cat_vals) %>% confint
#predicted biomass
cat_vals %>%
    group_by(ID) %>%
    summarise(
        mean(B_predicted), sd(B_predicted)
    )
lm(B_predicted~ID, data=cat_vals) %>% summary
lm(B_predicted~ID, data=cat_vals) %>% confint
#abundance
cat_vals %>%
    group_by(ID) %>%
    summarise(
        mean(n), sd(n)
    )
lm(n~ID, data=cat_vals) %>% summary
lm(n~ID, data=cat_vals) %>% confint

#EVI
cat_vals %>%
    group_by(ID) %>%
    summarise(
        mean(EVI), sd(EVI)
    )
lm(EVI~ID, data=cat_vals) %>% summary
lm(EVI~ID, data=cat_vals) %>% confint

######################
#Make panel A
######################
PanelA<-ggplot(cat_vals)+
    geom_vline(xintercept=0, linetype=2)+
    geom_boxplot(aes(x=dev, y=ID, color=ID))+
    scale_color_manual(values=palette, name = "Land Category")+
    theme_classic()+
    theme( axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    labs(x ="Deviation", y = "")

lm(dev~1+ID, data=cat_vals) %>% summary
lm(dev~1+ID, data=cat_vals) %>% confint

######################
#Make panel B
######################
rsqs <- cat_vals %>%
    group_by(ID) %>%
    summarise(rsq=rsq(b_scaled, B_predicted_scaled)) %>%
    mutate(rsq_plot=paste0( "R2=",round(rsq, 2) )) %>%
    mutate(model="EoS")

PanelB<-cat_vals %>%
    ggplot(data=., aes(x=b_scaled, y=B_predicted_scaled, shape=ID)) +
        xlim(-5,5) + ylim(-5,5)+
        geom_hex() +
        geom_smooth(method="lm", aes(color=ID), size=2, linetype=2, show.legend=FALSE)+
        scale_color_manual(values=palette)+
        geom_label(data=rsqs, aes(label=rsq_plot), x=3.5, y=-3.5) +
        facet_wrap(~ID)+
        scale_fill_viridis()+
        theme_classic()+
        labs(x="Observed biomass (scaled)", y="Predicted biomass (scaled)")

######################
#Make panel C
######################
getSpatialCorrelations<-function(landCoverCodes) {
    raster_cat<-biomasses %>% crop(adir[adir$LCCode %in% landCoverCodes ], mask=TRUE)

    fits<-list()
    for (i in seq(from=3, to=33, by=10)) {
        fits[[as.character(i)]]<-focalPairs(raster_cat, w=i, "pearson", na.rm=TRUE, filename=file.path(tempdir(), paste0("corRast_", runif(1), ".tif")))
        print(paste0(i, " complete"))
    }

    vals_l<-lapply(fits, FUN=values, na.rm=TRUE)

    output_modEsts<-lapply(
        vals_l, 
        FUN=mean#function(item) {summary(lm(item~1))$coefficients[,c("t value", "Estimate", "Pr(>|t|)")]}
    ) %>%
        unlist #bind_rows %>% 
        #mutate(w=names(vals_l))
    output_modEsts<-cbind(output_modEsts, w=names(vals_l)) %>%
        as_tibble

    output<-list("modEsts"=output_modEsts, "cor_rasts"=fits)
    return(output)
}
getTotalCor<-function(landCoverCodes) {
    corMat<-biomasses %>% 
        crop(adir[adir$LCCode %in% landCoverCodes ], mask=TRUE) %>%
        values %>% 
        as_tibble %>%
        filter(complete.cases(.)) %>%
        cor
    return(corMat[1,2])
}

#get biomass
biomasses<-c(vars[['B_predicted_scaled']], vars[['b_scaled']])

#total cors
wTotal<-getTotalCor(c(7))
sTotal<-getTotalCor(c(8, 9, 10))
rTotal<-getTotalCor(c(1,2,3,4,5,6,11,12,13))

#spatial cors
wCors<-getSpatialCorrelations(c(7))
sCors<-getSpatialCorrelations(c(8, 9, 10))
rCors<-getSpatialCorrelations(c(1,2,3,4,5,6,11,12,13))

cor_df<-rbind(
    cbind(wCors[[1]], "cat"=rep("w", 4)),
    cbind(sCors[[1]], "cat"=rep("s", 4)),
    cbind(rCors[[1]], "cat"=rep("r", 4))) %>%
        as_tibble()

PanelC<-cor_df %>%
    mutate(w=as.numeric(w)^2) %>%
    mutate(output_modEsts=as.numeric(output_modEsts)) %>%
    mutate(cat=as.factor(cat)) %>%
    mutate("Land Category" = fct_recode(cat, "Wilderness"="w", "Semi-wilderness"="s", "Resource Management"="r")) %>%
    ggplot(data=.) +
        geom_hline(yintercept=wTotal, color="blue") +
        geom_hline(yintercept=sTotal, color="grey") +
        geom_hline(yintercept=rTotal, color="red") +
        geom_line(aes(y=output_modEsts, x=w, color=`Land Category`, group=cat),size=1.5, show.legend=FALSE)+
        scale_color_manual(values=palette)+
        ylim(0,0.4)+
        theme_classic()+
        labs(x="Local window size (# pixels)", y="Correlation")


#####################
# Mkae panel D
# requires category-wise MLMs and LMs
########################
getTestTrainList<-function(split, data) {
    nsamples<-dim(data)[1]
    testRows<-sample(1:nsamples, nsamples*split)
    test<-data[testRows,]
    train<-data[-testRows,]
    return(list("test"=test, "train"=train))
}

# create test train split
TestSamplePercent<-0.5
set.seed(1)
w_split<-cat_vals %>%
    filter(ID=="Wilderness") %>%
    getTestTrainList(split=TestSamplePercent, data=.)
s_split<-cat_vals %>%
    filter(ID=="Semi-wilderness") %>%
    getTestTrainList(split=TestSamplePercent, data=.)
r_split<-cat_vals %>%
    filter(ID=="Resource Management") %>%
    getTestTrainList(split=TestSamplePercent, data=.)

# fit random forests
set.seed(1)
wrf<-ranger::ranger(b~n+s+EVI,
    data = w_split[["train"]], 
    importance = 'permutation',
    scale.permutation.importance = TRUE,
    mtry = 2,
    num.trees=1001)
p_wrf<-predict(wrf, w_split[["test"]])
rsqs<-rbind(rsqs, c("Wilderness", rsq(p_wrf$predictions, w_split[["test"]]$b), NA, "RF"))
vip::vip(wrf)

set.seed(1)
srf<-ranger::ranger(b~n+s+EVI,
    data = s_split[["train"]], 
    importance = 'permutation',
    scale.permutation.importance = TRUE,
    mtry = 2,
    num.trees=1001)
p_srf<-predict(srf, s_split[["test"]])
rsqs<-rbind(rsqs, c("Semi-wilderness", rsq(p_srf$predictions, s_split[["test"]]$b), NA, "RF"))
vip::vip(srf)

set.seed(1)
rrf<-ranger::ranger(b~n+s+EVI,
    data = r_split[["train"]], 
    importance = 'permutation',
    scale.permutation.importance = TRUE,
    mtry = 2,
    num.trees=1001)
p_rrf<-predict(rrf, r_split[["test"]])
rsqs<-rbind(rsqs, c("Resource Management", rsq(p_rrf$predictions, r_split[["test"]]$b), NA, "RF"))
vip::vip(rrf)

# fit linear models
wlm<-lm(b~n*s*EVI,
    data = w_split[["train"]])
p_wlm<-predict(wlm, w_split[["test"]])
rsqs<-rbind(rsqs, c("Wilderness", rsq(p_wlm, w_split[["test"]]$b), NA, "LM"))

slm<-lm(b~n*s*EVI,
    data = s_split[["train"]])
p_slm<-predict(slm, s_split[["test"]])
rsqs<-rbind(rsqs, c("Semi-wilderness", rsq(p_slm, s_split[["test"]]$b), NA, "LM"))

rlm<-lm(b~n*s*EVI,
    data = r_split[["train"]])
p_rlm<-predict(rlm, r_split[["test"]])
rsqs<-rbind(rsqs, c("Resource Management", rsq(p_rlm, r_split[["test"]]$b), NA, "LM"))


PanelD<-rsqs %>%
    mutate(rsq=as.numeric(rsq)) %>%
    group_by(model) %>%
    mutate(varProp=rsq/max(rsq)) %>%
    ggplot(data=.) +
        geom_line(aes(y=varProp, x=ID, linetype=model, group=model))+
        theme_classic()+
        ylim(0,1)+
        labs(x="", y="Proportion of maximum\nvariance explained for\nany land category")+ 
        guides(linetype=guide_legend(title="Prediction process"))

######################
#Save figure
######################
png(file.path("Figures", paste0("Figure3.png")), height = 10, width = 10, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(PanelB, x=0.035, y=0.75, width=0.93, height=0.25)+
    cowplot::draw_plot(PanelA, x=0.06, y=0.5, width=0.95, height=0.25)+
    cowplot::draw_plot(PanelC, x=0.04, y=0.25, width=0.77, height=0.25)+
    cowplot::draw_plot(PanelD, x=0, y=0, width=0.98, height=0.25)+
    cowplot::draw_plot_label(   label = c("A", "B", "C", "D"), 
                        size = 15, 
                        x = c(0, 0, 0, 0), 
                        y = c(1, 0.75, 0.5, 0.25))
dev.off()
