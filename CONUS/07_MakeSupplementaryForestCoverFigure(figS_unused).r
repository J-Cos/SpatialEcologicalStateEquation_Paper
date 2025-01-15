#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")


# mask model difference and sample size plot
#regular samples of increasing size
set.seed(2)
sample_l<-list()
df<-data.frame(sampleSize=NA, "t"=NA, "pval"=NA, "t_lm"=NA, "t_rf"=NA, "t_mte"=NA)
n<-1
numcells<-dim(vars)[1]*dim(vars)[2]
for (i in  seq(from=numcells*0.0005, to=numcells*0.1, by=numcells*0.0005)) {
    sample_l[[n]]<-spatSample(vars, i, "regular", na.rm=FALSE, replace=FALSE) 
    df[n,]<-c(
        i,
        summary(lm(dev~mask, sample_l[[n]]))$coef["mask", "t value"],
        summary(lm(dev~mask, sample_l[[n]]))$coef["mask", "Pr(>|t|)"],
        summary(lm(dev_lm~mask, sample_l[[n]]))$coef["mask", "t value"],
        summary(lm(dev_rf~mask, sample_l[[n]]))$coef["mask", "t value"],
        summary(lm(dev_mte~mask, sample_l[[n]]))$coef["mask", "t value"]

    )
    print(n)
    n<-n+1
}

fig3b<-df %>%
    #mutate(Significant=pval<0.05) %>%
    select(-pval)%>%
    pivot_longer(-sampleSize, names_to="model", values_to="t") %>%
    cbind("Model"=c("EEOS", "LM", "MLM", "Productivity"))%>%
    ggplot(aes(y=t, x=sampleSize, color=Model))+
        scale_color_manual(values=palette_models1)+
        geom_point(alpha=0.5)+
        geom_smooth()+
        geom_vline(xintercept=numcells*0.01, linetype=2)+
        geom_vline(xintercept=numcells*0.025, linetype=2)+
        geom_vline(xintercept=numcells*0.05, linetype=2)+
        geom_vline(xintercept=numcells*0.1, linetype=2)+
        xlim(0, NA)+ylim(0, NA)+
        theme_classic()+
        xlab("Number of pixels sampled")
        

###########################

 sample_l[[20]] %>%
    lm(dev~mask, .) %>%
    confint
    summary



GetForestCoverExtractedPlot<-function(sample, col, model="EEOS"){
    if (model=="EEOS") {
        sample_l[[sample]] %>%
            ggplot(aes(x=mask, y=dev))+
                geom_point(alpha=0.5, size=0.75, color=col )+
                geom_smooth(method="lm", color=col)+
                geom_hline(yintercept=0, linetype=2)+
                xlab("Forest cover (%)") + ylab("Deviation")+
                theme_classic()+
                ylim(-2, 2)
    } else if (model=="RF") {
        sample_l[[sample]] %>%
            ggplot(aes(x=mask, y=dev_rf))+
                geom_point(alpha=0.5, size=0.75, color=col )+
                geom_smooth(method="lm", color=col)+
                geom_hline(yintercept=0, linetype=2)+
                xlab("Forest cover (%)") + ylab("Deviation")+
                theme_classic()+
                ylim(-2, 2)
    }
}

EEOS_l<- list()
EEOS_l[[1]]<-GetForestCoverExtractedPlot(20,  "#154360") +ggtitle("1% of pixels (example given in text)")
EEOS_l[[2]]<-GetForestCoverExtractedPlot(50, "#154360") +ggtitle("2.5% of pixels ")
EEOS_l[[3]]<-GetForestCoverExtractedPlot(100, "#154360") +ggtitle("5% of pixels ")
EEOS_l[[4]]<-GetForestCoverExtractedPlot(200, "#154360") +ggtitle("10% of pixels ")

RF_l<-list()
RF_l[[1]]<-GetForestCoverExtractedPlot(20, "#FF5733", "RF") +ggtitle("1% of pixels ")
RF_l[[2]]<-GetForestCoverExtractedPlot(50, "#FF5733", "RF") +ggtitle("2.5% of pixels ")
RF_l[[3]]<-GetForestCoverExtractedPlot(100, "#FF5733", "RF") +ggtitle("5% of pixels ")
RF_l[[4]]<-GetForestCoverExtractedPlot(200, "#FF5733", "RF") +ggtitle("10% of pixels ")


eeos_extracts<-cowplot::plot_grid(plotlist=EEOS_l, labels = c('A', 'B', "C", "D"), label_size = 12, ncol=4)
rf_extracts<-cowplot::plot_grid(plotlist=RF_l, labels = c('F', 'G', "H", "I"), label_size = 12, ncol=4)

cowplot::plot_grid(eeos_extracts, fig3b, rf_extracts, labels = c('', 'E', ""), label_size = 12, ncol=1)

ggsave("Figures/FigureS_unused.png", height=15, width=15)