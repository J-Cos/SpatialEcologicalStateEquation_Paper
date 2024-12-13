#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")


#6) make fig 2 - deviation

#protection level tests
#get dataframes
site_df<-values(vars, dataframe=TRUE) %>%
    as_tibble %>%
    filter(!is.na(B_predicted)) %>%
    filter(!is.na(states)) %>%
    filter(PA_cover>0.99 | PA_cover<0.01) %>%
    group_by(states, WDPAID, IUCN_CAT) %>%
    summarise(  s=mean(s),
                b=mean(b),
                n=mean(n),
                e=mean(e),
                dev=mean(dev),
                dev_mte=mean(dev_mte),
                dev_lm=mean(dev_lm),
                dev_rf=mean(dev_rf),
                mask=mean(mask, na.rm=TRUE)) %>%
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "V", NA)) %>%
        mutate( status = case_when(
            IUCN_CAT %in% c("Ia", "Ib", "II") ~ "High Protection",
            IUCN_CAT == "V" ~ "Moderate Protection",
            .default = "No Protection")
    )

#site averages panel
status_anova<-aov(dev~status, site_df) 
summary(status_anova)
TukeyHSD(status_anova)

fig2a<-site_df %>%
    ggplot(aes(y=dev, x=status))+
        geom_boxplot(outliers=FALSE)+
        geom_jitter(size=0.2, alpha=0.75, width = 0.3)+
        geom_signif(
            y_position = c(1.5, 1.75, 1.25), xmin = c(1,1,2), xmax = c(2, 3, 3),
            annotation = c("***", "*", "NS"), tip_length = 0
        ) +
        ylab("Deviation")+
        ggtitle("Average per site")+
        boxplotTheme


#mask panel
set.seed(2)
mask_df<-
    values( 
        spatSample(
            vars, 
            dim(vars)[1]*dim(vars)[2]*0.05, #1% of cells
            "regular", 
            na.rm=FALSE, 
            replace=FALSE,
            as.raster=TRUE), 
        dataframe=TRUE
    ) %>%
    as_tibble %>%
    filter(!is.na(B_predicted))  %>%
    filter(!is.na(mask))
    
mask_df %>%
    lm(dev~mask, .) %>%
    summary


fig2b<-mask_df %>%
    mutate(forest_sparsity=1-mask) %>%
    #select(forest_sparsity, dev, dev_mte, dev_lm, dev_rf) %>%
    #pivot_longer(-forest_sparsity, names_to="model", values_to="deviation") %>%
    ggplot(aes(x=forest_sparsity, y=dev))+
        geom_point(alpha=0.5, size=0.75 )+
        geom_smooth(method="lm", color="black")+
        geom_hline(yintercept=0, linetype=2)+
        ggtitle("forest sparsity (% non-forest per pixel) and deviation relationship (***)")+
        theme_classic()

#make combined figure
cowplot::plot_grid(fig2a, fig2b, labels = c('A', 'B'), label_size = 12, ncol = 2)
ggsave("Figures/Figure2.png", height=7.5, width=15)