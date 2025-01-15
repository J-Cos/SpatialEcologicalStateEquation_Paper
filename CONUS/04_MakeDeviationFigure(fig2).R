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
        geom_jitter(alpha=0.75, width = 0.3)+
        geom_signif(
            y_position = c(1.5, 1.75, 1.25), xmin = c(1,1,2), xmax = c(2, 3, 3),
            annotation = c("***", "*", "NS"), tip_length = 0
        ) +
        ylab("Deviation\nlog(observed) - log(predicted)")+
        #ggtitle("Average per site")+
        boxplotTheme


#make combined figure
fig2a
ggsave("Figures/Figure2.png")
