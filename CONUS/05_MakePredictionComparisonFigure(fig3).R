#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables.tif")



# for EEOS
fit_df<-values(vars, dataframe=TRUE) %>%
    as_tibble %>%
    filter(!is.na(B_predicted))


long_df<-values(vars, dataframe=TRUE) %>%
    as_tibble %>%
    filter(!is.na(B_predicted)) %>%
    select(b, B_predicted, e, B_lm, B_rf) %>%
    pivot_longer(-b)



#for EEOS
lm(log(b)~log(B_predicted), fit_df) %>% summary
# for MTE
lm(log(b)~log(e), fit_df) %>% summary
# for RF
lm(log(b)~log(B_rf), fit_df) %>% summary
# for lm
lm(log(b)~log(B_lm), fit_df) %>% summary

#test r2
rss<-sum(  (log(fit_df$b) - log(fit_df$B_predicted))^2, na.rm=TRUE)
tss<-sum((log(fit_df$b)-mean(log(fit_df$b)))^2)
1-(rss/tss)



fig3a<-long_df %>%
    ggplot(aes(x=log(b), y=log(value)))+
        geom_hex() +
        scale_fill_viridis()+
        geom_smooth(method="lm", color="black")+
        geom_abline(linetype=2)+
        xlim(6.5, 17)+ylim(6.5, 17)+
        facet_wrap(~name)+
        theme_classic()
#ggsave("Figures/Figure2.png")

#unused code to inspect fit by protection status
fit_df %>%
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "V", NA)) %>%
        mutate( status = case_when(
            IUCN_CAT %in% c("Ia", "Ib", "II") ~ "High Protection",
            IUCN_CAT == "V" ~ "Moderate Protection",
            .default = "No Protection")
    ) %>%
    ggplot(aes(x=log(b), y=log(B_predicted)))+
        geom_hex() +
        scale_fill_viridis()+
        geom_smooth(method="lm", color="black")+
        geom_abline(linetype=2)+
        xlim(7, 17)+ylim(7, 17)+
        facet_wrap(~status)



# mask model difference and sample size plot
#regular samples of increasing size
set.seed(2)
sample_l<-list()
df<-data.frame(sampleSize=NA, "t"=NA, "pval"=NA, "t_lm"=NA, "t_rf"=NA, "t_mte"=NA)
n<-1
numcells<-dim(vars)[1]*dim(vars)[2]
for (i in  seq(from=numcells*0.0001, to=numcells*0.1, by=numcells*0.0005)) {
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
    ggplot(aes(y=t, x=sampleSize, color=model))+
        geom_point(alpha=0.5)+
        geom_smooth()+
        geom_vline(xintercept=numcells*0.01, linetype=2)+
        xlim(0, NA)+ylim(0, NA)+
        theme_classic()


sample_l[[50]] %>%
    ggplot(aes(x=mask, y=dev))+
        geom_point()+
        geom_smooth(method="lm")


# get difference in models panel

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



fig3c<-rbind(
        "EEOS"=TukeyHSD(aov(dev~status, site_df))$status[,"diff"],
        "MTE" = TukeyHSD(aov(dev_mte~status, site_df))$status[,"diff"],
        "MLM" = TukeyHSD(aov(dev_rf~status, site_df))$status[,"diff"],
        "LM" = TukeyHSD(aov(dev_lm~status, site_df))$status[,"diff"]
    ) %>%
        as_data_frame()%>%
        cbind("model"=c("EEOS", "MTE", "MLM", "LM"))%>%
        as_tibble %>%
        pivot_longer(-model, names_to="comparison", values_to="difference") %>%
        ggplot()+
            geom_line(aes(x=comparison, y=difference, color=model, group=model))+
            theme_classic()


model.sel(
m1<-lm(dev~mask+status, site_df),
m2<-lm(dev~mask*status, site_df))

summary(m1)

#make combined figure
bottom_row <- cowplot::plot_grid(fig3b, fig3c, labels = c('B', 'C'), label_size = 12, ncol=1)
cowplot::plot_grid(fig3a, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)
ggsave("Figures/Figure3.png", height=15, width=10)

