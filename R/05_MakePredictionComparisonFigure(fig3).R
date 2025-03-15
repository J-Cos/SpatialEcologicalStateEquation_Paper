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
rss<-sum(  (log(fit_df$b) - log(fit_df$B_rf))^2, na.rm=TRUE)
tss<-sum((log(fit_df$b)-mean(log(fit_df$b)))^2)
1-(rss/tss)

model <- c(
  B_lm = "LM",
  B_predicted = "EEOS",
  B_rf = "MLM",
  e = "Productivity")


fig3a<-long_df %>%
    ggplot(aes(x=log(b), y=log(value)))+
        geom_hex() +
        scale_fill_viridis(name="Number\nof pixels")+
        geom_smooth(method="lm", aes(color=name), show.legend=FALSE)+
        scale_color_manual(values=palette_models2)+
        geom_abline(linetype=2)+
        xlim(6.5, 17)+ylim(6.5, 17)+
        facet_wrap(~name, labeller = labeller(name = model))+
        xlab("log(Observed)") + ylab("log(Predicted)")+
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
        "Productivity" = TukeyHSD(aov(dev_mte~status, site_df))$status[,"diff"],
        "MLM" = TukeyHSD(aov(dev_rf~status, site_df))$status[,"diff"],
        "LM" = TukeyHSD(aov(dev_lm~status, site_df))$status[,"diff"]
    ) %>%
        as_data_frame()%>%
        cbind("model"=c("EEOS", "Productivity", "MLM", "LM"))%>%
        as_tibble %>%
        pivot_longer(-model, names_to="comparison", values_to="Difference in deviation (Tukeys HSD)") %>%
        ggplot()+
            geom_line(aes(x=comparison, y=`Difference in deviation (Tukeys HSD)`, color=model, group=model))+
            scale_color_manual(name="Model", values=palette_models1)+
            theme_classic()+
            xlab("")


model.sel(
m1<-lm(dev~mask+status, site_df),
m2<-lm(dev~mask*status, site_df))

summary(m1)

#make combined figure
cowplot::plot_grid(fig3a, fig3c, labels = c('A', 'B'), label_size = 12, ncol=1, rel_heights = c(2, 1))
ggsave("Figures/Figure3.png", height=15, width=10)