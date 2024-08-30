#make figure 5

library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridis)

# load
#specify region
#region<-"Redwoods"
region<-"Adirondacks"

vars<-rast(paste0("Outputs/AllPredictedVariables_", region, ".tif"))
r2_df<-readRDS("Outputs/R2dataframe.RDS")

# make correlation panel
cor_panel<-vars%>% 
    as_tibble %>%
    filter(complete.cases(.)) %>%
    select(b, B_predicted, B_rf, B_lm) %>%
    rename("EEOS"=B_predicted, "LM"=B_lm, "MLM"=B_rf) %>%
    pivot_longer(!b, names_to="model", values_to="prediction") %>%
    ggplot(data=., aes(x=b, y=prediction)) +
        geom_hex() +
        geom_smooth(method="lm", color="black", aes(linetype=model))+
        scale_linetype(guide="none")+
        scale_fill_viridis()+
        facet_wrap(~model)+
        theme_classic()+
        xlim(0,11000)+ ylim(0,11000)+
        xlab("Biomass") + ylab("Predicted\nBiomass")+
        labs(fill = "Number\nof pixels")


r2_panel<-r2_df %>%
    mutate(rsq=as.numeric(rsq)) %>%
    group_by(model) %>%
    mutate(varProp=rsq/max(rsq)) %>%
    ggplot(data=.) +
        geom_line(aes(y=varProp, x=ID, linetype=model, group=model))+
        scale_linetype( labels=c("EEOS", "LM", "MLM" ))+
        theme_classic()+
        ylim(0,1)+
        labs(x="", y="Proportion of maximum\nvariance explained for\nany land category")+ 
        guides(linetype=guide_legend(title=""))




png(file.path("Figures", paste0("Figure5.png")), height = 5, width = 10, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(r2_panel, x=0, y=0.5, width=1, height=0.5)+
    cowplot::draw_plot(cor_panel, x=0.005, y=0, width=1, height=0.5)+
    cowplot::draw_plot_label(   label = c("A", "B"), 
                        size = 15, 
                        x = c(0, 0), 
                        y = c(1, 0.5))
dev.off()