library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridis)

# load
#specify region
#region<-"Redwoods"
region<-"Adirondacks"

# load
vars<-rast(paste0("Outputs/AllPredictedVariables_", region, ".tif"))
adir<-vect('Outputs/LandClasses')


#funtions
makeVariablePanel<-function(variable, title){
    ggplot() +
        geom_spatraster(data=vars[[variable]])+
        scale_fill_viridis(limits = c(0, max(values(vars[[variable]]), na.rm=TRUE)), na.value = "transparent")+
        geom_spatvector(data=lakes, alpha=1, fill="black", color="black")+
        labs(fill=title)+
        theme_minimal()
}

rsq <- function (x, y) cor(x, y) ^ 2

#get lakes
lakes<-adir[adir$LCCode %in% c(15)]  %>% aggregate(dissolve=TRUE)


#make map panels
s_panel<-makeVariablePanel('s', "Richness")
n_panel<-makeVariablePanel('n', "Abundance")
e_panel<-makeVariablePanel('EVI', "EVI")
b_panel<-makeVariablePanel('b', "Observed\nBiomass\n(tons)")
b_eos_panel<-makeVariablePanel('B_predicted', "Predicted\nBiomass\n(unitless)")
b_rf_panel<-makeVariablePanel('B_rf', "Random\nForest\nBiomass")
b_lm_panel<-makeVariablePanel('B_lm', "Linear\nModel\nBiomass")


#get stats for results
var_df<-values(vars) %>%
    as_tibble %>%
    filter(complete.cases(.))

#corr test
cor.test(var_df$b, var_df$B_predicted)
rsq(var_df$b, var_df$B_predicted)
    #note that rsq for MLM and LM are calculated against test subsamples in model fitting script
#get means
mean(var_df$b); sd(var_df$b)
mean(var_df$B_predicted); sd(var_df$B_predicted)




#save figure
png(file.path("Figures", paste0("Figure2_", region, ".png")), height = 10, width = 15, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(s_panel, x=0, y=0.66, width=0.33, height=0.33)+
    cowplot::draw_plot(n_panel, x=0.33, y=0.66, width=0.33, height=0.33)+
    cowplot::draw_plot(e_panel, x=0.66, y=0.66, width=0.33, height=0.33)+
    cowplot::draw_plot(b_panel, x=0, y=0, width=0.5, height=0.66)+
    cowplot::draw_plot(b_eos_panel, x=0.5, y=0, width=0.5, height=0.66)
dev.off()

# make correlation panel
cor_panel<-vars%>% 
    as_tibble %>%
    filter(complete.cases(.)) %>%
    select(b, B_predicted, B_rf, B_lm) %>%
    rename("EEOS"=B_predicted, "LM"=B_lm, "MLM"=B_rf) %>%
    pivot_longer(!b, names_to="model", values_to="prediction") %>%
    ggplot(data=., aes(x=b, y=prediction)) +
        geom_hex() +
        geom_smooth(method="lm", color="black", size=2, linetype=2)+
        scale_fill_viridis()+
        facet_wrap(~model)+
        theme_classic()+
        xlim(0,11000)+ ylim(0,11000)+
        xlab("Biomass") + ylab("Predicted\nBiomass")+
        labs(fill = "Number\nof pixels")

#save correlation panels for figure 5
saveRDS(cor_panel, "Outputs/Figure5CorrelationGrob.RDS")
