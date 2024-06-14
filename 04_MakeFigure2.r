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

#funtions
makeVariablePanel<-function(variable, title){
    ggplot() +
        geom_spatraster(data=vars[[variable]])+
        scale_fill_viridis(limits = c(0, max(values(vars[[variable]]), na.rm=TRUE)), na.value = "transparent")+
        labs(fill=title)+
        theme_minimal()
}
makeCorrelationPanel<-function(biomassPrediction, method){
    column <- ensym(biomassPrediction)
    ggplot(data=var_df, aes(x=b, y=!!column)) +
        geom_hex() +
        geom_smooth(method="lm", color="black", size=2, linetype=2)+
        scale_fill_viridis()+
        theme_classic()+
        xlab("Biomass") + ylab(paste0(method, " Predicted\nBiomass"))+
        labs(fill = "Number\nof points")
}
rsq <- function (x, y) cor(x, y) ^ 2

#make map panels
s_panel<-makeVariablePanel('s', "Richness")
n_panel<-makeVariablePanel('n', "Abundance")
e_panel<-makeVariablePanel('EVI', "EVI")
b_panel<-makeVariablePanel('b', "Observed\nBiomass")
b_eos_panel<-makeVariablePanel('B_predicted', "Predicted\nBiomass")
b_rf_panel<-makeVariablePanel('B_rf', "Random\nForest\nBiomass")
b_lm_panel<-makeVariablePanel('B_lm', "Linear\nModel\nBiomass")

# make correlation panels
var_df<-vars%>% 
    as_tibble %>%
    filter(complete.cases(.))
eos_corPanel<-makeCorrelationPanel('B_predicted', "EoS")
rf_corPanel<-makeCorrelationPanel('B_rf', "RF")
lm_corPanel<-makeCorrelationPanel('B_lm', "LM")


#get stats for results
var_df<-values(vars) %>%
    as_tibble %>%
    filter(complete.cases(.))

#corr test
cor.test(var_df$b, var_df$B_predicted)
    #note that rsq for MLM and LM are calculated against test subsamples in model fitting script
#get means
mean(var_df$b); sd(var_df$b)
mean(var_df$B_predicted); sd(var_df$B_predicted)




#save figure
png(file.path("Figures", paste0("Figure2_", region, ".png")), height = 10, width = 10, units = 'in', res = 300)
cowplot::ggdraw()+
    cowplot::draw_plot(s_panel, x=0, y=0.75, width=0.33, height=0.25)+
    cowplot::draw_plot(n_panel, x=0.33, y=0.75, width=0.33, height=0.25)+
    cowplot::draw_plot(e_panel, x=0.66, y=0.75, width=0.33, height=0.25)+
    cowplot::draw_plot(b_panel, x=0, y=0.25, width=0.5, height=0.5)+
    cowplot::draw_plot(b_eos_panel, x=0.5, y=0.25, width=0.5, height=0.5)+
    cowplot::draw_plot(eos_corPanel, x=0, y=0, width=0.33, height=0.25)+
    cowplot::draw_plot(rf_corPanel, x=0.33, y=0, width=0.33, height=0.25)+
    cowplot::draw_plot(lm_corPanel, x=0.66, y=0, width=0.33, height=0.25)
dev.off()
