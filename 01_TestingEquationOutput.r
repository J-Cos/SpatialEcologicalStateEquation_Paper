library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)

# load
vars<-rast('Outputs/AdirondacksEquationVariables.tif')
adir<-vect('Outputs/LandClasses')
boundary<-vect('Outputs/AdirondacksBoundary')


#confirm
ggplot() +
    geom_spatvector(data=adir)+
    geom_spatraster(data = vars, aes(fill=e))+
    geom_spatvector(data=boundary)+
    scale_fill_manual(values=c("#56ddc5", "#ff3db7", "#4699dd"), na.value = "transparent")


#calculate deviation
fit<-c("B_predicted"=bp, var['b'], "dev"=bp-var['b'], var['EVI'])
plot(fit)

# make wilderness and res management polygons
wild<-adir[adir$LCCode==7 ] %>% aggregate()
resman<-adir[adir$LCCode==5 ] %>% aggregate()

cats<-adir[adir$LCCode==7 | adir$LCCode==5] %>% aggregate(by="LCCode")

#confirm polygons
ggplot() +
    geom_spatvector(data=cats, aes(fill=LCCode))


#get values
cat_vals<-terra::extract(fit, cats, weights=TRUE) %>%
    filter(weight>0.99) %>%
    filter(!is.na(dev)) %>%
    mutate(ID=as.factor(ID)) %>%
    mutate(ID = fct_recode(ID, "Wilderness"="2", "Resource Management"="1"))

#difference in deviation
ggplot() +
    geom_boxplot(data=cat_vals, aes(x=ID, y=dev))
lm(data=cat_vals, dev~1+ID) %>% summary
lm(data=cat_vals, dev~1+ID) %>% confint

resman_fit<-mask(fit, resman)
wild_fit<-mask(fit, wild)

require(gridExtra)
require(viridis)
require(ggpointdensity)

ggplot() +
    geom_spatraster(data = fit['dev'])+
    scale_fill_viridis(na.value = "transparent")+
    ggtitle("deviation")


plot1=ggplot() +
    geom_spatraster(data = resman_fit['dev'])+
    scale_fill_viridis(limits = c(0, 8000))+
    ggtitle("Res. Management")
plot2=ggplot() +
    geom_spatraster(data = wild_fit['dev'])+
    scale_fill_viridis(limits = c(0, 8000))+
    ggtitle("Wilderness")

grid.arrange(plot1, plot2, ncol=1)

# model fit tests
ggplot(data=cat_vals, aes(x=b, y=B_predicted, shape=ID)) +
     geom_hex() +
    geom_smooth(method="lm", color="black", size=2, linetype=2)+
    facet_wrap(~ID)+
    scale_fill_viridis()+
    theme_classic()

summary(lm(data=cat_vals, b~B_predicted*ID))

cor(cat_vals$b, cat_vals$B_predicted)
cor(res_df$b, res_df$B_predicted)
cor(wild_df$b, wild_df$B_predicted)
rsq <- function (x, y) cor(x, y) ^ 2
rsq(cat_vals$b, cat_vals$B_predicted)
rsq(res_df$b, res_df$B_predicted)
rsq(wild_df$b, wild_df$B_predicted)

rmse<- function(obs, pred) sqrt(mean((obs - pred)^2))
rmse(cat_vals$b, cat_vals$B_predicted)
rmse(res_df$b, res_df$B_predicted)
rmse(wild_df$b, wild_df$B_predicted)77

rsq2<- function(obs, pred) sum((pred-obs)^2) / sum( obs-mean(obs)^2)
rsq2(cat_vals$b, cat_vals$B_predicted)
rsq2(res_df$b, res_df$B_predicted)
rsq2(wild_df$b, wild_df$B_predicted)


res_df<-cat_vals %>%
    filter(ID=="Resource Management")
summary(lm(data=res_df, b~B_predicted))$r.squared

wild_df<-cat_vals %>%
    filter(ID=="Wilderness")
summary(lm(data=wild_df, b~B_predicted))$r.squared

##aic check
selection<-MuMIn::model.sel(
    "f1"=lm(data=cat_vals, b~B_predicted),
    "f2"=lm(data=cat_vals, b~ID),
    "f3"=lm(data=cat_vals, b~B_predicted:ID),
    "f4"=lm(data=cat_vals, b~B_predicted+B_predicted:ID),
    "f5"=lm(data=cat_vals, b~ID+B_predicted:ID),
    "f6"=lm(data=cat_vals, b~B_predicted*ID))

MuMIn::model.avg(selection, subset=delta<2) %>% summary
# biomass prediction significantly correlated with observed for wilderness, but not for disturbed; wilderness also has significantly lower observed biomass




# comparison with pure ndvi predictor - exactly as above but ndvi swapped for B_predicted
ggplot(data=cat_vals, aes(x=b, y=EVI, shape=ID)) +
     geom_hex() +
    geom_smooth(method="lm", color="black", size=2, linetype=2)+
    facet_wrap(~ID)+
    scale_fill_viridis()+
    theme_classic()


summary(lm(data=cat_vals, b~EVI*ID))

res_df<-cat_vals %>%
    filter(ID=="Resource Management")
summary(lm(data=res_df, b~e))$r.squared

wild_df<-cat_vals %>%
    filter(ID=="Wilderness")
summary(lm(data=wild_df, b~e))$r.squared

##aic check
selection<-MuMIn::model.sel(
    "f1"=lm(data=cat_vals, b~e),
    "f2"=lm(data=cat_vals, b~ID),
    "f3"=lm(data=cat_vals, b~e:ID),
    "f4"=lm(data=cat_vals, b~e+e:ID),
    "f5"=lm(data=cat_vals, b~ID+e:ID),
    "f6"=lm(data=cat_vals, b~e*ID))

MuMIn::model.avg(selection, subset=delta<2) %>% summary
# higher productivity significantly correlated with lower biomass overall , and this relationship is significantly stronger where ecosystems disturbed.