library(terra)
library(tidyverse)
library(tidyterra)
library(randomForest)

#load
bp<-rast('Outputs/B_predicted.tif')
var<-rast("Data/Adirondacks_variables.tif")
adir<-vect('Data/apaLandClass202306')

#align projections
crs(bp)<-crs(var)
ext(bp)<-ext(var)
adir<-project(adir, var)

#confirm
ggplot() +
  geom_spatraster(data = var)+
  geom_spatvector(data=adir)


var_df<-var%>% 
  as_tibble %>%
  filter(complete.cases(.))


  rf<-randomForest(b~., data=var_df, ntree=50)

  varImpPlot(rf)
  bp_rf<-predict(var, rf)

  plot(var$b)
  plot(bp_rf)
  plot(bp)

dev<-c(var['b'], "bp"=bp_rf, "dev"=bp-bp_rf)

#get values
dev_vals<-terra::extract(dev, cats, weights=TRUE) %>%
    filter(weight>0.99) %>%
    filter(!is.na(dev)) %>%
    mutate(ID=as.factor(ID)) %>%
    mutate(ID = fct_recode(ID, "Wilderness"="2", "Resource Management"="1"))

#difference in deviation
ggplot() +
    geom_boxplot(data=dev_vals, aes(x=ID, y=dev))
lm(data=dev_vals, dev~ID) %>% summary


### non functional play testing - delete me and sort of scripts properly ----
everything<-c(bp, var)
cat_vals<-terra::extract(var, cats, weights=TRUE) %>%
    filter(weight>0.99) %>%
    filter(!is.na(b)) %>%
    mutate(ID=as.factor(ID)) %>%
    mutate(ID = fct_recode(ID, "Wilderness"="2", "Resource Management"="1"))

cat_vals_df<-cat_vals%>% 
  as_tibble %>%
  filter(complete.cases(.))

rm_rf<-cat_vals_df %>%
  filter(ID=="Resource Management") %>%
  randomForest(b~n+s+EVI, data=., ntree=500, mtry=2)

plot(rm_rf)
varImpPlot(rm_rf)


w_rf<-cat_vals_df %>%
  filter(ID=="Wilderness") %>%
  randomForest(b~n+s+EVI, data=., ntree=500, mtry=2)

plot(w_rf)
varImpPlot(w_rf)


w_eos_rf<-cat_vals_df %>%
  filter(ID=="Wilderness") %>%
  randomForest(b~n+s+e+B_predicted, data=., ntree=500, mtry=2)

plot(w_eos_rf)
varImpPlot(w_eos_rf)


###################
# lm to predict biomass
lm_fit<-lm(b~n+s+EVI, data=cat_vals_df)
summary(lm_fit)

w_lm_fit<-cat_vals_df %>%
  filter(ID=="Wilderness") %>%
  lm(b~n+s+EVI, data=.)
summary(w_lm_fit)

rm_lm_fit<-cat_vals_df %>%
  filter(ID=="Resource Management") %>%
  lm(b~n+s+EVI, data=.)
summary(rm_lm_fit)

aov(b~ID, data=cat_vals_df ) %>% summary
aov(s~ID, data=cat_vals_df ) %>% summary
aov(n~ID, data=cat_vals_df ) %>% summary
aov(e~ID, data=cat_vals_df ) %>% summary

ggplot() +
    geom_boxplot(data=cat_vals_df, aes(x=ID, y=b))
ggplot() +
    geom_boxplot(data=cat_vals_df, aes(x=ID, y=s))
ggplot() +
    geom_boxplot(data=cat_vals_df, aes(x=ID, y=n))
ggplot() +
    geom_boxplot(data=cat_vals_df, aes(x=ID, y=e))































############################### other code from other scripts





























































#autocorraltion
autocor(vars[['dev']], global=TRUE, method="moran")
( autocor(vars[['dev']], global=FALSE, method="moran")> ((-1) / (9-1)) )%>% plot
autocor(vars[['dev']], global=TRUE, method="moran")

N<-sum(!is.na(values(vars[["dev"]])))
EI<- (-1) / (N-1)

EI<- (-1) / (9-1)


# predicted - observed corraltion
biomasses<-c(vars[['B_predicted_scaled']], vars[['b_scaled']])
fit<-focalPairs(biomasses, w=5, "pearson", na.rm=TRUE)
plot(fit)

fit_df<-terra::extract(fit, land_cats, weights=TRUE) %>%
    filter(weight>0.99) %>%
    filter(!is.na(lyr1)) %>%
    mutate(ID=as.factor(ID)) %>%
    mutate(ID = fct_recode(ID, "Wilderness"="1", "Semi-wilderness"="2", "Resource Management"="3"))

head(fit_df)
ggplot(fit_df)+
    geom_boxplot(aes(x=ID, y=lyr1))

#make panel showing distribution of extreme deivations by category

lm(data=filter(fit_df, ID=="Resource Management"), lyr1~1) %>% summary
lm(data=filter(fit_df, ID!="Resource Management"), lyr1~1) %>% summary


getSpatialCorrelations<-function(landCoverCode) {
    raster_cat<-biomasses %>% crop(adir[adir$LCCode==landCoverCode ], mask=TRUE)

    fits<-list()
    for (i in seq(from=3, to=33, by=10)) {
        fits[[as.character(i)]]<-focalPairs(raster_cat, w=i, "pearson", na.rm=TRUE, filename=file.path(tempdir(), paste0("corRast_", runif(1), ".tif")))
        print(paste0(i, " complete"))
    }

    vals_l<-lapply(fits, FUN=values, na.rm=TRUE)

    output_modEsts<-lapply(
        vals_l, 
        FUN=function(item) {summary(lm(item~1))$coefficients[,c("t value", "Estimate", "Pr(>|t|)")]}
    ) %>%
        bind_rows %>%
        mutate(w=names(vals_l))

    output<-list("modEsts"=output_modEsts, "cor_rasts"=fits)
    return(output)
}

w_cors<-getSpatialCorrelations(7)
rm_cors<-getSpatialCorrelations(5)

cors<-rbind(
    cbind(w_cors[[1]], "ID"="w"),
    cbind(rm_cors[[1]], "ID"="rm"))

ggplot(cors)+
    geom_line(aes(x=as.numeric(w), y=`Estimate`, color=ID))




























# make panel map of deviation
deviationMapPanel<-ggplot() +
    geom_spatraster(data = vars[['dev']])+
    scale_fill_gradient2(low="blue", high="red", mid="gray90", na.value="transparent")+
    theme_minimal()

























#try model predicted deviation from pop and category
mod_vars<-c(vars, pop)
cat_rast<-rasterize(cats, mod_vars, "LCCode")
modvar_df<-c(mod_vars, cat_rast) %>% 
    values(na.rm=TRUE) %>% 
    as_tibble %>%
    mutate(dev=abs(dev)) %>%
    mutate(LCCode=as.factor(LCCode)) %>%
    mutate(LCCode = fct_recode(LCCode, "Wilderness"="7", "Resource Management"="5")) %>%
    select(dev, population_density, LCCode) %>%
    filter(population_density<5)

lm(dev~LCCode+population_density:LCCode, data=modvar_df) %>% summary
ggplot(modvar_df, aes(x=population_density, y=dev, color=LCCode))+
    geom_point()+
    geom_smooth(method="lm")


#try predicting prediction from observed and pop and category
mod_vars<-c(vars, pop)
cat_rast<-rasterize(cats, mod_vars, "LCCode")
modvar_df<-c(mod_vars, cat_rast) %>% 
    values(na.rm=TRUE) %>% 
    as_tibble %>%
    mutate(LCCode=as.factor(LCCode)) %>%
    mutate(LCCode = fct_recode(LCCode, "Wilderness"="7", "Resource Management"="5")) %>%
    select(B_predicted_scaled, b_scaled, population_density, LCCode) %>%
    mutate(pop_bins=cut_width(population_density, 25))

lm(B_predicted_scaled~b_scaled*LCCode+ b_scaled*population_density+ b_scaled*population_density*LCCode, data=modvar_df) %>% summary
ggplot(modvar_df, aes(x=b_scaled, y=B_predicted_scaled, color=LCCode))+
    geom_point()+
    geom_smooth(method="lm")+
    facet_wrap(~pop_bins)









#####################
####################
####################


resman_fit<-mask(fit, resman)
wild_fit<-mask(fit, wild)

ggplot() +
    geom_violin(data=cat_vals, aes(x=ID, y=(dev)))

(vars[['dev']]>2) %>% plot
(vars[['dev']]<(-2)) %>% plot

dat<-mask(vars['dev']>(2), resman)
ggplot() +
    geom_spatraster(data = dat)
    scale_fill_viridis(na.value="transparent")



dat<-mask(vars['dev']>(2), cats, inverse=TRUE)
ggplot() +
    geom_spatraster(data = dat)





dat<-mask(vars['dev']<(-2), cats, inverse=TRUE)
ggplot() +
    geom_spatraster(data = dat)+
    scale_fill_viridis(na.value="transparent")

dat<-vars['dev']<(-2)
ggplot() +
    geom_spatraster(data = dat)+
    scale_fill_viridis(na.value="transparent")
















var_df<-vars%>% 
    as_tibble %>%
    filter(complete.cases(.))

ggplot() +
    geom_density(data=var_df, aes(y=dev))

ggplot(data=var_df, aes(x=b, y=B_predicted)) +
     geom_hex() +
    geom_smooth(method="lm", color="black", size=2, linetype=2)+
    scale_fill_viridis()+
    theme_classic()

ggplot(data=var_df, aes(x=b_scaled, y=B_predicted_scaled)) +
     geom_hex() +
    geom_smooth(method="lm", color="black", size=2, linetype=2)+
    scale_fill_viridis()+
    theme_classic()

rsq <- function (x, y) cor(x, y) ^ 2
rsq(var_df$b, var_df$B_predicted)
rsq(var_df$b_scaled, var_df$B_predicted_scaled)


#
raster_rm<-biomasses %>% crop(adir[adir$LCCode==5 ], mask=TRUE)
raster_w<-biomasses %>% crop(adir[adir$LCCode==7 ], mask=TRUE)
var_df<-raster_rm%>% 
    as_tibble %>%
    filter(complete.cases(.))
cor(var_df$b_scaled, var_df$B_predicted_scaled, method="pearson")
var_df<-raster_w%>% 
    as_tibble %>%
    filter(complete.cases(.))
cor(var_df$b_scaled, var_df$B_predicted_scaled, method="pearson")
