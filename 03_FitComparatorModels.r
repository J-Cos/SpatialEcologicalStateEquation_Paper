library(terra)
library(tidyverse)
library(tidyterra)
library(ranger)
library(vip)

# functions
rsq <- function (x, y) cor(x, y) ^ 2
# needed for raster predictions with ranger
predfun <- function(...) predict(...)$predictions

#specify region
#region<-"Redwoods"
region<-"Adirondacks"



# load
vars<-rast(paste0("Outputs/", region, "EquationVariables.tif"))


# make dataframe
var_df<-vars%>% 
  as_tibble %>%
  filter(complete.cases(.)) %>%
  select(b, n, s, EVI)

# create test train split
# seed set for replicability
set.seed(1)
TestSamplePercent<-0.5
nsamples<-dim(var_df)[1]
testRows<-sample(1:nsamples, nsamples*TestSamplePercent)
test<-var_df[testRows,]
train<-var_df[-testRows,]


# fit random forest
set.seed(1)
rf <- ranger(b~n+s+EVI,
  data = train, 
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  mtry = 2,
  num.trees=1001)
p_rf<-predict(rf, test)
rsq(p_rf$predictions, test$b)
vip::vip(rf)
rf$variable.importance


# fit lm
f1=lm(b~n+s+EVI, data=train)
f2=lm(b~n*s+EVI, data=train)
f3=lm(b~n+s*EVI, data=train)
f4=lm(b~n*EVI+s, data=train)
f6=lm(b~n*EVI+n*s, data=train)
f7=lm(b~EVI*n+EVI*s, data=train)
f8=lm(b~s*EVI+s*n, data=train)
f5=lm(b~n*s*EVI, data=train)

selection<-MuMIn::model.sel(f1, f2, f3, f4, f5, f6, f7, f8)

#f5 by far best
summary(f5)
confint(f5)
p_lm<-predict(f5, test)
rsq(p_lm, test$b)

# test ln(b)~ ln(e) 
rsq(log(var_df$EVI), log(var_df$b))

# save new raster
B_rf <- terra::predict(vars, rf, fun=predfun, na.rm=TRUE)
B_lm <- terra::predict(vars, f5)
allVars=c(
  vars, 
  tidyterra::rename(B_rf, "B_rf" = "lyr1"),
  tidyterra::rename(B_lm, "B_lm" = "lyr1"))
terra::writeRaster(allVars, filename=paste0("Outputs/AllPredictedVariables_", region, ".tif"), overwrite=TRUE)