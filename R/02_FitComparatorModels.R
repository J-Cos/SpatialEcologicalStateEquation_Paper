#source packages and themes
source("Code/CONUS/00_packagesAndThemes.R")

# 1) load data
pas<- vect("Outputs/PAs")
usa<- vect("Outputs/States")
vars<-rast("Outputs/Variables_EeosOnly.tif")


#2) get dataframe for modelling
# make dataframe
modeling_df<-values(vars, dataframe=TRUE) %>%
    as_tibble %>%
    filter(!is.na(B_predicted)) %>%
    select(b, n, s, e) %>%
    filter(complete.cases(.))


# 3) create test train split
# seed set for replicability
set.seed(1)
TestSamplePercent<-0.5
nsamples<-dim(modeling_df)[1]
testRows<-sample(1:nsamples, nsamples*TestSamplePercent)
test<-modeling_df[testRows,]
train<-modeling_df[-testRows,]


# 4) fit random forest
set.seed(1)
rf <- ranger::ranger(b~n+s+e,
  data = train, 
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  mtry = 2,
  num.trees=1001)
p_rf<-predict(rf, test)
lm(p_rf$predictions~test$b) %>% summary
vip::vip(rf)
rf$variable.importance


# 5) fit lm
f1=lm(b~n+s+e, data=train)
f2=lm(b~n*s+e, data=train)
f3=lm(b~n+s*e, data=train)
f4=lm(b~n*e+s, data=train)
f6=lm(b~n*e+n*s, data=train)
f7=lm(b~e*n+e*s, data=train)
f8=lm(b~s*e+s*n, data=train)
f5=lm(b~n*s*e, data=train)

selection<-MuMIn::model.sel(f1, f2, f3, f4, f5, f6, f7, f8)

#inspect best - varies by area
summary(f5)
confint(f5)
p_lm<-predict(f5, test)
lm(p_lm~test$b) %>% summary


# 6) make new raster
predfun <- function(...) predict(...)$predictions
B_rf <- terra::predict(vars[[1:4]], rf, fun=predfun, na.rm=TRUE)
B_lm <- terra::predict(vars, f5)
vars=c(
  vars, 
  tidyterra::rename(B_rf, "B_rf" = "lyr1"),
  tidyterra::rename(B_lm, "B_lm" = "lyr1"))

# 7) create other devs
vars[["dev_mte"]]<-log(vars[["b"]])-log(vars[["e"]])
vars[["dev_rf"]]<-log(vars[["b"]])-log(vars[["B_rf"]])
vars[["dev_lm"]]<-log(vars[["b"]])-log(vars[["B_lm"]])

# 8) save raster with additional variables
writeRaster(vars, "Outputs/Variables.tif", overwrite=TRUE)



