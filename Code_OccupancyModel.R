library(unmarked)
library(ggplot2)

library(tidyverse)
library(ubms) # Bayesiano
library(AICcmodavg) # Prueba MB
library(mgcv)  # GAM to residuals occ model
library(ncf) #correlograms

library(patchwork)
library(tictoc)
library(beepr)
library(writexl)

########################### Modelling Occupancy ###############################
################################# MODEL BASIC #####################################
#First run a the simplest model ("m1"), without covariates
m1 <- occu(~1 ~1, data = OFC) #model with constant detection (p) and occupancy (psi)
m1


################################# Detection Probability #####################################
################################# MODEL MaxTa #####################################
#Detection: MaxTa
m2 <- occu(~Ta ~1, data = OFC) 
m2


################################# MODEL Slope #####################################
#Detection: Slope
m3 <- occu(~Slope ~1, data = OFC) 
m3


################################# MODEL Device #####################################
#Detection: Device
m4 <- occu(~Model ~1, data = OFC) 
m4


################################# MODEL Rain #####################################
#Detection: Rain
m5 <- occu(~Rain ~1, data = OFC) 
m5


################################# MODEL Slope + Ta + Rain + SurveyEffort#####################################
#Detection: Slope + Ta + Rain + SurveyEffort
m6 <- occu(~Slope+Ta+Rain+Model ~1, data = OFC) 
m6


######################################SelectModels####
library("AICcmodavg")
model_list <- fitList(
  'p(.) psi(.)' = m1,
  'p(Ta) psi(.)' = m2,
  'p(Slope) psi(.)' = m3,
  'p(Device) psi(.)' = m4,
  'p(Rain) psi(.)' = m5,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6)

aic_results <- modSel(model_list)
print(aic_results)


summary(m6)
FigureS1 <- plotEffects(m6, type = "det", "Model")



################################# MODEL Human Activity #####################################
#Human Activity Model
m7 <- occu(~Slope+Ta+Rain+Model ~crops+urban+hfi+fire, data = OFC) 
m7

m8 <- occu(~~Slope+Ta+Rain+Model ~crops+human_pop+hfi+fire, data = OFC) 
m8

m9 <- occu(~~Slope+Ta+Rain+Model ~crops, data = OFC) 
m9

m10 <- occu(~~Slope+Ta+Rain+Model ~urban, data = OFC)
m10

m11 <- occu(~~Slope+Ta+Rain+Model ~hfi, data = OFC) 
m11

m12 <- occu(~~Slope+Ta+Rain+Model ~fire, data = OFC) 
m12

m13 <- occu(~~Slope+Ta+Rain+Model ~human_pop, data = OFC) 
m13

model_list <- fitList(
  'p(.) psi(.)' = m1,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6,
  'p(Slope+Ta+Rain+Device) psi(crops+urban+hfi+fire)' = m7,
  'p(Slope+Ta+Rain+Device) psi(crops+human_pop+hfi+fire)' = m8,
  'p(Slope+Ta+Rain+Device) psi(crops)' = m9,
  'p(Slope+Ta+Rain+Device) psi(urban)' = m10,
  'p(Slope+Ta+Rain+Device) psi(hfi)' = m11,
  'p(Slope+Ta+Rain+Device) psi(fire)' = m12,
  'p(Slope+Ta+Rain+Device) psi(human_pop)' = m13)

aic_results <- modSel(model_list)
print(aic_results)

################################# MODEL ForestQuality #####################################

# Univariate models (Forest only)
m14 <- occu(~Slope+Ta+Rain+Model ~ Suitable250, data = OFC)

m15 <- occu(~Slope+Ta+Rain+Model ~ Suitable2km, data = OFC)

m16 <- occu(~~Slope+Ta+Rain+Model ~ Suitable1250, data = OFC)


model_list <- fitList(
  # Univariate forest models
  'p(.) psi(.)' = m1,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6,
  'p(Slope+Ta+Rain+Device) psi(Forest_0.25km)' = m14,
  'p(Slope+Ta+Rain+Device) psi(Forest_2km)' = m15,
  'p(Slope+Ta+Rain+Device) psi(Forest_6.25km)' = m16)

aic_results <- modSel(model_list)
print(aic_results)


#### 1. Univariate Models ###
  m15 <- occu(~Slope+Ta+Rain+Model ~ Suitable2km, data = OFC)
  m16 <- occu(~Slope+Ta+Rain+Model ~ Suitable1250, data = OFC)
  m17 <- occu(~Slope+Ta+Rain+Model ~ TreeHeight, data = OFC)
  m18 <-  occu(~Slope+Ta+Rain+Model ~ NDVI, data = OFC)
  

  ### 3. Full Models ###
  m19 <- occu(~Slope+Ta+Rain+Model ~ Suitable2km + TreeHeight + NDVI, data = OFC)
 
 
  m20 <- occu(~Slope+Ta+Rain+Model ~ Suitable1250 + TreeHeight + NDVI, data = OFC)
  
  
  ### Model Selection ###
  model_list <- fitList(
    "p(.) psi(.)" = m1,
    'p(Slope+Ta+Rain+Device) psi(.)' = m6,
    'p(Slope+Ta+Rain+Device) psi(Forest_2km)' = m15,
    'p(Slope+Ta+Rain+Device) psi(Forest_6.25km)' = m16,
    'p(Slope+Ta+Rain+Device) psi(TreeHeight)' = m17,
    'p(Slope+Ta+Rain+Device) psi(NDVI)' = m18,
    'p(Slope+Ta+Rain+Device) psi(Forest_2km+TreeHeight+NDVI)' = m19,
    'p(Slope+Ta+Rain+Device) psi(Forest_6.25km+TreeHeight+NDVI)' = m20
    )
  
  # Get AIC table
  aic_results <- modSel(model_list)
  print(aic_results)

################################# MODEL Forest Structure #####################################

### Univariate models ###
# PatchDistance at different scales
  m21 <- occu(~Slope+Ta+Rain+Model ~ PatchDistance250, data = OFC)
  m22 <- occu(~Slope+Ta+Rain+Model ~ PatchDistance2km, data = OFC)
  m23 <- occu(~Slope+Ta+Rain+Model ~ PatchDistance1250, data = OFC)

  # Univariate forest models
model_list <- fitList(
  'p(.) psi(.)' = m1,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_0.25km)' = m21,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_2km)' = m22,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_6.25km)' = m23)
aic_results <- modSel(model_list)
print(aic_results)

# Univariate Models
m24 <- occu(~Slope+Ta+Rain+Model ~ EdgeDensity, data = OFC)


### Bivariate models ###
m25 <- occu(~Slope+Ta+Rain+Model ~ EdgeDensity + PatchDistance2km, data = OFC)

m26 <- occu(~Slope+Ta+Rain+Model ~ EdgeDensity + PatchDistance1250, data = OFC)


### Model selection ###
model_list <- fitList(
  "p(.) psi(.)" = m1,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_2km)' = m22,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_6.25km)' = m23,
  'p(Slope+Ta+Rain+Device) psi(EdgeDensity)' = m24,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_2km+EdgeDensity)' = m25,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_6.25km+EdgeDensity)' = m26
)

# Get AIC table
aic_results <- modSel(model_list)
print(aic_results)


################################# MODEL Full #####################################

m27 <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable2km, data = OFC) 
m27

m28 <- occu(~Slope+Ta+Rain+Model ~TreeHeight+EdgeDensity+hfi+Suitable2km, data = OFC) 
m28

m29 <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable2km+PatchDistance2km, data = OFC) 
m29

m30 <- occu(~Slope+Ta+Rain+Model ~TreeHeight+EdgeDensity+hfi+Suitable2km+PatchDistance2km, data = OFC) 
m30

m31 <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable2km+PatchDistance1250, data = OFC) 
m31

m32 <- occu(~Slope+Ta+Rain+Model ~TreeHeight+EdgeDensity+hfi+Suitable2km+PatchDistance1250, data = OFC) 
m32

m33 <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable1250+PatchDistance1250, data = OFC) 
m33

m34 <- occu(~Slope+Ta+Rain+Model ~TreeHeight+EdgeDensity+hfi+Suitable1250+PatchDistance1250, data = OFC) 
m34

m35 <- occu(~Slope+Ta+Rain+Model ~TreeHeight+EdgeDensity+hfi+Suitable1250, data = OFC)
m35

m36 <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable1250, data = OFC)
m36

model_list <- fitList(
  "p(.) psi(.)" = m1,
  'p(Slope+Ta+Rain+Device) psi(.)' = m6,
  'p(Slope+Ta+Rain+Device) psi(crops+urban+hfi+fire)' = m7,
  'p(Slope+Ta+Rain+Device) psi(crops+human_pop+hfi+fire)' = m8,
  'p(Slope+Ta+Rain+Device) psi(crops)' = m9,
  'p(Slope+Ta+Rain+Device) psi(urban)' = m10,
  'p(Slope+Ta+Rain+Device) psi(hfi)' = m11,
  'p(Slope+Ta+Rain+Device) psi(fire)' = m12,
  'p(Slope+Ta+Rain+Device) psi(human_pop)' = m13,
  'p(Slope+Ta+Rain+Device) psi(Forest_2km)' = m15,
  'p(Slope+Ta+Rain+Device) psi(Forest_6.25km)' = m16,
  'p(Slope+Ta+Rain+Device) psi(TreeHeight)' = m17,
  'p(Slope+Ta+Rain+Device) psi(NDVI)' = m18,
  'p(Slope+Ta+Rain+Device) psi(Forest_2km+TreeHeight+NDVI)' = m19,
  'p(Slope+Ta+Rain+Device) psi(Forest_6.25km+TreeHeight+NDVI)' = m20,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_2km)' = m22,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_6.25km)' = m23,
  'p(Slope+Ta+Rain+Device) psi(EdgeDensity)' = m24,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_2km+EdgeDensity)' = m25,
  'p(Slope+Ta+Rain+Device) psi(PatchDistance_6.25km+EdgeDensity)' = m26,
  'psi(TreeHeight*EdgeDensity+hfi+Forest2km)' = m27,
  'psi(TreeHeight+hfi+EdgeDensity+Forest2km)' = m28
)

model_selection <- modSel(model_list)
print(model_selection)

#MB-TEST Fitness#
best <- occu(~Slope+Ta+Rain+Model ~TreeHeight*EdgeDensity+hfi+Suitable2km, data = OFC) 
tic(); mb_test <- mb.gof.test(mod = best, # model
                              print.table= T, # show table with results
                              nsim = 1000, # number of simulations
                              plot.hist = T, # show histogram
                              report = T, # show report
                              parallel = T); toc();beep(sound = 4) # use several nuclei to speed up the process. If you have a multicore machine, set parallel = T. If you have a single core machine, set parallel = F.

#Predict occupancy
EdgeDensity <- rast("XXXX")
TreeHeight2km = rast("XXXX")
hfi = rast("XXXX")
forest = rast("XXXX")


names(EdgeDensity) <- "EdgeDensity" 
names(TreeHeight2km) <- "TreeHeight2km" 
names(hfi) <- "hfi"
names(suitable2kme) <- "forest"

ED <- scale(Covariates$EdgeDensity)
attr(ED, "scaled:center")
attr(ED, "scaled:scale")

EdgeDensitye <- (EdgeDensity-75.8889)/41.68581

TH <- scale(Covariates$TreeHeight2km)
attr(TH, "scaled:center")
attr(TH, "scaled:scale")

TreeHeight2kme <- (TreeHeight2km-30.34071)/6.458333

HFI <- scale(Covariates$hfi)
attr(HFI, "scaled:center")
attr(HFI, "scaled:scale")

hfie <- (hfi-10.59475)/5.562548

Suitable2km <- scale(Covariates$Suitable2km)
attr(Suitable2km, "scaled:center")
attr(Suitable2km, "scaled:scale")

suitable2kme <- (forest-0.5118001)/0.2947601

# Create a raster stack with your scaled rasters
raster_stack <- c(EdgeDensitye, TreeHeight2kme, hfie, suitable2kme)

current <- predict(best, 
                       newdata = raster_stack, 
                       type = "state")