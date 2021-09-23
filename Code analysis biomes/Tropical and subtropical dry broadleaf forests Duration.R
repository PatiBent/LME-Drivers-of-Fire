#*****************************************************************#
#           Model diagnostic Fire Duration                        #
#            Tropical and subtropical dry broadleaf forests       #
#*****************************************************************#
# This code is grouped into two steps:
# -Step 1: Data exploration and processing
#    - Data exploration and preparation
#    - Check relationships between the response variable and the explanatory variables
#    - Check collinearity
# -Step 2: Fitting models  
#    - Fit linear model 
#    - Fit (weighted) mixed effect model
#         - find best random structure
#         - find best fixed structure
#         - backward selection
#    - Fit generalized additive model


#clean workspace
rm(list=ls(all=TRUE))

#read libraries
library(RColorBrewer) # for color ramps
library(nlme) #for gls and lme
library(lme4) #for glmer
library(data.table) 
library(raster)
library(ggplot2)
library(rcompanion) #for plotNormalHistogram
library(piecewiseSEM) # to calculate R^2
library(MASS) # for stepAIC()
library(mgcv) #for GAM
#to save data to excel
library(devtools)
library(Rcpp)
library(openxlsx)

#enhance memory limit (only if necessary)
# Check memory limit
memory.limit()
# Change memory limit
memory.limit(Size= 12000)
memory.size(max = 7500)

#Define color ramp for plots
display.brewer.all(n=11,type="div"); title(main = "Divergent color palette")
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(10)

#set path
setwd("~/Github/GLM-Drivers-of-Fire/Data")

#read data for presence data only 
GlobalDriversFire.dt<-fread("~/GitHub/GLM-Drivers-of-Fire/Processed Data/GFED4.0_Presence2003-2016.csv", header = T, sep = ",", dec = ".")

#Filter data on required biome and drop the year 2016, as the data is incomplete for the data set distance to urban population
Data<-GlobalDriversFire.dt[Year!=2016 & Biomes==2,]
#We only want data were fires are actually possible, so no bare lands
plot(rasterFromXYZ(GlobalDriversFire.dt[,.(x, y,as.numeric(LC_Bare))]),main= "Fire Duration ",col=mycolors)
Data<-Data[LC_Bare!=1]

#check for missing values
colSums(is.na(Data))
Data <- na.omit(Data) #drop data with missing values

# Transform to factors
Data[,c("Month","GFED_Regions","IDL_Layer","Biomes","Continent")] <- lapply(Data[,c("Month","GFED_Regions","IDL_Layer","Biomes","Continent")], factor)
str(Data)

#rename levels of IDL_Layer
levels(Data$IDL_Layer) <- c("IDL","No_IDL")

#-------
#Step1 Data exploration and processing
#Get an overview of the data
head(Data) 
names(Data)
str(Data)
colSums(Data!=0)
colSums(Data==0)
#plot some categorical variables and the response variable fire Duration
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(Duration))]),main= "Fire Duration ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(GFED_Regions))]),main= "Fire Duration ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(Continent))]),main= "Fire Duration ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(IDL_Layer))]),main= "Fire Duration ",col=mycolors) 
#view fire Duration indigenous vs none indigenous land
plot(rasterFromXYZ(Data[IDL_Layer=="IDL",.(x, y,as.numeric(Duration))]),main= "IDL ",col=mycolors)
plot(rasterFromXYZ(Data[IDL_Layer=="No_IDL",.(x, y,as.numeric(Duration))]),main= "No IDL  ",col=mycolors)

#View histogram/distribution of response variable and transform if necessary
#Duration
summary(Data$Duration)
range(Data$Duration)
hist(Data$Duration) #right skewed
plotNormalHistogram(sqrt(Data$Duration))
plotNormalHistogram(sign(Data$Duration)*abs(Data$Duration)^(1/3))
plotNormalHistogram(log(Data$Duration))
plotNormalHistogram(log10(Data$Duration))
Data$Duration <- (sign(Data$Duration)*abs(Data$Duration)^(1/3))
plotNormalHistogram(Data$Duration)

#View histogram/distribution of explanatory variable and transform if necessary (only if extremly right skewed)
#Nr wet days
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(NrWetDays))]),main= "Nr Wet Days ",col=mycolors)
plotNormalHistogram(Data$NrWetDays) 
range(Data$NrWetDays)

#DTR
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(DTR))]),main= "DTR ",col=mycolors)
range(Data$DTR)
plotNormalHistogram(Data$DTR) 

#TMax
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(TMax))]),main= "Tmax ",col=mycolors)
range(Data$TMax)
plotNormalHistogram(Data$TMax) 

#PET
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(PET))]),main= "PET ",col=mycolors)
range(Data$PET)
plotNormalHistogram(Data$PET) 

#Precipitation
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(Precipitation))]),main= "Precipitation",col=mycolors)
range(Data$Precipitation)
plotNormalHistogram(Data$Precipitation) #might need to be transformed
plotNormalHistogram(sqrt(Data$Precipitation))
plotNormalHistogram(log(Data$Precipitation+1))
plotNormalHistogram(log10(Data$Precipitation+1))
#Data$Precipitation <- log(Data$Precipitation+1)
#plotNormalHistogram(Data$Precipitation)

#LAI
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LAI))]),main= "LAI ",col=mycolors)
plotNormalHistogram(Data$LAI) 
range(Data$LAI)
plotNormalHistogram(sqrt(Data$LAI))
plotNormalHistogram(log(Data$LAI))


#fPAR
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(fPAR))]),main= "fPAR ",col=mycolors)
plotNormalHistogram(Data$fPAR) 
range(Data$fPAR)

#LC Tree
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Tree))]),main= "Landcover Tree ",col=mycolors)
plotNormalHistogram(Data$LC_Tree) # might still be alright
range(Data$LC_Tree)
plotNormalHistogram(sqrt(Data$LC_Tree))
plotNormalHistogram(sign(Data$LC_Tree)*abs(Data$LC_Tree)^(1/3))
plotNormalHistogram(log(Data$LC_Tree+1))


#LC Shrub
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Shrub))]),main= "Landcover Shrub ",col=mycolors)
plotNormalHistogram(Data$LC_Shrub)  # might still be alright
range(Data$LC_Shrub)
plotNormalHistogram(sqrt(Data$LC_Shrub))
plotNormalHistogram(sign(Data$LC_Shrub)*abs(Data$LC_Shrub)^(1/3))
plotNormalHistogram(log(Data$LC_Shrub+1))
#Data$LC_Shrub <- (sign(Data$LC_Shrub)*abs(Data$LC_Shrub)^(1/3))

#LC Crop
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Crop))]),main= "Landcover Crop ",col=mycolors)
plotNormalHistogram(Data$LC_Crop) 
range(Data$LC_Crop)

#LC Herbs
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Herbs))]),main= "Landcover Herbs",col=mycolors)
plotNormalHistogram(Data$LC_Herbs) 
range(Data$LC_Herbs)

#Pop Density
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(PopDensity))]),main= "Pop density ",col=mycolors)
plotNormalHistogram(Data$PopDensity) #extremly right skewed
range(Data$PopDensity)
#possible transformations
plotNormalHistogram(log(Data$PopDensity+1))
plotNormalHistogram(log10(Data$PopDensity+1))
#transform data
Data$PopDensity<- log(Data$PopDensity+1) 
setnames(Data, old="PopDensity", new=c("PopDensity_log")) #rename column

#Road Density
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(RoadDensity))]),main= "Road density ",col=mycolors)
plotNormalHistogram(Data$RoadDensity) #extremly right skewed
range(Data$RoadDensity)
#possible transformations
plotNormalHistogram(sqrt(Data$RoadDensity))
plotNormalHistogram(sign(Data$RoadDensity)*abs(Data$RoadDensity)^(1/3))
plotNormalHistogram(log(Data$RoadDensity+1))
plotNormalHistogram(log10(Data$RoadDensity+1))
#transform data
Data$RoadDensity <- sqrt(Data$RoadDensity)
setnames(Data, old="RoadDensity", new=c("RoadDensity_sqrt"))

#Distance to urban population
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(DistUrbPop))]),main= "Distance to urban pop ",col=mycolors)
plotNormalHistogram(Data$DistUrbPop) #might be still alright
range(Data$DistUrbPop)
#possible transformations
plotNormalHistogram(sqrt(Data$DistUrbPop))
plotNormalHistogram(log(Data$DistUrbPop+1))

#Explore data
#View the relationship of all continuous explanatory variables to the response variable
attach(Data)
par(mfrow = c(2,2))
plot(Duration~TMax) 
plot(Duration~DTR) 
plot(Duration~NrWetDays) 
plot(Duration~Precipitation) 
plot(Duration~PET)
plot(Duration~RoadDensity_sqrt)
plot(Duration~PopDensity_log) 
plot(Duration~DistUrbPop) 
plot(Duration~LC_Shrub) 
plot(Duration~LC_Tree)   
plot(Duration~LC_Crop) 
plot(Duration~LC_Herbs) 
plot(Duration~LAI) 
plot(Duration~fPAR) 
detach(Data)
dev.off()


#View the relationship of all categorical explanatory variables to the response variable
barplot(table(Data$IDL_Layer), ylab = "Frequency", main = "IDL") # skewed towards No IDL
boxplot( Duration ~ interaction(IDL_Layer,Month), data=Data )
boxplot( Duration ~ interaction(IDL_Layer,Year), data=Data )
boxplot( Duration ~ interaction(IDL_Layer,Continent), data=Data )
boxplot( Duration ~ interaction(IDL_Layer,GFED_Regions), data=Data )
boxplot( Duration ~ IDL_Layer, data=Data )
boxplot( Duration ~ Month, data=Data )

#Check for correlations
cor(Data[,c(6:19,28)], use="pairwise.complete.obs")>0.7
#Correlations
#TMax with PET and LAI with fPAR, drop LAI and PET

#Now we scale all explanatory variables to better compare estimates of the models
Data[,c(6:19,28)] <- lapply(Data[,c(6:19,28)], scale)

#------------------
#Step 2 Fitting models 
plotNormalHistogram(Data$Duration)
plotNormalHistogram(sqrt(Data$Duration))
plotNormalHistogram(sign(Data$Duration)*abs(Data$Duration)^(1/3))
plotNormalHistogram(log(Data$Duration))

#1 linear model
f2<-formula(Duration~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs
            +IDL_Layer*DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+ IDL_Layer*NrWetDays+IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop)

#linear model, considering quadratic terms
f<-formula(Duration~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs
           +IDL_Layer*DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+IDL_Layer*NrWetDays +IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop
           +IDL_Layer*I(TMax^2)+IDL_Layer*I(DTR^2)+IDL_Layer*I(fPAR^2)+ IDL_Layer*I(LC_Tree^2) +IDL_Layer*I(LC_Shrub^2)+IDL_Layer*I(LC_Crop^2)+IDL_Layer*I(LC_Herbs^2)+IDL_Layer*I(Precipitation^2) +IDL_Layer*I(NrWetDays^2)
           +IDL_Layer*I(RoadDensity_sqrt^2)+IDL_Layer*I(PopDensity_log^2)+IDL_Layer*I(DistUrbPop^2)
)

LM <-lm(f2,   data =  Data)
LM2<-lm(f, data= Data)

summary(LM)
summary(LM2)
AIC(LM)
AIC(LM2) # adding quadratic terms improves the results

#find the best LM with automatic model selection with StepAIC
LMfinal<-stepAIC(LM2)
summary(LMfinal)
AIC(LMfinal)

#validate model
par(mfrow = c(2,2))
plot(LMfinal)
dev.off()
plot(LMfinal,which=1)
#Make a histogram of the residuals to verify normality. 
hist(residuals(LMfinal))

#-> Homogenity might be violated (trend in data), normality of the residuals looks ok, no outliers

#save the complete model output
#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Model Results")
saveRDS(LMfinal, "LMBiome2Duration.rds")


#save linear model statistical output for thesis
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Statistic LM Results")
lmOut(LMfinal, file="my-results.xlsx")
#end 1 linear model

#*********************#
#2 Best (weighted) random/mixed effects model

#Generalised least squares (GLS) model
#needed to compare AIC in further analysis; identical to the linear model
GLM<-gls(f,   data =  Data)
AIC(GLM)
summary(GLM)
plot(GLM)

#1 Find a good random structure
#GLS allows for / to deal with heterogeneity in the model. This is essentially a weighted linear regression.
#Find a good variance structure
#varexp
vf1<- varExp(form =~fPAR)
M.gls1 <- gls(f, data =Data,
              weights = vf1)
vf2<- varExp(form =~TMax)
M.gls2 <- gls(f, data =Data,
              weights = vf2)
vf3<- varExp(form =~Precipitation)
M.gls3 <- gls(f, data =Data,
              weights = vf3)
vf4<- varExp(form =~LC_Crop)
M.gls4 <- gls(f, data =Data,
              weights = vf4)
vf5<- varExp(form =~LC_Tree)
M.gls5 <- gls(f, data =Data,
              weights = vf5)
vf6<- varExp(form =~RoadDensity_sqrt)
M.gls6 <- gls(f, data =Data,
              weights = vf6)
vf7<- varExp(form =~PopDensity_log)
M.gls7 <- gls(f, data =Data,
              weights = vf7)
vf8<- varExp(form =~LC_Shrub)
M.gls8 <- gls(f, data =Data,
              weights = vf8)
vf9<- varExp(form =~LC_Herbs)
M.gls9 <- gls(f, data =Data,
              weights = vf9)
vf10<- varExp(form =~DistUrbPop)
M.gls10 <- gls(f, data =Data,
              weights = vf10)
AIC(GLM,M.gls1,M.gls2,M.gls3,M.gls4,M.gls5,M.gls6,M.gls7,M.gls8, M.gls9, M.gls10)

#plot residuals with best AIC and check whether the effect of heterogeneity decreased
plot(GLM)
plot(M.gls1, main="M.gls1")
plot(M.gls2, main="M.gls2")
plot(M.gls3, main="M.gls3")
plot(M.gls4, main="M.gls4")
plot(M.gls5, main="M.gls5")
plot(M.gls6, main="M.gls6")
plot(M.gls7, main="M.gls7")
plot(M.gls8, main="M.gls8")
plot(M.gls9, main="M.gls9")
plot(M.gls10, main="M.gls10")

#combined variance structure
vf11 <- varComb(
  varExp(form =~fPAR),
  varExp(form =~Precipitation),
  varExp(form =~LC_Herbs)
)

M.glsw <- gls(f, data =Data,
              weights = vf11)


AIC(GLM,M.gls1,M.gls2,M.gls3,M.gls4,M.gls5,M.gls6,M.gls7, M.gls8, M.gls9, M.gls10, M.glsw)

#check homogeneity of variance again
summary(M.glsw)
plot(M.glsw, main="M.glsw")
#End find a good variance structure; homogenity looks good now

#linear mixed effect models
#consider further random effects like nested effects 
#effect of spatial and temporal autocorellation are unfortunately not feasible with the amount of data we have
#consider the nested effect of GFED Regions
start_time <- Sys.time()
M.glswc <- lme(f, data =Data,
               random = ~ 1 |GFED_Regions, weights = vf11 )
end_time <- Sys.time()
start_time-end_time

AIC(GLM,M.gls1,M.gls2,M.gls3,M.gls4,M.gls5,M.gls6,M.gls7, M.gls8, M.gls9, M.gls10, M.glsw,M.glswc) #considerably better
summary(M.glswc)

plot(M.glswc)
#Best Model considers the nested effect of GFED Regions

#2 Find a good fixed term structure via backward selection
#to compare fixed structure we need to we need to change the method to "ML"
#mixed effect model with best random structure with ML instead of RML
Mfix.glswc <- lme(f, data =Data, method="ML",
               random = ~ 1 |GFED_Regions, weights = vf11  )
summary(Mfix.glswc)

#drop terms that are not significant step by step and check whether this decreases the AIC
#drop IDL_LayerNo_IDL:I(RoadDensity_sqrt^2) & IDL_LayerNo_IDL:DTR
ffix1<-formula(Duration~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs
               +DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+IDL_Layer*NrWetDays +IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop
               +IDL_Layer*I(TMax^2)+IDL_Layer*I(DTR^2)+IDL_Layer*I(fPAR^2)+ IDL_Layer*I(LC_Tree^2) +IDL_Layer*I(LC_Shrub^2)+IDL_Layer*I(LC_Crop^2)+IDL_Layer*I(LC_Herbs^2)+IDL_Layer*I(Precipitation^2) +IDL_Layer*I(NrWetDays^2)
               +I(RoadDensity_sqrt^2)+IDL_Layer*I(PopDensity_log^2)+IDL_Layer*I(DistUrbPop^2)
)

Mfix.glswc1 <- lme(ffix1, data =Data, method="ML",
                   random = ~ 1 |GFED_Regions, weights = vf11 )
#compare AICs
AIC(Mfix.glswc, Mfix.glswc1)
summary(Mfix.glswc1)

#drop I(LC_Crop^2)    
ffix2<-formula(Duration~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs
               +DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+IDL_Layer*NrWetDays +IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop
               +IDL_Layer*I(TMax^2)+IDL_Layer*I(DTR^2)+IDL_Layer*I(fPAR^2)+ IDL_Layer*I(LC_Tree^2) +IDL_Layer*I(LC_Shrub^2)+IDL_Layer*I(LC_Herbs^2)+IDL_Layer*I(Precipitation^2) +IDL_Layer*I(NrWetDays^2)
               +I(RoadDensity_sqrt^2)+IDL_Layer*I(PopDensity_log^2)+IDL_Layer*I(DistUrbPop^2)
)

Mfix.glswc2 <- lme(ffix2, data =Data, method="ML",
                   random = ~ 1 |GFED_Regions, weights = vf11 )
#compare AICs
AIC(Mfix.glswc, Mfix.glswc1, Mfix.glswc2)
summary(Mfix.glswc2)
#final output
LME.final.ML<-Mfix.glswc2

#now that we found the model with the best random and fixed structure -> refit it with REML estimation
LME.final<-lme(ffix2, data =Data, method="REML",
              random = ~ 1 |GFED_Regions, weights = vf11 )

summary(LME.final)
AIC(LME.final)
#Model validation

#Plot (standardised) residuals against fitted values to assess homogeneity
plot(LME.final)
#Make a histogram of the residuals to verify normality. 
hist(residuals(LME.final))

#-> Homogenity and normality of the residuals looks good and ok respectively. Model with the best AIC as well

#Calculate R^2
rsquared(LME.final)

#save linear model output for thesis
lme_coef <- summary(LME.final)$tTable
#round coeffiecents
formatter <- function(x) format(round(x,5),nsmall=5)
Variables<-formatter(lme_coef)
#get variable names
lme_names<- variable.names(lm(ffix2,data = Data))
#combine data
lme_info <- data.table(lme_names,Variables)
lme_info
#get lme formula
LME.final$call

#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Statistic LME Results")

#save output
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- createWorkbook()
addWorksheet(wb, "Biome")
writeData(wb, "Biome", lme_info, headerStyle = header_style)
freezePane(wb, "Biome", firstRow = TRUE)
setColWidths(wb, "Biome", cols = 1:ncol(lme_info), widths = "auto")
saveWorkbook(wb, file = "my-results.xlsx", overwrite = TRUE)

#save the complete model output
#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Model Results")
saveRDS(M.glswc, "lmeBiome2Duration.rds")
#test<- readRDS("lmeBiome7.rds")

#End 2 Best (weighted) random/mixed effects model

#*********************'
#3 Generalized additive model (GAM)
sf<-formula(Duration~IDL_Layer+s(LC_Tree)+s(LC_Crop)+s(LC_Shrub)+s(LC_Herbs)
            +s(DTR)+s(TMax)+s(Precipitation)+s(NrWetDays)
            +s(fPAR)+s(PopDensity_log)+s(RoadDensity_sqrt)+s(DistUrbPop))

GAM <- gam(sf,data = Data)
summary(GAM)
GAM.info<-summary(GAM)
GAM.linear<-GAM.info$p.table
GAM.smooth<-GAM.info$s.table
plot(GAM)

#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Statistic GAM Results")

#save output
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- createWorkbook()
addWorksheet(wb, "Biome")
addWorksheet(wb, "Biomesmooth")
writeData(wb, "Biome", GAM.linear, headerStyle = header_style)
writeData(wb, "Biomesmooth", GAM.smooth, headerStyle = header_style)
freezePane(wb, "Biome", firstRow = TRUE)
setColWidths(wb, "Biome", cols = 1:ncol(GAM.linear), widths = "auto")
setColWidths(wb, "Biomesmooth", cols = 1:ncol(GAM.smooth), widths = "auto")
saveWorkbook(wb, file = "my-results.xlsx", overwrite = TRUE)

#save the complete model output
#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Model Results")
saveRDS(GAM, "GAMBiome2Duration.rds")
#test<- readRDS("lmeBiome7.rds")


