#*****************************************************************#
#           Model diagnostic Fire Presence/Absence                #
#     Tropical and subtropical moist broadleaf forests            #
#*****************************************************************#
# This code is grouped into two steps:
# -Step 1: Data exploration and processing
#    - Data exploration and preparation
#    - Check collinearity
# -Step 2: Fitting models  
#    - Fit logistic regression model 

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
library(rsq) # to calculate R^2
#to save data to excel
library(devtools)
library(Rcpp)
library(openxlsx)

#enhance memory limit (only if necessary)
# Check memory limit
memory.limit()
# Change memory limit
memory.limit(9999999999)
memory.size(max = 7500)

#Define color ramp for plots
display.brewer.all(n=11,type="div"); title(main = "Divergent color palette")
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(10)

#set path
setwd("~/Github/GLM-Drivers-of-Fire/Data")

#read data for presence data only 
GlobalDriversFire.dt<-fread("~/GitHub/GLM-Drivers-of-Fire/Processed Data/GlobalDriversFire2003-2016.csv", header = T, sep = ",", dec = ".")

#Filter data on required biome and drop the year 2016, as the data is incomplete for the data set distance to urban population
Data<-GlobalDriversFire.dt[Year!=2016 & Biomes==1,]
#We only want data were fires are actually possible, so no bare lands
Data<-Data[LC_Bare!=1]
#to save space and RAM, clean up
remove(GlobalDriversFire.dt)

#We wan only absence 0 and presence 1
Data$Size[Data$Size>0] <- 1
#rename to fire as presence absence of fire
setnames(Data, old="Size", new=c("Fire"))
range(Data$Fire)

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
#plot some categorical variables and the response variable fire Fire
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(Fire))]),main= "Fire Fire ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(GFED_Regions))]),main= "Fire Fire ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(Continent))]),main= "Fire Fire ",col=mycolors) 
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(IDL_Layer))]),main= "Fire Fire ",col=mycolors) 
#view fire Fire indigenous vs none indigenous land
plot(rasterFromXYZ(Data[IDL_Layer=="IDL",.(x, y,as.numeric(Fire))]),main= "IDL ",col=mycolors)
plot(rasterFromXYZ(Data[IDL_Layer=="No_IDL",.(x, y,as.numeric(Fire))]),main= "No IDL  ",col=mycolors)

#View histogram/distribution of response variable and transform if necessary
#Fire
summary(Data$Fire)
range(Data$Fire)
hist(Data$Fire) 


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
plotNormalHistogram(Data$Precipitation) #right skewed
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
#Data$LAI <- sqrt(Data$LAI)
#plotNormalHistogram(Data$LAI)

#fPAR
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(fPAR))]),main= "fPAR ",col=mycolors)
plotNormalHistogram(Data$fPAR) 
range(Data$fPAR)

#LC Tree
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Tree))]),main= "Landcover Tree ",col=mycolors)
plotNormalHistogram(Data$LC_Tree) 
range(Data$LC_Tree)
plotNormalHistogram(sqrt(Data$LC_Tree))

#LC Shrub
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(LC_Shrub))]),main= "Landcover Shrub ",col=mycolors)
plotNormalHistogram(Data$LC_Shrub) 
range(Data$LC_Shrub)
plotNormalHistogram(sqrt(Data$LC_Shrub))

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
plotNormalHistogram(Data$PopDensity) 
range(Data$PopDensity)
plotNormalHistogram(log(Data$PopDensity+1))
plotNormalHistogram(log10(Data$PopDensity+1))
Data$PopDensity<- log(Data$PopDensity+1)
#rename column
setnames(Data, old="PopDensity", new=c("PopDensity_log"))

#Road Density
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(RoadDensity))]),main= "Road density ",col=mycolors)
plotNormalHistogram(Data$RoadDensity) 
range(Data$RoadDensity)
plotNormalHistogram(sqrt(Data$RoadDensity))
plotNormalHistogram(sign(Data$RoadDensity)*abs(Data$RoadDensity)^(1/3))
plotNormalHistogram(log(Data$RoadDensity+1))
plotNormalHistogram(log10(Data$RoadDensity+1))
#transform data
Data$RoadDensity <- sqrt(Data$RoadDensity)
setnames(Data, old="RoadDensity", new=c("RoadDensity_sqrt"))

#Distance to urban population
plot(rasterFromXYZ(Data[,.(x, y,as.numeric(DistUrbPop))]),main= "Distance to urban pop ",col=mycolors)
plotNormalHistogram(Data$DistUrbPop) 
range(Data$DistUrbPop)
plotNormalHistogram(sqrt(Data$DistUrbPop))


#View the relationship of all categorical explanatory variables to the response variable
barplot(table(Data$IDL_Layer), ylab = "Frequency", main = "IDL")
boxplot( Fire ~ interaction(IDL_Layer,Month), data=Data )
boxplot( Fire ~ interaction(IDL_Layer,Year), data=Data )
boxplot( Fire ~ interaction(IDL_Layer,Continent), data=Data )
boxplot( Fire ~ interaction(IDL_Layer,GFED_Regions), data=Data )
boxplot( Fire ~ IDL_Layer, data=Data )
boxplot( Fire ~ Month, data=Data )

#Check for correlations
cor(Data[,c(6:19,28)], use="pairwise.complete.obs")>0.7
#Correlations
#LAI with fPAR, drop LAI 

#Now we scale all explanatory variables to better compare estimates of the models
Data[,c(6:19,28)] <- lapply(Data[,c(6:19,28)], scale)

#------------------
#Step 2 Fitting models  

#1 linear model
f2<-formula(Fire~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs+IDL_Layer*PET
            +IDL_Layer*DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+ IDL_Layer*NrWetDays+IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop)

#linear model, considering quadratic terms
f<-formula(Fire~IDL_Layer*LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs+IDL_Layer*PET
           +IDL_Layer*DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+IDL_Layer*NrWetDays +IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop
           +IDL_Layer*I(TMax^2)+IDL_Layer*I(DTR^2)+IDL_Layer*I(fPAR^2)+ IDL_Layer*I(LC_Tree^2) +IDL_Layer*I(LC_Shrub^2)+IDL_Layer*I(LC_Crop^2)+IDL_Layer*I(LC_Herbs^2)+IDL_Layer*I(Precipitation^2) +IDL_Layer*I(NrWetDays^2)+IDL_Layer*I(PET^2)
           +IDL_Layer*I(RoadDensity_sqrt^2)+IDL_Layer*I(PopDensity_log^2)+IDL_Layer*I(DistUrbPop^2)
)

#binomial
GLM2<-glm(f2,data=Data,family = binomial)
GLM<-glm(f,data=Data,family = binomial)

summary(GLM2)
summary(GLM) # adding quadratic terms improves the result

#drop terms that are not significant step by step and check whether this decreases the AIC
#drop IDL_LayerNo IDL:Tree
ffix1<-formula(Fire~IDL_Layer+LC_Tree+IDL_Layer*LC_Shrub+IDL_Layer*LC_Herbs+IDL_Layer*PET
               +IDL_Layer*DTR+IDL_Layer*TMax+IDL_Layer*fPAR+IDL_Layer*LC_Crop+IDL_Layer*Precipitation+IDL_Layer*NrWetDays +IDL_Layer*RoadDensity_sqrt+IDL_Layer*PopDensity_log+IDL_Layer*DistUrbPop
               +IDL_Layer*I(TMax^2)+IDL_Layer*I(DTR^2)+IDL_Layer*I(fPAR^2)+ IDL_Layer*I(LC_Tree^2) +IDL_Layer*I(LC_Shrub^2)+IDL_Layer*I(LC_Crop^2)+IDL_Layer*I(LC_Herbs^2)+IDL_Layer*I(Precipitation^2) +IDL_Layer*I(NrWetDays^2)+IDL_Layer*I(PET^2)
               +IDL_Layer*I(RoadDensity_sqrt^2)+IDL_Layer*I(PopDensity_log^2)+IDL_Layer*I(DistUrbPop^2)
)
GLM.fix1<-glm(ffix1,data=Data,family = binomial)

AIC(GLM,GLM.fix1)

GLM.final<-GLM.fix1
#calculate Rsquared
rsq(GLM2)
rsq(GLM.final)# adding quadratic terms improves the result
AIC(GLM.final)

#validate model; validating logistic models is more an art than science
par(mfrow = c(2,2))
plot(GLM.final)
dev.off()


#-----------------
#save linear model output for thesis
GLM_coef <- summary(GLM.final)$coefficients
#round coeffiecents
formatter <- function(x) format(round(x,6),nsmall=6)
Variables<-formatter(GLM_coef)
GLM_names<- variable.names(GLM.final)
GLM_info <- data.table(GLM_names,Variables)

#set path
setwd("~/GitHub/GLM-Drivers-of-Fire/Output/Statistic Presence-Absence Results")

#save output
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- createWorkbook()
addWorksheet(wb, "Biome")
writeData(wb, "Biome", GLM_info, headerStyle = header_style)
freezePane(wb, "Biome", firstRow = TRUE)
setColWidths(wb, "Biome", cols = 1:ncol(GLM_info), widths = "auto")
saveWorkbook(wb, file = "my-results.xlsx", overwrite = TRUE)
