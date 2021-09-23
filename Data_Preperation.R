#*****************************************************************#
#                     Data preparation                            #
#*****************************************************************#
#*
#*The aim of this script is to create a global data set with monthly data on a 0.25 degree grid from the years 2003 to 2015. It is generally understood that fire regimes are closely linked to vegetation, climate  and human activities. Thus we used the following variables (sources are in brackets[]): 
#*
#Climate:
#Max Temperature in degree C (TMax) [Climatic Research Unit (CRU)]
#Diurnal temperature range in degree C (DTR) [Climatic Research Unit (CRU)]
#Number of wet days (NrWetDays) in days [Climatic Research Unit (CRU)]
#Potential evapotranspiration (PET) [Climatic Research Unit (CRU)]
#Precipitation in mm [Global Precipitation Climatology Centre (GPCC)]

#Land cover / Vegetation (all fractional data (0-1) except LAI):
#Fraction of absorbed Photosynthetic Active Radiation (fPAR) [Climate change initiative ESA (CCI)]
#Leaf area index in m^2/m^2 (LAI)  [Climate change initiative ESA (CCI)]
#Fraction of tree land cover  (LC_Tree) [Climate change initiative ESA (CCI)]
#Fraction of shrub land cover (LC_Shrub) [Climate change initiative ESA (CCI)]
#Fraction of crop land cover  (LC_Crop) [Climate change initiative ESA (CCI)]
#Fraction of herbs land cover (LC_Herbs) [Climate change initiative ESA (CCI)]
#Fraction of bare land cover (LC_Bare) [Climate change initiative ESA (CCI)]

#Anthropogenic influences:
#Road density in m/km^2  [Global Roads Inventory Project (GRIP)]
#Population density in inhabitants/km^2 (PopDensity) [History database of the Global Environment (HYDE)]
#Distance to urban population in m (DistUrbPop) [History database of the Global Environment (HYDE)]
#Indigenous Peoples land (IDL) as categorical variable IDL, No IDL  (IDL_Layer) [Garnett et al. 2018: "A spatial overview of the global importance of Indigenous lands for conservation"]

#Response variables:
#Presence/Absence of fire Global fire atlas [Global Fire Emissions Database (GFED4), Global fire atlas]
#Fire size in km^2  [Global Fire Emissions Database (GFED4), Global fire atlas]
#Fire duration in days [Global Fire Emissions Database (GFED4), Global fire atlas]
#Fire ignitions [Global Fire Emissions Database (GFED4), Global fire atlas]
#The following two data sets were not used in the study:
#Fraction of burned area (0-1) [Global Fire Emissions Database (GFED4s)]
#Fire emissions gC/m^2 [Global Fire Emissions Database (GFED4s)]

#regional filters
#Biomes [Olson et al. 2001: "Terrestrial Ecoregions of the World: A New Map of Life on Earth]
#Continents [Environmental Systems Research Institute (ESRI) from ArcGIS]
#GFED [Global Fire Emissions Database (GFED4)]

#All data except the data on Indigenous Peoples land is freely available and was uploaded. Data on Indigenous Peoples land can be requested by the authorship team of Garnett et al. (2018. Nature Sustainability, 1, 369, doi.org/10.1038/s41893-018-0100-6)

#If necessary data sets were regridded to 0.25 x 0.25 degree. For most data sets this was done beforehand by Mukunga, Tichaona Tavare using the Climate Data Operators software.

#remove all variables
rm(list=ls(all=TRUE))
#set path
setwd("~/GLM-Drivers-of-Fire/Data")

#read libraries
library(ggplot2)
library(raster)
library(rgdal)
library(sf)
library(fasterize)
library(ncdf4) # package for netcdf manipulation
library(rhdf5) # to read HDF5 files
library(data.table)
library(RColorBrewer)
#install.packages("data.table")

#Define color ramp
display.brewer.all(n=11,type="div"); title(main = "Divergent color palette")
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(10)

#***************************#
#import data and process data
#***************************#
#define raster extent and resolution for all data sets
rtemp <- raster(xmn=-180,xmx=180,ymn=-90,ymx=90, res=1/4)

#------------------------
#*****************************#
#   Indigenous Peoples Land   #
#*****************************#
#import data
#set path
setwd("~/GLM-Drivers-of-Fire/Data")
#set path to file
file <- "IndigenousPeoplesLands2017/IPL_2017.shp"
#read shapefile
IDL.shp <- shapefile(file)
#view projection and class
projection(IDL.shp)
class(IDL.shp)
names(IDL.shp)
extent(IDL.shp)
IDL.shp$Name_
#need to change projection to longlat
IDL_Reprojected.shp <- spTransform(IDL.shp, CRS("+proj=longlat +elips=WGS84"))
#raster field cannot be char so we add a numeric ID
IDL_Reprojected.shp$ID<-1

#create IDL raster
IDL.raster <- rasterize(IDL_Reprojected.shp, rtemp, field="ID")
#convert raster to data table
IDL.dt<- setDT(as.data.frame(rasterToPoints(IDL.raster)))
#create new column 
IDL.dt$IDL_Layer<-as.factor("Indigenous Land")
#drop unnecessary column
IDL.dt[,layer:=NULL]

#Clean workplace
rm("IDL_Reprojected.shp","IDL.shp")

#Check the data with plot
plot(IDL.raster)
plot(rasterFromXYZ(IDL.dt[,.(x,y, IDL_Layer)]))
IDL.raster


#------------------------
#**************************************************#
#  GFED4 data (without small fires; fire > 21ha    #
#**************************************************#

#*********************#
#  1  Size in km^2     #
#*********************#

#import and process tiff Size data from Global Fire Atlas
#import data
#set path data
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas/MonthlyGrid")
data.dir <- 'Size'
#read all tiff files
Size.files <- list.files(path = data.dir , pattern = "\\.tif$", full.names = T)
#stack tiff files
Size.stack <- stack(Size.files)
#view files
Size.stack

#get length of the time frame
t<-dim(Size.stack)[3]

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the size of the fire data month by month
for (i in seq_along(1:t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month of the raster; slice data into monthly portions
  Size.raster<-subset(Size.stack,i)
  
  #convert raster to data table
  TempSize.dt<- setDT(as.data.frame(rasterToPoints(Size.raster)))
  #rename columns
  setnames(TempSize.dt, old=3, new=c("Size"))
  #add the year and month of the precipitation data
  TempSize.dt$Year = timeseq$Years[i]
  TempSize.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    Size.dt<- TempSize.dt
  }
  else{
    Size.dt <- rbind(Size.dt, TempSize.dt)
  }
}
#Clean workplace
rm("TempSize.dt")

#Reorder the columns
Size.dt<-Size.dt[, c(1,2,4,5,3)]

#view data
plot(Size.raster)
Test<-Size.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y, Size)]),main= "Size in kmÂ²", col=mycolors)

#Check for obvious data errors 
Size.dt[, .N, by = .(Year)] 
Size.dt[, .N, by = .(Month)] 


#*********************#
#  2   Ignitions      #
#*********************#

#import and process tiff Ignitions data from Global Fire Atlas
#import data
#set path data
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas/MonthlyGrid")
data.dir <- 'Ignitions'
#read all tiff files
Ignitions.files <- list.files(path = data.dir , pattern = "\\.tif$", full.names = T)
#stack tiff files
Ignitions.stack <- stack(Ignitions.files)
#view files
Ignitions.stack

#get length of the time frame
t<-dim(Ignitions.stack)[3]

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the Sizedata month by month
for (i in seq_along(1:t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month of the raster; slice data into monthly portions
  Ignitions.raster<-subset(Ignitions.stack,i)
  
  #convert raster to data table
  TempIgnitions.dt<- setDT(as.data.frame(rasterToPoints(Ignitions.raster)))
  #rename columns
  setnames(TempIgnitions.dt, old=3, new=c("Ignitions"))
  #add the year and month of the precipitation data
  TempIgnitions.dt$Year = timeseq$Years[i]
  TempIgnitions.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    Ignitions.dt<- TempIgnitions.dt
  }
  else{
    Ignitions.dt <- rbind(Ignitions.dt, TempIgnitions.dt)
  }
}
#Clean workplace
rm("TempIgnitions.dt")

#Reorder the columns
Ignitions.dt<-Ignitions.dt[, c(1,2,4,5,3)]

#view data
plot(rasterFromXYZ(Ignitions.dt[,.(x, y, Ignitions)]),col="black")
Test<-Ignitions.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y, Ignitions)]), main= "Ignitions", col=mycolors)

#Check for obvious data errors 
Ignitions.dt[, .N, by = .(Year)] 
Ignitions.dt[, .N, by = .(Month)] 


#****************************#
# 3    Duration in days      #
#****************************#

#import and process tiff Duration data from Global Fire Atlas
#import data
#set path data
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas/MonthlyGrid")
data.dir <- 'Duration'
#read all tiff files
Duration.files <- list.files(path = data.dir , pattern = "\\.tif$", full.names = T)
#stack tiff files
Duration.stack <- stack(Duration.files)
#view files
Duration.stack

#get length of the time frame
t<-dim(Duration.stack)[3]
t
#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the duration data month by month
for (i in seq_along(1:t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month of the raster; slice data into monthly portions
  Duration.raster<-subset(Duration.stack,i)
  
  #convert raster to data table
  TempDuration.dt<- setDT(as.data.frame(rasterToPoints(Duration.raster)))
  #rename columns
  setnames(TempDuration.dt, old=3, new=c("Duration"))
  #add the year and month of the precipitation data
  TempDuration.dt$Year = timeseq$Years[i]
  TempDuration.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    Duration.dt<- TempDuration.dt
  }
  else{
    Duration.dt <- rbind(Duration.dt, TempDuration.dt)
  }
}
#Clean workplace
rm("TempDuration.dt")

#Reorder the columns
Duration.dt<-Duration.dt[, c(1,2,4,5,3)]

#view data
plot(rasterFromXYZ(Duration.dt[,.(x, y, Duration)]),col="black")
Test<-Duration.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y, Duration)]), main= "Duration in days", col=mycolors)

#Check for obvious data errors 
Duration.dt[, .N, by = .(Year)] 
Duration.dt[, .N, by = .(Month)] 

#*********************#
#  4   Speed          #
#*********************#

#import and process tiff Speed data from Global Fire Atlas
#import data
#set path data
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas/MonthlyGrid")
data.dir <- 'Speed'
#read all tiff files
Speed.files <- list.files(path = data.dir , pattern = "\\.tif$", full.names = T)
#stack tiff files
Speed.stack <- stack(Speed.files)
#view files
Speed.stack

#get length of the time frame
t<-dim(Speed.stack)[3]
t
#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the speed data month by month
for (i in seq_along(1:t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month of the raster; slice data into monthly portions
  Speed.raster<-subset(Speed.stack,i)
  
  #convert raster to data table
  TempSpeed.dt<- setDT(as.data.frame(rasterToPoints(Speed.raster)))
  #rename columns
  setnames(TempSpeed.dt, old=3, new=c("Speed"))
  #add the year and month of the precipitation data
  TempSpeed.dt$Year = timeseq$Years[i]
  TempSpeed.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    Speed.dt<- TempSpeed.dt
  }
  else{
    Speed.dt <- rbind(Speed.dt, TempSpeed.dt)
  }
}
#Clean workplace
rm("TempSpeed.dt")

#Reorder the columns
Speed.dt<-Speed.dt[, c(1,2,4,5,3)]

#view data
plot(rasterFromXYZ(Speed.dt[,.(x, y, Speed)]),col="black")

#------------------------
#**************************************************#
#  GFED4.1s data (with small fires)                #
#**************************************************#
#*
#*Note that this data set does not contain information to duration, speed and size
#*It contains data to fraction of burned area and the emissions of the fire what we use 
#*as an proxy to fire severity

#*************************************#
#  1  Fraction of burned area         #
#*************************************#

#import and process hdf5 fraction of burned area data from Global Fire Atlas
#set path
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas")
data.dir <- 'GFED4_1s'
#read all tiff files
BurnedFraction.files <- list.files(path = data.dir , pattern = "\\.hdf5$", full.names = T)

#view structure of hdf5 file
h5ls(BurnedFraction.files[2])

#read long and lat for tests
#LonLat_Test<-h5read(BurnedFraction.files[2],"lon") #matches with grid!

#Create a grid to match the data on
rtemp <- raster(xmn=-180,xmx=180,ymn=-90,ymx=90, res=1/4)
#convert raster to data table
Grid<- setDT(as.data.frame(rasterToPoints(rtemp)))

#create a timeseq of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#create a sequence of years of the data
years<-unique(timeseq$Years)

#Retrieve the data
for (i in seq_along(years)) {
  #for (i in 1:2) {
  #retrieve the data year by year
  #read hdf5 file
  GFED_BurnedFraction<-h5read(BurnedFraction.files[i] ,"burned_area")
  
  for (j in 1:12) {
    #retrieve the data month by month
    MonthlyBurnFr<-GFED_BurnedFraction[[j]][[1]]
    
    #change the output from a matrix into the form long lat year month data
    #retrieve every single column from the dataset
    for (k in 1:720) { 
      #retrieve the i'th column
      tempBurntFraction.dt<-setDT(as.data.frame(MonthlyBurnFr[,k]))
      #change column name to a standard
      setnames(tempBurntFraction.dt, old=1, new=c("BurnedFraction"))
      
      #Append the data to the final data table
      if(k==1){
        MonthBurntFraction.dt<- tempBurntFraction.dt
      }
      else{
        MonthBurntFraction.dt<- rbind(MonthBurntFraction.dt, tempBurntFraction.dt)
      }
    } 
    
    #add the month to the data
    MonthBurntFraction.dt$Month = timeseq$Month[j]
    #add the year to the data
    MonthBurntFraction.dt$Year = years[i]
    
    #add lan and lon data from grid; grid matches with lon lat data from hdf5 file
    MonthBurntFraction.dt<-cbind(Grid, MonthBurntFraction.dt)
    
    #Append the data to the yearly data table
    if(j==1){
      YearBurntFraction.dt<- MonthBurntFraction.dt
    }
    else{
      YearBurntFraction.dt<- rbind(YearBurntFraction.dt, MonthBurntFraction.dt)
    }
  }
  
  #add the year to the data
  MonthBurntFraction.dt$Year = years[i]
  
  #Append the data to the final data table
  if(i==1){
    BurntFraction.dt<- YearBurntFraction.dt
  }
  else{
    BurntFraction.dt<- rbind(BurntFraction.dt, YearBurntFraction.dt)
  }
  
}
#Clean workplace
rm("MonthBurntFraction.dt","tempBurntFraction.dt","MonthlyBurnFr","YearBurntFraction.dt")

#optional; Remove entries with no fire
#BurntFraction.dt<-BurntFraction.dt[BurnedFraction!=0,]

#Reorder the columns
BurntFraction.dt<-BurntFraction.dt[,c(1,2,5,4,3)]

BurntFraction.dt[, .N, by = .(Year)] 
#check the result
plot(rasterFromXYZ(BurntFraction.dt[,.(x,y, as.numeric(BurnedFraction))]), main= "Burned ",
     col=mycolors)
Test<-BurntFraction.dt[Year==2012,]
plot(rasterFromXYZ(Test[BurnedFraction!=0,.(x, y,as.numeric(BurnedFraction))]),
     main= "Fraction of burned area",col=mycolors)

#Check for obvious data errors 
BurntFraction.dt[, .N, by = .(Year)] 
BurntFraction.dt[, .N, by = .(Month)]
range(BurntFraction.dt$BurnedFraction)

#*************************************#
#  2  Emissions of gram C per mÂ²      #
#*************************************#

#import and process hdf5 emissions data from Global Fire Atlas
#set path
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas")
data.dir <- 'GFED4_1s'
#read all tiff files
Emissions.files <- list.files(path = data.dir , pattern = "\\.hdf5$", full.names = T)

#view structure of hdf5 file
h5ls(Emissions.files[2])

#read long and lat for tests
#LonLat_Test<-h5read(Emissions.files[2],"lon") #matches with grid!

#Create a grid to match the data on
rtemp <- raster(xmn=-180,xmx=180,ymn=-90,ymx=90, res=1/4)
#convert raster to data table
Grid<- setDT(as.data.frame(rasterToPoints(rtemp)))

#create a timeseq of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#create a sequence of years of the data
years<-unique(timeseq$Years)

#Retrieve the data
for (i in seq_along(years)) {
  #  for (i in 1:2) {
  #retrieve the data year by year
  #read hdf5 file
  GFED_Emissions<-h5read(Emissions.files[i] ,"emissions")
  
  for (j in 1:12) {
    #retrieve the data month by month
    MonthlyEmissions<-GFED_Emissions[[j]][[1]]
    
    #change the output from a matrix into the form long lat year month data
    #retrieve every single column from the dataset
    for (k in 1:720) { 
      #retrieve the i'th column
      tempEmissions.dt<-setDT(as.data.frame(MonthlyEmissions[,k]))
      #change column name to a standard
      setnames(tempEmissions.dt, old=1, new=c("Emissions"))
      
      #Append the data to the final data table
      if(k==1){
        MonthEmissions.dt<- tempEmissions.dt
      }
      else{
        MonthEmissions.dt<- rbind(MonthEmissions.dt, tempEmissions.dt)
      }
    } 
    
    #add the month to the data
    MonthEmissions.dt$Month = timeseq$Month[j]
    #add the year to the data
    MonthEmissions.dt$Year = years[i]
    
    #add lan and lon data from grid; grid matches with lon lat data from hdf5 file
    MonthEmissions.dt<-cbind(Grid, MonthEmissions.dt)
    
    #Append the data to the yearly data table
    if(j==1){
      YearEmissions.dt<- MonthEmissions.dt
    }
    else{
      YearEmissions.dt<- rbind(YearEmissions.dt, MonthEmissions.dt)
    }
  }
  
  #add the year to the data
  MonthEmissions.dt$Year = years[i]
  
  #Append the data to the final data table
  if(i==1){
    Emissions.dt<- setDT(YearEmissions.dt)
  }
  else{
    Emissions.dt<- rbind(Emissions.dt, YearEmissions.dt)
  }
  
}
#Clean workplace
rm("MonthEmissions.dt","tempEmissions.dt","MonthlyEmissions","YearEmissions.dt")

#optional; Remove entries with no fire
#Emissions.dt<-Emissions.dt[Emissions!=0,]

#Reorder the columns
Emissions.dt<-Emissions.dt[,c(1,2,5,4,3)]

#check the result
plot(rasterFromXYZ(Emissions.dt[,.(x,y, as.numeric(Emissions))]), main= "Burned ",
     col=mycolors)
Test<-Emissions.dt[Year==2012,]
plot(rasterFromXYZ(Test[Emissions!=0,.(x, y, Emissions)]),
     main= "Emissions in g C per mÂ²",col=mycolors)

#Check for obvious data errors 
Emissions.dt[, .N, by = .(Year)] 
Emissions.dt[, .N, by = .(Month)]
range(Emissions.dt$Emissions)


#*************************************************************************#
#                       Climate data                                      #
#*************************************************************************#

#------------------------
#**************************#
#     Precipitation in mm  #
#**************************#
#import and process netcdf precipitation data
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/GPCCPrecipitation")
#open nc precipitation data
nc_data <- nc_open('full_data_monthly_v2018_025.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('GPCC_precip_2003-2016_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the precipitation data in a 3-dimensional array
precip.array <- ncvar_get(nc_data, "precip")
#check dimensions
dim(precip.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "precip", "_FillValue")
fillvalue
#close nc precipitation data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and Precip in mm

#Change the fillvalue to R standard NA
precip.array[precip.array == fillvalue$value] <- NA

#create a time sequence of the precipitation data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the precipitation data month by month
for (i in seq_along(t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  precip.slice <- precip.array[, , i]
  #create precipitation raster for the i'th month
  precip.raster <- raster(t(precip.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),  crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  precip.raster <- flip(precip.raster, direction='y')
  #resample the data to 0.25 resolution
  precip.raster <- resample(precip.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempPrecip.dt<- setDT(as.data.frame(rasterToPoints(precip.raster)))
  #rename columns
  setnames(TempPrecip.dt, old=c("layer"), new=c("Precipitation"))
  #add the year and month of the precipitation data
  TempPrecip.dt$Year = timeseq$Years[i]
  TempPrecip.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    precip.dt<- TempPrecip.dt
  }
  else{
    precip.dt <- rbind(precip.dt, TempPrecip.dt)
  }
}
#Clean workplace
rm("TempPrecip.dt","precip.slice")

#Reorder the columns
precip.dt<-precip.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(precip.dt)/7
#Check the data with plot
plot(precip.raster)
Test<-precip.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y, Precipitation)]),main= "Precipitation in mm",col=mycolors)
precip.raster

#Check for obvious data errors 
precip.dt[, .N, by = .(Year)] 
precip.dt[, .N, by = .(Month)]

#save data into csv
#write.csv(precip.dt,"~/GLM-Drivers-of-Fire/ProcessedData/Precip.csv", row.names = TRUE)
#read csv
#test<-read.csv2("~/GLM-Drivers-of-Fire/ProcessedData/Precip.csv", header = T, sep = ",", dec = ".", row.names = 1)

#-------------
#************************#
#   Max temperature      #
#************************#
#import and process netcdf maximal temperature data from CRU
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CRU")
#open data
nc_data <- nc_open('cru_ts4.04.1901.2019.tmx.dat.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CRU_tmx_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the max temperature data in a 3-dimensional array
tmax.array <- ncvar_get(nc_data, "tmx")
#check dimensions
dim(tmax.array)

#Retrieve the fillvalue of the data
fillvalue <- ncatt_get(nc_data, "tmx", "_FillValue")
fillvalue
#close  data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and Tmax in Celsius

#Change the fillvalue to R standard NA
tmax.array[tmax.array == fillvalue$value] <- NA

#create a timeseq of the precipitation data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the max temperature data month by month
for (i in seq_along(t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  tmax.slice <- tmax.array[, , i]
  #create tmax raster for the i'th month
  tmax.raster <- raster(t(tmax.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  tmax.raster <- flip(tmax.raster, direction='y')
  #resample the data to 0.25 resolution
  tmax.raster <- resample(tmax.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  Temptmax.dt<- setDT(as.data.frame(rasterToPoints(tmax.raster)))
  #rename columns
  setnames(Temptmax.dt, old=c("layer"), new=c("TMax"))
  #add the year and month of the tmax data
  Temptmax.dt$Year = timeseq$Years[i]
  Temptmax.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    tmax.dt<- Temptmax.dt
  }
  else{
    tmax.dt <- rbind(tmax.dt, Temptmax.dt)
  }
}
#Clean workplace
rm("Temptmax.dt","tmax.slice")

#Reorder the columns
tmax.dt<-tmax.dt[, c(1,2,4,5,3)]

#Check that number of row are correct
nrow(tmax.dt)/3
#Check the data with plot
plot(tmax.raster)
Test<-tmax.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,TMax)]),main= "Maximum temperature in Â°C",col=mycolors)
tmax.raster

#Check for obvious data errors 
tmax.dt[, .N, by = .(Year)] 
tmax.dt[, .N, by = .(Month)]

#-----------
#*******************#
#Number of Wet Days #
#*******************#
#*
#import and process netcdf number of wet days data from CRU
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CRU")
#open data
nc_data <- nc_open('cru_ts4.04.1901.2019.wet.dat.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CRU_NrWetDays_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the number of wet days data in a 3-dimensional array
NrWetDays.array <- ncvar_get(nc_data, "wet")
#check dimensions
dim(NrWetDays.array)

#Retrieve the fillvalue of the data
fillvalue <- ncatt_get(nc_data, "wet", "_FillValue")
fillvalue
#close data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and NrWetDays 

#Change the fillvalue to R standard NA
NrWetDays.array[NrWetDays.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the number of wets days data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  NrWetDays.slice <- NrWetDays.array[, , i]
  #create NrWetDays raster for the i'th month
  NrWetDays.raster <- raster(t(NrWetDays.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  NrWetDays.raster <- flip(NrWetDays.raster, direction='y')
  #resample the data to 0.25 resolution
  NrWetDays.raster<- resample(NrWetDays.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempNrWetDays.dt<- setDT(as.data.frame(rasterToPoints(NrWetDays.raster)))
  #rename columns
  setnames(TempNrWetDays.dt, old=c("layer"), new=c("NrWetDays"))
  #add the year and month of the NrWetDays data
  TempNrWetDays.dt$Year = timeseq$Years[i]
  TempNrWetDays.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    NrWetDays.dt<- TempNrWetDays.dt
  }
  else{
    NrWetDays.dt <- rbind(NrWetDays.dt, TempNrWetDays.dt)
  }
}
#Clean workplace
rm("TempNrWetDays.dt","NrWetDays.slice")

#Reorder the columns
NrWetDays.dt<-NrWetDays.dt[, c(1,2,4,5,3)]

#Check the data with plot
plot(NrWetDays.raster)
Test<-NrWetDays.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,NrWetDays)]),main= "Number of wet days",col=mycolors)
NrWetDays.raster

#Check for obvious data errors 
NrWetDays.dt[, .N, by = .(Year)] 
NrWetDays.dt[, .N, by = .(Month)] 

#-------------------
#******************************#
# Diurnal Temperature Range    #
#******************************#
#import and process netcdf DTR data from CRU
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CRU")
#open data
nc_data <- nc_open('cru_ts4.04.1901.2019.dtr.dat.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CRU_DTR_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
DTR.array <- ncvar_get(nc_data, "dtr")
#check dimensions
dim(DTR.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "dtr", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and DTR 

#Change the fillvalue to R standard NA
DTR.array[DTR.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the fire ignitions data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  DTR.slice <- DTR.array[, , i]
  #create DTR raster for the i'th month
  DTR.raster <- raster(t(DTR.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  DTR.raster <- flip(DTR.raster, direction='y')
  #resample the data to 0.25 resolution
  DTR.raster<- resample(DTR.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempDTR.dt<- setDT(as.data.frame(rasterToPoints(DTR.raster)))
  #rename columns
  setnames(TempDTR.dt, old=c("layer"), new=c("DTR"))
  #add the year and month of the DTR data
  TempDTR.dt$Year = timeseq$Years[i]
  TempDTR.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    DTR.dt<- TempDTR.dt
  }
  else{
    DTR.dt <- rbind(DTR.dt, TempDTR.dt)
  }
}
#Clean workplace
rm("TempDTR.dt","DTR.slice")

#Reorder the columns
DTR.dt<-DTR.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(DTR.dt)/3
#Check the data with plot
plot(DTR.raster)
Test<-DTR.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,DTR)]),main= "Diurnal temp range in Â°C",col=mycolors)
DTR.raster

#Check for obvious data errors 
DTR.dt[, .N, by = .(Year)] 
DTR.dt[, .N, by = .(Month)] 

#-------------------

#*********************************#
#   Potential evapotranspiration  #
#*********************************#
#*
#import and process netcdf PET data from CRU
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CRU")
#open data
nc_data <- nc_open('cru_ts4.04.1901.2019.pet.dat.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CRU_PET_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the number of wet days data in a 3-dimensional array
PET.array <- ncvar_get(nc_data, "pet")
#check dimensions
dim(PET.array)

#Retrieve the fillvalue of the data
fillvalue <- ncatt_get(nc_data, "pet", "_FillValue")
fillvalue
#close data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and PET 

#Change the fillvalue to R standard NA
PET.array[PET.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  PET.slice <- PET.array[, , i]
  #create PET raster for the i'th month
  PET.raster <- raster(t(PET.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  PET.raster <- flip(PET.raster, direction='y')
  #resample the data to 0.25 resolution
  PET.raster<- resample(PET.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempPET.dt<- setDT(as.data.frame(rasterToPoints(PET.raster)))
  #rename columns
  setnames(TempPET.dt, old=c("layer"), new=c("PET"))
  #add the year and month of the PET data
  TempPET.dt$Year = timeseq$Years[i]
  TempPET.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    PET.dt<- TempPET.dt
  }
  else{
    PET.dt <- rbind(PET.dt, TempPET.dt)
  }
}
#Reorder the columns
PET.dt<-PET.dt[, c(1,2,4,5,3)]

#Check the data with plot
plot(PET.raster)
Test<-PET.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,PET)]),main= "Potential evapotranspiration in mm/day",col=mycolors)
PET.raster

#Clean workplace
rm("TempPET.dt","PET.slice",PET.array)

#Check for obvious data errors 
PET.dt[, .N, by = .(Year)] 
PET.dt[, .N, by = .(Month)] 


#*******************#
#   Soil Moisture   #
#*******************#
#import and process netcdf Soil moisture data from CCI
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CCI")
#open data
nc_data <- nc_open('ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-2003-2017-fv04.7.remap0.25.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CCI_SoilMoisture_2003-2017_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
SoilMoisture.array <- ncvar_get(nc_data, "sm")
#check dimensions
dim(SoilMoisture.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "sm", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and SoilMoisture 

#Change the fillvalue to R standard NA
SoilMoisture.array[SoilMoisture.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2017,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the fire ignitions data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  SoilMoisture.slice <- SoilMoisture.array[, , i]
  #create SoilMoisture raster for the i'th month
  SoilMoisture.raster <- raster(t(SoilMoisture.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  SoilMoisture.raster <- flip(SoilMoisture.raster, direction='y')
  #resample the data to 0.25 resolution
  SoilMoisture.raster<- resample(SoilMoisture.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempSoilMoisture.dt<- setDT(as.data.frame(rasterToPoints(SoilMoisture.raster)))
  #rename columns
  setnames(TempSoilMoisture.dt, old=c("layer"), new=c("SoilMoisture"))
  #add the year and month of the SoilMoisture data
  TempSoilMoisture.dt$Year = timeseq$Years[i]
  TempSoilMoisture.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    SoilMoisture.dt<- TempSoilMoisture.dt
  }
  else{
    SoilMoisture.dt <- rbind(SoilMoisture.dt, TempSoilMoisture.dt)
  }
}
#Clean workplace
rm("TempSoilMoisture.dt","SoilMoisture.slice")

#Reorder the columns
SoilMoisture.dt<-SoilMoisture.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(SoilMoisture.dt)/3
#Check the data with plot
plot(SoilMoisture.raster)
Test<-SoilMoisture.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,SoilMoisture)]),main= "Soil moisture in mÂ³",col=mycolors)
SoilMoisture.raster

#Check for obvious data errors 
SoilMoisture.dt[, .N, by = .(Year)] 
SoilMoisture.dt[, .N, by = .(Month)] 

#-------------------
#*************************************************************************#
#                      Vegetation/ Fuel data                              #
#*************************************************************************#

#******************************#
#    Above ground biomass      #
#******************************#
#import and process netcdf biomass data from CCI
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/CCI")
#open data
nc_data <- nc_open('ESACCI-BIOMASS-L4-AGB-MERGED-0d25-2017-fv1.0_merged.nc.2003.2018.remap.0.25.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CCI_Biomass_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
Biomass.array <- ncvar_get(nc_data, "agb")
#check dimensions
dim(Biomass.array)

#Retrieve the fillvalue of the  data
fillvalue <- ncatt_get(nc_data, "agb", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and Biomass 

#Change the fillvalue to R standard NA
Biomass.array[Biomass.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the fire ignitions data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  Biomass.slice <- Biomass.array[, , i]
  #create Biomass raster for the i'th month
  Biomass.raster <- raster(t(Biomass.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  #Biomass.raster <- flip(Biomass.raster, direction='y')
  #resample the data to 0.25 resolution
  Biomass.raster<- resample(Biomass.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempBiomass.dt<- setDT(as.data.frame(rasterToPoints(Biomass.raster)))
  #rename columns
  setnames(TempBiomass.dt, old=c("layer"), new=c("Biomass"))
  #add the year and month of the Biomass data
  TempBiomass.dt$Year = timeseq$Years[i]
  TempBiomass.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    Biomass.dt<- TempBiomass.dt
  }
  else{
    Biomass.dt <- rbind(Biomass.dt, TempBiomass.dt)
  }
}
#Clean workplace
rm("TempBiomass.dt","Biomass.slice")

#Reorder the columns
Biomass.dt<-Biomass.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(Biomass.dt)/3
#Check the data with plot
plot(Biomass.raster)
Test<-Biomass.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,Biomass)]),main= "Biomass in Mg/ha",col=mycolors)
Biomass.raster

#Check for obvious data errors 
Biomass.dt[, .N, by = .(Year)] 
Biomass.dt[, .N, by = .(Month)] 



#***********************************#
#           fPAR                    #
#***********************************#
#import and process netcdf 
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/LAIfPAR")
#open data
nc_data <- nc_open('MODIS_MOD15A2H_LAIfPAR_Global_monmean.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('fPAR_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
fPAR.array <- ncvar_get(nc_data, "fPAR")
#check dimensions
dim(fPAR.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "fPAR", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and fPAR 

#Change the fillvalue to R standard NA
fPAR.array[fPAR.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  fPAR.slice <- fPAR.array[, , i]
  #create fPAR raster for the i'th month
  fPAR.raster <- raster(t(fPAR.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  fPAR.raster <- flip(fPAR.raster, direction='y')
  #resample the data to 0.25 resolution
  fPAR.raster<- resample(fPAR.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempfPAR.dt<- setDT(as.data.frame(rasterToPoints(fPAR.raster)))
  #rename columns
  setnames(TempfPAR.dt, old=c("layer"), new=c("fPAR"))
  #add the year and month of the fPAR data
  TempfPAR.dt$Year = timeseq$Years[i]
  TempfPAR.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    fPAR.dt<- TempfPAR.dt
  }
  else{
    fPAR.dt <- rbind(fPAR.dt, TempfPAR.dt)
  }
}
#Clean workplace
rm("TempfPAR.dt","fPAR.slice")

#Reorder the columns
fPAR.dt<-fPAR.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(fPAR.dt)/3
#Check the data with plot
plot(fPAR.raster)
Test<-fPAR.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,fPAR)]),
     main= "Fraction of Absorbed Photosynthetically Active Radiation in %",col=mycolors)
fPAR.raster

#Check for obvious data errors 
fPAR.dt[, .N, by = .(Year)] 
fPAR.dt[, .N, by = .(Month)] 

#***********************************#
#           LAI                     #
#***********************************#
#import and process netcdf 
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/LAIfPAR")
#open data
nc_data <- nc_open('MODIS_MOD15A2H_LAIfPAR_Global_monmean.nc_remap0.25.2003.2018.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('LAI_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
LAI.array <- ncvar_get(nc_data, "LAI")
#check dimensions
dim(LAI.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "LAI", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and LAI 

#Change the fillvalue to R standard NA
LAI.array[LAI.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  LAI.slice <- LAI.array[, , i]
  #create LAI raster for the i'th month
  LAI.raster <- raster(t(LAI.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  LAI.raster <- flip(LAI.raster, direction='y')
  #resample the data to 0.25 resolution
  LAI.raster<- resample(LAI.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempLAI.dt<- setDT(as.data.frame(rasterToPoints(LAI.raster)))
  #rename columns
  setnames(TempLAI.dt, old=c("layer"), new=c("LAI"))
  #add the year and month of the LAI data
  TempLAI.dt$Year = timeseq$Years[i]
  TempLAI.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    LAI.dt<- TempLAI.dt
  }
  else{
    LAI.dt <- rbind(LAI.dt, TempLAI.dt)
  }
}
#Clean workplace
rm("TempLAI.dt","LAI.slice")

#Reorder the columns
LAI.dt<-LAI.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(LAI.dt)/3
#Check the data with plot
plot(LAI.raster)
Test<-LAI.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,LAI)]), main= "Leaf Area Index in mÂ²/mÂ²", col=mycolors)
LAI.raster

#Check for obvious data errors 
LAI.dt[, .N, by = .(Year)] 
LAI.dt[, .N, by = .(Month)] 


#**********************************#
#           Land cover             #
#**********************************#

#**********************************#
#  Tree cover evergreen broadleaf  #
#**********************************#
#read csv
LC_TreeBE<-fread("~/GLM-Drivers-of-Fire/Data/Land cover/LC_TreeBE.csv",
                 header = T, sep = ",", dec = ".")
head(LC_TreeBE)
Test<-LC_TreeBE[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_TreeBE)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_TreeBE[, .N, by = .(Year)] 
LC_TreeBE[, .N, by = .(Month)] 
colSums(is.na(LC_TreeBE))

#**********************************#
#  Tree cover evergreen needleleaf #
#**********************************#
#*
#read csv
LC_TreeNE<-fread("~/GLM-Drivers-of-Fire/Data/Land cover/LC_TreeNE.csv",
                 header = T, sep = ",", dec = ".")
head(LC_TreeNE)
#Reorder the columns
LC_TreeNE<-LC_TreeNE[, c(1,2,4,5,3)]
Test<-LC_TreeNE[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_TreeNE)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_TreeNE[, .N, by = .(Year)] 
LC_TreeNE[, .N, by = .(Month)] 
colSums(is.na(LC_TreeNE))

#**********************************#
#  Tree cover deciduous broadleaf #
#**********************************#
#*
#read csv
LC_TreeBD<-fread("~/GLM-Drivers-of-Fire/Data/Land cover/LC_TreeBD.csv",
                 header = T, sep = ",", dec = ".")
head(LC_TreeBD)
Test<-LC_TreeBD[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_TreeBD)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_TreeBD[, .N, by = .(Year)] 
LC_TreeBD[, .N, by = .(Month)] 
colSums(is.na(LC_TreeBD))

#**********************************#
#  Tree cover deciduous needleleaf #
#**********************************#
#*
#read csv
LC_TreeND<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_TreeND.csv",
                 header = T, sep = ",", dec = ".")
head(LC_TreeND)
Test<-LC_TreeND[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_TreeND)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_TreeND[, .N, by = .(Year)] 
LC_TreeND[, .N, by = .(Month)] 
colSums(is.na(LC_TreeND))

#**********************************#
#  Shrub cover evergreen broadleaf #
#**********************************#
#*

#read csv
LC_ShrubBE<-fread("~/GLM-Drivers-of-Fire/Data/Land cover/LC_ShrubBE.csv",
                  header = T, sep = ",", dec = ".")
head(LC_ShrubBE)
Test<-LC_ShrubBE[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_ShrubBE)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_ShrubBE[, .N, by = .(Year)] 
LC_ShrubBE[, .N, by = .(Month)] 
colSums(is.na(LC_ShrubBE))

#**********************************#
#  Shrub cover evergreen needleaf #
#**********************************#
#*
#read csv
LC_ShrubNE<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_ShrubNE.csv",
                  header = T, sep = ",", dec = ".")
head(LC_ShrubNE)
Test<-LC_ShrubNE[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_ShrubNE)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_ShrubNE[, .N, by = .(Year)] 
LC_ShrubNE[, .N, by = .(Month)] 
colSums(is.na(LC_ShrubNE))

#**********************************#
#  Shrub cover deciduous broadleaf #
#**********************************#
#*
#read csv
LC_ShrubBD<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_ShrubBD.csv",
                  header = T, sep = ",", dec = ".")
head(LC_ShrubBD)
Test<-LC_ShrubBD[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_ShrubBD)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_ShrubBD[, .N, by = .(Year)] 
LC_ShrubBD[, .N, by = .(Month)] 
colSums(is.na(LC_ShrubBD))

#**********************************#
#           Croplands              #
#**********************************#
#read csv
LC_Crop<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_Crop.csv",
               header = T, sep = ",", dec = ".")
head(LC_Crop)
Test<-LC_Crop[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Crop)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_Crop[, .N, by = .(Year)] 
LC_Crop[, .N, by = .(Month)] 
colSums(is.na(LC_Crop))

#**********************************#
#           Herbs                  #
#**********************************#   
#*     
#read csv
LC_Herbs<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_Herbs.csv",
                header = T, sep = ",", dec = ".")
head(LC_Herbs)
Test<-LC_Herbs[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Herbs)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_Herbs[, .N, by = .(Year)] 
LC_Herbs[, .N, by = .(Month)] 
colSums(is.na(LC_Herbs))

#**********************************#
#           Bare land               #
#**********************************#

#read csv
LC_Bare<-fread("~/GLM-Drivers-of-Fire/Data/Land cover2/LC_Bare.csv",
               header = T, sep = ",", dec = ".")
head(LC_Bare)
Test<-LC_Bare[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Bare)]),main= "Fraction of land cover ",col=mycolors)

#Check for obvious data errors 
LC_Bare[, .N, by = .(Year)] 
LC_Bare[, .N, by = .(Month)] 
colSums(is.na(LC_Bare))


################
#Summarize all different tree cover types as tree
Tree.dt <- merge(GFED_Regions_Grid.dt,  LC_TreeBE , by= c("x","y","Year","Month") 
                 ,all.x=TRUE, allow.cartesian=TRUE)

Tree.dt <- merge(Tree.dt,  LC_TreeNE , by= c("x","y","Year","Month") 
                 ,all.x=TRUE, allow.cartesian=TRUE)

Tree.dt <- merge(Tree.dt,  LC_TreeBD , by= c("x","y","Year","Month") 
                 ,all.x=TRUE, allow.cartesian=TRUE)
Tree.dt <- merge(Tree.dt,  LC_TreeND, by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)

head(Tree.dt)
#Sum different tree covers // note that NA's are counted as 0
#We thereby assume that missing values mean no tree cover (0)
Tree.dt$LC_Tree <- rowSums(Tree.dt[ , c(7:10)], na.rm=TRUE)
head(Tree.dt)
#count missing values
colSums(is.na(Tree.dt))
#view location of missing values
plot(rasterFromXYZ(Tree.dt[is.na(LC_TreeBE),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

Test<-Tree.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Tree)]),main= "Test correctness ",col=mycolors)

#check valid range of data
range(Tree.dt$LC_Tree)

#drop all unncessary columns
Tree.dt<- Tree.dt[,c(1:4,12)]
head(Tree.dt)       

################
#Summarize all different Shrub cover types as Shrub
Shrub.dt <- merge(GFED_Regions_Grid.dt,  LC_ShrubBE , by= c("x","y","Year","Month") 
                  ,all.x=TRUE, allow.cartesian=TRUE)

Shrub.dt <- merge(Shrub.dt,  LC_ShrubNE , by= c("x","y","Year","Month") 
                  ,all.x=TRUE, allow.cartesian=TRUE)

Shrub.dt <- merge(Shrub.dt,  LC_ShrubBD , by= c("x","y","Year","Month") 
                  ,all.x=TRUE, allow.cartesian=TRUE)


head(Shrub.dt)
#Sum different Shrub covers // note that NA's are counted as 0
#We thereby assume that missing values mean no Shrub cover (0)
Shrub.dt$LC_Shrub <- rowSums(Shrub.dt[ , c(7:9)], na.rm=TRUE)
head(Shrub.dt)
#count missing values
colSums(is.na(Shrub.dt))
#view location of missing values
plot(rasterFromXYZ(Shrub.dt[is.na(LC_ShrubBE),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

Test<-Shrub.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Shrub)]),main= "Test correctness ",col=mycolors)

#check valid range of data
range(Shrub.dt$LC_Shrub)

#drop all unncessary columns
Shrub.dt<- Shrub.dt[,c(1:4,10)]
head(Shrub.dt)    

######################
#  Edit   Croplands   

#merge with final data set
Crop.dt <- merge(GFED_Regions_Grid.dt,  LC_Crop , by= c("x","y","Year","Month") 
                 ,all.x=TRUE, allow.cartesian=TRUE)

#count missing values
colSums(is.na(Crop.dt))
#view location of missing values
plot(rasterFromXYZ(Crop.dt[is.na(LC_Crop),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

head(Crop.dt)
#set NA's to 0
# we thereby assume that missing values mean no cropland (0)
Crop.dt$LC_Crop[is.na(Crop.dt$LC_Crop)] <- 0

#verify result
colSums(is.na(Shrub.dt))

Test<-Crop.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Crop)]),main= "Test correctness ",col=mycolors)

#check valid range of data
range(Crop.dt$LC_Crop)

#drop all unncessary columns
Crop.dt<- Crop.dt[,c(1:4,7)]
head(Crop.dt)  

######################
#  Edit   Herbs  

#merge with final data set
Herbs.dt <- merge(GFED_Regions_Grid.dt,  LC_Herbs , by= c("x","y","Year","Month") 
                  ,all.x=TRUE, allow.cartesian=TRUE)

#count missing values
colSums(is.na(Herbs.dt))
#view location of missing values
plot(rasterFromXYZ(Herbs.dt[is.na(LC_Herbs),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

head(Herbs.dt)
#set NA's to 0
# we thereby assume that missing values mean no cropland (0)
Herbs.dt$LC_Herbs[is.na(Herbs.dt$LC_Herbs)] <- 0

#verify result
colSums(is.na(Shrub.dt))

Test<-Herbs.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Herbs)]),main= "Test correctness ",col=mycolors)

#check valid range of data
range(Herbs.dt$LC_Herbs)

#drop all unncessary columns
Herbs.dt<- Herbs.dt[,c(1:4,7)]
head(Herbs.dt)  

######################
#  Edit   Bare  
#only needed to verify bare areas in other data sets were it's data was missing

#merge with final data set
Bare.dt <- merge(GFED_Regions_Grid.dt,  LC_Bare , by= c("x","y","Year","Month") 
                 ,all.x=TRUE, allow.cartesian=TRUE)

#count missing values
colSums(is.na(Bare.dt))
#view location of missing values
plot(rasterFromXYZ(Bare.dt[is.na(LC_Bare),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

head(Bare.dt)
#set NA's to 0
# we thereby assume that missing values mean no cropland (0)
Bare.dt$LC_Bare[is.na(Bare.dt$LC_Bare)] <- 0

#verify result
colSums(is.na(Shrub.dt))

Test<-Bare.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Bare)]),main= "Test correctness ",col=mycolors)

#check valid range of data
range(Bare.dt$LC_Bare)

#drop all unncessary columns
Bare.dt<- Bare.dt[,c(1:4,7)]
head(Bare.dt)  

#-------------------
#*************************************************************************#
#                       Anthropogenic data                                #
#*************************************************************************#


#*******************#
#   Road Density    #
#*******************#
#import and process netcdf Road Density data from Grip
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/RoadDensity")
#open data
nc_data <- nc_open('grip4_total_dens_m_km2_0d2_selvar.nc.2003.2018.remap.0.25.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('CCI_RoadDensity_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
t

# store the fire ignitions data in a 3-dimensional array
RoadDensity.array <- ncvar_get(nc_data, "road_density")
#check dimensions
dim(RoadDensity.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "road_density", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and RoadDensity 

#Change the fillvalue to R standard NA
RoadDensity.array[RoadDensity.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2018,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  RoadDensity.slice <- RoadDensity.array[, , i]
  #create RoadDensity raster for the i'th month
  RoadDensity.raster <- raster(t(RoadDensity.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  RoadDensity.raster <- flip(RoadDensity.raster, direction='y')
  #resample the data to 0.25 resolution
  RoadDensity.raster<- resample(RoadDensity.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempRoadDensity.dt<- setDT(as.data.frame(rasterToPoints(RoadDensity.raster)))
  #rename columns
  setnames(TempRoadDensity.dt, old=c("layer"), new=c("RoadDensity"))
  #add the year and month of the RoadDensity data
  TempRoadDensity.dt$Year = timeseq$Years[i]
  TempRoadDensity.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    RoadDensity.dt<- TempRoadDensity.dt
  }
  else{
    RoadDensity.dt <- rbind(RoadDensity.dt, TempRoadDensity.dt)
  }
}
#Clean workplace
rm("TempRoadDensity.dt","RoadDensity.slice")

#Reorder the columns
RoadDensity.dt<-RoadDensity.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(RoadDensity.dt)/3
#Check the data with plot
plot(RoadDensity.raster)
Test<-RoadDensity.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,RoadDensity)]),main= "Road density in m/km^2",
     col=mycolors)
plot(rasterFromXYZ(Test[RoadDensity<=1000,.(x, y,RoadDensity)]),
     main= "Road density in m/km^2", col=mycolors)

#Check for obvious data errors 
RoadDensity.dt[, .N, by = .(Year)] 
RoadDensity.dt[, .N, by = .(Month)] 


#***********************************#
#   Distance to Urban population    #
#***********************************#
#import and process netcdf Distance to Urban population data from Grip
#import data
#set path to nc data
setwd("~/GLM-Drivers-of-Fire/Data/Hyde")
#open data
nc_data <- nc_open('HYDE_urbc_distance_T0_2001_2017_Res083WGS84.nc_monthly.2003.2017.remap0.25.chname.nc4c.nc')
# Save the print(nc) dump to a text file
{
  sink('Hyde_DistUrbPop_2003-2018_metadata.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "year")
t

# store the fire ignitions data in a 3-dimensional array
DistUrbPop.array <- ncvar_get(nc_data, "Distance_to_populated_areas")
#check dimensions
dim(DistUrbPop.array)

#Retrieve the fillvalue of the precipitation data
fillvalue <- ncatt_get(nc_data, "Distance_to_populated_areas", "_FillValue")
fillvalue
#close the data
nc_close(nc_data) 

#Process the data
#We'd like to have a data table with lon, lat, time (year, month) and DistUrbPop 

#Change the fillvalue to R standard NA
DistUrbPop.array[DistUrbPop.array == fillvalue$value] <- NA

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

#get the data month by month
for (i in seq_along(t)) {
  #for (i in 1:3) { 
  print(i)
  #retrieve the i'th month; slice data into monthly portions
  DistUrbPop.slice <- DistUrbPop.array[, , i]
  #create DistUrbPop raster for the i'th month
  DistUrbPop.raster <- raster(t(DistUrbPop.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  DistUrbPop.raster <- flip(DistUrbPop.raster, direction='y')
  #resample the data to 0.25 resolution
  DistUrbPop.raster<- resample(DistUrbPop.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempDistUrbPop.dt<- setDT(as.data.frame(rasterToPoints(DistUrbPop.raster)))
  #rename columns
  setnames(TempDistUrbPop.dt, old=c("layer"), new=c("DistUrbPop"))
  #add the year and month of the DistUrbPop data
  TempDistUrbPop.dt$Year = timeseq$Years[i]
  TempDistUrbPop.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    DistUrbPop.dt<- TempDistUrbPop.dt
  }
  else{
    DistUrbPop.dt <- rbind(DistUrbPop.dt, TempDistUrbPop.dt)
  }
}
#Clean workplace
rm("TempDistUrbPop.dt","DistUrbPop.slice")

#Reorder the columns
DistUrbPop.dt<-DistUrbPop.dt[, c(1,2,4,5,3)]

#Check that number of rows are correct
nrow(DistUrbPop.dt)/3
#Check the data with plot
plot(DistUrbPop.raster)
Test<-DistUrbPop.dt[Year==2012,]
plot(rasterFromXYZ(Test[,.(x, y,DistUrbPop)]),main= "Distance to populated areas in m",col=mycolors)
DistUrbPop.raster

#Check for obvious data errors 
DistUrbPop.dt[, .N, by = .(Year)] 
DistUrbPop.dt[, .N, by = .(Month)] 

#************************#
#   Population Density   #
#************************#
#import and process population density data from Hyde3.2
#set path
setwd("~/GLM-Drivers-of-Fire/Data")
data.dir <- 'Hyde Pop Density'
#read all asc files
PopDensity.files <- list.files(path = data.dir , pattern = "\\.asc$", full.names = T)
#stack asc files
PopDensity.stack <- stack(PopDensity.files)
#view files
PopDensity.stack

#get length of the time frame
t<-dim(PopDensity.stack)[3]

#create a time sequence of the data
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=1))

#get the PopDensity  year by year
for (i in seq_along(1:t)) {
  #for (i in 1:12) { 
  print(i)
  #retrieve the i'th year of the raster; slice data into yearly portions
  PopDensity.raster<-subset(PopDensity.stack,i)
  
  #resample the data to 0.25 resolution
  PopDensity.raster <- resample(PopDensity.raster, rtemp,  method = 'ngb')
  
  #convert raster to data table
  TempPopDensity.dt<- setDT(as.data.frame(rasterToPoints(PopDensity.raster)))
  #rename columns
  setnames(TempPopDensity.dt, old=3, new=c("PopDensity"))
  #add the year of the data
  TempPopDensity.dt$Year = timeseq$Years[i]
  
  #Append the data to the final data table
  if(i==1){
    PopDensity.dt<- TempPopDensity.dt
  }
  else{
    PopDensity.dt <- rbind(PopDensity.dt, TempPopDensity.dt)
  }
}
#Reorder the columns
PopDensity.dt<-PopDensity.dt[, c(1,2,4,3)]

#view data
head(PopDensity.dt)
Test<-PopDensity.dt[Year==2016,]
plot(rasterFromXYZ(Test[,.(x, y, PopDensity)]), main= "PopDensity in inhabitants per kmÂ²", col=mycolors)
#log transform to better see differences in the data 
Test$PopDensity_log <- log(Test$PopDensity+1)
plot(rasterFromXYZ(Test[,.(x,y, PopDensity_log)]), col=mycolors)

#Check for obvious data errors 
PopDensity.dt[, .N, by = .(Year)] 

#Clean workplace
rm("TempPopDensity.dt", PopDensity.stack, PopDensity.files )


#------------------------
#*************************************************************************#
#            Filter /Data sets to group Global Fire Atlas data by         #
#*************************************************************************#

#1. GFED regions
#set path
setwd("~/GLM-Drivers-of-Fire/Data/GlobalFireAtlas/GFED4_1s")
#view structure of hdf5 file
h5ls("GFED4.1s_2012.hdf5")

#read hdf5 file
GFED_Regions_Base<-h5read("GFED4.1s_2012.hdf5","ancill")                   
str(GFED_Regions_Base)

#read long and lat for GFED_Regions_Grids
#LonLat_GFED_Regions_Grid<-h5read("GFED_Regions_Grid/GFED4.1s_2010.hdf5","lon") #matches with grid!

#Create a grid to match the data on
rtemp <- raster(xmn=-180,xmx=180,ymn=-90,ymx=90, res=1/4)
#convert raster to data table
Grid<- setDT(as.data.frame(rasterToPoints(rtemp)))

#retrieve GFED regions information
GFED_Regions_Base<-GFED_Regions_Base[[1]]

#change the output from a matrix into the form long lat data
#retrieve every single column from the dataset
for (k in 1:720) { 
  #retrieve the i'th column
  tempGFED_Regions.dt<-setDT(as.data.frame(GFED_Regions_Base[,k]))
  #change column name to a standard
  setnames(tempGFED_Regions.dt, old=1, new=c("GFED_Regions"))
  
  #Append the data to the final data table
  if(k==1){
    GFED_Regions<- tempGFED_Regions.dt
  }
  else{
    GFED_Regions<- rbind(GFED_Regions, tempGFED_Regions.dt)
  }
} 

#add lan and lon data from grid; grid matches with lon lat data from hdf5 file
GFED_Regions<-cbind(Grid, GFED_Regions)

#change raw format to numeric
GFED_Regions$GFED_Regions<-as.numeric(GFED_Regions$GFED_Regions)
str(GFED_Regions)

#The dataset comes without a description for the regions
#-> create regions description dataset (verified via research)
Regions <- c(0:14)
GFED_Regions_Desc <- c("Sea",
                       "BONA",
                       "TENA",
                       "CEAM",
                       "NHSA",
                       "SHSA",
                       "EURO",
                       "MIDE",
                       "NHAF",
                       "SHAF",
                       "BOAS",
                       "CEAS",
                       "SEAS",
                       "EQAS",
                       "AUST"
)
Regions_code <- setDT(data.frame(Regions,GFED_Regions_Desc))

#Merge region descriptions to GFED Regions
GFED_Regions <- merge(GFED_Regions, Regions_code , by.x= "GFED_Regions", by.y="Regions",all.x=TRUE, allow.cartesian=TRUE)

#change char format to factor
GFED_Regions$GFED_Regions_Desc<-as.factor(GFED_Regions$GFED_Regions_Desc)

#Clean workplace
rm("tempGFED_Regions.dt","GFED_Regions_Base")

#check the result
plot(rasterFromXYZ(GFED_Regions[,.(x,y, as.numeric(GFED_Regions))]))

#Drop all unnessecary data cells where no fire is possible
#GFED_regions where fire is possible (no sea & poles)
GFED_Regions<-GFED_Regions[GFED_Regions!=0,]
plot(rasterFromXYZ(GFED_Regions[,.(x,y, as.numeric(GFED_Regions))]))


#2. Continents
#set path
setwd("~/GLM-Drivers-of-Fire/Data")
#set path to file
file <- "World_Continents/Continents.shp"
#read shapefile
continent_shp  <- shapefile(file)

#view projection and class
projection(continent_shp )
class(continent_shp )
names(continent_shp)
#create continent raster
continent.raster <- rasterize(continent_shp, rtemp, field="FID")
#check data by viewing it
plot(continent.raster)
#convert raster to data table
continent.dt<- setDT(as.data.frame(rasterToPoints(continent.raster)))
#rename column
setnames(continent.dt, old=c("layer"), new=c("FID"))

#Add the name of the continents to the data set
Continent_codes <- unique(as.data.table(continent_shp[,c("FID","CONTINENT")]))
continent.dt <- merge(continent.dt,  Continent_codes , by= c("FID") ,all.x=TRUE, allow.cartesian=TRUE)
#change char variable to factor
continent.dt[,"CONTINENT"] <- lapply(continent.dt[,"CONTINENT"], as.factor)
#rename column
setnames(continent.dt, old=c("CONTINENT"), new=c("Continent"))
#drop unnecessary column
continent.dt[,"FID":=NULL]

#Clean workplace
rm("Continent_codes", "continent_shp" )

#Check the data with plot
plot(rasterFromXYZ(continent.dt[,.(x, y,as.numeric(Continent))]),col=mycolors)


#3. Biomes (Olson 2001)
#set path
setwd("~/GLM-Drivers-of-Fire/Data")
#set path to file
file <- "BiomesOlson/wwf_terr_ecos.shp"
#read shapefile
biomes_shp  <- shapefile(file)
names(biomes_shp)

#view projection and class
projection(biomes_shp )
class(biomes_shp )
names(biomes_shp)
biomes_shp$BIOME

#create biomes raster
biomes.raster <- rasterize(biomes_shp, rtemp, field="BIOME")
#check data by viewing it
plot(biomes.raster)
#convert raster to data table
biomes.dt<- setDT(as.data.frame(rasterToPoints(biomes.raster)))
#rename column
setnames(biomes.dt, old=c("layer"), new=c("Biomes"))
#Delete rows that have no valid Biome ID
biomes.dt<-biomes.dt[Biomes %in% c(1:14)]
#Check the result
plot(rasterFromXYZ(biomes.dt[,.(x,y, Biomes)]))

#The biomes dataset comes without a description for the biomes
#-> create biomes description dataset (verified via research)
Biomes <- c(1:14)
Biomes_Desc <- c("Tropical and subtropical moist broadleaf forests", 
                 "Tropical and subtropical dry broadleaf forests",
                 "Tropical and subtropical coniferous forests",
                 "Temperate broadleaf and mixed forests",
                 "Temperate Coniferous Forest",
                 "Boreal forests / Taiga",
                 "Tropical and subtropical grasslands, savannas and shrublands",
                 "Temperate grasslands, savannas and shrublands",
                 "Flooded grasslands and savannas",
                 "Montane grasslands and shrublands",
                 "Tundra",
                 "Mediterranean Forests, woodlands and scrubs",
                 "Deserts and xeric shrublands",
                 "Mangroves"
)
biomes_code <- setDT(data.frame(Biomes, Biomes_Desc))

#Merge biome descriptions to biomes
biomes.dt <- merge(biomes.dt, biomes_code , by= "Biomes",all.x=TRUE, allow.cartesian=TRUE)

#Clean workplace
rm("biomes_code","Biomes_Desc","Biomes","biomes_shp")

#Reorder the columns
biomes.dt<-biomes.dt[,c(2,3,1,4)]

#Check the data with plot
plot(rasterFromXYZ(biomes.dt[,.(x, y,as.numeric(Biomes))]),col=mycolors)

#--------

#*******************************************************************************#
#             Create base grid for all data from GFED regions                   #
#*******************************************************************************#
#* All data will be joined to this base grid

#Create base grid to join all data from 2003 - 2016 on using GFED regions
#Note that GFED regions where filtered on relevant land bodies for fire (so the sea & the poles 
#are excluded on this base grid)

#duplicate the base grid for each month and year for the time range 2003 to 2016 
timeseq<-data.table(Years=rep(seq(2003,2016,1), times=1, each=12),Month=rep(seq(1,12,1), times=1, each=1))

for (i in seq_along(1:nrow(timeseq))) {
  #for (i in 1:3) { 
  print(i)
  TempGFED_Regions_Grid.dt<- GFED_Regions
  #add the year and month of the data
  TempGFED_Regions_Grid.dt$Year = timeseq$Years[i]
  TempGFED_Regions_Grid.dt$Month = timeseq$Month[i]
  
  #Append the data to the final data table
  if(i==1){
    GFED_Regions_Grid.dt<- TempGFED_Regions_Grid.dt
  }
  else{
    GFED_Regions_Grid.dt <- rbind(GFED_Regions_Grid.dt, TempGFED_Regions_Grid.dt)
  }
}
#Clean workplace
rm("TempGFED_Regions_Grid.dt")

#Reorder the columns
GFED_Regions_Grid.dt<-GFED_Regions_Grid.dt[, c(2,3,5,6,1,4)]

#Verify correctness of the grid
GFED_Regions_Grid.dt[, .N, by = .(Year)]


#--------
#**************************#
# Create the final dataset #
#**************************#
#Create the final dataset
##########
#1Add Tmax 
dim(tmax.dt)
GlobalDriversFire.dt <- merge(GFED_Regions_Grid.dt,  tmax.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(TMax),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,TMax)]),main= "Test correctness ",col=mycolors)

##########
#Add DTR 
dim(DTR.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  DTR.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(DTR),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,DTR)]),main= "Test correctness ",col=mycolors)

##########
#Add NrWetDays 
dim(NrWetDays.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  NrWetDays.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(NrWetDays),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,NrWetDays)]),main= "Test correctness ",col=mycolors)

##########
#Add PET 
dim(PET.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  PET.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(PET),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,PET)]),main= "Test correctness ",col=mycolors)


##########
#Add precipitation 
dim(precip.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  precip.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Precipitation),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,Precipitation)]),main= "Test correctness ",col=mycolors)

##########
#Add RoadDensity 
dim(RoadDensity.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  RoadDensity.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(RoadDensity),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,RoadDensity)]),main= "Test correctness ",col=mycolors)

##########
#Add Distance to Urbance Population 
dim(DistUrbPop.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  DistUrbPop.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#3 months of data are missing in 2016
colSums(is.na(GlobalDriversFire.dt[Year==2016,]))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire2.dt[is.na(DistUrbPop)& Year==2015,.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,DistUrbPop)]),main= "Test correctness ",col=mycolors)

##########
#Add PopDensity 
dim(PopDensity.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  PopDensity.dt , 
                              by= c("x","y","Year") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(PopDensity),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#delete all rows with at least one NA
GlobalDriversFire.dt<-na.omit(GlobalDriversFire.dt)
#verify it
colSums(is.na(GlobalDriversFire.dt))
Test<-GlobalDriversFire.dt[Year==2003,]
plot(rasterFromXYZ(Test[,.(x, y,PopDensity)]),main= "Test correctness ",col=mycolors)


##########
#Add LAI
dim(LAI.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  LAI.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(LAI),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#through seasonal affects there is no information on LAI e.g. northern hemisphere in winter
#keep NA's for now
Test<-GlobalDriversFire.dt[Month==05 & Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,LAI)]),main= "Test correctness ",col=mycolors)

##########
#Add fPAR
dim(fPAR.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  fPAR.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(fPAR),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#through seasonal affects there is no information on fPAR e.g. northern hemisphere in winter
#keep NA's for now
Test<-GlobalDriversFire.dt[Month==05 & Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,fPAR)]),main= "Test correctness ",col=mycolors)

##########
#Add Tree cover
#Missing values were set to 0, we therby assume that missing values equal no tree cover (0)
dim(Tree.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Tree.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(LC_Tree),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Tree)]),main= "Test correctness ",col=mycolors)

##########
#Add Shrub cover
#Missing values were set to 0, we thereby assume that missing values equal no Shrub cover (0)
dim(Shrub.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Shrub.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(LC_Shrub),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Shrub)]),main= "Test correctness ",col=mycolors)

##########
#Add Crop cover
#Missing values were set to 0, we thereby assume that missing values equal no Crop cover (0)
dim(Crop.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Crop.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(LC_Crop),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Crop)]),main= "Test correctness ",col=mycolors)

##########
#Add Herbs cover
#Missing values were set to 0, we thereby assume that missing values equal no Herbs cover (0)
dim(Herbs.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Herbs.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,LC_Herbs)]),main= "Test correctness ",col=mycolors)

##########
#Add Bare cover

#only needed to verify bare areas in other data sets were we set NA's to 0 

#Missing values were set to 0, we thereby assume that missing values equal no Bare cover (0)
dim(Bare.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Bare.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[LC_Bare>0.8,.(x, y,LC_Tree)]),main= "Test correctness ",col=mycolors)

#verify that setting NA's to 0 was correct 
plot(rasterFromXYZ(Test[LC_Bare>0.8,.(x, y,LC_Tree)]),main= "Test correctness ",col=mycolors)
plot(rasterFromXYZ(Test[LC_Bare>0.8,.(x, y,LC_Shrub)]),main= "Test correctness ",col=mycolors)
plot(rasterFromXYZ(Test[LC_Bare>0.8,.(x, y,LC_Crop)]),main= "Test correctness ",col=mycolors)
plot(rasterFromXYZ(Test[LC_Bare>0.8,.(x, y,LC_Herbs)]),main= "Test correctness ",col=mycolors)

#*******************#
# Add data from GFED #
#*******************#
#*
##########
#Add Fire Size
dim(Size.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Size.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Size),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#as GFED data set contains only data of fire, it is fair to assume that no data means no fire
#thus set NA's after merge to 0
GlobalDriversFire.dt$Size[is.na(GlobalDriversFire.dt$Size)] <- 0

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,Size)]),main= "Test correctness ",col=mycolors)

##########
#Add Fire Duration
dim(Duration.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Duration.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Duration),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#as GFED data set contains only data of fire, it is fair to assume that no data means no fire
#thus set NA's after merge to 0
GlobalDriversFire.dt$Duration[is.na(GlobalDriversFire.dt$Duration)] <- 0

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,Duration)]),main= "Test correctness ",col=mycolors)


##########
#Add Fire Ignitions
dim(Ignitions.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Ignitions.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Ignitions),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#as GFED data set contains only data of fire, it is fair to assume that no data means no fire
#thus set NA's after merge to 0
GlobalDriversFire.dt$Ignitions[is.na(GlobalDriversFire.dt$Ignitions)] <- 0

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,Ignitions)]),main= "Test correctness ",col=mycolors)


#********************#
# Data layers        #
#********************#
#*
##########
#Add IDL layer
dim(IDL.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  IDL.dt , 
                              by= c("x","y") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(IDL_Layer),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

#For the indigenous peoples land (IDL_Layer) change NA to None Indigenous Land
#to do this change factor to char
GlobalDriversFire.dt[,"IDL_Layer"] <- lapply(GlobalDriversFire.dt[,"IDL_Layer"], as.character)
GlobalDriversFire.dt$IDL_Layer[is.na(GlobalDriversFire.dt$IDL_Layer)] <- "None Indigenous Land"
#change char back to factor
GlobalDriversFire.dt[,"IDL_Layer"] <- lapply(GlobalDriversFire.dt[,"IDL_Layer"], as.factor)

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,as.numeric(IDL_Layer))]),main= "Test correctness ",col=mycolors)


##########
#Add biomes layer
dim(biomes.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  biomes.dt , 
                              by= c("x","y") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Biomes),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

dim(GlobalDriversFire.dt)
#delete all rows with all choosen entries being NA
GlobalDriversFire.dt <- GlobalDriversFire.dt[rowSums(is.na(GlobalDriversFire.dt[,c(25,26)])) 
                                             != ncol(GlobalDriversFire.dt[,c(25,26)]), ] 
#verify it
colSums(is.na(GlobalDriversFire.dt))

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,as.numeric(Biomes))]),main= "Test correctness ",col=mycolors)


#********************#
# Data layers        #
#********************#
#*
##########
#Add continent layer
dim(continent.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  continent.dt , 
                              by= c("x","y") ,all.x=TRUE, allow.cartesian=TRUE)
#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))
#view location of missing values
plot(rasterFromXYZ(GlobalDriversFire.dt[is.na(Continent),.(x, y,GFED_Regions)]),
     main= "Missing values ",col="black")

dim(GlobalDriversFire.dt)
#delete all rows with all choosen entries being NA
GlobalDriversFire.dt <- GlobalDriversFire.dt[rowSums(is.na(GlobalDriversFire.dt[,c(27)])) 
                                             != ncol(GlobalDriversFire.dt[,c(27)]), ] 
#verify results
colSums(is.na(GlobalDriversFire.dt))
#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,as.numeric(Continent))]),main= "Test correctness ",col=mycolors)


#******************************************#
# Add data from GFED 4.1s with small fires #
#******************************************#
#*
##########
#Add Fire BurnedFraction
dim(BurntFraction.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  BurntFraction.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)

#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,BurnedFraction)]),main= "Test correctness ",col=mycolors)

##########
#Add Fire Emissions
dim(Emissions.dt)
GlobalDriversFire.dt <- merge(GlobalDriversFire.dt,  Emissions.dt , 
                              by= c("x","y","Year","Month") ,all.x=TRUE, allow.cartesian=TRUE)

#view data after merge
head(GlobalDriversFire.dt)
#count missing values
colSums(is.na(GlobalDriversFire.dt))

#check data
Test<-GlobalDriversFire.dt[ Year==2006,]
plot(rasterFromXYZ(Test[,.(x, y,Emissions)]),main= "Test correctness ",col=mycolors)

#create data set with only presence data of fire // GFED4.0 (no small fires)
GFED4.0_Presence.dt<-GlobalDriversFire.dt[Size!=0 & Ignitions!=0 & Duration !=0,]
fwrite(GFED4.0_Presence.dt, "GFED4.0_Presence2003-2016.csv")

#create data set with only presence data of fire // GFED4.1 (with small fires)
GFED4.1s_Presence.dt<-GlobalDriversFire.dt[BurnedFraction!=0 & Emissions!=0,]
fwrite(GFED4.1s_Presence.dt, "GFED4.1s_Presence2003-2016.csv")

#----------------
#only relevant for exporting data
#Drop description for faster save and write 
GlobalDriversFire.dt<-GlobalDriversFire.dt[,-c(6,26)]


Test<-GlobalDriversFire.dt[ Year==2006,]
fwrite(Test, "GlobalDriversFire2016.csv")

#save data into csv
setwd("~/GLM-Drivers-of-Fire/ProcessedData")
fwrite(GlobalDriversFire.dt, "GlobalDriversFire2003-2016.csv")
#reading data in works correctly
#read csv
Verify<-fread("~/GLM-Drivers-of-Fire/ProcessedData/GlobalDriversFire2006.csv", 
              header = T, sep = ",", dec = ".")
head(Verify)
dim(Test)
dim(Verify)
#count missing values
colSums(is.na(Verify))
colSums(is.na(Test))
