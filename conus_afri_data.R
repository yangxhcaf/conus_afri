
#packages needed
library(dplyr)
library(tidyr)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(scales)
library(sf)
library(bfastSpatial)
library(greenbrown)
### need to: find a way to calculate cv and temporal trend for graphics in a clean ggplot
### graphics of summary stats withine ach ecoregion (according to their scale) and within the entire western US rangelands

########CONUS AFRI

# basic importing of ecoegions --------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)


# temporal data sgs -----------------------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 5000000] <- NA
plot (AFRI_RegionSite_Raster)
extent(AFRI_RegionSite_Raster)

#zoom 
extent_region_1_semi_arid <- extent(-106.7361 , -99.0754 , 30.70696 , 43.76846) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_1_semi_arid)
plot(AFRI_RegionSite_Raster)

#temporal data
#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack>= 65535] <- NA
filenames_stack_sgs = crop(filenames_stack, extent_region_1_semi_arid) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_sgs, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_sgs<-crop(filenames_stack_sgs,ex)
new_all_years_sgs<-mask(filenames_stack_sgs,AFRI_RegionSite_Raster)
plot(new_all_years_sgs)
title("CONUS Semi-arid Steppe 1986")

#summary stats
time_series_mean_sgs<-cellStats(new_all_years_sgs, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_sgs<-cellStats(new_all_years_sgs, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_sgs<-as.data.frame(time_series_mean_sgs)
#temporal variability
sd(data_frame_time_series_mean_sgs$time_series_mean) #454.7334
#long-term mean
mean(data_frame_time_series_mean_sgs$time_series_mean) #2132.732
min(data_frame_time_series_mean_sgs$time_series_mean) #1225.721
max(data_frame_time_series_mean_sgs$time_series_mean) #2984.161

#relative pulse
(2984.161-2132.732)/2132.732 #0.4
#relative decline
(2132.732-1225.721)/2132.732 #0.43

#sd - spatial variability in npp
data_frame_time_series_sgs_sd<-as.data.frame(time_series_sd_sgs)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_sd$time_series_sd_sgs) #691.0563 #average spatial variability

#summaries for graphics
#mean by pixel
sgs_spatiotemporal_mean<-summaryBrick(new_all_years_sgs, mean, na.rm=TRUE)

#standard dev
sgs_spatiotemporal_sd<-summaryBrick(new_all_years_sgs, sd, na.rm=TRUE)

#temporal trend
trend_sgs<-TrendRaster(new_all_years_sgs, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)

# temporal data california ------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)

#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to just california grassland
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 2000000] <- NA
plot (AFRI_RegionSite_Raster)
#extent(AFRI_RegionSite_Raster)

#define the extent
extent_region_cali <- extent(-123.6578 , -115.8161, 32.39994 , 41.22494 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_cali ) 

#stacked
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_cali = crop(filenames_stack,extent_region_cali) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_cali, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_cali<-crop(filenames_stack_cali,ex)
new_all_years_cali<-mask(filenames_stack_cali,AFRI_RegionSite_Raster)
plot(new_all_years_cali)

######## summary stats
time_series_mean_cali<-cellStats(new_all_years_cali, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_cali<-cellStats(new_all_years_cali, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_cali<-as.data.frame(time_series_mean_cali) #mean regional npp for each year
#long-term mean npp
mean(data_frame_time_series_mean_cali$time_series_mean) #3473.923
#year with minimum npp
min(data_frame_time_series_mean_cali$time_series_mean) #2890.078
#year with maximum npp
max(data_frame_time_series_mean_cali$time_series_mean) #4121.861
#year to year variation
sd(data_frame_time_series_mean_cali$time_series_mean) #348.4559

#relative pulse
(4121.861-3473.923)/3473.923 #0.19
#relative decline
(3473.923-2890.078)/3473.923 #0.17

#sd - spatial variability in npp
data_frame_time_series_sgs_cali<-as.data.frame(time_series_sd_cali)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_cali$time_series_sd_cali) #1163.254 #average spatial variability

#summaries for graphics
#mean by pixel
cali_spatiotemporal_mean<-summaryBrick(new_all_years_cali, mean, na.rm=TRUE)

#standard dev
cali_spatiotemporal_sd<-summaryBrick(new_all_years_cali, sd, na.rm=TRUE)

#temporal trend
trend_cali<-TrendRaster(new_all_years_cali, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)


# temporal data hot deserts -------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)

#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#hot deserts
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 3000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 4000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_desert_1<- extent(-119.8845 , -99.26385  , 27.97906 ,  38.0031 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_desert_1)
plot(AFRI_RegionSite_Raster)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack>= 65535] <- NA
filenames_stack_hot_deserts = crop(filenames_stack, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_hot_deserts, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_hot_deserts<-crop(filenames_stack_hot_deserts,ex)
new_all_years_hot_deserts<-mask(filenames_stack_hot_deserts,AFRI_RegionSite_Raster)
plot(new_all_years_hot_deserts)

#summary stats
time_series_mean_hot_deserts<-cellStats(new_all_years_hot_deserts, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_hot_deserts<-cellStats(new_all_years_hot_deserts, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_hot_deserts<-as.data.frame(time_series_mean_hot_deserts)
# year to year variation
sd(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #146.0075
#long-term mean
mean(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #505.499
#minimum
min(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #243.3728
#maximum
max(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #779.5133

#relative pulse
(779.5133-505.499)/505.499 #0.54
#relative decline
(505.499-243.3728)/505.499 #0.52

#sd - spatial variability in npp
data_frame_time_series_sgs_sd<-as.data.frame(time_series_sd_hot_deserts)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_sd$time_series_sd_hot_deserts) #495.5031

#summaries for graphics
#mean by pixel
hot_deserts_spatiotemporal_mean<-summaryBrick(new_all_years_hot_deserts, mean, na.rm=TRUE)

#standard dev
hot_deserts_spatiotemporal_sd<-summaryBrick(new_all_years_hot_deserts, sd, na.rm=TRUE)

#temporal trend
#temporal trend
trend_hot_deserts<-TrendRaster(new_all_years_hot_deserts, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)

# temporal data cold deserts ----------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to region
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 2000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 3000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_cold_desert<- extent(-122.317, -103.774, 33.99615 ,  48.65204 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_cold_desert)
plot(AFRI_RegionSite_Raster)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_cold_deserts = crop(filenames_stack, extent_cold_desert) 
library(devtools)
install_github('dutri001/bfastSpatial')
library(bfastSpatial)
cold_deserts_spatiotemporal_mean<-summaryBrick(filenames_stack_cold_deserts, mean, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_mean)
cold_deserts_spatiotemporal_sd<-summaryBrick(filenames_stack_cold_deserts, sd, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_sd)

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_cold_deserts , "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_cold_deserts <-crop(filenames_stack_cold_deserts ,ex)
new_all_years_cold_deserts<-mask(filenames_stack_cold_deserts ,AFRI_RegionSite_Raster)
plot(new_all_years_cold_deserts)

#summary stats
time_series_mean_cold_deserts<-cellStats(new_all_years_cold_deserts, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_cold_deserts<-cellStats(new_all_years_cold_deserts, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_cold_deserts<-as.data.frame(time_series_mean_cold_deserts)
# year to year variation
sd(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #154.8888
#long-term mean
mean(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #993.7573
#minimum
min(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #727.2936
#maximum
max(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #1287.623

#relative pulse
(1287.623-993.7573)/993.7573 #0.30
#relative decline
(993.7573-727.2936)/993.7573 #0.27

#sd - spatial variability in npp
data_frame_time_series_cold_deserts_sd<-as.data.frame(time_series_sd_cold_deserts)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
#man spatial variability
mean(data_frame_time_series_cold_deserts_sd$time_series_sd_cold_deserts) #682.0946

#summaries for graphics
#mean by pixel
cold_desert_spatiotemporal_mean<-summaryBrick(new_all_years_cold_deserts, mean, na.rm=TRUE)

#standard dev
cold_deserts_spatiotemporal_sd<-summaryBrick(new_all_years_cold_deserts, sd, na.rm=TRUE)

#temporal trend
trend_cold_deserts<-TrendRaster(new_all_years_cold_deserts, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)
plot(cold_deserts_spatiotemporal_sd)
# temporal data northern mixed grass --------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"
#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

###
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 4000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 5000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_northern_mixed_prairie_1<- extent(-116, -97.48181 , 38 , 52.0) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_northern_mixed_prairie_1)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)
filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_northern_mixed = crop(filenames_stack, extent_northern_mixed_prairie_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_northern_mixed , "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_northern_mixed <-crop(filenames_stack_northern_mixed ,ex)
new_all_years_northern_mixed<-mask(filenames_stack_northern_mixed,AFRI_RegionSite_Raster)
plot(new_all_years_northern_mixed)

#summary stats
time_series_mean_northern_mixed<-cellStats(new_all_years_northern_mixed, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_northern_mixed<-cellStats(new_all_years_northern_mixed, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_northern_mixed<-as.data.frame(time_series_mean_northern_mixed)
# year to year variation
sd(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #271.4897
#long-term mean
mean(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #2401.067
#minimum
min(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #1802.697
#maximum
max(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #2813.676

#relative pulse
(2813.676-2401.067)/2401.0673 #0.17
#relative decline
(2401.067-1802.697)/2401.067 #0.25

#sd - spatial variability in npp
data_frame_time_series_northern_mixed_sd<-as.data.frame(time_series_sd_northern_mixed)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
#man spatial variability
mean(data_frame_time_series_northern_mixed_sd$time_series_sd_northern_mixed) #889.0735

#summaries for graphics
#mean by pixel
northern_mixed_desert_spatiotemporal_mean<-summaryBrick(new_all_years_northern_mixed, mean, na.rm=TRUE)

#standard dev
northern_mixed_deserts_spatiotemporal_sd<-summaryBrick(new_all_years_northern_mixed, sd, na.rm=TRUE)

#temporal trend
install.packages("greenbrown", repos="http://R-Forge.R-project.org",dependencies=TRUE)
install.packages("greenbrown_2.4.2.tar.gz", repos = NULL, type="source",dependencies=TRUE)
logo.trend <- raster.kendall(new_all_years_northern_mixed, p.value=TRUE, confidence=TRUE)
library(spatialEco)
library(greenbrown)
library(trend)
trend<-TrendRaster(new_all_years_northern_mixed, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)
plot(trend)

# all regions -------------------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#define extent
extent(AFRI_RegionSite_Raster)
extent_all<- extent(AFRI_RegionSite_Raster )

#conus data
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_all = crop(filenames_stack, extent_all)
plot(filenames_stack_all)

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_all , "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_all <-crop(filenames_stack_all ,ex)
new_all_years_all_sites<-mask(filenames_stack_all ,AFRI_RegionSite_Raster)

#mean by pixel
all_spatiotemporal_mean<-summaryBrick(new_all_years_all_sites, mean, na.rm=TRUE)

#standard dev
all_spatiotemporal_sd<-summaryBrick(new_all_years_all_sites, sd, na.rm=TRUE)

#temporal trend
trend_all<-TrendRaster(new_all_years_all_sites, start = c(1986, 1), freq = 1, method = c("AAT"), mosum.pval = 0.05, h = 0.15, breaks = 0)


# turning into derived data frame -----------------------------------------

site<-c("Northern Mixed grass","Cold Deserts","Hot deserts","California annuals","Semi-arid steppe")
mean_npp <-c("2401.067","993.7573","505.499","2890.078","2132.732")
temporal_sd <-c("271.4897","154.8888","146.0075","348.4559","454.7334")
npp_pulse<-c("0.17","0.30","0.54","0.19","0.4")
npp_minimum<-c("0.25","0.27","0.52","0.17","0.43")
spatial_var<-c("889.0735","682.0946","495.5031","1163.254","691.0563")

spatial_sumamries<-data.frame(site,mean_npp,temporal_sd,npp_pulse,npp_minimum,spatial_var)
str(spatial_sumamries)
spatial_sumamries$mean_npp <- as.numeric(as.character(spatial_sumamries$mean_npp))
spatial_sumamries$temporal_sd  <- as.numeric(as.character(spatial_sumamries$temporal_sd))
spatial_sumamries$npp_pulse  <- as.numeric(as.character(spatial_sumamries$npp_pulse))
spatial_sumamries$npp_minimum  <- as.numeric(as.character(spatial_sumamries$npp_minimum))
spatial_sumamries$spatial_var <- as.numeric(as.character(spatial_sumamries$spatial_var))
# high versus low npp -----------------------------------------------------

#semiarid low npp
year.2011<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2011.tif' 
year_2011_semi_arid=raster(year.2011)
year_2011_semi_arid[year_2011_semi_arid>= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, year_2011_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2011_semi_arid<-crop(year_2011_semi_arid,ex)
new_2011<-mask(year_2011_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("Lowest regional NPP: Semi-arid Steppe (2011)")

#semi-arid highest npp

year.1992<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1992.tif' 
year_1992_semi_arid=raster(year.1992)
year_1992_semi_arid[year_1992_semi_arid >= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, year_1992_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1992_semi_arid<-crop(year_1992_semi_arid,ex)
new_1992<-mask(year_1992_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("Highest regional NPP: Semi-arid Steppe (1992)")

stacked<-stack(year.2011,year.1992)
raster(stacked)
plot(stacked)
stacked[stacked>= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, stacked, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
stacked<-crop(stacked,ex)
new<-mask(stacked,AFRI_RegionSite_Raster)
plot(new)
title("Lowest regional NPP: Semi-arid Steppe (2011)")


# ggplot ------------------------------------------------------------------

library(RColorBrewer)
library(scales)
library(viridis)
library(ggthemes)
library(rasterVis)
library(ggplot2)
#color pallete info
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

#sd
p_sd = rasterToPoints(northern_mixed_deserts_spatiotemporal_sd); df_sd = data.frame(p_sd)
colnames(df_sd) = c("x", "y", "sd")
hist(df_sd$sd,main="",xlab="sd")
summary(df_sd)

#mean
p_mean = rasterToPoints(northern_mixed_desert_spatiotemporal_mean); df_mean = data.frame(p_mean)
colnames(df_mean) = c("x", "y", "mean")
#df_mean$log.mean<-df_mean$ln(mean)
summary(df_mean)
hist(df_mean$mean,main="",xlab="mean")
head(df_mean)

#cv
p_sd_mean<-merge(df_mean,df_sd,by=c("x","y")) #merge sd and mean datasets
colnames(p_sd_mean) = c("x","y","mean","sd")

## add a CV column
p_sd_mean$cv<-((p_sd_mean$sd)/(p_sd_mean$mean))*100 
summary(p_sd_mean)
hist(p_sd_mean$cv,main="",xlab="cv")

#trend
p_trend = rasterToPoints(p_sd_mean); df_trend = data.frame(cv)
hist(df_trend$SlopeSEG1,main="",xlab="slope")
head(df_trend)
summary(df_trend)


##
  ggplot(data=p_sd_mean) + geom_tile(aes(x, y, fill=sd)) +
  coord_equal() + labs(x=NULL, y=NULL) + 
  scale_fill_gradient2(low="blue",mid="yellow2",high="red", midpoint=500,
                       na.value="white") +
    xlab("") +
    ylab("") +
    ggtitle("") +
    #coord_equal() +
    theme_map() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    theme(legend.key.height = unit(.2, "cm"))

  hist(df_trend$SlopeSEG1)
  
  get_col <- colorRampPalette(c("#0f0f20","#20100f"))  # make fun to interpolate colors
  quantiles <- (0:6) / 6    
  quantile.vals <- quantile(p_sd_mean$cv, quantiles, names=F,na.rm=TRUE) # the values for each quantile
  colours <- rgb(get_col(quantiles), max=1335)       # 7 evenly interpolated colors 
  val.remap <- (quantile.vals - min(p_sd_mean$cv,na.rm=TRUE)) / 
    diff(range(p_sd_mean$cv,na.rm=TRUE))
  
range(p_sd_mean$cv,na.rm=TRUE)
  br.2 <-c(17,2,1000,1500,2000,3500,3000)
  br.2 <-round(quantile.vals,0)
  colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
  
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide="legend") +
