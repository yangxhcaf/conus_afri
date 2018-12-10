library(raster)
library(rgdal)
library(dplyr)
library(naniar)
library(sp)
library(devtools)
#devtools::install_github('loicdtx/bfastSpatial')
library(bfastSpatial)
library(reshape2)

#http://rspatial.org/analysis/rst/7-spregression.html


# importing ----------------------------------------------------------------


#uploading and processing
filenames <- dir('/Users/A02296270/Desktop/CONUS_AFRI/Raster_Files/', full.names = T) #the count rastercount
raster_test <- raster(file.path(filenames , "landsat-6000-npp-count.tif"))
plot(raster_test)
title('no masking')
raster_test[raster_test>= 65535] <- NA
raster_test[raster_test == -1] <-NA
npp_test_p = rasterToPoints(raster_test); npp_test_df = data.frame(npp_test_p)
head(npp_test_df)
par(mfrow=c(2,2)) #set up a 2x2 plot

#calculating thresholds
#50% of all pixels not used
.5*53824

# 75% of all pixels not used
.25*53824
#13456

#90% of pixels not used
0.1*53824
#53824

#########create npp raster########################## 
#CREATE DIFFERENT NPP RASTERS

#remove all cells with >50% 30m NA values
df_50 <-npp_test_df %>%
  dplyr::filter(landsat.6000.npp.count > 26912)
summary(df_50)
npp_test_raster_50<- rasterFromXYZ(df_50) #change back to raster
plot(npp_test_raster_50)
title('50% threshold')

#remove all cells with <25% 30m NA values
df_75 <-npp_test_df %>%
  dplyr::filter(landsat.6000.npp.count > 13456)
summary(df_75)
npp_test_raster_75<- rasterFromXYZ(df_75) #change back to raster
plot(npp_test_raster_75)
title('25% theshold')

#remove all cells with <10% 30m NA values
df_90 <-npp_test_df %>%
  dplyr::filter(landsat.6000.npp.count > 5382.4)
summary(df_90)
npp_test_raster_90<- rasterFromXYZ(df_90)
plot(npp_test_raster_90)
title('10% threshold')

########### SITE RASTERS #############
sites <- "/Users/A02296270/Desktop/CONUS_AFRI/CONUS/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

raster_sites_rounded <- round(raster_sites/1000000)
plot(raster_sites_rounded)
?round
#Create a raster for the different regions, I guess this makes the colors more clear?
AFRI_Site_raster <- raster_sites_rounded - raster_sites*1000000
plot(AFRI_Site_raster)

#look at whole-map differences in NPP among the different masking schemes
#initial import of npp data
conus_npp <- dir('/Users/A02296270/Desktop/CONUS_AFRI/Raster_Files/landsat-6000reduced-npp/', full.names = T)
npp_stack <-stack(conus_npp)
npp_stack[npp_stack>= 65535] <- NA
par(mfrow=c(2,2))
plot(npp_stack)
##################whole-continent comparirons############
#summary stats for everything, unmasked
spatiotemporal_mean_all<-summaryBrick(npp_stack, mean, na.rm=TRUE)
spatiotemporal_sd_all<-summaryBrick(npp_stack, sd, na.rm=TRUE)
plot(npp_stack)
#mean npp
#no masking

npp_stack_all_mean = crop(spatiotemporal_mean_all, raster_test) 
npp_stack_all_mean_masked<-mask(npp_stack_all_mean, raster_test)
plot(npp_stack_all_mean_masked)
title('no masking')

#10% threshold
npp_stack_all_90_mean = crop(spatiotemporal_mean_all, npp_test_raster_90) 
npp_stack_all_mean_90_masked<-mask(npp_stack_all_90_mean, npp_test_raster_90)
plot(npp_stack_all_mean_90_masked)
title('10% threshold')

#25% threshold
npp_stack_all_75_mean = crop(spatiotemporal_mean_all, npp_test_raster_75) 
npp_stack_all_mean_75_masked<-mask(npp_stack_all_75_mean, npp_test_raster_75)
plot(npp_stack_all_mean_75_masked)
title('25% threshold')

#50% threshold

npp_stack_50 = crop(npp_stack, npp_test_raster_50) 
npp_stack_50<-mask(npp_stack_50, npp_test_raster_50)
plot(npp_stack_50)
title('50% threshold')

#sd npp
#no masking

npp_stack_all_sd = crop(spatiotemporal_sd_all, raster_test) 
npp_stack_all_sd_masked<-mask(npp_stack_all_sd, raster_test)
plot(npp_stack_all_sd_masked)
title('no masking')

#10% threshold
npp_stack_all_90_sd = crop(spatiotemporal_sd_all, npp_test_raster_90) 
npp_stack_all_sd_90_masked<-mask(npp_stack_all_90_sd, npp_test_raster_90)
plot(npp_stack_all_sd_90_masked)
title('10% threshold')

#25% threshold
npp_stack_all_75_sd = crop(spatiotemporal_sd_all, npp_test_raster_75) 
npp_stack_all_sd_75_masked<-mask(npp_stack_all_75_sd, npp_test_raster_75)
plot(npp_stack_all_sd_75_masked)
title('25% threshold')

#50% threshold

npp_stack_all_50_sd = crop(spatiotemporal_sd_all, npp_test_raster_50) 
npp_stack_all_sd_50_masked<-mask(npp_stack_all_50_sd, npp_test_raster_50)
plot(npp_stack_all_sd_50_masked)
title('50% threshold')

###########within site comparisons#############
sites_p = rasterToPoints(raster_sites_rounded); sites_df = data.frame(sites_p)
head(sites_df)
summary(sites_df)

####shortgrass steppe####
sites_df_sgs <-sites_df %>%
  dplyr::filter(layer >= 5.000)
summary(sites_df_sgs)
sgs_raster<- rasterFromXYZ(sites_df_sgs) #change back to raster
plot(sgs_raster)

#have not found a more elegent way to do this yet....
raster_sites_rounded[raster_sites_rounded < 5] <- NA
plot(raster_sites_rounded)
extent_region_1_semi_arid <- extent(-106.7361 , -99.0754 , 30.70696 , 43.76846)
sgs = crop(raster_sites_rounded,extent_region_1_semi_arid) 
plot(sgs)
npp_stack_sgs<-crop(npp_stack_all_mean,extent_region_1_semi_arid)
plot(npp_stack_sgs)
ex_sgs<-extent(sgs_raster)

#all
sgs.new.all = resample(npp_stack, sgs_raster, "bilinear")
plot(sgs.new.all)
npp_sgs<-mask(npp_stack_sgs, sgs_raster)
plot(npp_sgs)
#summary stat
sgs_spatiotemporal_mean_all<-summaryBrick(npp_sgs, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_all<-summaryBrick(npp_sgs, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_all)

#10%
#mask the 90% raster by the sgs site raster
sgs_90_extent_crop<-crop(npp_test_raster_90,npp_sgs)
plot(sgs_90_extent_crop)
sgs_90_mask<-mask(sgs_90_extent_crop,npp_sgs)
plot(sgs_90_mask)

#mask the npp sgs raster by the 90% sgs raster
npp_sgs_90<-mask(npp_stack_sgs, sgs_90_mask)
plot(npp_sgs_90)
#summary_stats
sgs_spatiotemporal_mean_90<-summaryBrick(npp_sgs_90, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_90<-summaryBrick(npp_sgs_90, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_90)

#25%
#mask the 75% raster by the sgs site raster
sgs_75_extent_crop<-crop(npp_test_raster_75,npp_sgs)
plot(sgs_75_extent_crop)
sgs_75_mask<-mask(sgs_75_extent_crop,npp_sgs)
plot(sgs_75_mask)

#mask the npp sgs raster by the 75% sgs raster
npp_sgs_75<-mask(npp_stack_sgs, sgs_75_mask)
plot(npp_sgs_75)
#summary_stats
sgs_spatiotemporal_mean_75<-summaryBrick(npp_sgs_75, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_75<-summaryBrick(npp_sgs_75, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_75)

#50%
#mask the 50% raster by the sgs site raster
sgs_50_extent_crop<-crop(npp_test_raster_50,npp_sgs)
plot(sgs_50_extent_crop)
sgs_50_mask<-mask(sgs_50_extent_crop,npp_sgs)
plot(sgs_50_mask)

#mask the npp sgs raster by the 50% sgs raster
npp_sgs_50<-mask(npp_stack_sgs, sgs_50_mask)
plot(npp_sgs_50)
#summary_stats
sgs_spatiotemporal_mean_50<-summaryBrick(npp_sgs_50, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_50<-summaryBrick(npp_sgs_50, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_50)
title('no masking')

#mean sgs graphs
par(mfrow=c(2,2))
plot(sgs_spatiotemporal_mean_all)
title('sgs no masking')
plot(sgs_spatiotemporal_mean_90)
title('sgs 10% threshold')
plot(sgs_spatiotemporal_mean_75)
title('sgs 25% threshold')
plot(sgs_spatiotemporal_mean_50)
title('sgs 50% threshold')

#sd sgs graphs
par(mfrow=c(2,2))
plot(sgs_spatiotemporal_sd_all)
title('sgs no masking')
plot(sgs_spatiotemporal_sd_90)
title('sgs 10% threshold')
plot(sgs_spatiotemporal_sd_75)
title('sgs 25% threshold')
plot(sgs_spatiotemporal_sd_50)
title('sgs 50% threshold')

#all pixels
#change to dataframes to look at precip-npp relationship
p_npp_sgs = rasterToPoints(npp_sgs); df_npp_sgs = data.frame(p_npp_sgs)
df_npp_sgs <- df_npp_sgs[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_sgs)[3:32] <-paste(1986:2015) #rename columns
sgs_npp_all_melted <- melt(df_npp_sgs, 
            id.vars = c("x", "y"),
            variable.name = "year")
sgs_npp_all_mean<-aggregate(value~x + y,mean,data=sgs_npp_all_melted)

#changing extent of precip
annual_precip_allyears_sgs<-crop(precip_stack,npp_sgs)
plot(annual_precip_allyears_sgs)

#turning precip raster into-  use for all masking
annualprecip_sgs_p = rasterToPoints(annual_precip_allyears_sgs); df_annualprecip_sgs = data.frame(annualprecip_sgs_p)
head(df_annualprecip_sgs)
colnames(df_annualprecip_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualprecip_melted <- melt(df_annualprecip_sgs, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
sgs_annualprecip_melted$mm<-sgs_annualprecip_melted$value #change to mm
sgs_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=sgs_annualprecip_melted)

#merge
merge_sgs_npp_annualprecip_allpixels<-merge(sgs_npp_all_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip_allpixels)
plot(value~mm,data=merge_sgs_npp_annualprecip_allpixels)

#10% threshold
#change to dataframes to look at precip-npp relationship
p_npp_sgs_90 = rasterToPoints(npp_sgs_90); df_npp_sgs_90 = data.frame(p_npp_sgs_90)
df_npp_sgs_90 <- df_npp_sgs_90[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_sgs_90)[3:32] <-paste(1986:2015) #rename columns
sgs_npp_90_melted <- melt(df_npp_sgs_90, 
                           id.vars = c("x", "y"),
                           variable.name = "year")
sgs_npp_90_mean<-aggregate(value~x + y,mean,data=sgs_npp_90_melted)

#merge with precip
merge_sgs_npp_annualprecip_90<-merge(sgs_npp_90_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip_90)
plot(value~mm,data=merge_sgs_npp_annualprecip_90)

#25% threshold
p_npp_sgs_75 = rasterToPoints(npp_sgs_75); df_npp_sgs_75 = data.frame(p_npp_sgs_75)
df_npp_sgs_75 <- df_npp_sgs_75[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_sgs_75)[3:32] <-paste(1986:2015) #rename columns
sgs_npp_75_melted <- melt(df_npp_sgs_75, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
sgs_npp_75_mean<-aggregate(value~x + y,mean,data=sgs_npp_75_melted)

#merge with precip
merge_sgs_npp_annualprecip_75<-merge(sgs_npp_75_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip_75)
plot(value~mm,data=merge_sgs_npp_annualprecip_75)

#50% threshold
p_npp_sgs_50 = rasterToPoints(npp_sgs_50); df_npp_sgs_50 = data.frame(p_npp_sgs_50)
df_npp_sgs_50 <- df_npp_sgs_50[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_sgs_50)[3:32] <-paste(1986:2015) #rename columns
sgs_npp_50_melted <- melt(df_npp_sgs_50, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
sgs_npp_50_mean<-aggregate(value~x + y,mean,data=sgs_npp_50_melted)

#merge with precip
merge_sgs_npp_annualprecip_50<-merge(sgs_npp_50_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip_50)
plot(value~mm,data=merge_sgs_npp_annualprecip_50)

#plots
par(mfrow=c(2,2))
plot(value~mm,data=merge_sgs_npp_annualprecip_allpixels)
title('no masking')
plot(value~mm,data=merge_sgs_npp_annualprecip_90)
title('10% threshold')
plot(value~mm,data=merge_sgs_npp_annualprecip_75)
title('25% threshold')
plot(value~mm,data=merge_sgs_npp_annualprecip_50)
title('50% threshold')

###hot deserts##############

sites_df_hot_deserts <-sites_df %>%
  dplyr::filter(layer == 4)
#summary(sites_df_hot_deserts)
hot_deserts_raster<- rasterFromXYZ(sites_df_hot_deserts) #change back to raster
plot(hot_deserts_raster)

#all
hot_deserts.new.all = resample(npp_stack, hot_deserts_raster, "bilinear")
plot(hot_deserts.new.all)
npp_hot_deserts<-mask(hot_deserts.new.all, hot_deserts_raster)
plot(npp_hot_deserts)
#summary stat
hot_deserts_spatiotemporal_mean_all<-summaryBrick(npp_hot_deserts, mean, na.rm=TRUE)
hot_deserts_spatiotemporal_sd_all<-summaryBrick(npp_hot_deserts, sd, na.rm=TRUE)
plot(hot_deserts_spatiotemporal_mean_all)

#10%
#mask the 90% raster by the hot_deserts site raster
hot_deserts_90_extent_crop<-crop(npp_test_raster_90,npp_hot_deserts)
plot(hot_deserts_90_extent_crop)
hot_deserts_90_mask<-mask(hot_deserts_90_extent_crop,npp_hot_deserts)
plot(hot_deserts_90_mask)

#mask the npp hot_deserts raster by the 90% hot_deserts raster
npp_hot_deserts_90<-mask(npp_hot_deserts, hot_deserts_90_mask)
plot(npp_hot_deserts_90)
#summary_stats
hot_deserts_spatiotemporal_mean_90<-summaryBrick(npp_hot_deserts_90, mean, na.rm=TRUE)
hot_deserts_spatiotemporal_sd_90<-summaryBrick(npp_hot_deserts_90, sd, na.rm=TRUE)
plot(hot_deserts_spatiotemporal_mean_90)

#25%
#mask the 75% raster by the hot_deserts site raster
hot_deserts_75_extent_crop<-crop(npp_test_raster_75,npp_hot_deserts)
plot(hot_deserts_75_extent_crop)
hot_deserts_75_mask<-mask(hot_deserts_75_extent_crop,npp_hot_deserts)
plot(hot_deserts_75_mask)

#mask the npp hot_deserts raster by the 75% hot_deserts raster
npp_hot_deserts_75<-mask(npp_hot_deserts, hot_deserts_75_mask)
plot(npp_hot_deserts_75)
#summary_stats
hot_deserts_spatiotemporal_mean_75<-summaryBrick(npp_hot_deserts_75, mean, na.rm=TRUE)
hot_deserts_spatiotemporal_sd_75<-summaryBrick(npp_hot_deserts_75, sd, na.rm=TRUE)
plot(hot_deserts_spatiotemporal_mean_75)

#50%
#mask the 50% raster by the hot_deserts site raster
hot_deserts_50_extent_crop<-crop(npp_test_raster_50,npp_hot_deserts)
plot(hot_deserts_50_extent_crop)
hot_deserts_50_mask<-mask(hot_deserts_50_extent_crop,npp_hot_deserts)
plot(hot_deserts_50_mask)

#mask the npp hot_deserts raster by the 50% hot_deserts raster
npp_hot_deserts_50<-mask(npp_hot_deserts, hot_deserts_50_mask)
plot(npp_hot_deserts_50)
#summary_stats
hot_deserts_spatiotemporal_mean_50<-summaryBrick(npp_hot_deserts_50, mean, na.rm=TRUE)
hot_deserts_spatiotemporal_sd_50<-summaryBrick(npp_hot_deserts_50, sd, na.rm=TRUE)
plot(hot_deserts_spatiotemporal_mean_50)
title('no masking')

#mean hot_deserts graphs
par(mfrow=c(2,2))
plot(hot_deserts_spatiotemporal_mean_all)
title('hot_deserts no masking')
plot(hot_deserts_spatiotemporal_mean_90)
title('hot_deserts 10% threshold')
plot(hot_deserts_spatiotemporal_mean_75)
title('hot_deserts 25% threshold')
plot(hot_deserts_spatiotemporal_mean_50)
title('hot_deserts 50% threshold')

#sd hot_deserts graphs
par(mfrow=c(2,2))
plot(hot_deserts_spatiotemporal_sd_all)
title('hot_deserts no masking')
plot(hot_deserts_spatiotemporal_sd_90)
title('hot_deserts 10% threshold')
plot(hot_deserts_spatiotemporal_sd_75)
title('hot_deserts 25% threshold')
plot(hot_deserts_spatiotemporal_sd_50)
title('hot_deserts 50% threshold')

#all pixels
#change to dataframes to look at precip-npp relationship
p_npp_hot_deserts = rasterToPoints(npp_hot_deserts); df_npp_hot_deserts = data.frame(p_npp_hot_deserts)
df_npp_hot_deserts <- df_npp_hot_deserts[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_hot_deserts)[3:32] <-paste(1986:2015) #rename columns
hot_deserts_npp_all_melted <- melt(df_npp_hot_deserts, 
                           id.vars = c("x", "y"),
                           variable.name = "year")
hot_deserts_npp_all_mean<-aggregate(value~x + y,mean,data=hot_deserts_npp_all_melted)

#changing extent of precip
annual_precip_allyears_hot_deserts<-crop(precip_stack,npp_hot_deserts)
plot(annual_precip_allyears_hot_deserts)

#turning precip raster into-  use for all masking
annualprecip_hot_deserts_p = rasterToPoints(annual_precip_allyears_hot_deserts); df_annualprecip_hot_deserts = data.frame(annualprecip_hot_deserts_p)
head(df_annualprecip_hot_deserts)
colnames(df_annualprecip_hot_deserts)[3:32] <-paste(1986:2015) #rename columns to years
hot_deserts_annualprecip_melted <- melt(df_annualprecip_hot_deserts, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
hot_deserts_annualprecip_melted$mm<-hot_deserts_annualprecip_melted$value #change to mm
hot_deserts_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=hot_deserts_annualprecip_melted)

#merge
merge_hot_deserts_npp_annualprecip_allpixels<-merge(hot_deserts_npp_all_mean,hot_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_annualprecip_allpixels)
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_allpixels)

#10% threshold
#change to dataframes to look at precip-npp relationship
p_npp_hot_deserts_90 = rasterToPoints(npp_hot_deserts_90); df_npp_hot_deserts_90 = data.frame(p_npp_hot_deserts_90)
df_npp_hot_deserts_90 <- df_npp_hot_deserts_90[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_hot_deserts_90)[3:32] <-paste(1986:2015) #rename columns
hot_deserts_npp_90_melted <- melt(df_npp_hot_deserts_90, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
hot_deserts_npp_90_mean<-aggregate(value~x + y,mean,data=hot_deserts_npp_90_melted)

#merge with precip
merge_hot_deserts_npp_annualprecip_90<-merge(hot_deserts_npp_90_mean,hot_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_annualprecip_90)
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_90)

#25% threshold
p_npp_hot_deserts_75 = rasterToPoints(npp_hot_deserts_75); df_npp_hot_deserts_75 = data.frame(p_npp_hot_deserts_75)
df_npp_hot_deserts_75 <- df_npp_hot_deserts_75[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_hot_deserts_75)[3:32] <-paste(1986:2015) #rename columns
hot_deserts_npp_75_melted <- melt(df_npp_hot_deserts_75, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
hot_deserts_npp_75_mean<-aggregate(value~x + y,mean,data=hot_deserts_npp_75_melted)

#merge with precip
merge_hot_deserts_npp_annualprecip_75<-merge(hot_deserts_npp_75_mean,hot_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_annualprecip_75)
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_75)

#50% threshold
p_npp_hot_deserts_50 = rasterToPoints(npp_hot_deserts_50); df_npp_hot_deserts_50 = data.frame(p_npp_hot_deserts_50)
df_npp_hot_deserts_50 <- df_npp_hot_deserts_50[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_hot_deserts_50)[3:32] <-paste(1986:2015) #rename columns
hot_deserts_npp_50_melted <- melt(df_npp_hot_deserts_50, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
hot_deserts_npp_50_mean<-aggregate(value~x + y,mean,data=hot_deserts_npp_50_melted)

#merge with precip
merge_hot_deserts_npp_annualprecip_50<-merge(hot_deserts_npp_50_mean,hot_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_annualprecip_50)
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_50)

#plots
par(mfrow=c(2,2))
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_allpixels)
title('no masking')
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_90)
title('10% threshold')
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_75)
title('25% threshold')
plot(value~mm,data=merge_hot_deserts_npp_annualprecip_50)
title('50% threshold')

######northern mixed######
sites_df_northern_mixed <-sites_df %>%
  dplyr::filter(layer == 4)
#summary(sites_df_northern_mixed)
northern_mixed_raster<- rasterFromXYZ(sites_df_northern_mixed) #change back to raster
plot(northern_mixed_raster)

#all
northern_mixed.new.all = resample(npp_stack, northern_mixed_raster, "bilinear")
plot(northern_mixed.new.all)
npp_northern_mixed<-mask(northern_mixed.new.all, northern_mixed_raster)
plot(npp_northern_mixed)
#summary stat
northern_mixed_spatiotemporal_mean_all<-summaryBrick(npp_northern_mixed, mean, na.rm=TRUE)
northern_mixed_spatiotemporal_sd_all<-summaryBrick(npp_northern_mixed, sd, na.rm=TRUE)
plot(northern_mixed_spatiotemporal_mean_all)

#10%
#mask the 90% raster by the northern_mixed site raster
northern_mixed_90_extent_crop<-crop(npp_test_raster_90,npp_northern_mixed)
plot(northern_mixed_90_extent_crop)
northern_mixed.90_resample = resample(northern_mixed_90_extent_crop, npp_northern_mixed, "bilinear") #have to resample because extents still slightly off
plot(northern_mixed.90_resample)
northern_mixed_90_mask<-mask(northern_mixed.90_resample,npp_northern_mixed)
plot(northern_mixed_90_mask)

#mask the npp northern_mixed raster by the 90% northern_mixed raster
npp_northern_mixed_90<-mask(npp_northern_mixed, northern_mixed_90_mask)
plot(npp_northern_mixed_90)
#summary_stats
northern_mixed_spatiotemporal_mean_90<-summaryBrick(npp_northern_mixed_90, mean, na.rm=TRUE)
northern_mixed_spatiotemporal_sd_90<-summaryBrick(npp_northern_mixed_90, sd, na.rm=TRUE)
plot(northern_mixed_spatiotemporal_mean_90)

#25%
#mask the 75% raster by the northern_mixed site raster
northern_mixed_75_extent_crop<-crop(npp_test_raster_75,npp_northern_mixed)
plot(northern_mixed_75_extent_crop)
northern_mixed.75_resample = resample(northern_mixed_75_extent_crop, npp_northern_mixed, "bilinear") #have to resample because extents still slightly off
plot(northern_mixed.75_resample)
northern_mixed_75_mask<-mask(northern_mixed.75_resample,npp_northern_mixed)
plot(northern_mixed_75_mask)

#mask the npp northern_mixed raster by the 75% northern_mixed raster
npp_northern_mixed_75<-mask(npp_northern_mixed, northern_mixed_75_mask)
plot(npp_northern_mixed_75)
#summary_stats
northern_mixed_spatiotemporal_mean_75<-summaryBrick(npp_northern_mixed_75, mean, na.rm=TRUE)
northern_mixed_spatiotemporal_sd_75<-summaryBrick(npp_northern_mixed_75, sd, na.rm=TRUE)
plot(northern_mixed_spatiotemporal_mean_75)

#50%
#mask the 50% raster by the northern_mixed site raster
northern_mixed_50_extent_crop<-crop(npp_test_raster_50,npp_northern_mixed)
plot(northern_mixed_50_extent_crop)
northern_mixed.50_resample = resample(northern_mixed_50_extent_crop, npp_northern_mixed, "bilinear") #have to resample because extents still slightly off
plot(northern_mixed.50_resample)
northern_mixed_50_mask<-mask(northern_mixed.50_resample,npp_northern_mixed)
plot(northern_mixed_50_mask)

#mask the npp northern_mixed raster by the 50% northern_mixed raster
npp_northern_mixed_50<-mask(npp_northern_mixed, northern_mixed_50_mask)
plot(npp_northern_mixed_50)
#summary_stats
northern_mixed_spatiotemporal_mean_50<-summaryBrick(npp_northern_mixed_50, mean, na.rm=TRUE)
northern_mixed_spatiotemporal_sd_50<-summaryBrick(npp_northern_mixed_50, sd, na.rm=TRUE)
plot(northern_mixed_spatiotemporal_mean_50)
title('no masking')

#mean northern_mixed graphs
par(mfrow=c(2,2))
plot(northern_mixed_spatiotemporal_mean_all)
title('northern_mixed no masking')
plot(northern_mixed_spatiotemporal_mean_90)
title('northern_mixed 10% threshold')
plot(northern_mixed_spatiotemporal_mean_75)
title('northern_mixed 25% threshold')
plot(northern_mixed_spatiotemporal_mean_50)
title('northern_mixed 50% threshold')

#sd northern_mixed graphs
par(mfrow=c(2,2))
plot(northern_mixed_spatiotemporal_sd_all)
title('northern_mixed no masking')
plot(northern_mixed_spatiotemporal_sd_90)
title('northern_mixed 10% threshold')
plot(northern_mixed_spatiotemporal_sd_75)
title('northern_mixed 25% threshold')
plot(northern_mixed_spatiotemporal_sd_50)
title('northern_mixed 50% threshold')

#all pixels
#change to dataframes to look at precip-npp relationship
p_npp_northern_mixed = rasterToPoints(npp_northern_mixed); df_npp_northern_mixed = data.frame(p_npp_northern_mixed)
df_npp_northern_mixed <- df_npp_northern_mixed[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_northern_mixed)[3:32] <-paste(1986:2015) #rename columns
northern_mixed_npp_all_melted <- melt(df_npp_northern_mixed, 
                           id.vars = c("x", "y"),
                           variable.name = "year")
northern_mixed_npp_all_mean<-aggregate(value~x + y,mean,data=northern_mixed_npp_all_melted)

#changing extent of precip
annual_precip_allyears_northern_mixed<-crop(precip_stack,npp_northern_mixed)
plot(annual_precip_allyears_northern_mixed)

#turning precip raster into-  use for all masking
annualprecip_northern_mixed_p = rasterToPoints(annual_precip_allyears_northern_mixed); df_annualprecip_northern_mixed = data.frame(annualprecip_northern_mixed_p)
head(df_annualprecip_northern_mixed)
colnames(df_annualprecip_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_annualprecip_melted <- melt(df_annualprecip_northern_mixed, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
northern_mixed_annualprecip_melted$mm<-northern_mixed_annualprecip_melted$value #change to mm
northern_mixed_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=northern_mixed_annualprecip_melted)

#merge
merge_northern_mixed_npp_annualprecip_allpixels<-merge(northern_mixed_npp_all_mean,northern_mixed_annualprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip_allpixels)
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_allpixels)

#10% threshold
#change to dataframes to look at precip-npp relationship
p_npp_northern_mixed_90 = rasterToPoints(npp_northern_mixed_90); df_npp_northern_mixed_90 = data.frame(p_npp_northern_mixed_90)
df_npp_northern_mixed_90 <- df_npp_northern_mixed_90[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_northern_mixed_90)[3:32] <-paste(1986:2015) #rename columns
northern_mixed_npp_90_melted <- melt(df_npp_northern_mixed_90, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
northern_mixed_npp_90_mean<-aggregate(value~x + y,mean,data=northern_mixed_npp_90_melted)

#merge with precip
merge_northern_mixed_npp_annualprecip_90<-merge(northern_mixed_npp_90_mean,northern_mixed_annualprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip_90)
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_90)

#25% threshold
p_npp_northern_mixed_75 = rasterToPoints(npp_northern_mixed_75); df_npp_northern_mixed_75 = data.frame(p_npp_northern_mixed_75)
df_npp_northern_mixed_75 <- df_npp_northern_mixed_75[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_northern_mixed_75)[3:32] <-paste(1986:2015) #rename columns
northern_mixed_npp_75_melted <- melt(df_npp_northern_mixed_75, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
northern_mixed_npp_75_mean<-aggregate(value~x + y,mean,data=northern_mixed_npp_75_melted)

#merge with precip
merge_northern_mixed_npp_annualprecip_75<-merge(northern_mixed_npp_75_mean,northern_mixed_annualprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip_75)
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_75)

#50% threshold
p_npp_northern_mixed_50 = rasterToPoints(npp_northern_mixed_50); df_npp_northern_mixed_50 = data.frame(p_npp_northern_mixed_50)
df_npp_northern_mixed_50 <- df_npp_northern_mixed_50[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_northern_mixed_50)[3:32] <-paste(1986:2015) #rename columns
northern_mixed_npp_50_melted <- melt(df_npp_northern_mixed_50, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
northern_mixed_npp_50_mean<-aggregate(value~x + y,mean,data=northern_mixed_npp_50_melted)

#merge with precip
merge_northern_mixed_npp_annualprecip_50<-merge(northern_mixed_npp_50_mean,northern_mixed_annualprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip_50)
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_50)

#plots
par(mfrow=c(2,2))
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_allpixels)
title('no masking')
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_90)
title('10% threshold')
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_75)
title('25% threshold')
plot(value~mm,data=merge_northern_mixed_npp_annualprecip_50)
title('50% threshold')

########cold deserts###########

sites_df_cold_deserts <-sites_df %>%
  dplyr::filter(layer == 2)
#summary(sites_df_cold_deserts)
cold_deserts_raster<- rasterFromXYZ(sites_df_cold_deserts) #change back to raster
plot(cold_deserts_raster)

#all
cold_deserts.new.all = resample(npp_stack, cold_deserts_raster, "bilinear")
plot(cold_deserts.new.all)
npp_cold_deserts<-mask(cold_deserts.new.all, cold_deserts_raster)
plot(npp_cold_deserts)
#summary stat
cold_deserts_spatiotemporal_mean_all<-summaryBrick(npp_cold_deserts, mean, na.rm=TRUE)
cold_deserts_spatiotemporal_sd_all<-summaryBrick(npp_cold_deserts, sd, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_mean_all)

#10%
#mask the 90% raster by the cold_deserts site raster
cold_deserts_90_extent_crop<-crop(npp_test_raster_90,npp_cold_deserts)
plot(cold_deserts_90_extent_crop)
cold_deserts_90_mask<-mask(cold_deserts_90_extent_crop,npp_cold_deserts)
plot(cold_deserts_90_mask)

#mask the npp cold_deserts raster by the 90% cold_deserts raster
npp_cold_deserts_90<-mask(npp_cold_deserts, cold_deserts_90_mask)
plot(npp_cold_deserts_90)
#summary_stats
cold_deserts_spatiotemporal_mean_90<-summaryBrick(npp_cold_deserts_90, mean, na.rm=TRUE)
cold_deserts_spatiotemporal_sd_90<-summaryBrick(npp_cold_deserts_90, sd, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_mean_90)

#25%
#mask the 75% raster by the cold_deserts site raster
cold_deserts_75_extent_crop<-crop(npp_test_raster_75,npp_cold_deserts)
plot(cold_deserts_75_extent_crop)
cold_deserts_75_mask<-mask(cold_deserts_75_extent_crop,npp_cold_deserts)
plot(cold_deserts_75_mask)

#mask the npp cold_deserts raster by the 75% cold_deserts raster
npp_cold_deserts_75<-mask(npp_cold_deserts, cold_deserts_75_mask)
plot(npp_cold_deserts_75)
#summary_stats
cold_deserts_spatiotemporal_mean_75<-summaryBrick(npp_cold_deserts_75, mean, na.rm=TRUE)
cold_deserts_spatiotemporal_sd_75<-summaryBrick(npp_cold_deserts_75, sd, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_mean_75)

#50%
#mask the 50% raster by the cold_deserts site raster
cold_deserts_50_extent_crop<-crop(npp_test_raster_50,npp_cold_deserts)
plot(cold_deserts_50_extent_crop)
cold_deserts_50_mask<-mask(cold_deserts_50_extent_crop,npp_cold_deserts)
plot(cold_deserts_50_mask)

#mask the npp cold_deserts raster by the 50% cold_deserts raster
npp_cold_deserts_50<-mask(npp_cold_deserts, cold_deserts_50_mask)
plot(npp_cold_deserts_50)
#summary_stats
cold_deserts_spatiotemporal_mean_50<-summaryBrick(npp_cold_deserts_50, mean, na.rm=TRUE)
cold_deserts_spatiotemporal_sd_50<-summaryBrick(npp_cold_deserts_50, sd, na.rm=TRUE)
plot(cold_deserts_spatiotemporal_mean_50)
title('no masking')

#mean cold_deserts graphs
par(mfrow=c(2,2))
plot(cold_deserts_spatiotemporal_mean_all)
title('cold_deserts no masking')
plot(cold_deserts_spatiotemporal_mean_90)
title('cold_deserts 10% threshold')
plot(cold_deserts_spatiotemporal_mean_75)
title('cold_deserts 25% threshold')
plot(cold_deserts_spatiotemporal_mean_50)
title('cold_deserts 50% threshold')

#sd cold_deserts graphs
par(mfrow=c(2,2))
plot(cold_deserts_spatiotemporal_sd_all)
title('cold_deserts no masking')
plot(cold_deserts_spatiotemporal_sd_90)
title('cold_deserts 10% threshold')
plot(cold_deserts_spatiotemporal_sd_75)
title('cold_deserts 25% threshold')
plot(cold_deserts_spatiotemporal_sd_50)
title('cold_deserts 50% threshold')

#all pixels
#change to dataframes to look at precip-npp relationship
p_npp_cold_deserts = rasterToPoints(npp_cold_deserts); df_npp_cold_deserts = data.frame(p_npp_cold_deserts)
df_npp_cold_deserts <- df_npp_cold_deserts[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cold_deserts)[3:32] <-paste(1986:2015) #rename columns
cold_deserts_npp_all_melted <- melt(df_npp_cold_deserts, 
                           id.vars = c("x", "y"),
                           variable.name = "year")
cold_deserts_npp_all_mean<-aggregate(value~x + y,mean,data=cold_deserts_npp_all_melted)

#changing extent of precip
annual_precip_allyears_cold_deserts<-crop(precip_stack,npp_cold_deserts)
plot(annual_precip_allyears_cold_deserts)

#turning precip raster into-  use for all masking
annualprecip_cold_deserts_p = rasterToPoints(annual_precip_allyears_cold_deserts); df_annualprecip_cold_deserts = data.frame(annualprecip_cold_deserts_p)
head(df_annualprecip_cold_deserts)
colnames(df_annualprecip_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_annualprecip_melted <- melt(df_annualprecip_cold_deserts, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
cold_deserts_annualprecip_melted$mm<-cold_deserts_annualprecip_melted$value #change to mm
cold_deserts_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cold_deserts_annualprecip_melted)

#merge
merge_cold_deserts_npp_annualprecip_allpixels<-merge(cold_deserts_npp_all_mean,cold_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip_allpixels)
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_allpixels)

#10% threshold
#change to dataframes to look at precip-npp relationship
p_npp_cold_deserts_90 = rasterToPoints(npp_cold_deserts_90); df_npp_cold_deserts_90 = data.frame(p_npp_cold_deserts_90)
df_npp_cold_deserts_90 <- df_npp_cold_deserts_90[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cold_deserts_90)[3:32] <-paste(1986:2015) #rename columns
cold_deserts_npp_90_melted <- melt(df_npp_cold_deserts_90, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cold_deserts_npp_90_mean<-aggregate(value~x + y,mean,data=cold_deserts_npp_90_melted)

#merge with precip
merge_cold_deserts_npp_annualprecip_90<-merge(cold_deserts_npp_90_mean,cold_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip_90)
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_90)

#25% threshold
p_npp_cold_deserts_75 = rasterToPoints(npp_cold_deserts_75); df_npp_cold_deserts_75 = data.frame(p_npp_cold_deserts_75)
df_npp_cold_deserts_75 <- df_npp_cold_deserts_75[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cold_deserts_75)[3:32] <-paste(1986:2015) #rename columns
cold_deserts_npp_75_melted <- melt(df_npp_cold_deserts_75, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cold_deserts_npp_75_mean<-aggregate(value~x + y,mean,data=cold_deserts_npp_75_melted)

#merge with precip
merge_cold_deserts_npp_annualprecip_75<-merge(cold_deserts_npp_75_mean,cold_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip_75)
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_75)

#50% threshold
p_npp_cold_deserts_50 = rasterToPoints(npp_cold_deserts_50); df_npp_cold_deserts_50 = data.frame(p_npp_cold_deserts_50)
df_npp_cold_deserts_50 <- df_npp_cold_deserts_50[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cold_deserts_50)[3:32] <-paste(1986:2015) #rename columns
cold_deserts_npp_50_melted <- melt(df_npp_cold_deserts_50, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cold_deserts_npp_50_mean<-aggregate(value~x + y,mean,data=cold_deserts_npp_50_melted)

#merge with precip
merge_cold_deserts_npp_annualprecip_50<-merge(cold_deserts_npp_50_mean,cold_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip_50)
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_50)

#plots
par(mfrow=c(2,2))
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_allpixels)
title('no masking')
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_90)
title('10% threshold')
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_75)
title('25% threshold')
plot(value~mm,data=merge_cold_deserts_npp_annualprecip_50)
title('50% threshold with outliers')

#testing for outliers
library(car)
library(MASS)
cold.50<-lm(value~mm,data=merge_cold_deserts_npp_annualprecip_50)
outlierTest(cold.50)
cold.50.2<-merge_cold_deserts_npp_annualprecip_50[-c(10050,16431,16732,7779,4960,10445,10140,16423,16510,10940),]
?mfrow
par(mfrow=c(1,2))
plot(value~mm,data=cold.50.2)
title('cold desert 50 outliers removed')

###########cali annuals##########
sites_df_cali <-sites_df %>%
  dplyr::filter(layer == 1)
#summary(sites_df_hot_deserts)
cali_raster<- rasterFromXYZ(sites_df_cali) #change back to raster
plot(cali_raster)

#all
cali.new.all = resample(npp_stack, cali_raster, "bilinear")
plot(cali.new.all)
npp_cali<-mask(cali.new.all, cali_raster)
plot(npp_cali)
#summary stat
cali_spatiotemporal_mean_all<-summaryBrick(npp_cali, mean, na.rm=TRUE)
cali_spatiotemporal_sd_all<-summaryBrick(npp_cali, sd, na.rm=TRUE)
plot(cali_spatiotemporal_mean_all)

#10%
#mask the 90% raster by the cali site raster
cali_90_extent_crop<-crop(npp_test_raster_90,npp_cali)
plot(cali_90_extent_crop)
cali_90_mask<-mask(cali_90_extent_crop,npp_cali)
plot(cali_90_mask)

#mask the npp cali raster by the 90% cali raster
npp_cali_90<-mask(npp_cali, cali_90_mask)
plot(npp_cali_90)
#summary_stats
cali_spatiotemporal_mean_90<-summaryBrick(npp_cali_90, mean, na.rm=TRUE)
cali_spatiotemporal_sd_90<-summaryBrick(npp_cali_90, sd, na.rm=TRUE)
plot(cali_spatiotemporal_mean_90)

#25%
#mask the 75% raster by the cali site raster
cali_75_extent_crop<-crop(npp_test_raster_75,npp_cali)
plot(cali_75_extent_crop)
cali_75_mask<-mask(cali_75_extent_crop,npp_cali)
plot(cali_75_mask)

#mask the npp cali raster by the 75% cali raster
npp_cali_75<-mask(npp_cali, cali_75_mask)
plot(npp_cali_75)
#summary_stats
cali_spatiotemporal_mean_75<-summaryBrick(npp_cali_75, mean, na.rm=TRUE)
cali_spatiotemporal_sd_75<-summaryBrick(npp_cali_75, sd, na.rm=TRUE)
plot(cali_spatiotemporal_mean_75)

#50%
#mask the 50% raster by the cali site raster
cali_50_extent_crop<-crop(npp_test_raster_50,npp_cali)
plot(cali_50_extent_crop)
cali_50_mask<-mask(cali_50_extent_crop,npp_cali)
plot(cali_50_mask)

#mask the npp cali raster by the 50% cali raster
npp_cali_50<-mask(npp_cali, cali_50_mask)
plot(npp_cali_50)
#summary_stats
cali_spatiotemporal_mean_50<-summaryBrick(npp_cali_50, mean, na.rm=TRUE)
cali_spatiotemporal_sd_50<-summaryBrick(npp_cali_50, sd, na.rm=TRUE)
plot(cali_spatiotemporal_mean_50)
title('no masking')

#mean cali graphs
par(mfrow=c(2,2))
plot(cali_spatiotemporal_mean_all)
title('cali no masking')
plot(cali_spatiotemporal_mean_90)
title('cali 10% threshold')
plot(cali_spatiotemporal_mean_75)
title('cali 25% threshold')
plot(cali_spatiotemporal_mean_50)
title('cali 50% threshold')

#sd cali graphs
par(mfrow=c(2,2))
plot(cali_spatiotemporal_sd_all)
title('cali no masking')
plot(cali_spatiotemporal_sd_90)
title('cali 10% threshold')
plot(cali_spatiotemporal_sd_75)
title('cali 25% threshold')
plot(cali_spatiotemporal_sd_50)
title('cali 50% threshold')

#all pixels
#change to dataframes to look at precip-npp relationship
p_npp_cali = rasterToPoints(npp_cali); df_npp_cali = data.frame(p_npp_cali)
df_npp_cali <- df_npp_cali[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cali)[3:32] <-paste(1986:2015) #rename columns
cali_npp_all_melted <- melt(df_npp_cali, 
                           id.vars = c("x", "y"),
                           variable.name = "year")
cali_npp_all_mean<-aggregate(value~x + y,mean,data=cali_npp_all_melted)

#changing extent of precip
annual_precip_allyears_cali<-crop(precip_stack,npp_cali)
plot(annual_precip_allyears_cali)

#turning precip raster into-  use for all masking
annualprecip_cali_p = rasterToPoints(annual_precip_allyears_cali); df_annualprecip_cali = data.frame(annualprecip_cali_p)
head(df_annualprecip_cali)
colnames(df_annualprecip_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_annualprecip_melted <- melt(df_annualprecip_cali, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
cali_annualprecip_melted$mm<-cali_annualprecip_melted$value #change to mm
cali_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cali_annualprecip_melted)

#merge
merge_cali_npp_annualprecip_allpixels<-merge(cali_npp_all_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip_allpixels)
plot(value~mm,data=merge_cali_npp_annualprecip_allpixels)

#10% threshold
#change to dataframes to look at precip-npp relationship
p_npp_cali_90 = rasterToPoints(npp_cali_90); df_npp_cali_90 = data.frame(p_npp_cali_90)
df_npp_cali_90 <- df_npp_cali_90[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cali_90)[3:32] <-paste(1986:2015) #rename columns
cali_npp_90_melted <- melt(df_npp_cali_90, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cali_npp_90_mean<-aggregate(value~x + y,mean,data=cali_npp_90_melted)

#merge with precip
merge_cali_npp_annualprecip_90<-merge(cali_npp_90_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip_90)
plot(value~mm,data=merge_cali_npp_annualprecip_90)

#25% threshold
p_npp_cali_75 = rasterToPoints(npp_cali_75); df_npp_cali_75 = data.frame(p_npp_cali_75)
df_npp_cali_75 <- df_npp_cali_75[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cali_75)[3:32] <-paste(1986:2015) #rename columns
cali_npp_75_melted <- melt(df_npp_cali_75, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cali_npp_75_mean<-aggregate(value~x + y,mean,data=cali_npp_75_melted)

#merge with precip
merge_cali_npp_annualprecip_75<-merge(cali_npp_75_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip_75)
plot(value~mm,data=merge_cali_npp_annualprecip_75)

#50% threshold
p_npp_cali_50 = rasterToPoints(npp_cali_50); df_npp_cali_50 = data.frame(p_npp_cali_50)
df_npp_cali_50 <- df_npp_cali_50[,-c(33:34)] #remove 2016 and 2017 since no climate data for these years
colnames(df_npp_cali_50)[3:32] <-paste(1986:2015) #rename columns
cali_npp_50_melted <- melt(df_npp_cali_50, 
                          id.vars = c("x", "y"),
                          variable.name = "year")
cali_npp_50_mean<-aggregate(value~x + y,mean,data=cali_npp_50_melted)

#merge with precip
merge_cali_npp_annualprecip_50<-merge(cali_npp_50_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip_50)
plot(value~mm,data=merge_cali_npp_annualprecip_50)

#plots
par(mfrow=c(2,2))
plot(value~mm,data=merge_cali_npp_annualprecip_allpixels)
title('no masking')
plot(value~mm,data=merge_cali_npp_annualprecip_90)
title('10% threshold')
plot(value~mm,data=merge_cali_npp_annualprecip_75)
title('25% threshold')
plot(value~mm,data=merge_cali_npp_annualprecip_50)
title('50% threshold')




