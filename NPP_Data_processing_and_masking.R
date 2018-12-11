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

#uploading the raster count file: how many 30 m pixels used in aggregation
count<-'/Users/A02296270/Desktop/CONUS_AFRI/Raster_Files/7km_masking_test/landsat-6000-npp-count.tif' #the count rastercount
raster_test <- raster(count)
plot(raster_test)

#get rid of junk data
raster_test[raster_test>= 65535] <- NA
raster_test[raster_test == -1] <-NA

#change to dataframe to subset sites more easily
npp_test_p = rasterToPoints(raster_test); npp_test_df = data.frame(npp_test_p)
head(npp_test_df)

#calculating threshold
#50% of all pixels not used (53824 is the max # of pixels used)
.5*53824 #26912

#########create npp 50% masking raster########################## 
#CREATE DIFFERENT NPP RASTERS

#remove all cells with >50% 30m NA values
df_50 <-npp_test_df %>%
  dplyr::filter(landsat.6000.npp.count > 26912)
summary(df_50)
npp_test_raster_50<- rasterFromXYZ(df_50) #change back to raster
plot(npp_test_raster_50)
title('50% threshold')

########### region-identifying rasters  #############
sites <- "/Users/A02296270/Desktop/CONUS_AFRI/CONUS/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

#raster math
raster_sites_rounded <- round(raster_sites/1000000)
plot(raster_sites_rounded) #see what it looks like

#Create a raster for the different regions, I guess this makes the colors more clear?
AFRI_Site_raster <- raster_sites_rounded - raster_sites*1000000
plot(AFRI_Site_raster)

########import NPP Data###########
#look at whole-map differences in NPP among the different masking schemes
#initial import of npp data
conus_npp <- dir('/Users/A02296270/Desktop/CONUS_AFRI/Raster_Files/landsat-6000reduced-npp/', full.names = T)
npp_stack <-stack(conus_npp) #stack all years of raster data
#get rid of junk values
npp_stack[npp_stack>= 65535] <- NA
plot(npp_stack)

##################whole-continent comparirons############
#summary stats for all data, unmasked
spatiotemporal_mean_all<-summaryBrick(npp_stack, mean, na.rm=TRUE)
spatiotemporal_sd_all<-summaryBrick(npp_stack, sd, na.rm=TRUE)
#mean npp
#no masking for comparing
npp_stack_all_mean = crop(spatiotemporal_mean_all, raster_test) 
npp_stack_all_mean_masked<-mask(npp_stack_all_mean, raster_test)
plot(npp_stack_all_mean_masked)
title('no masking')

#50% threshold
#mask out values that didn't meet the 50% theshold
npp_stack_50 = crop(npp_stack, npp_test_raster_50) 
npp_stack_50<-mask(npp_stack_50, npp_test_raster_50)
plot(npp_stack_50)

#sd npp
#no masking for comparison
npp_stack_all_sd = crop(spatiotemporal_sd_all, raster_test) 
npp_stack_all_sd_masked<-mask(npp_stack_all_sd, raster_test)
plot(npp_stack_all_sd_masked)
title('no masking')

#50% threshold
npp_stack_all_50_sd = crop(spatiotemporal_sd_all, npp_test_raster_50) 
npp_stack_all_sd_50_masked<-mask(npp_stack_all_50_sd, npp_test_raster_50)
plot(npp_stack_all_sd_50_masked)
title('50% threshold')

###########within region comparisons#############
sites_p = rasterToPoints(raster_sites_rounded); sites_df = data.frame(sites_p)
head(sites_df)
summary(sites_df) #see sites are identified 1 - 5
####shortgrass steppe####
sites_df_sgs <-sites_df %>%
  dplyr::filter(layer >= 5.000)
summary(sites_df_sgs)
sgs_raster<- rasterFromXYZ(sites_df_sgs) #change back to raster
plot(sgs_raster)

#unmasked
sgs.new.all = resample(npp_stack, sgs_raster, "bilinear")
plot(sgs.new.all)
npp_sgs<-mask(sgs.new.all, sgs_raster)
plot(npp_sgs)

#summary stats
sgs_spatiotemporal_mean_all<-summaryBrick(npp_sgs, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_all<-summaryBrick(npp_sgs, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_all)

#50%
#mask the 50% raster by the sgs site raster to get the proper extent
sgs_50_extent_crop<-crop(npp_test_raster_50,npp_sgs)
plot(sgs_50_extent_crop) #shows the number of 30 m pixels per 6km pixel aggregation
sgs_50_mask<-mask(sgs_50_extent_crop,npp_sgs) 
plot(sgs_50_mask)

#mask the npp sgs raster by the 50% sgs raster
npp_sgs_50<-mask(npp_sgs, sgs_50_mask)
plot(npp_sgs_50)

#summary_stats
sgs_spatiotemporal_mean_50<-summaryBrick(npp_sgs_50, mean, na.rm=TRUE)
sgs_spatiotemporal_sd_50<-summaryBrick(npp_sgs_50, sd, na.rm=TRUE)
plot(sgs_spatiotemporal_mean_50)

###hot deserts##############

sites_df_hot_deserts <-sites_df %>%
  dplyr::filter(layer == 3)

#summary(sites_df_hot_deserts)
hot_deserts_raster<- rasterFromXYZ(sites_df_hot_deserts) #change back to raster
plot(hot_deserts_raster)

#all
hot_deserts.new.all = resample(npp_stack, hot_deserts_raster, "bilinear")
plot(hot_deserts.new.all)
npp_hot_deserts<-mask(hot_deserts.new.all, hot_deserts_raster)
plot(npp_hot_deserts)

#summary stats
hot_deserts_spatiotemporal_mean_all<-summaryBrick(npp_hot_deserts, mean, na.rm=TRUE)
hot_deserts_spatiotemporal_sd_all<-summaryBrick(npp_hot_deserts, sd, na.rm=TRUE)
plot(hot_deserts_spatiotemporal_mean_all) #plot mean npp no masking

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
