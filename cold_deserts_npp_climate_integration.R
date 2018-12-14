#cold_deserts code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_cold_deserts<-extent(npp_cold_deserts_50)
annual_precip_allyears_cold_deserts<-crop(precip_stack,ex_cold_deserts)
annual_temp_allyears_cold_deserts<-crop(temp_stack,ex_cold_deserts) #temperature
annual_transp_allyears_cold_deserts<-crop(transp_stack,ex_cold_deserts) #transpiration

plot(npp_cold_deserts_50)
plot(annual_precip_allyears_cold_deserts)
plot(annual_temp_allyears_cold_deserts)
plot(annual_transp_allyears_cold_deserts)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_cold_deserts_p = rasterToPoints(npp_cold_deserts_50); df_npp_cold_deserts = data.frame(npp_cold_deserts_p)
colnames(df_npp_cold_deserts)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_2<-df_npp_cold_deserts[-c(33,34)] #get rid of 2016, 2017 columns
cold_deserts_npp_melted <- melt(df_npp_cold_deserts_2, 
                                  id.vars = c("x", "y"),
                                  variable.name = "year") #melt to long format
summary(cold_deserts_npp_melted)
cold_deserts_npp_melted$npp<-cold_deserts_npp_melted$value/10 #change npp value to g/m2 scale
cold_deserts_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=cold_deserts_npp_melted) #mean annual npp

#annual precipitation
annualprecip_cold_deserts_p = rasterToPoints(annual_precip_allyears_cold_deserts); df_annualprecip_cold_deserts = data.frame(annualprecip_cold_deserts_p)
head(df_annualprecip_cold_deserts)
colnames(df_annualprecip_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_annualprecip_melted <- melt(df_annualprecip_cold_deserts, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") #melt to long format

cold_deserts_annualprecip_melted$mm<-cold_deserts_annualprecip_melted$value*10 #change to mm
cold_deserts_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cold_deserts_annualprecip_melted) #get mean annual precip values

#annual temperature
annualtemp_cold_deserts_p = rasterToPoints(annual_temp_allyears_cold_deserts); df_annualtemp_cold_deserts = data.frame(annualtemp_cold_deserts_p)
head(df_annualtemp_cold_deserts)
colnames(df_annualtemp_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_annualtemp_melted <- melt(df_annualtemp_cold_deserts, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") 
cold_deserts_annualtemp_melted$temp<-cold_deserts_annualtemp_melted$value #rename column temp
cold_deserts_annualtemp_melted <- cold_deserts_annualtemp_melted[-4] #get rid of value column
cold_deserts_annualtemp_melted_mean<-aggregate(temp~x + y,mean,data=cold_deserts_annualtemp_melted) #mean annual temp
head(cold_deserts_annualtemp_melted_mean)

#annual transpiration
annualtransp_cold_deserts_p = rasterToPoints(annual_transp_allyears_cold_deserts); df_annualtransp_cold_deserts = data.frame(annualtransp_cold_deserts_p)
head(df_annualtransp_cold_deserts)
colnames(df_annualtransp_cold_deserts)[3:32] <-paste(1986:2015) #rename columns to years
cold_deserts_annualtransp_melted <- melt(df_annualtransp_cold_deserts, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") 
cold_deserts_annualtransp_melted$transp<-cold_deserts_annualtransp_melted$value #rename column transp
cold_deserts_annualtransp_melted <- cold_deserts_annualtransp_melted[-4] #get rid of value column
cold_deserts_annualtransp_melted_mean<-aggregate(transp~x + y,mean,data=cold_deserts_annualtransp_melted) #mean annual transp
head(cold_deserts_annualtransp_melted_mean)

#merge the mean npp and ppt datasets
merge_cold_deserts_npp_annualprecip<-merge(cold_deserts_npp_melted_mean,cold_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip)

#merge mean annual temperature data with this
merge_cold_deserts_npp_annualprecip_temp<-merge(cold_deserts_annualtemp_melted_mean,merge_cold_deserts_npp_annualprecip,by=c("x","y"))
head(merge_cold_deserts_npp_annualprecip_temp)

######identifying outliers, odd values to be removed##########
# spatial npp-map relationship
cold_deserts.spatial.map.lm = lm(npp ~ mm, data=merge_cold_deserts_npp_annualprecip_temp)
summary(cold_deserts.spatial.map.lm)

#adding a residuals column to the spatial npp-map relationship to dataframe
merge_cold_deserts_npp_annualprecip_temp$residuals_map <- residuals(cold_deserts.spatial.map.lm)

# spatial npp-map + mat relationship
cold_deserts.spatial.map.mat.lm = lm(npp ~ mm + temp, data=merge_cold_deserts_npp_annualprecip_temp)
summary(cold_deserts.spatial.map.mat.lm) #slightly higher r-square
par(mfrow=c(2,2))
plot(cold_deserts.spatial.map.mat.lm) 
merge_cold_deserts_npp_annualprecip_temp$residuals_map_mat <- residuals(cold_deserts.spatial.map.mat.lm)

#produce a raster of the spatial npp spatial models
head(merge_cold_deserts_npp_annualprecip_temp)
cold_deserts_residual_comp<-merge_cold_deserts_npp_annualprecip_temp[-c(3,4,5)]
cold_deserts_residual_comp_raster<-rasterFromXYZ(cold_deserts_residual_comp)
plot(cold_deserts_residual_comp_raster)

#get mean pue for each grid cell: use for
merge_cold_deserts_npp_annualprecip_temp$pue<-merge_cold_deserts_npp_annualprecip_temp$npp/merge_cold_deserts_npp_annualprecip_temp$mm
head(merge_cold_deserts_npp_annualprecip_temp)
npp_mm_pue_resid_cold_deserts<-merge_cold_deserts_npp_annualprecip_temp[-c(3,7)]

#diagnostic raster/plot
npp_mm_pue_resid_raster_cold_deserts<-rasterFromXYZ(npp_mm_pue_resid_cold_deserts)
plot(npp_mm_pue_resid_raster_cold_deserts)

#isolating pue: analzing the structure
cold_deserts_pue<-merge_cold_deserts_npp_annualprecip_temp[-c(3,4,5,6,7)]
head(cold_deserts_pue)
hist(cold_deserts_pue$pue)
sd(cold_deserts_pue$pue)
mean(cold_deserts_pue$pue)
min(cold_deserts_pue$pue)
max(cold_deserts_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
3*0.15 + 0.30 #0.30 as threshold for high values
0.30 - (3*0.15) #-0.15 as threshold for low values

#cold_deserts actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_cold_deserts <- cold_deserts_pue %>%
  dplyr::filter(pue > 0.75 | pue < -0.15) #lower than the actual min, indicates right skew
summary(pue_anamolies_cold_deserts)
pue_anamolies_cold_deserts_raster<- rasterFromXYZ(pue_anamolies_cold_deserts)
plot(pue_anamolies_cold_deserts_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_cold_deserts_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_cold_deserts_raster, "cold_deserts_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope
merge_cold_deserts_npp_annualprecip_allyears<-merge(cold_deserts_annualprecip_melted,cold_deserts_npp_melted ,by=c("x","y","year"))
head(merge_cold_deserts_npp_annualprecip_allyears)
merge_cold_deserts_npp_annualprecip_allyears_2<-merge_cold_deserts_npp_annualprecip_allyears[-c(4,6)]
head(merge_cold_deserts_npp_annualprecip_allyears_2)
test_cold_deserts<-rasterFromXYZ(merge_cold_deserts_npp_annualprecip_allyears_2) #didn't work
#for generating slope dataset
slope_spatial_cold_deserts <- merge_cold_deserts_npp_annualprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cold_deserts)
cold_deserts_coef_only<- slope_spatial_cold_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(cold_deserts_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

cold_deserts_pue_slope<-merge(cold_deserts_coef_only,cold_deserts_pue,by=c('x','y'))
head(cold_deserts_pue_slope)
summary(cold_deserts_pue_slope)

#mask out odd pue values
library(dplyr)
cold_deserts_pue_slope_2 <- dplyr::filter(cold_deserts_pue_slope,pue<0.75,pue>-0.15)
#cold_deserts_pue_slope_2  <-filter(cold_deserts_pue_slope_2, pue > -0.15)
summary(cold_deserts_pue_slope_2)
cold_deserts_pue_slope_3<-cold_deserts_pue_slope_2[-c(4)]
head(cold_deserts_pue_slope_3)

#summary stats of pixel slopes
sd(cold_deserts_pue_slope_3$coef)
mean(cold_deserts_pue_slope_3$coef)
hist(cold_deserts_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.13 + (3*0.077) #0.36
0.13 - (3*0.077) #-0.1

cold_deserts_slope_anamolies <- cold_deserts_pue_slope_3 %>%
  dplyr::filter(coef > 0.36 | coef < -0.1) #isolate odd slope values
summary(cold_deserts_slope_anamolies)


#change to raster
cold_deserts_slope_anamolies_raster<- rasterFromXYZ(cold_deserts_slope_anamolies)
plot(cold_deserts_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(cold_deserts_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(cold_deserts_slope_anamolies_raster, "cold_deserts_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
summary(cold_deserts_slope_final)
cold_deserts_slope_final<-filter(cold_deserts_pue_slope_3, coef < 0.36,coef > -0.1)
summary(cold_deserts_slope_final)

#make a masking raster for slope
cold_deserts_slope_masking_raster<- rasterFromXYZ(cold_deserts_slope_final)
plot(cold_deserts_slope_masking_raster)
#mask npp
npp_slope_mask_cold_deserts_extent<-crop(npp_cold_deserts_50,cold_deserts_slope_masking_raster)
npp_slope_mask_cold_deserts_mask<-crop(npp_slope_mask_cold_deserts_extent,cold_deserts_slope_masking_raster)
plot(npp_slope_mask_cold_deserts_mask)
#mask ppt
precip_slope_mask_cold_deserts_extent<-crop(annual_precip_allyears_cold_deserts,cold_deserts_slope_masking_raster)
precip_slope_mask_cold_deserts_mask<-crop(precip_slope_mask_cold_deserts_extent,cold_deserts_slope_masking_raster)
plot(precip_slope_mask_cold_deserts_mask)
#turn into dataframes
#npp
npp_cold_deserts_masked_round_one_p = rasterToPoints(npp_slope_mask_cold_deserts_mask); df_npp_cold_deserts_masked_round_one = data.frame(npp_cold_deserts_masked_round_one_p)
colnames(df_npp_cold_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_masked_round_one_2<-df_npp_cold_deserts_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cold_deserts_masked_round_one_final <- melt(df_npp_cold_deserts_masked_round_one_2, 
                                                     id.vars = c("x", "y"),
                                                     variable.name = "year") #melt to long format
df_npp_cold_deserts_masked_round_one_final$npp<-df_npp_cold_deserts_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_cold_deserts_masked_round_one_final)
df_npp_cold_deserts_masked_round_one_final_2<-df_npp_cold_deserts_masked_round_one_final[-c(4)]
head(df_npp_cold_deserts_masked_round_one_final_2)

#precip
precip_cold_deserts_masked_round_one_p = rasterToPoints(precip_slope_mask_cold_deserts_mask); df_precip_cold_deserts_masked_round_one = data.frame(precip_cold_deserts_masked_round_one_p )
colnames(df_precip_cold_deserts_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cold_deserts_masked_final_round_one <- melt(df_precip_cold_deserts_masked_round_one, 
                                                        id.vars = c("x", "y"),
                                                        variable.name = "year") #melt to long format

df_precip_cold_deserts_masked_final_round_one$mm<-df_precip_cold_deserts_masked_final_round_one$value*10
df_precip_cold_deserts_masked_final_round_one_2<-df_precip_cold_deserts_masked_final_round_one[-c(4)]
head(df_precip_cold_deserts_masked_final_round_one_2)

#merge these two
cold_deserts_npp_mm_slope_masked<-merge(df_precip_cold_deserts_masked_final_round_one_2,df_npp_cold_deserts_masked_round_one_final_2,by=c('x','y','year'))
#make a pue column
cold_deserts_npp_mm_slope_masked$pue<-cold_deserts_npp_mm_slope_masked$npp/cold_deserts_npp_mm_slope_masked$mm
cold_deserts_npp_mm_slope_masked<-cold_deserts_npp_mm_slope_masked[-c(4,5)]
head(cold_deserts_npp_mm_slope_masked)
summary(cold_deserts_npp_mm_slope_masked)
cold_deserts_npp_mm_slope_masked_2<-na.omit(cold_deserts_npp_mm_slope_masked)
summary(cold_deserts_npp_mm_slope_masked_2)
#yearly pue anamolies 
sd(cold_deserts_npp_mm_slope_masked_2$pue,na.rm=TRUE)
mean(cold_deserts_npp_mm_slope_masked_2$pue,na.rm=TRUE)
min(cold_deserts_npp_mm_slope_masked_2$pue)
#3 sd for yearly pue
0.31 + 3*0.19 #0.88
0.31 - 3*0.19 #-0.26 #below minimum

yearly_pue_anamolies_cold_deserts<-filter(cold_deserts_npp_mm_slope_masked_2,pue > 0.88)
yearly_pue_anamolies_cold_deserts_final<-yearly_pue_anamolies_cold_deserts[-c(3)]
summary(yearly_pue_anamolies_cold_deserts_final)
odd_pue_values_cold_deserts<-rasterFromXYZ(yearly_pue_anamolies_cold_deserts_final) #works
plot(odd_pue_values_cold_deserts)

#mask out bad pue pixels from slope masking raster
cold_deserts_masking_extent<-crop(cold_deserts_slope_masking_raster,odd_pue_values_cold_deserts)
cold_deserts_masking<-mask(cold_deserts_masking_extent,odd_pue_values_cold_deserts,inverse=TRUE) #mask out pixels with bad pue
plot(cold_deserts_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_cold_deserts<-crop(npp_cold_deserts_50,cold_deserts_masking)
plot(npp_extent_crop_cold_deserts)
npp_cold_deserts_masked<-mask(npp_extent_crop_cold_deserts,cold_deserts_masking)
plot(npp_cold_deserts_masked)

#turn into dataframe
npp_cold_deserts_masked_p = rasterToPoints(npp_cold_deserts_masked); df_npp_cold_deserts_masked = data.frame(npp_cold_deserts_masked_p)
colnames(df_npp_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cold_deserts_masked_2<-df_npp_cold_deserts_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cold_deserts_masked_final <- melt(df_npp_cold_deserts_masked_2, 
                                           id.vars = c("x", "y"),
                                           variable.name = "year") #melt to long format
summary(df_npp_cold_deserts_masked_final)
df_npp_cold_deserts_masked_final$npp<-df_npp_cold_deserts_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_cold_deserts_masked_final)
df_npp_cold_deserts_masked_final_2<-df_npp_cold_deserts_masked_final[-c(4)]
summary(df_npp_cold_deserts_masked_final_2)
str(df_npp_cold_deserts_masked_final_2)
#precipitation
precip_extent_crop_cold_deserts<-crop(annual_precip_allyears_cold_deserts,cold_deserts_masking)
plot(precip_extent_crop_cold_deserts)
precip_cold_deserts_masked<-mask(precip_extent_crop_cold_deserts,cold_deserts_masking)
plot(precip_cold_deserts_masked)

#turn into dataframe
precip_cold_deserts_masked_p = rasterToPoints(precip_cold_deserts_masked); df_precip_cold_deserts_masked = data.frame(precip_cold_deserts_masked_p)
colnames(df_precip_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cold_deserts_masked_final <- melt(df_precip_cold_deserts_masked, 
                                              id.vars = c("x", "y"),
                                              variable.name = "year") #melt to long format
summary(df_precip_cold_deserts_masked_final)
df_precip_cold_deserts_masked_final$mm<-df_precip_cold_deserts_masked_final$value*10
df_precip_cold_deserts_masked_final_2<-df_precip_cold_deserts_masked_final[-c(4)]
summary(df_precip_cold_deserts_masked_final_2)
str(df_precip_cold_deserts_masked_final_2)
#merge
df_npp_ppt_cold_deserts_masked_final<-merge(df_precip_cold_deserts_masked_final_2,df_npp_cold_deserts_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_cold_deserts_masked_final)
plot(npp~mm,data=df_npp_ppt_cold_deserts_masked_final)

#make pue column
df_npp_ppt_cold_deserts_masked_final$pue<-df_npp_ppt_cold_deserts_masked_final$npp/df_npp_ppt_cold_deserts_masked_final$mm
plot(pue~mm,data=df_npp_ppt_cold_deserts_masked_final)
summary(df_npp_ppt_cold_deserts_masked_final)

#temperature
temp_extent_crop_cold_deserts<-crop(annual_temp_allyears_cold_deserts,cold_deserts_masking)
plot(temp_extent_crop_cold_deserts)
temp_cold_deserts_masked<-mask(temp_extent_crop_cold_deserts,cold_deserts_masking)
plot(temp_cold_deserts_masked)
temp_cold_deserts_masked_p = rasterToPoints(temp_cold_deserts_masked); df_temp_cold_deserts_masked = data.frame(temp_cold_deserts_masked_p)
colnames(df_temp_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_temp_cold_deserts_masked_final <- melt(df_temp_cold_deserts_masked, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") #melt to long format
summary(df_temp_cold_deserts_masked_final)
df_temp_cold_deserts_masked_final$temp<-df_temp_cold_deserts_masked_final$value
head(df_temp_cold_deserts_masked_final)
df_temp_cold_deserts_masked_final_2<-df_temp_cold_deserts_masked_final[-c(4)]

#merge
df_npp_ppt_temp_cold_deserts_masked_final<-merge(df_temp_cold_deserts_masked_final_2,df_npp_ppt_cold_deserts_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_cold_deserts_masked_final)

#transpiration
transp_extent_crop_cold_deserts<-crop(annual_transp_allyears_cold_deserts,cold_deserts_masking)
plot(transp_extent_crop_cold_deserts)
transp_cold_deserts_masked<-mask(transp_extent_crop_cold_deserts,cold_deserts_masking)
plot(transp_cold_deserts_masked)
transp_cold_deserts_masked_p = rasterToPoints(transp_cold_deserts_masked); df_transp_cold_deserts_masked = data.frame(transp_cold_deserts_masked_p)
colnames(df_transp_cold_deserts_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_transp_cold_deserts_masked_final <- melt(df_transp_cold_deserts_masked, 
                                              id.vars = c("x", "y"),
                                              variable.name = "year") #melt to long format
summary(df_transp_cold_deserts_masked_final)
df_transp_cold_deserts_masked_final$transp<-df_transp_cold_deserts_masked_final$value
head(df_transp_cold_deserts_masked_final)
df_transp_cold_deserts_masked_final_2<-df_transp_cold_deserts_masked_final[-c(4)]

#merge
df_npp_ppt_temp_transp_cold_deserts_masked_final<-merge(df_transp_cold_deserts_masked_final_2,df_npp_ppt_temp_cold_deserts_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_transp_cold_deserts_masked_final)
plot(pue~transp,data=df_npp_ppt_temp_transp_cold_deserts_masked_final)
hist(df_npp_ppt_temp_transp_cold_deserts_masked_final$pue)
summary(df_npp_ppt_temp_transp_cold_deserts_masked_final)


# plotting -----------------------------------------------------------------

#develop a color gradient for reference
cold_deserts_break_npp_ap_slope<-quantile(cold_deserts_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

library(colorspace)
library(sp)
library(colorRamps)
library(latticeExtra) #to add an extra layer
library(gridExtra)

resid.plot<-spplot(cold_deserts_raster_residual,#scales = list(draw = TRUE),
                   at=cold_deserts_break_npp_map_residuals,
                   asp=1,
                   col.regions =
                     rev(heat_hcl(length(cold_deserts_break_npp_map_residuals)-1)),
                   main="cold_deserts spatial npp-map residuals") +
  latticeExtra::layer(sp.polygons(states_steppe, lwd = .75))

cold_deserts_map_residuals<-merge_cold_deserts_npp_annualprecip_temp[-c(3,4,5,7,8)]
head(cold_deserts_map_residuals)

library(spdep)
https://www.youtube.com/watch?v=b3HtV2Mhmvk

?poly2nb
SpatialPolygons(cold_deserts_raster_residual)
neighbor<-poly2nb(cold_deserts_raster_residual)
#autocorrelation
Moran(cold_deserts_raster_residual) #0.62


#npp-map slopes
spplot(cold_deserts_raster_coef,#scales = list(draw = TRUE),
       at=cold_deserts_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(cold_deserts_break_npp_ap_slope)-1)),
       main="") + 
  layer(sp.polygons(states_steppe, lwd = .75))

?Moran

# ggplot ------------------------------------------------------------------

library(ggplot2)
ggplot(df_npp_ppt_masked_final,aes(mm,pue,na.rm=TRUE)) +
  #scale_color_manual(values=c('increase'='blue','decrease'='red'),name="") +
  #geom_histogram(binwidth=0.01) +
  geom_point(pch=1,size=.5) +
  #stat_summary(fun.y="mean",geom="point") +
  scale_y_continuous(expand = c(0,0)) +
  #stat_summary(fun.y="count",geom="bar")
  #geom_hline(yintercept=0) +
  #geom_vline(xintercept=c(0.756595,0.1730594)) + PUE thresholds
  #geom_vline(xintercept=c(0.4927924,0.07177322)) +
  #geom_point() +
  #geom_smooth(method="lm",se=TRUE,linetype="dashed") +
  #geom_hline(yintercept = 713.97,color="black",size=.5) +
  #stat_smooth(method = "lm", formula = y ~ poly(x, 2),color="red",size = 1,se=TRUE) + 
  #ylab("Net primary production") +
  xlab("NPP sensitivity (slope)") +
  #xlab("Precipitation use efficiency") +
  #xlab("Mean annual precipitation (mm)") +
  #xlab("Annnual precipitation (mm)") +
  ggtitle("Shortgrass steppe temporal slope") +
  #ylab(bquote('ANPP ('*g/m^2*')')) +
  #stat_summary(fun.y="mean",geom="point",size=6,pch=19) +
  #geom_boxjitter(outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
  #jitter.height = 0.05, jitter.width = 0.075, errorbar.draw = TRUE) +
  #geom_point(size=4,pch=21,color="black") +
  #stat_summary(geom="point",fun.y="identity",size=5,color="black",aes(fill=as.factor(manipulation)),shape=21,show.legend =FALSE) +
  #scale_fill_manual(values=c('Increase'='blue','Decrease'='red'),name="") +
  #geom_smooth(method="lm",se=FALSE,linetype="dashed",color="black",aes(fill=Treatment),show.legend=FALSE) +
  #geom_smooth(method="lm",se=TRUE,color="red",linetype="dashed",size=1.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), linetype="dashed",size = 1,se=FALSE,color="black") + #geom_smooth(method="lm",se=FALSE,color="black") +
  #xlab("Duration of study") +
  #xlab("% Precipiaton deviation from control") +
  #xlab("% Precipiaton deviation from median") +
  #ylab("ANPP effect size") +
  #ylab("Precipitation treatment") +
  #geom_smooth(method="lm",se=TRUE,color="black",size=0.5) +
  #stat_summary(fun.y = "mean",geom="point") +
  #geom_hline(yintercept = 20,color="black",size=1) +
  theme(
    axis.text = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=24),
    axis.ticks = element_line(color='black'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=22),
    #legend.position = c(.7,.7),
    legend.position = c(.7,.25),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

ggsave("cold_deserts_npp_pue_ap_6000m.pdf",width = 8, height = 6, units = c("in"))
