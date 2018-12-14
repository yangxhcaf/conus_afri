#northern_mixed code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_northern_mixed<-extent(npp_northern_mixed_50)
annual_precip_allyears_northern_mixed<-crop(precip_stack,ex_northern_mixed)
annual_temp_allyears_northern_mixed<-crop(temp_stack,ex_northern_mixed) #temperature
annual_transp_allyears_northern_mixed<-crop(transp_stack,ex_northern_mixed) #transpiration

plot(npp_northern_mixed_50)
plot(annual_precip_allyears_northern_mixed)
plot(annual_temp_allyears_northern_mixed)
plot(annual_transp_allyears_northern_mixed)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_northern_mixed_p = rasterToPoints(npp_northern_mixed_50); df_npp_northern_mixed = data.frame(npp_northern_mixed_p)
colnames(df_npp_northern_mixed)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_2<-df_npp_northern_mixed[-c(33,34)] #get rid of 2016, 2017 columns
northern_mixed_npp_melted <- melt(df_npp_northern_mixed_2, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format
summary(northern_mixed_npp_melted)
northern_mixed_npp_melted$npp<-northern_mixed_npp_melted$value/10 #change npp value to g/m2 scale
northern_mixed_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=northern_mixed_npp_melted) #mean annual npp

#annual precipitation
annualprecip_northern_mixed_p = rasterToPoints(annual_precip_allyears_northern_mixed); df_annualprecip_northern_mixed = data.frame(annualprecip_northern_mixed_p)
head(df_annualprecip_northern_mixed)
colnames(df_annualprecip_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_annualprecip_melted <- melt(df_annualprecip_northern_mixed, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format

northern_mixed_annualprecip_melted$mm<-northern_mixed_annualprecip_melted$value*10 #change to mm
northern_mixed_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=northern_mixed_annualprecip_melted) #get mean annual precip values

#annual temperature
annualtemp_northern_mixed_p = rasterToPoints(annual_temp_allyears_northern_mixed); df_annualtemp_northern_mixed = data.frame(annualtemp_northern_mixed_p)
head(df_annualtemp_northern_mixed)
colnames(df_annualtemp_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_annualtemp_melted <- melt(df_annualtemp_northern_mixed, 
                              id.vars = c("x", "y"),
                              variable.name = "year") 
northern_mixed_annualtemp_melted$temp<-northern_mixed_annualtemp_melted$value #rename column temp
northern_mixed_annualtemp_melted <- northern_mixed_annualtemp_melted[-4] #get rid of value column
northern_mixed_annualtemp_melted_mean<-aggregate(temp~x + y,mean,data=northern_mixed_annualtemp_melted) #mean annual temp
head(northern_mixed_annualtemp_melted_mean)

#annual transpiration
annualtransp_northern_mixed_p = rasterToPoints(annual_transp_allyears_northern_mixed); df_annualtransp_northern_mixed = data.frame(annualtransp_northern_mixed_p)
head(df_annualtransp_northern_mixed)
colnames(df_annualtransp_northern_mixed)[3:32] <-paste(1986:2015) #rename columns to years
northern_mixed_annualtransp_melted <- melt(df_annualtransp_northern_mixed, 
                                id.vars = c("x", "y"),
                                variable.name = "year") 
northern_mixed_annualtransp_melted$transp<-northern_mixed_annualtransp_melted$value #rename column transp
northern_mixed_annualtransp_melted <- northern_mixed_annualtransp_melted[-4] #get rid of value column
northern_mixed_annualtransp_melted_mean<-aggregate(transp~x + y,mean,data=northern_mixed_annualtransp_melted) #mean annual transp
head(northern_mixed_annualtransp_melted_mean)

#merge the mean npp and ppt datasets
merge_northern_mixed_npp_annualprecip<-merge(northern_mixed_npp_melted_mean,northern_mixed_annualprecip_melted_mean,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip)

#merge mean annual temperature data with this
merge_northern_mixed_npp_annualprecip_temp<-merge(northern_mixed_annualtemp_melted_mean,merge_northern_mixed_npp_annualprecip,by=c("x","y"))
head(merge_northern_mixed_npp_annualprecip_temp)

######identifying outliers, odd values to be removed##########
# spatial npp-map relationship
northern_mixed.spatial.map.lm = lm(npp ~ mm, data=merge_northern_mixed_npp_annualprecip_temp)
summary(northern_mixed.spatial.map.lm)

#adding a residuals column to the spatial npp-map relationship to dataframe
merge_northern_mixed_npp_annualprecip_temp$residuals_map <- residuals(northern_mixed.spatial.map.lm)

# spatial npp-map + mat relationship
northern_mixed.spatial.map.mat.lm = lm(npp ~ mm + temp, data=merge_northern_mixed_npp_annualprecip_temp)
summary(northern_mixed.spatial.map.mat.lm) #slightly higher r-square
par(mfrow=c(2,2))
plot(northern_mixed.spatial.map.mat.lm) 
merge_northern_mixed_npp_annualprecip_temp$residuals_map_mat <- residuals(northern_mixed.spatial.map.mat.lm)

#produce a raster of the spatial npp spatial models
head(merge_northern_mixed_npp_annualprecip_temp)
northern_mixed_residual_comp<-merge_northern_mixed_npp_annualprecip_temp[-c(3,4,5)]
northern_mixed_residual_comp_raster<-rasterFromXYZ(northern_mixed_residual_comp)
plot(northern_mixed_residual_comp_raster)

#get mean pue for each grid cell: use for
merge_northern_mixed_npp_annualprecip_temp$pue<-merge_northern_mixed_npp_annualprecip_temp$npp/merge_northern_mixed_npp_annualprecip_temp$mm
head(merge_northern_mixed_npp_annualprecip_temp)
npp_mm_pue_resid_northern_mixed<-merge_northern_mixed_npp_annualprecip_temp[-c(3,7)]

#diagnostic raster/plot
npp_mm_pue_resid_raster_northern_mixed<-rasterFromXYZ(npp_mm_pue_resid_northern_mixed)
plot(npp_mm_pue_resid_raster_northern_mixed)

#isolating pue: analzing the structure
northern_mixed_pue<-merge_northern_mixed_npp_annualprecip_temp[-c(3,4,5,6,7)]
head(northern_mixed_pue)
hist(northern_mixed_pue$pue)
sd(northern_mixed_pue$pue)
mean(northern_mixed_pue$pue)
min(northern_mixed_pue$pue)
max(northern_mixed_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
3*0.12 + 0.52 #0.88 as threshold for high values
0.52 - (3*0.12) #0.16 as threshold for low values

#northern_mixed actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_northern_mixed <- northern_mixed_pue %>%
  dplyr::filter(pue > 0.88 | pue < 0.16) #lower than the actual min, indicates right skew
summary(pue_anamolies_northern_mixed)
pue_anamolies_northern_mixed_raster<- rasterFromXYZ(pue_anamolies_northern_mixed)
plot(pue_anamolies_northern_mixed_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_northern_mixed_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_northern_mixed_raster, "northern_mixed_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope
merge_northern_mixed_npp_annualprecip_allyears<-merge(northern_mixed_annualprecip_melted,northern_mixed_npp_melted ,by=c("x","y","year"))
head(merge_northern_mixed_npp_annualprecip_allyears)
merge_northern_mixed_npp_annualprecip_allyears_2<-merge_northern_mixed_npp_annualprecip_allyears[-c(4,6)]
head(merge_northern_mixed_npp_annualprecip_allyears_2)
test_northern_mixed<-rasterFromXYZ(merge_northern_mixed_npp_annualprecip_allyears_2) #worked
#for generating slope dataset
library(dplyr)
slope_spatial_northern_mixed <- merge_northern_mixed_npp_annualprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_northern_mixed)
northern_mixed_coef_only<- slope_spatial_northern_mixed[ -c(3) ] #isolate coefficient so only slope is graphed
head(northern_mixed_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

northern_mixed_pue_slope<-merge(northern_mixed_coef_only,northern_mixed_pue,by=c('x','y'))
head(northern_mixed_pue_slope)
summary(northern_mixed_pue_slope)
#mask out odd pue values
northern_mixed_pue_slope_2 <- filter(northern_mixed_pue_slope,pue < 0.88,pue > 0.16 ) 
summary(northern_mixed_pue_slope_2)
northern_mixed_pue_slope_3<-northern_mixed_pue_slope_2.1[-c(4)]
head(northern_mixed_pue_slope_3)

#summary stats of pixel slopes
sd(northern_mixed_pue_slope_3$coef)
mean(northern_mixed_pue_slope_3$coef)
hist(northern_mixed_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.24 + (3*0.082) #0.49
0.24 - (3*0.082) #0.07

northern_mixed_slope_anamolies <- northern_mixed_pue_slope_3 %>%
  dplyr::filter(coef > 0.49 | coef < -0.006) #isolate odd slope values
summary(northern_mixed_slope_anamolies)
summary(northern_mixed_pue_slope_3)

#change to raster
northern_mixed_slope_anamolies_raster<- rasterFromXYZ(northern_mixed_slope_anamolies)
plot(northern_mixed_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(northern_mixed_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(northern_mixed_slope_anamolies_raster, "northern_mixed_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels

summary(northern_mixed_slope_final)
northern_mixed_slope_final<-filter(northern_mixed_pue_slope_3, coef < 0.49,coef > -0.006)
summary(northern_mixed_slope_final)

#make a masking raster for slope
northern_mixed_slope_masking_raster<- rasterFromXYZ(northern_mixed_slope_final)
plot(northern_mixed_slope_masking_raster)
#mask npp
npp_slope_mask_northern_mixed_extent<-crop(npp_northern_mixed_50,northern_mixed_slope_masking_raster)
npp_slope_mask_northern_mixed_mask<-crop(npp_slope_mask_northern_mixed_extent,northern_mixed_slope_masking_raster)
plot(npp_slope_mask_northern_mixed_mask)
#mask ppt
precip_slope_mask_northern_mixed_extent<-crop(annual_precip_allyears_northern_mixed,northern_mixed_slope_masking_raster)
precip_slope_mask_northern_mixed_mask<-crop(precip_slope_mask_northern_mixed_extent,northern_mixed_slope_masking_raster)
plot(precip_slope_mask_northern_mixed_mask)
#turn into dataframes
#npp
npp_northern_mixed_masked_round_one_p = rasterToPoints(npp_slope_mask_northern_mixed_mask); df_npp_northern_mixed_masked_round_one = data.frame(npp_northern_mixed_masked_round_one_p)
colnames(df_npp_northern_mixed_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_masked_round_one_2<-df_npp_northern_mixed_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_northern_mixed_masked_round_one_final <- melt(df_npp_northern_mixed_masked_round_one_2, 
                                          id.vars = c("x", "y"),
                                          variable.name = "year") #melt to long format
df_npp_northern_mixed_masked_round_one_final$npp<-df_npp_northern_mixed_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_northern_mixed_masked_round_one_final)
df_npp_northern_mixed_masked_round_one_final_2<-df_npp_northern_mixed_masked_round_one_final[-c(4)]
head(df_npp_northern_mixed_masked_round_one_final_2)

#precip
precip_northern_mixed_masked_round_one_p = rasterToPoints(precip_slope_mask_northern_mixed_mask); df_precip_northern_mixed_masked_round_one = data.frame(precip_northern_mixed_masked_round_one_p )
colnames(df_precip_northern_mixed_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_northern_mixed_masked_final_round_one <- melt(df_precip_northern_mixed_masked_round_one, 
                                             id.vars = c("x", "y"),
                                             variable.name = "year") #melt to long format

df_precip_northern_mixed_masked_final_round_one$mm<-df_precip_northern_mixed_masked_final_round_one$value*10
df_precip_northern_mixed_masked_final_round_one_2<-df_precip_northern_mixed_masked_final_round_one[-c(4)]
head(df_precip_northern_mixed_masked_final_round_one_2)

#merge these two
northern_mixed_npp_mm_slope_masked<-merge(df_precip_northern_mixed_masked_final_round_one_2,df_npp_northern_mixed_masked_round_one_final_2,by=c('x','y','year'))
#make a pue column
northern_mixed_npp_mm_slope_masked$pue<-northern_mixed_npp_mm_slope_masked$npp/northern_mixed_npp_mm_slope_masked$mm
northern_mixed_npp_mm_slope_masked<-northern_mixed_npp_mm_slope_masked[-c(4,5)]
head(northern_mixed_npp_mm_slope_masked)

#yearly pue anamolies (incorporate later)
sd(northern_mixed_npp_mm_slope_masked$pue)
mean(northern_mixed_npp_mm_slope_masked$pue)
min(northern_mixed_npp_mm_slope_masked$pue)
summary(northern_mixed_npp_mm_slope_masked)
#3 sd for yearly pue
0.53 + 3*0.16 #1.01
0.53 - 3*0.16 #0.05

yearly_pue_anamolies_northern_mixed<-dplyr::filter(northern_mixed_npp_mm_slope_masked,pue>1.01 | pue < .05)
summary(yearly_pue_anamolies_northern_mixed)
yearly_pue_anamolies_northern_mixed_final<-yearly_pue_anamolies_northern_mixed[-c(3)]
head(yearly_pue_anamolies_northern_mixed_final)
odd_pue_values_northern_mixed<-rasterFromXYZ(yearly_pue_anamolies_northern_mixed)
plot(odd_pue_values_northern_mixed)

#mask out bad pue pixels from slope masking raster
northern_mixed_masking_extent<-crop(northern_mixed_slope_masking_raster,odd_pue_values_northern_mixed)
northern_mixed_masking<-mask(northern_mixed_masking_extent,odd_pue_values_northern_mixed,inverse=TRUE) #mask out pixels with bad pue
plot(northern_mixed_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_northern_mixed<-crop(npp_northern_mixed_50,northern_mixed_masking)
plot(npp_extent_crop_northern_mixed)
npp_northern_mixed_masked<-mask(npp_extent_crop_northern_mixed,northern_mixed_masking)
plot(npp_northern_mixed_masked)

#turn into dataframe
npp_northern_mixed_masked_p = rasterToPoints(npp_northern_mixed_masked); df_npp_northern_mixed_masked = data.frame(npp_northern_mixed_masked_p)
colnames(df_npp_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_northern_mixed_masked_2<-df_npp_northern_mixed_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_northern_mixed_masked_final <- melt(df_npp_northern_mixed_masked_2, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
summary(df_npp_northern_mixed_masked_final)
df_npp_northern_mixed_masked_final$npp<-df_npp_northern_mixed_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_northern_mixed_masked_final)
df_npp_northern_mixed_masked_final_2<-df_npp_northern_mixed_masked_final[-c(4)]
summary(df_npp_northern_mixed_masked_final_2)
str(df_npp_northern_mixed_masked_final_2)
#precipitation
precip_extent_crop_northern_mixed<-crop(annual_precip_allyears_northern_mixed,northern_mixed_masking)
plot(precip_extent_crop_northern_mixed)
precip_northern_mixed_masked<-mask(precip_extent_crop_northern_mixed,northern_mixed_masking)
plot(precip_northern_mixed_masked)

#turn into dataframe
precip_northern_mixed_masked_p = rasterToPoints(precip_northern_mixed_masked); df_precip_northern_mixed_masked = data.frame(precip_northern_mixed_masked_p)
colnames(df_precip_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_northern_mixed_masked_final <- melt(df_precip_northern_mixed_masked, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
summary(df_precip_northern_mixed_masked_final)
df_precip_northern_mixed_masked_final$mm<-df_precip_northern_mixed_masked_final$value*10
df_precip_northern_mixed_masked_final_2<-df_precip_northern_mixed_masked_final[-c(4)]
summary(df_precip_northern_mixed_masked_final_2)
str(df_precip_northern_mixed_masked_final_2)
#merge
df_npp_ppt_northern_mixed_masked_final<-merge(df_precip_northern_mixed_masked_final_2,df_npp_northern_mixed_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_northern_mixed_masked_final)
plot(npp~mm,data=df_npp_ppt_northern_mixed_masked_final)

#make pue column
df_npp_ppt_northern_mixed_masked_final$pue<-df_npp_ppt_northern_mixed_masked_final$npp/df_npp_ppt_northern_mixed_masked_final$mm
plot(pue~mm,data=df_npp_ppt_northern_mixed_masked_final)
summary(df_npp_ppt_northern_mixed_masked_final)

#temperature
temp_extent_crop_northern_mixed<-crop(annual_temp_allyears_northern_mixed,northern_mixed_masking)
plot(temp_extent_crop_northern_mixed)
temp_northern_mixed_masked<-mask(temp_extent_crop_northern_mixed,northern_mixed_masking)
plot(temp_northern_mixed_masked)
temp_northern_mixed_masked_p = rasterToPoints(temp_northern_mixed_masked); df_temp_northern_mixed_masked = data.frame(temp_northern_mixed_masked_p)
colnames(df_temp_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_temp_northern_mixed_masked_final <- melt(df_temp_northern_mixed_masked, 
                                 id.vars = c("x", "y"),
                                 variable.name = "year") #melt to long format
summary(df_temp_northern_mixed_masked_final)
df_temp_northern_mixed_masked_final$temp<-df_temp_northern_mixed_masked_final$value
head(df_temp_northern_mixed_masked_final)
df_temp_northern_mixed_masked_final_2<-df_temp_northern_mixed_masked_final[-c(4)]

#merge
df_npp_ppt_temp_northern_mixed_masked_final<-merge(df_temp_northern_mixed_masked_final_2,df_npp_ppt_northern_mixed_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_northern_mixed_masked_final)

#transpiration
transp_extent_crop_northern_mixed<-crop(annual_transp_allyears_northern_mixed,northern_mixed_masking)
plot(transp_extent_crop_northern_mixed)
transp_northern_mixed_masked<-mask(transp_extent_crop_northern_mixed,northern_mixed_masking)
plot(transp_northern_mixed_masked)
transp_northern_mixed_masked_p = rasterToPoints(transp_northern_mixed_masked); df_transp_northern_mixed_masked = data.frame(transp_northern_mixed_masked_p)
colnames(df_transp_northern_mixed_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_transp_northern_mixed_masked_final <- melt(df_transp_northern_mixed_masked, 
                                   id.vars = c("x", "y"),
                                   variable.name = "year") #melt to long format
summary(df_transp_northern_mixed_masked_final)
df_transp_northern_mixed_masked_final$transp<-df_transp_northern_mixed_masked_final$value
head(df_transp_northern_mixed_masked_final)
df_transp_northern_mixed_masked_final_2<-df_transp_northern_mixed_masked_final[-c(4)]

#merge
df_npp_ppt_temp_transp_northern_mixed_masked_final<-merge(df_transp_northern_mixed_masked_final_2,df_npp_ppt_temp_northern_mixed_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_transp_northern_mixed_masked_final)
plot(pue~transp,data=df_npp_ppt_temp_transp_northern_mixed_masked_final)
hist(df_npp_ppt_temp_transp_northern_mixed_masked_final$pue)
summary(df_npp_ppt_temp_transp_northern_mixed_masked_final)


# plotting -----------------------------------------------------------------

#develop a color gradient for reference
northern_mixed_break_npp_ap_slope<-quantile(northern_mixed_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

library(colorspace)
library(sp)
library(colorRamps)
library(latticeExtra) #to add an extra layer
library(gridExtra)

resid.plot<-spplot(northern_mixed_raster_residual,#scales = list(draw = TRUE),
                   at=northern_mixed_break_npp_map_residuals,
                   asp=1,
                   col.regions =
                     rev(heat_hcl(length(northern_mixed_break_npp_map_residuals)-1)),
                   main="northern_mixed spatial npp-map residuals") +
  latticeExtra::layer(sp.polygons(states_steppe, lwd = .75))

northern_mixed_map_residuals<-merge_northern_mixed_npp_annualprecip_temp[-c(3,4,5,7,8)]
head(northern_mixed_map_residuals)

library(spdep)
https://www.youtube.com/watch?v=b3HtV2Mhmvk

?poly2nb
SpatialPolygons(northern_mixed_raster_residual)
neighbor<-poly2nb(northern_mixed_raster_residual)
#autocorrelation
Moran(northern_mixed_raster_residual) #0.62


#npp-map slopes
spplot(northern_mixed_raster_coef,#scales = list(draw = TRUE),
       at=northern_mixed_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(northern_mixed_break_npp_ap_slope)-1)),
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

ggsave("northern_mixed_npp_pue_ap_6000m.pdf",width = 8, height = 6, units = c("in"))
