#cali code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_cali<-extent(npp_cali_50)
annual_precip_allyears_cali<-crop(precip_stack,ex_cali)
annual_temp_allyears_cali<-crop(temp_stack,ex_cali) #temperature
annual_transp_allyears_cali<-crop(transp_stack,ex_cali) #transpiration

plot(npp_cali_50)
plot(annual_precip_allyears_cali)
plot(annual_temp_allyears_cali)
plot(annual_transp_allyears_cali)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_cali_p = rasterToPoints(npp_cali_50); df_npp_cali = data.frame(npp_cali_p)
colnames(df_npp_cali)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_2<-df_npp_cali[-c(33,34)] #get rid of 2016, 2017 columns
cali_npp_melted <- melt(df_npp_cali_2, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format
summary(cali_npp_melted)
cali_npp_melted$npp<-cali_npp_melted$value/10 #change npp value to g/m2 scale
cali_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=cali_npp_melted) #mean annual npp

#annual precipitation
annualprecip_cali_p = rasterToPoints(annual_precip_allyears_cali); df_annualprecip_cali = data.frame(annualprecip_cali_p)
head(df_annualprecip_cali)
colnames(df_annualprecip_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_annualprecip_melted <- melt(df_annualprecip_cali, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") #melt to long format

cali_annualprecip_melted$mm<-cali_annualprecip_melted$value*10 #change to mm
cali_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cali_annualprecip_melted) #get mean annual precip values

#annual temperature
annualtemp_cali_p = rasterToPoints(annual_temp_allyears_cali); df_annualtemp_cali = data.frame(annualtemp_cali_p)
head(df_annualtemp_cali)
colnames(df_annualtemp_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_annualtemp_melted <- melt(df_annualtemp_cali, 
                                       id.vars = c("x", "y"),
                                       variable.name = "year") 
cali_annualtemp_melted$temp<-cali_annualtemp_melted$value #rename column temp
cali_annualtemp_melted <- cali_annualtemp_melted[-4] #get rid of value column
cali_annualtemp_melted_mean<-aggregate(temp~x + y,mean,data=cali_annualtemp_melted) #mean annual temp
head(cali_annualtemp_melted_mean)

#annual transpiration
annualtransp_cali_p = rasterToPoints(annual_transp_allyears_cali); df_annualtransp_cali = data.frame(annualtransp_cali_p)
head(df_annualtransp_cali)
colnames(df_annualtransp_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_annualtransp_melted <- melt(df_annualtransp_cali, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") 
cali_annualtransp_melted$transp<-cali_annualtransp_melted$value #rename column transp
cali_annualtransp_melted <- cali_annualtransp_melted[-4] #get rid of value column
cali_annualtransp_melted_mean<-aggregate(transp~x + y,mean,data=cali_annualtransp_melted) #mean annual transp
head(cali_annualtransp_melted_mean)

#merge the mean npp and ppt datasets
merge_cali_npp_annualprecip<-merge(cali_npp_melted_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip)

#merge mean annual temperature data with this
merge_cali_npp_annualprecip_temp<-merge(cali_annualtemp_melted_mean,merge_cali_npp_annualprecip,by=c("x","y"))
head(merge_cali_npp_annualprecip_temp)

######identifying outliers, odd values to be removed##########
# spatial npp-map relationship
cali.spatial.map.lm = lm(npp ~ mm, data=merge_cali_npp_annualprecip_temp)
summary(cali.spatial.map.lm)

#adding a residuals column to the spatial npp-map relationship to dataframe
merge_cali_npp_annualprecip_temp$residuals_map <- residuals(cali.spatial.map.lm)

# spatial npp-map + mat relationship
cali.spatial.map.mat.lm = lm(npp ~ mm + temp, data=merge_cali_npp_annualprecip_temp)
summary(cali.spatial.map.mat.lm) #slightly higher r-square
par(mfrow=c(2,2))
plot(cali.spatial.map.mat.lm) 
merge_cali_npp_annualprecip_temp$residuals_map_mat <- residuals(cali.spatial.map.mat.lm)

#produce a raster of the spatial npp spatial models
head(merge_cali_npp_annualprecip_temp)
cali_residual_comp<-merge_cali_npp_annualprecip_temp[-c(3,4,5)]
cali_residual_comp_raster<-rasterFromXYZ(cali_residual_comp)
plot(cali_residual_comp_raster)

#get mean pue for each grid cell: use for
merge_cali_npp_annualprecip_temp$pue<-merge_cali_npp_annualprecip_temp$npp/merge_cali_npp_annualprecip_temp$mm
head(merge_cali_npp_annualprecip_temp)
npp_mm_pue_resid_cali<-merge_cali_npp_annualprecip_temp[-c(3,7)]

#diagnostic raster/plot
npp_mm_pue_resid_raster_cali<-rasterFromXYZ(npp_mm_pue_resid_cali)
plot(npp_mm_pue_resid_raster_cali)

#isolating pue: analzing the structure
cali_pue<-merge_cali_npp_annualprecip_temp[-c(3,4,5,6,7)]
head(cali_pue)
hist(cali_pue$pue)
sd(cali_pue$pue)
mean(cali_pue$pue)
min(cali_pue$pue)
max(cali_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
3*0.30 + 0.80 #0.30 as threshold for high values
0.30 - (3*0.80) #-0.15 as threshold for low values

#cali actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_cali <- cali_pue %>%
  dplyr::filter(pue > 1.7 | pue < -2.1) #lower than the actual min, indicates right skew
summary(pue_anamolies_cali)
pue_anamolies_cali_raster<- rasterFromXYZ(pue_anamolies_cali)
plot(pue_anamolies_cali_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_cali_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_cali_raster, "cali_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope
merge_cali_npp_annualprecip_allyears<-merge(cali_annualprecip_melted,cali_npp_melted ,by=c("x","y","year"))
head(merge_cali_npp_annualprecip_allyears)
merge_cali_npp_annualprecip_allyears_2<-merge_cali_npp_annualprecip_allyears[-c(4,6)]
head(merge_cali_npp_annualprecip_allyears_2)
summary(merge_cali_npp_annualprecip_allyears_2)
test_cali<-rasterFromXYZ(merge_cali_npp_annualprecip_allyears_2) #didn't work...

#for generating slope dataset
slope_spatial_cali <- merge_cali_npp_annualprecip_allyears_2 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp~mm, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_cali)
cali_coef_only<- slope_spatial_cali[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

cali_pue_slope<-merge(cali_coef_only,cali_pue,by=c('x','y'))
head(cali_pue_slope)
summary(cali_pue_slope)

#mask out odd pue values
library(dplyr)
cali_pue_slope_2 <- dplyr::filter(cali_pue_slope,pue<1.7,pue>-2.1)
#cali_pue_slope_2  <-filter(cali_pue_slope_2, pue > -0.15)
summary(cali_pue_slope_2)
cali_pue_slope_3<-cali_pue_slope_2[-c(4)]
head(cali_pue_slope_3)

#summary stats of pixel slopes
sd(cali_pue_slope_3$coef)
mean(cali_pue_slope_3$coef)
hist(cali_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.13 + (3*0.13) #0.52
0.13 - (3*0.13) #-0.26

cali_slope_anamolies <- cali_pue_slope_3 %>%
  dplyr::filter(coef > 0.52 | coef < -0.26) #isolate odd slope values
summary(cali_slope_anamolies)

#change to raster
cali_slope_anamolies_raster<- rasterFromXYZ(cali_slope_anamolies)
plot(cali_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(cali_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(cali_slope_anamolies_raster, "cali_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
summary(cali_slope_final)
cali_slope_final<-filter(cali_pue_slope_3, coef < 0.52,coef > -0.26)
summary(cali_slope_final)

#make a masking raster for slope
cali_slope_masking_raster<- rasterFromXYZ(cali_slope_final)
plot(cali_slope_masking_raster)
#mask npp
npp_slope_mask_cali_extent<-crop(npp_cali_50,cali_slope_masking_raster)
npp_slope_mask_cali_mask<-crop(npp_slope_mask_cali_extent,cali_slope_masking_raster)
plot(npp_slope_mask_cali_mask)
#mask ppt
precip_slope_mask_cali_extent<-crop(annual_precip_allyears_cali,cali_slope_masking_raster)
precip_slope_mask_cali_mask<-crop(precip_slope_mask_cali_extent,cali_slope_masking_raster)
plot(precip_slope_mask_cali_mask)
#turn into dataframes
#npp
npp_cali_masked_round_one_p = rasterToPoints(npp_slope_mask_cali_mask); df_npp_cali_masked_round_one = data.frame(npp_cali_masked_round_one_p)
colnames(df_npp_cali_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_masked_round_one_2<-df_npp_cali_masked_round_one[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cali_masked_round_one_final <- melt(df_npp_cali_masked_round_one_2, 
                                                   id.vars = c("x", "y"),
                                                   variable.name = "year") #melt to long format
df_npp_cali_masked_round_one_final$npp<-df_npp_cali_masked_round_one_final$value/10 #change npp value to g/m2 scale
head(df_npp_cali_masked_round_one_final)
df_npp_cali_masked_round_one_final_2<-df_npp_cali_masked_round_one_final[-c(4)]
head(df_npp_cali_masked_round_one_final_2)

#precip
precip_cali_masked_round_one_p = rasterToPoints(precip_slope_mask_cali_mask); df_precip_cali_masked_round_one = data.frame(precip_cali_masked_round_one_p )
colnames(df_precip_cali_masked_round_one)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cali_masked_final_round_one <- melt(df_precip_cali_masked_round_one, 
                                                      id.vars = c("x", "y"),
                                                      variable.name = "year") #melt to long format

df_precip_cali_masked_final_round_one$mm<-df_precip_cali_masked_final_round_one$value*10
df_precip_cali_masked_final_round_one_2<-df_precip_cali_masked_final_round_one[-c(4)]
head(df_precip_cali_masked_final_round_one_2)

#merge these two
cali_npp_mm_slope_masked<-merge(df_precip_cali_masked_final_round_one_2,df_npp_cali_masked_round_one_final_2,by=c('x','y','year'))
#make a pue column
cali_npp_mm_slope_masked$pue<-cali_npp_mm_slope_masked$npp/cali_npp_mm_slope_masked$mm
cali_npp_mm_slope_masked<-cali_npp_mm_slope_masked[-c(4,5)]
head(cali_npp_mm_slope_masked)
summary(cali_npp_mm_slope_masked)
cali_npp_mm_slope_masked_2<-na.omit(cali_npp_mm_slope_masked)
summary(cali_npp_mm_slope_masked_2)
#yearly pue anamolies 
sd(cali_npp_mm_slope_masked_2$pue,na.rm=TRUE)
mean(cali_npp_mm_slope_masked_2$pue,na.rm=TRUE)
min(cali_npp_mm_slope_masked_2$pue)
#3 sd for yearly pue
0.93 + 3*0.62 #2,79
0.93 - 3*0.62 #-0.93 #below minimum

yearly_pue_anamolies_cali<-filter(cali_npp_mm_slope_masked_2,pue > 2.79)
yearly_pue_anamolies_cali_final<-yearly_pue_anamolies_cali[-c(3)]
summary(yearly_pue_anamolies_cali_final)
odd_pue_values_cali<-rasterFromXYZ(yearly_pue_anamolies_cali_final) #works
plot(odd_pue_values_cali)

#mask out bad pue pixels from slope masking raster
cali_masking_extent<-crop(cali_slope_masking_raster,odd_pue_values_cali)
cali_masking<-mask(cali_masking_extent,odd_pue_values_cali,inverse=TRUE) #mask out pixels with bad pue
plot(cali_masking)

#######producing dataframes#########
#produce a final dataframe with all years for analysis
#net primary production
#mask the slope raster by the npp rasterbrick

npp_extent_crop_cali<-crop(npp_cali_50,cali_masking)
plot(npp_extent_crop_cali)
npp_cali_masked<-mask(npp_extent_crop_cali,cali_masking)
plot(npp_cali_masked)

#turn into dataframe
npp_cali_masked_p = rasterToPoints(npp_cali_masked); df_npp_cali_masked = data.frame(npp_cali_masked_p)
colnames(df_npp_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_cali_masked_2<-df_npp_cali_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_cali_masked_final <- melt(df_npp_cali_masked_2, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") #melt to long format
summary(df_npp_cali_masked_final)
df_npp_cali_masked_final$npp<-df_npp_cali_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_cali_masked_final)
df_npp_cali_masked_final_2<-df_npp_cali_masked_final[-c(4)]
summary(df_npp_cali_masked_final_2)
str(df_npp_cali_masked_final_2)
#precipitation
precip_extent_crop_cali<-crop(annual_precip_allyears_cali,cali_masking)
plot(precip_extent_crop_cali)
precip_cali_masked<-mask(precip_extent_crop_cali,cali_masking)
plot(precip_cali_masked)

#turn into dataframe
precip_cali_masked_p = rasterToPoints(precip_cali_masked); df_precip_cali_masked = data.frame(precip_cali_masked_p)
colnames(df_precip_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_cali_masked_final <- melt(df_precip_cali_masked, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") #melt to long format
summary(df_precip_cali_masked_final)
df_precip_cali_masked_final$mm<-df_precip_cali_masked_final$value*10
df_precip_cali_masked_final_2<-df_precip_cali_masked_final[-c(4)]
summary(df_precip_cali_masked_final_2)
str(df_precip_cali_masked_final_2)
#merge
df_npp_ppt_cali_masked_final<-merge(df_precip_cali_masked_final_2,df_npp_cali_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_cali_masked_final)
plot(npp~mm,data=df_npp_ppt_cali_masked_final)

#make pue column
df_npp_ppt_cali_masked_final$pue<-df_npp_ppt_cali_masked_final$npp/df_npp_ppt_cali_masked_final$mm
plot(pue~mm,data=df_npp_ppt_cali_masked_final)
summary(df_npp_ppt_cali_masked_final)

#temperature
temp_extent_crop_cali<-crop(annual_temp_allyears_cali,cali_masking)
plot(temp_extent_crop_cali)
temp_cali_masked<-mask(temp_extent_crop_cali,cali_masking)
plot(temp_cali_masked)
temp_cali_masked_p = rasterToPoints(temp_cali_masked); df_temp_cali_masked = data.frame(temp_cali_masked_p)
colnames(df_temp_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_temp_cali_masked_final <- melt(df_temp_cali_masked, 
                                          id.vars = c("x", "y"),
                                          variable.name = "year") #melt to long format
summary(df_temp_cali_masked_final)
df_temp_cali_masked_final$temp<-df_temp_cali_masked_final$value
head(df_temp_cali_masked_final)
df_temp_cali_masked_final_2<-df_temp_cali_masked_final[-c(4)]

#merge
df_npp_ppt_temp_cali_masked_final<-merge(df_temp_cali_masked_final_2,df_npp_ppt_cali_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_cali_masked_final)

#transpiration
transp_extent_crop_cali<-crop(annual_transp_allyears_cali,cali_masking)
plot(transp_extent_crop_cali)
transp_cali_masked<-mask(transp_extent_crop_cali,cali_masking)
plot(transp_cali_masked)
transp_cali_masked_p = rasterToPoints(transp_cali_masked); df_transp_cali_masked = data.frame(transp_cali_masked_p)
colnames(df_transp_cali_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_transp_cali_masked_final <- melt(df_transp_cali_masked, 
                                            id.vars = c("x", "y"),
                                            variable.name = "year") #melt to long format
summary(df_transp_cali_masked_final)
df_transp_cali_masked_final$transp<-df_transp_cali_masked_final$value
head(df_transp_cali_masked_final)
df_transp_cali_masked_final_2<-df_transp_cali_masked_final[-c(4)]

#merge
df_npp_ppt_temp_transp_cali_masked_final<-merge(df_transp_cali_masked_final_2,df_npp_ppt_temp_cali_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_transp_cali_masked_final)
plot(pue~transp,data=df_npp_ppt_temp_transp_cali_masked_final)
hist(df_npp_ppt_temp_transp_cali_masked_final$pue)
summary(df_npp_ppt_temp_transp_cali_masked_final)


# plotting -----------------------------------------------------------------

#develop a color gradient for reference
cali_break_npp_ap_slope<-quantile(cali_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

library(colorspace)
library(sp)
library(colorRamps)
library(latticeExtra) #to add an extra layer
library(gridExtra)

resid.plot<-spplot(cali_raster_residual,#scales = list(draw = TRUE),
                   at=cali_break_npp_map_residuals,
                   asp=1,
                   col.regions =
                     rev(heat_hcl(length(cali_break_npp_map_residuals)-1)),
                   main="cali spatial npp-map residuals") +
  latticeExtra::layer(sp.polygons(states_steppe, lwd = .75))

cali_map_residuals<-merge_cali_npp_annualprecip_temp[-c(3,4,5,7,8)]
head(cali_map_residuals)

library(spdep)
https://www.youtube.com/watch?v=b3HtV2Mhmvk

?poly2nb
SpatialPolygons(cali_raster_residual)
neighbor<-poly2nb(cali_raster_residual)
#autocorrelation
Moran(cali_raster_residual) #0.62


#npp-map slopes
spplot(cali_raster_coef,#scales = list(draw = TRUE),
       at=cali_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(cali_break_npp_ap_slope)-1)),
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

ggsave("cali_npp_pue_ap_6000m.pdf",width = 8, height = 6, units = c("in"))
