#SGS code: linking precip and NPP

#######importing of data##########
#setting extents for rasters
ex_sgs<-extent(npp_sgs_50)
annual_precip_allyears_sgs<-crop(precip_stack,ex_sgs)
annual_temp_allyears_sgs<-crop(temp_stack,ex_sgs) #temperature
annual_transp_allyears_sgs<-crop(transp_stack,ex_sgs) #transpiration

plot(npp_sgs_50)
plot(annual_precip_allyears_sgs)
plot(annual_temp_allyears_sgs)
plot(annual_transp_allyears_sgs)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_sgs_p = rasterToPoints(npp_sgs_50); df_npp_sgs = data.frame(npp_sgs_p)
head(df_npp_sgs_2)
colnames(df_npp_sgs)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_sgs_2<-df_npp_sgs[-c(33,34)] #get rid of 2016, 2017 columns
sgs_npp_melted <- melt(df_npp_sgs_2, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format
summary(sgs_npp_melted)
sgs_npp_melted$npp<-sgs_npp_melted$value/10 #change npp value to g/m2 scale
sgs_npp_melted_mean<-aggregate(npp ~ x + y,mean,data=sgs_npp_melted) #mean annual npp

#annual precipitation
annualprecip_sgs_p = rasterToPoints(annual_precip_allyears_sgs); df_annualprecip_sgs = data.frame(annualprecip_sgs_p)
head(df_annualprecip_sgs)
colnames(df_annualprecip_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualprecip_melted <- melt(df_annualprecip_sgs, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format

sgs_annualprecip_melted$mm<-sgs_annualprecip_melted$value*10 #change to mm
sgs_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=sgs_annualprecip_melted) #get mean annual precip values

#annual temperature
annualtemp_sgs_p = rasterToPoints(annual_temp_allyears_sgs); df_annualtemp_sgs = data.frame(annualtemp_sgs_p)
head(df_annualtemp_sgs)
colnames(df_annualtemp_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualtemp_melted <- melt(df_annualtemp_sgs, 
                                      id.vars = c("x", "y"),
                                      variable.name = "year") 
sgs_annualtemp_melted$temp<-sgs_annualtemp_melted$value #rename column temp
sgs_annualtemp_melted <- sgs_annualtemp_melted[-4] #get rid of value column
sgs_annualtemp_melted_mean<-aggregate(temp~x + y,mean,data=sgs_annualtemp_melted) #mean annual temp
head(sgs_annualtemp_melted_mean)

#annual transpiration
annualtransp_sgs_p = rasterToPoints(annual_transp_allyears_sgs); df_annualtransp_sgs = data.frame(annualtransp_sgs_p)
head(df_annualtransp_sgs)
colnames(df_annualtransp_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualtransp_melted <- melt(df_annualtransp_sgs, 
                              id.vars = c("x", "y"),
                              variable.name = "year") 
sgs_annualtransp_melted$transp<-sgs_annualtransp_melted$value #rename column transp
sgs_annualtransp_melted <- sgs_annualtransp_melted[-4] #get rid of value column
sgs_annualtransp_melted_mean<-aggregate(transp~x + y,mean,data=sgs_annualtransp_melted) #mean annual transp
head(sgs_annualtransp_melted_mean)

#merge the mean npp and ppt datasets
merge_sgs_npp_annualprecip<-merge(sgs_npp_melted_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip)

#merge mean annual temperature data with this
merge_sgs_npp_annualprecip_temp<-merge(sgs_annualtemp_melted_mean,merge_sgs_npp_annualprecip,by=c("x","y"))
head(merge_sgs_npp_annualprecip_temp)

######identifying outliers, odd values to be removed##########
# spatial npp-map relationship
sgs.spatial.map.lm = lm(npp ~ mm, data=merge_sgs_npp_annualprecip_temp)
summary(sgs.spatial.map.lm)

#adding a residuals column to the spatial npp-map relationship to dataframe
merge_sgs_npp_annualprecip_temp$residuals_map <- residuals(sgs.spatial.map.lm)

# spatial npp-map + mat relationship
sgs.spatial.map.mat.lm = lm(npp ~ mm + temp, data=merge_sgs_npp_annualprecip_temp)
summary(sgs.spatial.map.mat.lm) #slightly higher r-square
par(mfrow=c(2,2))
plot(sgs.spatial.map.mat.lm) 
merge_sgs_npp_annualprecip_temp$residuals_map_mat <- residuals(sgs.spatial.map.mat.lm)

#produce a raster of the spatial npp spatial models
head(merge_sgs_npp_annualprecip_temp)
sgs_residual_comp<-merge_sgs_npp_annualprecip_temp[-c(3,4,5)]
sgs_residual_comp_raster<-rasterFromXYZ(sgs_residual_comp)
plot(sgs_residual_comp_raster)

#get mean pue for each grid cell: use for
merge_sgs_npp_annualprecip_temp$pue<-merge_sgs_npp_annualprecip_temp$npp/merge_sgs_npp_annualprecip_temp$mm
head(merge_sgs_npp_annualprecip_temp)
npp_mm_pue_resid_sgs<-merge_sgs_npp_annualprecip_temp[-c(3,7)]

#diagnostic raster/plot
npp_mm_pue_resid_raster_sgs<-rasterFromXYZ(npp_mm_pue_resid_sgs)
plot(npp_mm_pue_resid_raster_sgs)

#isolating pue: analzing the structure
sgs_pue<-merge_sgs_npp_annualprecip_temp[-c(3,4,5,6,7)]
head(sgs_pue)
hist(sgs_pue$pue)
sd(sgs_pue$pue)
mean(sgs_pue$pue)
min(sgs_pue$pue)
max(sgs_pue$pue)

#use 3 standard deviations as criteria for anamolous mean PUE values
#round to two decimal places
3*0.097 + 0.46 #0.75 as threshold for high values
0.46 - (3*0.097) #0.17 as threshold for low values

#sgs actually seems fine, no extremely odd values...
#round to two decimal places

# isolate values greater than 3sd away
pue_anamolies_sgs <- sgs_pue %>%
  dplyr::filter(pue > 0.75 | pue < 0.17) #lower than the actual min, indicates right skew
summary(pue_anamolies_sgs)
pue_anamolies_sgs_raster<- rasterFromXYZ(pue_anamolies_sgs)
plot(pue_anamolies_sgs_raster)

#turn into geotiff for analaysis in google earth engine
crs(pue_anamolies_sgs_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(pue_anamolies_sgs_raster, "sgs_outliers", format = "GTiff",overwrite=TRUE)

#these outlier values are associated with agriculutre (assessed through google earth), 
#remove them and then look at temporal npp-ppt slopes for the next steps

#merge npp-ppt datasets all years to for dataset to generate slope
merge_sgs_npp_annualprecip_allyears<-merge(sgs_annualprecip_melted,sgs_npp_melted ,by=c("x","y","year"))
head(merge_sgs_npp_annualprecip_allyears)
merge_sgs_npp_annualprecip_allyears_2<-merge_sgs_npp_annualprecip_allyears[-c(4,6)]
head(merge_sgs_npp_annualprecip_allyears_2)

#for generating slope dataset
library(dplyr)
slope_spatial_sgs <- merge_sgs_npp_annualprecip_allyears_2 %>% group_by(x, y) %>%
  do(model = lm(npp~mm, data = .)) %>%
  mutate(coef=coef(model)[2]) ##generate dataset with slopes

head(slope_spatial_sgs)
sgs_coef_only<- slope_spatial_sgs[ -c(3) ] #isolate coefficient so only slope is graphed
head(sgs_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

sgs_pue_slope<-merge(sgs_coef_only,sgs_pue,by=c('x','y'))
head(sgs_pue_slope_2)
#mask out odd pue values
sgs_pue_slope_2 <- sgs_pue_slope %>%
  dplyr::filter(pue < 0.75 | pue > 0.17) # get rid of PUE outliers
head(sgs_pue_slope_2)
sgs_pue_slope_3<-sgs_pue_slope_2[-c(4)]
head(sgs_pue_slope_3)

#summary stats of pixel slopes
sd(sgs_pue_slope_3$coef)
mean(sgs_pue_slope_3$coef)
hist(sgs_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.28 + (3*0.070) #0.49
0.28 - (3*0.070) #0.07

sgs_slope_anamolies <- sgs_pue_slope_3 %>%
  dplyr::filter(coef > 0.49 | coef < 0.070) #isolate odd slope values
summary(sgs_slope_anamolies)

#change to raster
sgs_slope_anamolies_raster<- rasterFromXYZ(sgs_slope_anamolies)
plot(sgs_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(sgs_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(sgs_slope_anamolies_raster, "sgs_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre. remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
sgs_slope_final <- sgs_pue_slope_3 %>%
  dplyr::filter(coef < 0.49 | coef > 0.07) #isolate odd slope values
summary(sgs_slope_anamolies)

#while the cutoffs are rounded, raster values need to have up to 5 decomial places.
#turn into raster
sgs_slope_final_raster<- rasterFromXYZ(sgs_slope_final)
plot(npp_sgs_50)

#######producing dataframes#########
#produce a final dataframe with all years for analysis

#net primary production
#mask the slope raster by the npp rasterbrick
npp_extent_crop_sgs<-crop(npp_sgs_50,sgs_slope_final_raster)
plot(npp_extent_crop_sgs)
npp_sgs_masked<-mask(npp_extent_crop_sgs,sgs_slope_final_raster)
plot(npp_sgs_masked)

#turn into dataframe
npp_masked_p = rasterToPoints(npp_masked); df_npp_masked = data.frame(npp_masked_p)
colnames(df_npp_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_npp_masked_2<-df_npp_masked[-c(33,34)] #get rid of 2016, 2017 columns
df_npp_masked_final <- melt(df_npp_masked_2, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format
summary(df_npp_masked_final)
df_npp_masked_final$npp<-df_npp_masked_final$value/10 #change npp value to g/m2 scale
head(df_npp_masked_final)
df_npp_masked_final_2<-df_npp_masked_final[-c(4)]

#precipitation
precip_extent_crop_sgs<-crop(annual_precip_allyears_sgs,sgs_slope_final_raster)
plot(precip_extent_crop_sgs)
precip_sgs_masked<-mask(precip_extent_crop_sgs,sgs_slope_final_raster)
plot(precip_sgs_masked)

#turn into dataframe
precip_sgs_masked_p = rasterToPoints(precip_sgs_masked); df_precip_sgs_masked = data.frame(precip_sgs_masked_p)
colnames(df_precip_sgs_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_precip_masked_final <- melt(df_precip_sgs_masked, 
                            id.vars = c("x", "y"),
                            variable.name = "year") #melt to long format
summary(df_precip_masked_final)
df_precip_masked_final$mm<-df_precip_masked_final$value*10
head(df_precip_masked_final)
df_precip_masked_final_2<-df_precip_masked_final[-c(4)]

#merge
df_npp_ppt_masked_final<-merge(df_precip_masked_final_2,df_npp_masked_final_2,by=c('x','y','year'))
head(df_npp_ppt_masked_final)
plot(npp~mm,data=df_npp_ppt_masked_final)
#make pue column
df_npp_ppt_masked_final$pue<-df_npp_ppt_masked_final$npp/df_npp_ppt_masked_final$mm
plot(pue~mm,data=df_npp_ppt_masked_final)

#temperature
temp_extent_crop_sgs<-crop(annual_temp_allyears_sgs,sgs_slope_final_raster)
plot(temp_extent_crop_sgs)
temp_sgs_masked<-mask(temp_extent_crop_sgs,sgs_slope_final_raster)
plot(temp_sgs_masked)
temp_sgs_masked_p = rasterToPoints(temp_sgs_masked); df_temp_sgs_masked = data.frame(temp_sgs_masked_p)
colnames(df_temp_sgs_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_temp_masked_final <- melt(df_temp_sgs_masked, 
                               id.vars = c("x", "y"),
                               variable.name = "year") #melt to long format
summary(df_temp_masked_final)
df_temp_masked_final$temp<-df_temp_masked_final$value
head(df_temp_masked_final)
df_temp_masked_final_2<-df_temp_masked_final[-c(4)]

#merge
df_npp_ppt_temp_masked_final<-merge(df_temp_masked_final_2,df_npp_ppt_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_masked_final)

#transpiration
transp_extent_crop_sgs<-crop(annual_transp_allyears_sgs,sgs_slope_final_raster)
plot(transp_extent_crop_sgs)
transp_sgs_masked<-mask(transp_extent_crop_sgs,sgs_slope_final_raster)
plot(transp_sgs_masked)
transp_sgs_masked_p = rasterToPoints(transp_sgs_masked); df_transp_sgs_masked = data.frame(transp_sgs_masked_p)
colnames(df_transp_sgs_masked)[3:32] <-paste(1986:2015) #rename coluns to years
df_transp_masked_final <- melt(df_transp_sgs_masked, 
                             id.vars = c("x", "y"),
                             variable.name = "year") #melt to long format
summary(df_transp_masked_final)
df_transp_masked_final$transp<-df_transp_masked_final$value
head(df_transp_masked_final)
df_transp_masked_final_2<-df_transp_masked_final[-c(4)]

#merge
df_npp_ppt_temp_transp_masked_final<-merge(df_transp_masked_final_2,df_npp_ppt_temp_masked_final,by=c('x','y','year'))
head(df_npp_ppt_temp_transp_masked_final)
plot(sqrt(pue)~sqrt(transp),data=df_npp_ppt_temp_transp_masked_final)


# plotting -----------------------------------------------------------------

#develop a color gradient for reference
sgs_break_npp_ap_slope<-quantile(sgs_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

library(colorspace)
library(sp)
library(colorRamps)
library(latticeExtra) #to add an extra layer
library(gridExtra)

resid.plot<-spplot(sgs_raster_residual,#scales = list(draw = TRUE),
       at=sgs_break_npp_map_residuals,
       asp=1,
       col.regions =
         rev(heat_hcl(length(sgs_break_npp_map_residuals)-1)),
       main="SGS spatial npp-map residuals") +
  latticeExtra::layer(sp.polygons(states_steppe, lwd = .75))

sgs_map_residuals<-merge_sgs_npp_annualprecip_temp[-c(3,4,5,7,8)]
head(sgs_map_residuals)

library(spdep)
https://www.youtube.com/watch?v=b3HtV2Mhmvk

?poly2nb
SpatialPolygons(sgs_raster_residual)
neighbor<-poly2nb(sgs_raster_residual)
#autocorrelation
Moran(sgs_raster_residual) #0.62


#npp-map slopes
spplot(sgs_raster_coef,#scales = list(draw = TRUE),
       at=sgs_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(sgs_break_npp_ap_slope)-1)),
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

ggsave("sgs_npp_pue_ap_6000m.pdf",width = 8, height = 6, units = c("in"))
