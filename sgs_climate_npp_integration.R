#SGS code: linking precip and NPP

#setting extents for rasters
ex_sgs<-extent(npp_sgs_50)
annual_precip_allyears_sgs<-crop(precip_stack,ex_sgs)
annual_temp_allyears_sgs<-crop(temp_stack,ex_sgs) #temperature
annual_transp_allyears_sgs<-crop(transp_stack,ex_sgs) #transpiration

plot(annual_precip_allyears_sgs)
plot(new_all_years_sgs)
plot(annual_temp_allyears_sgs)
plot(annual_transp_allyears_sgs)

#changing rasters to dataframes
#NPP
library(dplyr)
library(reshape2)
npp_sgs_p = rasterToPoints(npp_sgs_50); df_npp_sgs = data.frame(npp_sgs_p)
head(df_npp_sgs)
colnames(df_npp_sgs)[3:32] <-paste(1986:2015) #rename coluns to years
sgs_npp_melted <- melt(df_npp_sgs, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format

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

#identifying outliers, odd values to be removed
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
3*0.09725593 + 0.4648272 #0.756595 as threshold for high values
0.4648272- (3*0.09725593) #0.1730594 as threshold for low values

#sgs actually seems fine, no extremely odd values...

# isolate values greater than 3sd away
pue_anamolies_sgs <- sgs_pue %>%
  dplyr::filter(pue > 0.756595 | pue < 0.1730594) #lower than the actual min, indicates right skew
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

library(dplyr)
slope_spatial_sgs <- merge_sgs_npp_annualprecip_allyears_2 %>% group_by(x, y) %>%
  do(model = lm(npp~mm, data = .)) %>%
  mutate(coef=coef(model)[2]) ##generate dataset with slopes

summary(slope_spatial_sgs$coef)
head(slope_spatial_sgs)
sgs_coef_only<- slope_spatial_sgs[ -c(3) ] #isolate coefficient so only slope is graphed
head(sgs_coef_only) #looks good

#merge this dataset with the pue dataset, so that outlier pue pixels can be removed before
#analyzing slope outliers

sgs_pue_slope<-merge(sgs_coef_only,sgs_pue,by=c('x','y'))
head(sgs_pue_slope)
sgs_pue_slope_2 <- sgs_pue_slope %>%
  dplyr::filter(pue < 0.756595 | pue > 0.1730594) # get rid of PUE outliers
head(sgs_pue_slope_2)
sgs_pue_slope_3<-sgs_pue_slope_2[-c(4)]

#summary stats of pixel slopes
sd(sgs_pue_slope_3$coef)
mean(sgs_pue_slope_3$coef)
hist(sgs_pue_slope_3$coef)

#values, using the 3 sd theshold again
0.2822828 + (3*0.07016986) #0.4927924
0.2822828 - (3*0.07016986) #0.07177322

sgs_slope_anamolies <- sgs_pue_slope_3 %>%
  dplyr::filter(coef > 0.4927924 | coef < 0.07177322) #isolate odd slope values
summary(sgs_slope_anamolies)

#change to raster
sgs_slope_anamolies_raster<- rasterFromXYZ(sgs_slope_anamolies)
plot(sgs_slope_anamolies_raster)
#turn to geotiff for analaysis in google earth engine
crs(sgs_slope_anamolies_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(sgs_slope_anamolies_raster, "sgs_slope_outliers", format = "GTiff",overwrite=TRUE)
#odd slope values associated with agriculutre 0 remove these and then proceed with analyses

#now produce dataset with final assemblage of pixels
sgs_slope_final <- sgs_pue_slope_3 %>%
  dplyr::filter(coef < 0.4927924 | coef > 0.07177322) #isolate odd slope values
summary(sgs_slope_anamolies)

#turn into raster
sgs_slope_final_raster<- rasterFromXYZ(sgs_slope_final)
plot(sgs_slope_final_raster)

#analyses
#produce a final dataframe with all years for analysis
sgs_npp_ppt_temp_all_years<-merge(merge_sgs_npp_annualprecip_allyears_2,sgs_annualtemp_melted, by=c('x','y','year'))
head(sgs_npp_ppt_temp_all_years)
sgs_npp_ppt_temp_transp_all_years<-merge(sgs_npp_ppt_temp_all_years,sgs_annualtransp_melted, by=c('x','y','year'))
head(sgs_npp_ppt_temp_transp_all_years)
sgs_npp_ppt_temp_transp_all_years$pue<-sgs_npp_ppt_temp_transp_all_years$npp/sgs_npp_ppt_temp_transp_all_years$mm
head(sgs_npp_ppt_temp_transp_all_years)
summary(sgs_npp_ppt_temp_transp_all_years)
sgs_final_raster<- rasterFromXYZ(sgs_npp_ppt_temp_transp_all_years) #change to raster
plot(sgs_final_raster)
sgs_final_raster_masked<-mask(sgs_final_raster,sgs_slope_final_raster) #mask this raster by raster with outliers removed
sgs_final_df<-rasterToPoints(sgs_final_raster_masked) #final dataframe for analyses
summary(sgs_final_df)
plot(sgs_final_raster_masked)
plot(pue~mm,data=sgs_final_df)

#develop a color gradient for reference
sgs_break_npp_ap_slope<-quantile(sgs_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

# plotting -----------------------------------------------------------------

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
ggplot(sgs_final_df,aes(mm,pue,na.rm=TRUE)) +
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
  geom_smooth(method="lm",se=TRUE,color="red",linetype="dashed",size=1.5) +
  #stat_smooth(method = "lm", formula = y ~ poly(x, 2), linetype="dashed",size = 1,se=FALSE,color="black") + #geom_smooth(method="lm",se=FALSE,color="black") +
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
