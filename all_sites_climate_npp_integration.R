#hot deserts code

#setting extent for precip
ex_all_sites<-extent(AFRI_RegionSite_Raster_john)
annual_precip_allyears_all_sites<-crop(precip_stack,ex_all_sites)
npp_allyears_all_sites<-crop(filenames_stack,ex_all_sites)

plot(annual_precip_allyears_all_sites)
plot(new_all_years_all_sites)

#changing to dataframes
#NPP
npp_all_sites_p = rasterToPoints(npp_allyears_all_sites); df_npp_all_sites = data.frame(npp_all_sites_p)
head(df_npp_all_sites)
colnames(df_npp_all_sites)[3:34] <-paste(1986:2017) #rename coluns to years

library(reshape2)
all_sites_npp_melted <- melt(df_npp_all_sites, 
                               id.vars = c("x", "y"),
                               variable.name = "year") #melt to long format

#get mean npp per grid cell
all_sites_npp_melted_mean<-aggregate(value ~ x + y,mean,data=all_sites_npp_melted)

#annual precipitation
annualprecip_all_sites_p = rasterToPoints(annual_precip_allyears_all_sites); df_annualprecip_all_sites = data.frame(annualprecip_all_sites_p)
head(df_annualprecip_all_sites)
colnames(df_annualprecip_all_sites)[3:32] <-paste(1986:2015) #rename columns to years
all_sites_annualprecip_melted <- melt(df_annualprecip_all_sites, 
                                        id.vars = c("x", "y"),
                                        variable.name = "year") #melt to long format

all_sites_annualprecip_melted$mm<-all_sites_annualprecip_melted$value*10 #change to mm
all_sites_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=all_sites_annualprecip_melted) #get mean values

#merge the mean datasets
merge_all_sites_npp_annualprecip<-merge(all_sites_npp_melted_mean,all_sites_annualprecip_melted_mean,by=c("x","y"))
head(merge_all_sites_npp_annualprecip)

plot(value~mm,merge_all_sites_npp_annualprecip)
mean_npp_precip_lm<-lm(value~mm,merge_all_sites_npp_annualprecip)
mean_npp_precip_poly<-lm(value~mm +I(mm^2),data=merge_all_sites_npp_annualprecip)
AIC(mean_npp_precip_lm,mean_npp_precip_poly) #nonlinear relationships better

#merge datasets
merge_all_sites_npp_annualprecip_allyears<-merge(all_sites_annualprecip_melted,all_sites_npp_melted ,by=c("x","y","year"))
head(merge_all_sites_npp_annualprecip_allyears)

library(dplyr)
slope_spatial_all_sites <- merge_all_sites_npp_annualprecip_allyears %>% group_by(x, y) %>%
  do(model = lm(value.y~mm, data = .)) %>%
  mutate(coef=coef(model)[2]) ##generate slopes

head(slope_spatial_all_sites)
all_sites_coef_only<- slope_spatial_all_sites[ -c(3) ] #isolate coefficient so only slope is graphed
head(all_sites_coef_only)
all_sites_raster_coef<- rasterFromXYZ(all_sites_coef_only) #convert to raster
plot(all_sites_raster_coef)

#develop a color gradient for reference
all_sites_break_npp_ap_slope<-quantile(all_sites_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

#combine slope and avergae map-npp datasets
merge_slope_map_all_sites<-merge(all_sites_coef_only,all_sites_annualprecip_melted_mean,by=c('x','y'))
head(merge_slope_map_all_sites)
plot(coef~mm,data=merge_slope_map_all_sites)

#see which model is better
slope_npp_map_lm_all_sites<-lm(coef~mm,merge_slope_map)
slope_npp_map_poly_all_sites<<-lm(coef~mm +I(mm^2),data=merge_slope_map)
AIC(slope_npp_map_lm_all_sites,slope_npp_map_poly_all_sites) #nonlinear relationships better

#add a pue column
#for mean values
merge_all_sites_npp_annualprecip$pue<-merge_all_sites_npp_annualprecip$value/(merge_all_sites_npp_annualprecip$mm)
head(merge_all_sites_npp_annualprecip)
plot(pue~mm,data=merge_all_sites_npp_annualprecip)
#for all values
merge_all_sites_npp_annualprecip_allyears$pue<-merge_all_sites_npp_annualprecip_allyears$value.y/(merge_all_sites_npp_annualprecip_allyears$mm)
head(merge_all_sites_npp_annualprecip_allyears)
plot(pue~mm,data=merge_all_sites_npp_annualprecip_allyears)

# sp plot -----------------------------------------------------------------

library(colorspace)
library(sp)
library(colorRamps)
###
library(latticeExtra) #to add an extra layer


spplot(all_sites_raster_coef,#scales = list(draw = TRUE),
       at=all_sites_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(all_sites_break_npp_ap_slope)-1)),
       main="") +
  layer(sp.polygons(states_all_sites, lwd = .75))

# ggplot ------------------------------------------------------------------

library(ggplot2)
ggplot(merge_cv_mean,aes(value,value.y,na.rm=TRUE)) +
  #scale_color_manual(values=c('increase'='blue','decrease'='red'),name="") +
  #geom_bar() +
  geom_point(pch=1,size=.5) +
  #stat_summary(fun.y="mean",geom="point") +
  #scale_y_continuous(expand = c(0,0)) +
  #stat_summary(fun.y="count",geom="bar")
  #geom_hline(yintercept=0) +
  #geom_vline(xintercept=0) +
  #geom_point() +
  #geom_smooth(method="lm",se=TRUE,linetype="dashed") +
  #geom_hline(yintercept = 713.97,color="black",size=.5) +
  #stat_smooth(method = "lm", formula = y ~ poly(x, 2),color="red",size = 1,se=TRUE) + 
  ylab("NPP variability (CV)") +
  xlab("Mean NPP") +
  #ylab("NPP sensitivity (slope)") +
#ylab("Precipitation use efficiency") +
  #xlab("Mean annual precipitation (mm)") +
  #xlab("CV of Annual precipitation (mm)") +
  #ggtitle("Shortgrass steppe") +
  #ylab(bquote('ANPP ('*g/m^2*')')) +
  #stat_summary(fun.y="mean",geom="point",size=6,pch=19) +
  #geom_boxjitter(outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
  #jitter.height = 0.05, jitter.width = 0.075, errorbar.draw = TRUE) +
  #geom_point(size=4,pch=21,color="black") +
  #stat_summary(geom="point",fun.y="identity",size=5,color="black",aes(fill=as.factor(manipulation)),shape=21,show.legend =FALSE) +
  #scale_fill_manual(values=c('Increase'='blue','Decrease'='red'),name="") +
  #geom_smooth(method="lm",se=FALSE,linetype="dashed",color="black",aes(fill=Treatment),show.legend=FALSE) +
  #geom_smooth(method="lm",se=FALSE,color="red",linetype="dashed",size=1.5) +
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

ggsave("all_sites_cv_mean_npp_6000m.pdf",width = 8, height = 6, units = c("in"))


# asymmetry ---------------------------------------------------------------


#npp pulse equation
npp_pulse <- function(x) {
  
  pulse <- ((max(x) - mean(x))/mean(x))*100
  
  return(pulse)
}

pulse_all_sites<-aggregate(value.y~ x + y,npp_pulse,data=merge_all_sites_npp_annualprecip_allyears)
head(pulse_all_sites)
all_sites_raster_pulse<- rasterFromXYZ(pulse_all_sites) #convert to raster
plot(all_sites_raster_pulse)

#develop a color gradient for reference
all_sites_break_npp_ap_pulse<-quantile(all_sites_raster_pulse$value.y,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

#plot
spplot(all_sites_raster_pulse,#scales = list(draw = TRUE),
       at=all_sites_break_npp_ap_pulse,
       asp=1,
       col.regions =
         rev(terrain_hcl(length(all_sites_break_npp_ap_pulse)-1)),
       main="NPP Pulse (% increase from mean NPP)") +
  layer(sp.polygons(states_all_sites, lwd = .5))
  
#npp decline equation
npp_decline <- function(x) {
  
  decline <- ((mean(x) - min(x))/mean(x))*100
  
  return(decline)
}

decline_all_sites<-aggregate(value.y~ x + y,npp_decline,data=merge_all_sites_npp_annualprecip_allyears)
head(decline_all_sites)
all_sites_raster_decline<- rasterFromXYZ(decline_all_sites) #convert to raster
plot(all_sites_raster_decline)

#develop a color gradient for reference
all_sites_break_npp_ap_decline<-quantile(all_sites_raster_decline$value.y,seq(from=0.01, to = .95,by=0.01),na.rm=TRUE)

#plot
spplot(all_sites_raster_decline,#scales = list(draw = TRUE),
       at=all_sites_break_npp_ap_decline,
       asp=1,
       col.regions =
         rev(heat_hcl(length(all_sites_break_npp_ap_decline)-1)),
       main="NPP decline (% decrease from mean NPP)") +
  layer(sp.polygons(states_all_sites, lwd = .5))

#max versus min
npp_max_versus_min <- function(x) {
  
  decline <- ((mean(x) - min(x))/mean(x))*100
  pulse <- ((max(x) - min(x))/mean(x))*100
  comp<- pulse/decline
  
  return(comp)
}

#aggregate
max_versus_min_all_sites<-aggregate(value.y~ x + y,npp_max_versus_min,data=merge_all_sites_npp_annualprecip_allyears)
head(max_versus_min_all_sites)
all_sites_raster_max_versus_min<- rasterFromXYZ(max_versus_min_all_sites) #convert to raster
plot(all_sites_raster_max_versus_min)

#develop a color gradient for reference
all_sites_break_npp_ap_max_versus_min<-quantile(all_sites_raster_max_versus_min$value.y,seq(from=0.01, to = .95,by=0.01),na.rm=TRUE)

#plot
spplot(all_sites_raster_max_versus_min,#scales = list(draw = TRUE),
       at=all_sites_break_npp_ap_max_versus_min,
       asp=1,
       col.regions =
         rev(terrain_hcl(length(all_sites_break_npp_ap_max_versus_min)-1)),
       main="Ratio of NPP pulse to decline") +
  layer(sp.polygons(states_all_sites, lwd = .5))


# Cv precip versus cv npp -------------------------------------------------

cv<-function(x) {
  
  sd <- sd(!is.na(x))
  mean <- mean(!is.na(x))
  cv<-(sd(x)/mean(x))*100
  
  return(cv)
}

cv_npp_all_sites<-aggregate(value.y~ x + y,cv,data=merge_all_sites_npp_annualprecip_allyears)
head(cv_npp_all_sites)
summary(cv_npp_all_sites)

cv_precip_all_sites<-aggregate(mm~ x + y,cv,data=merge_all_sites_npp_annualprecip_allyears)
head(cv_precip_all_sites)

merge_cv_npp_precip_all_sites<-merge(cv_npp_all_sites,cv_precip_all_sites,by=c('x','y'))
head(merge_cv_npp_precip_all_sites)

plot(value.y~mm,data=merge_cv_npp_precip_all_sites)
lm_cv_npp<-lm(value.y~mm,data=merge_cv_npp_precip_all_sites)
summary(lm_cv_npp)

#merge cv and mean
merge_cv_mean<-merge(all_sites_npp_melted_mean,cv_npp_all_sites,by=c('x','y'))
head(merge_cv_mean)
head(all_sites_npp_melted_mean)
summary(merge_cv_mean)
plot(value.y~value,data=merge_cv_mean)
