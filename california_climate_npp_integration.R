#california

#setting extent for precip
ex_cali<-extent(new_all_years_cali)
annual_precip_allyears_cali<-crop(precip_stack,ex_cali)
plot(annual_precip_allyears_cali)
plot(new_all_years_cali)

#changing to dataframes
#NPP
npp_cali_p = rasterToPoints(new_all_years_cali); df_npp_cali = data.frame(npp_cali_p)
head(df_npp_cali)
colnames(df_npp_cali)[3:34] <-paste(1986:2017) #rename coluns to years

cali_npp_melted <- melt(df_npp_cali, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format

#get mean npp per grid cell
cali_npp_melted_mean<-aggregate(value ~ x + y,mean,data=cali_npp_melted)

#annual precipitation
annualprecip_cali_p = rasterToPoints(annual_precip_allyears_cali); df_annualprecip_cali = data.frame(annualprecip_cali_p)
head(df_annualprecip_cali)
colnames(df_annualprecip_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_annualprecip_melted <- melt(df_annualprecip_cali, 
                                         id.vars = c("x", "y"),
                                         variable.name = "year") #melt to long format

cali_annualprecip_melted$mm<-cali_annualprecip_melted$value*10 #change to mm
cali_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=cali_annualprecip_melted) #get mean values

#merge the mean datasets
merge_cali_npp_annualprecip<-merge(cali_npp_melted_mean,cali_annualprecip_melted_mean,by=c("x","y"))
head(merge_cali_npp_annualprecip)

plot(value~mm,merge_cali_npp_annualprecip)
mean_npp_precip_lm<-lm(value~mm,merge_cali_npp_annualprecip)
mean_npp_precip_poly<-lm(value~mm +I(mm^2),data=merge_cali_npp_annualprecip)
AIC(mean_npp_precip_lm,mean_npp_precip_poly) #nonlinear relationships better

#merge datasets
merge_cali_npp_annualprecip_allyears<-merge(cali_annualprecip_melted,cali_npp_melted ,by=c("x","y","year"))
head(merge_cali_npp_annualprecip_allyears)

library(dplyr)
slope_spatial_cali <- merge_cali_npp_annualprecip_allyears %>% group_by(x, y) %>%
  do(model = lm(value.y~mm, data = .)) %>%
  mutate(coef=coef(model)[2]) ##generate slopes

head(slope_spatial_cali)
cali_coef_only<- slope_spatial_cali[ -c(3) ] #isolate coefficient so only slope is graphed
head(cali_coef_only)
cali_raster_coef<- rasterFromXYZ(cali_coef_only) #convert to raster
plot(cali_raster_coef)

#develop a color gradient for reference
cali_break_npp_ap_slope<-quantile(cali_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

#combine slope and avergae map-npp datasets
merge_slope_map_cali<-merge(cali_coef_only,cali_annualprecip_melted_mean,by=c('x','y'))
head(merge_slope_map_cali)
plot(coef~mm,data=merge_slope_map_cali)

#see which model is better
slope_npp_map_lm_cali<-lm(coef~mm,merge_slope_map)
slope_npp_map_poly_cali<<-lm(coef~mm +I(mm^2),data=merge_slope_map)
AIC(slope_npp_map_lm_cali,slope_npp_map_poly_cali) #nonlinear relationships better

#add a pue column
#for mean values
merge_cali_npp_annualprecip$pue<-merge_cali_npp_annualprecip$value/(merge_cali_npp_annualprecip$mm)
head(merge_cali_npp_annualprecip)
plot(pue~mm,data=merge_cali_npp_annualprecip)
#for all values
merge_cali_npp_annualprecip_allyears$pue<-merge_cali_npp_annualprecip_allyears$value.y/(merge_cali_npp_annualprecip_allyears$mm)
head(merge_cali_npp_annualprecip_allyears)
plot(pue~mm,data=merge_cali_npp_annualprecip_allyears)

# sp plot -----------------------------------------------------------------

library(colorspace)
library(sp)
library(colorRamps)
###
library(latticeExtra) #to add an extra layer


spplot(cali_raster_coef,#scales = list(draw = TRUE),
       at=cali_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(cali_break_npp_ap_slope)-1)),
       main="") +
  layer(sp.polygons(states_california, lwd = .75))

# ggplot ------------------------------------------------------------------

library(ggplot2)
ggplot(merge_cali_npp_annualprecip_allyears,aes(mm,pue,na.rm=TRUE)) +
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
  #ylab("Net primary production") +
  #ylab("NPP sensitivity (slope)") +
  ylab("Precipitation use efficiency") +
  #xlab("Mean annual precipitation (mm)") +
  xlab("Annual precipitation (mm)") +
  #ggtitle("Shortgrass steppe") +
  #ylab(bquote('ANPP ('*g/m^2*')')) +
  #stat_summary(fun.y="mean",geom="point",size=6,pch=19) +
  #geom_boxjitter(outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
  #jitter.height = 0.05, jitter.width = 0.075, errorbar.draw = TRUE) +
  #geom_point(size=4,pch=21,color="black") +
  #stat_summary(geom="point",fun.y="identity",size=5,color="black",aes(fill=as.factor(manipulation)),shape=21,show.legend =FALSE) +
  #scale_fill_manual(values=c('Increase'='blue','Decrease'='red'),name="") +
  #geom_smooth(method="lm",se=FALSE,linetype="dashed",color="black",aes(fill=Treatment),show.legend=FALSE) +
  #geom_smooth(method="lm",se=FALSE,color="black",linetype="dashed",size=1.5) +
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

ggsave("cali_npp_pue_ap_6000m.pdf",width = 8, height = 6, units = c("in"))
