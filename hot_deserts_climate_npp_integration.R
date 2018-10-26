#hot deserts code

#setting extent for precip
ex_hot_deserts<-extent(new_all_years_hot_deserts)
annual_precip_allyears_hot_deserts<-crop(precip_stack,ex_hot_deserts)

plot(annual_precip_allyears_hot_deserts)
plot(new_all_years_hot_deserts)

#changing to dataframes
#NPP
npp_hot_deserts_p = rasterToPoints(new_all_years_hot_deserts); df_npp_hot_deserts = data.frame(npp_hot_deserts_p)
head(df_npp_hot_deserts)
colnames(df_npp_hot_deserts)[3:34] <-paste(1986:2017) #rename coluns to years

hot_deserts_npp_melted <- melt(df_npp_hot_deserts, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format

#get mean npp per grid cell
hot_deserts_npp_melted_mean<-aggregate(value ~ x + y,mean,data=hot_deserts_npp_melted)

#annual precipitation
annualprecip_hot_deserts_p = rasterToPoints(annual_precip_allyears_hot_deserts); df_annualprecip_hot_deserts = data.frame(annualprecip_hot_deserts_p)
head(df_annualprecip_hot_deserts)
colnames(df_annualprecip_hot_deserts)[3:32] <-paste(1986:2015) #rename columns to years
hot_deserts_annualprecip_melted <- melt(df_annualprecip_hot_deserts, 
                                id.vars = c("x", "y"),
                                variable.name = "year") #melt to long format

hot_deserts_annualprecip_melted$mm<-hot_deserts_annualprecip_melted$value*10 #change to mm
hot_deserts_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=hot_deserts_annualprecip_melted) #get mean values

#merge the mean datasets
merge_hot_deserts_npp_annualprecip<-merge(hot_deserts_npp_melted_mean,hot_deserts_annualprecip_melted_mean,by=c("x","y"))
head(merge_hot_deserts_npp_annualprecip)

plot(value~mm,merge_hot_deserts_npp_annualprecip)
mean_npp_precip_lm<-lm(value~mm,merge_hot_deserts_npp_annualprecip)
mean_npp_precip_poly<-lm(value~mm +I(mm^2),data=merge_hot_deserts_npp_annualprecip)
AIC(mean_npp_precip_lm,mean_npp_precip_poly) #nonlinear relationships better

#merge datasets
merge_hot_deserts_npp_annualprecip_allyears<-merge(hot_deserts_annualprecip_melted,hot_deserts_npp_melted ,by=c("x","y","year"))
head(merge_hot_deserts_npp_annualprecip_allyears)

library(dplyr)
slope_spatial_hot_deserts <- merge_hot_deserts_npp_annualprecip_allyears %>% group_by(x, y) %>%
  do(model = lm(value.y~mm, data = .)) %>%
  mutate(coef=coef(model)[2]) ##generate slopes

head(slope_spatial_hot_deserts)
hot_deserts_coef_only<- slope_spatial_hot_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(hot_deserts_coef_only)
hot_deserts_raster_coef<- rasterFromXYZ(hot_deserts_coef_only) #convert to raster
plot(hot_deserts_raster_coef)

#develop a color gradient for reference
hot_deserts_break_npp_ap_slope<-quantile(hot_deserts_raster_coef$coef,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

#combine slope and avergae map-npp datasets
merge_slope_map<-merge(hot_deserts_coef_only,merge_hot_deserts_npp_annualprecip,by=c('x','y'))
head(merge_slope_map)
plot(coef~mm,data=merge_slope_map)

# sp plot -----------------------------------------------------------------

library(colorspace)
library(sp)
library(colorRamps)
###
library(latticeExtra) #to add an extra layer


spplot(hot_deserts_raster_coef,#scales = list(draw = TRUE),
       at=hot_deserts_break_npp_ap_slope,
       asp=1,
       col.regions =
         rev(heat_hcl(length(hot_deserts_break_npp_ap_slope)-1)),
       main="") +
  layer(sp.polygons(states_steppe, lwd = .75))

# ggplot ------------------------------------------------------------------

library(ggplot2)
ggplot(merge_hot_deserts_npp_annualprecip,aes(mm,value,na.rm=TRUE)) +
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
  stat_smooth(method = "lm", formula = y ~ poly(x, 2),color="red",size = 1,se=TRUE) + 
  ylab("Net primary production") +
  xlab("Annual precipitation (mm)") +
  ggtitle("Shortgrass steppe") +
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
