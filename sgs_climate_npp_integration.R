#SGS code: linking precip and NPP

new_all_years_sgs_df

p_sgs = rasterToPoints(new_all_years_sgs); df_sgs = data.frame(p_sgs)
colnames(df_sgs)[3:34] <-paste(1986:2017)
head(df_sgs)                      

library(reshape2)
ld2 <- melt(df_sgs, 
            id.vars = c("x", "y"),
            variable.name = "year")

ld2_ag<-aggregate(value~year,mean,data=ld2)
plot(value~year,ld2)

#dataframes of climate and npp
sgs_2002<-subset(ld2,year=="2002")
head(sgs_2002)
raster_npp_2002<- rasterFromXYZ(sgs_2002)

AnnualPrecip2002_p = rasterToPoints(AnnualPrecip2002); df_AnnualPrecip2002 = data.frame(AnnualPrecip2002_p)
head()
raster_precip_2002<- rasterFromXYZ(df_AnnualPrecip2002)

#setting extent for precip
ex<-extent(sgs_spatiotemporal_mean)
AnnualPrecip2002_sgs_1<-crop(raster_precip_2002,ex)
AnnualPrecip2002_sgs_2<-mask(AnnualPrecip2002_sgs_1,sgs_spatiotemporal_mean)
plot(AnnualPrecip2002_sgs_2)

#setting extent for npp
r.new = resample(AnnualPrecip2002_sgs_2, raster_npp_2002, "bilinear")
extent_sgs_precip<-extent(AnnualPrecip2002_sgs_2)
raster_npp_2002_sgs_1<-crop(raster_npp_2002,extent_sgs_precip)
raster_npp_2002_sgs_2<-mask(raster_npp_2002_sgs_1,sgs_spatiotemporal_mean)
plot(raster_npp_2002_sgs_1)

#changing to dataframes
npp_sgs_p = rasterToPoints(raster_npp_2002_sgs_1); df_npp_sgs_2002 = data.frame(npp_sgs_p)
head()
df_npp_sgs_2002$x<-df_npp_sgs_2002$npp.sgs.p
precip_sgs_p = rasterToPoints(AnnualPrecip2002_sgs_2); df_precip_sgs_2002 = data.frame(precip_sgs_p)

merge_sgs_2002<-merge(df_npp_sgs_2002,df_AnnualPrecip2002,by=c("x","y"))
plot(value~RasterbySiteID3,merge_sgs_2002)
