#SGS code: linking precip and NPP

new_all_years_sgs_df

p_sgs = rasterToPoints(new_all_years_sgs); df_sgs = data.frame(p_sgs)
colnames(df_sgs)[3:34] <-paste(1986:2017)
head(df_sgs)                      
?col
library(reshape2)
ld2 <- melt(df_sgs, 
            id.vars = c("x", "y"),
            variable.name = "year")

ld2_ag<-aggregate(value~year,mean,data=ld2)
plot(value~year,ld2)

#setting extent for precip
ex<-extent(new_all_years_sgs)
annual_precip_allyears_sgs<-crop(precip_stack,ex)

plot(annual_precip_allyears_sgs)
plot(new_all_years_sgs)

#changing to dataframes
#NPP
npp_sgs_p = rasterToPoints(new_all_years_sgs); df_npp_sgs = data.frame(npp_sgs_p)
head(df_npp_sgs)
colnames(df_npp_sgs)[3:34] <-paste(1986:2017) #rename coluns to years
sgs_npp_melted <- melt(df_npp_sgs, 
            id.vars = c("x", "y"),
            variable.name = "year") #melt to long format

sgs_npp_melted_mean<-aggregate(value ~ x + y,mean,data=sgs_npp_melted)

#annual precipitation
annualprecip_sgs_p = rasterToPoints(annual_precip_allyears_sgs); df_annualprecip_sgs = data.frame(annualprecip_sgs_p)
head(df_annualprecip_sgs)
colnames(df_annualprecip_sgs)[3:32] <-paste(1986:2015) #rename columns to years
sgs_annualprecip_melted <- melt(df_annualprecip_sgs, 
                       id.vars = c("x", "y"),
                       variable.name = "year") #melt to long format

sgs_annualprecip_melted$mm<-sgs_annualprecip_melted$value*10 #change to mm
sgs_annualprecip_melted_mean<-aggregate(mm ~ x + y,mean,data=sgs_annualprecip_melted) #get mean values

#merge the mean datasets
merge_sgs_npp_annualprecip<-merge(sgs_npp_melted_mean,sgs_annualprecip_melted_mean,by=c("x","y"))
head(merge_sgs_npp_annualprecip)

plot(value~mm,merge_sgs_npp_annualprecip)
mean_npp_precip_lm<-lm(value~mm,merge_sgs_npp_annualprecip)
mean_npp_precip_poly<-lm(value~mm +I(mm^2),data=merge_sgs_npp_annualprecip)
AIC(mean_npp_precip_lm,mean_npp_precip_poly) #nonlinear relationships better

#merge datasets
merge_sgs_npp_annualprecip_allyears<-merge(sgs_annualprecip_melted,sgs_npp_melted ,by=c("x","y"))
head(merge_sgs_npp_annualprecip_allyears)

#getting a slope value for each grid cell
slope  <-  function(x){
  if(all(is.na(x)))
    # if x is all missing, then lm will throw an error that we want to avoid
    return(NA)
  else
    return(coef(lm(I~x))[2])
}

slopetest<-aggregate(value~mm,slope,data=)

#
library(ggplot2)
ggplot(merge_sgs_npp_annualprecip,aes(mm,value,na.rm=TRUE)) +
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
