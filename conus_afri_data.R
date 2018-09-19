
#packages needed
library(dplyr)
library(tidyr)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(scales)
library(sf)

########CONUS AFRI

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)


# California grasslands ---------------------------------------------------


#subset to just california grassland
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 2000000] <- NA
plot (AFRI_RegionSite_Raster)
extent(AFRI_RegionSite_Raster)
#zoom in by hand
zoom_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_1) #derived new extend from zooming in by hand
extent_region_1 <- extent(-124.0, -116.0, 31,41.3) #round up for new extend
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_1 ) 

#zoom again....
zoom_2<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_2)
extent_region_1.2 <- extent(-123.6578 , -115.8161, 32.39994 , 41.22494 ) #second round of extents
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_1.2 ) 


##############CONUS

# conus cali 1986 --------------------------------------------------------------


#1986
year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
raster_1986=raster(year.1986)
plot(raster_1986)
raster_1986[raster_1986 >= 65535] <- NA
raster_1986 = crop(raster_1986, extent_region_1.2) 
extent(raster_1986)
em = merge(extent(AFRI_RegionSite_Raster),extent(raster_1986)) #different resolutions/extents...
plot(em, type="n")
plot(raster_1986, add=TRUE, legend=FALSE)
raster_1986

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, raster_1986, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
raster_1986<-crop(raster_1986,ex)
new<-mask(raster_1986,AFRI_RegionSite_Raster)
plot(new)
plot(raster_1986,name= "CONUS California 1986")
title("CONUS California Annuals 1986")



# conus cali 1996 --------------------------------------------------------------


year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
raster_1996=raster(year.1996)
plot(raster_1996)
raster_1996[raster_1996 >= 65535] <- NA
raster_1996 = crop(raster_1996, extent_region_1.2) 
extent(raster_1996)
em = merge(extent(AFRI_RegionSite_Raster),extent(raster_1996)) #different resolutions/extents...
plot(em, type="n")
plot(raster_1996, add=TRUE, legend=FALSE)
raster_1996

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, raster_1996, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
raster_1996<-crop(raster_1996,ex)
new<-mask(raster_1996,AFRI_RegionSite_Raster)
plot(new)
title("CONUS California Annuals 1996")


# Conus cali 2006 --------------------------------------------------------------
year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
raster_2006=raster(year.2006)
plot(raster_2006)
raster_2006[raster_2006 >= 65535] <- NA
raster_2006 = crop(raster_2006, extent_region_1.2) 
extent(raster_2006)
em = merge(extent(AFRI_RegionSite_Raster),extent(raster_2006)) #different resolutions/extents...
plot(em, type="n")
plot(raster_2006, add=TRUE, legend=FALSE)
raster_2006

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, raster_2006, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
raster_2006<-crop(raster_2006,ex)
new<-mask(raster_2006,AFRI_RegionSite_Raster)
plot(new)
title("CONUS California Annuals 2006")


# Conus cali 2016 --------------------------------------------------------------

year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
raster_2016=raster(year.2016)
plot(raster_2016)
raster_2016[raster_2016 >= 65535] <- NA
raster_2016 = crop(raster_2016, extent_region_1.2) 
extent(raster_2016)
em = merge(extent(AFRI_RegionSite_Raster),extent(raster_2016)) #different resolutions/extents...
plot(em, type="n")
plot(raster_2016, add=TRUE, legend=FALSE)
raster_2016

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, raster_2016, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
raster_2016<-crop(raster_2016,ex)
new<-mask(raster_2016,AFRI_RegionSite_Raster)
plot(new)
title("CONUS California Annuals 2016")



# Semi_arid Steppe --------------------------------------------------------


#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 5000000] <- NA
plot (AFRI_RegionSite_Raster)
extent(AFRI_RegionSite_Raster)

#zoom 
zoom_semi_arid_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_semi_arid_1)
extent_region_1_semi_arid <- extent(-106.7361 , -99.0754 , 30.70696 , 43.76846) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_1_semi_arid)
plot(AFRI_RegionSite_Raster)

#conus

# conus steppe 1986 -------------------------------------------------------

#1986
year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
year_1986_semi_arid=raster(year.1986)
plot(year_1986_semi_arid)
year_1986_semi_arid[year_1986_semi_arid >= 65535] <- NA
year_1986_semi_arid = crop(year_1986_semi_arid, extent_region_1_semi_arid) 


#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1986_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1986_semi_arid<-crop(year_1986_semi_arid,ex)
new<-mask(year_1986_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Semi-arid Steppe 1986")

# Conus steppe 1996 -------------------------------------------------------
year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
year_1996_semi_arid=raster(year.1996)
plot(year_1996_semi_arid)
year_1996_semi_arid[year_1996_semi_arid >= 65535] <- NA
year_1996_semi_arid = crop(year_1996_semi_arid, extent_region_1_semi_arid) 


#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1996_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1996_semi_arid<-crop(year_1996_semi_arid,ex)
new<-mask(year_1996_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Semi-arid Steppe 1996")

# Conus steppe 2006 -------------------------------------------------------
year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
year_2006_semi_arid=raster(year.2006)
plot(year_2006_semi_arid)
year_2006_semi_arid[year_2006_semi_arid >= 65535] <- NA
year_2006_semi_arid = crop(year_2006_semi_arid, extent_region_1_semi_arid) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2006_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2006_semi_arid<-crop(year_2006_semi_arid,ex)
new<-mask(year_2006_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Semi-arid Steppe 2006")

# Conus steppe 2016 -------------------------------------------------------
year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
year_2016_semi_arid=raster(year.2016)
plot(year_2016_semi_arid)
year_2016_semi_arid[year_2016_semi_arid >= 65535] <- NA
year_2016_semi_arid = crop(year_2016_semi_arid, extent_region_1_semi_arid) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2016_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2016_semi_arid<-crop(year_2016_semi_arid,ex)
new<-mask(year_2016_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Semi-arid Steppe 2016")





# mixed grass region --------------------------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

###
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 4000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 5000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
zoom_northern_mixed_prairie_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_northern_mixed_prairie_1)
extent_northern_mixed_prairie_1<- extent(-116, -97.48181 , 38 , 52.0) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_northern_mixed_prairie_1)
plot(AFRI_RegionSite_Raster)

# conus mixed grass 1986 --------------------------------------------------

#1986
year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
year_1986_northern_mixed_prairie=raster(year.1986)
plot(year_1986_northern_mixed_prairie)
year_1986_northern_mixed_prairie[year_1986_northern_mixed_prairie >= 65535] <- NA
year_1986_northern_mixed_prairie = crop(northern_mixed_prairie, extent_northern_mixed_prairie_1) 


#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1986_northern_mixed_prairie, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1986_northern_mixed_prairie<-crop(year_1986_northern_mixed_prairie,ex)
new<-mask(year_1986_northern_mixed_prairie,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Northern Mixed-grass 1986")


# Conus_mixed_1996 --------------------------------------------------------
year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
year_1996_northern_mixed_prairie=raster(year.1996)
plot(year_1996_northern_mixed_prairie)
year_1996_northern_mixed_prairie[year_1996_northern_mixed_prairie >= 65535] <- NA
year_1996_northern_mixed_prairie = crop(northern_mixed_prairie, extent_northern_mixed_prairie_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1996_northern_mixed_prairie, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1996_northern_mixed_prairie<-crop(year_1996_northern_mixed_prairie,ex)
new<-mask(year_1996_northern_mixed_prairie,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Northern Mixed-grass 1996")

# conus mixed grass 2006 --------------------------------------------------
year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
year_2006_northern_mixed_prairie=raster(year.2006)
plot(year_2006_northern_mixed_prairie)
year_2006_northern_mixed_prairie[year_2006_northern_mixed_prairie >= 65535] <- NA
year_2006_northern_mixed_prairie = crop(northern_mixed_prairie, extent_northern_mixed_prairie_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2006_northern_mixed_prairie, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2006_northern_mixed_prairie<-crop(year_2006_northern_mixed_prairie,ex)
new<-mask(year_2006_northern_mixed_prairie,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Northern Mixed-grass 2006")


# conus mixed grass 2016 --------------------------------------------------
year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
year_2016_northern_mixed_prairie=raster(year.2016)
plot(year_2016_northern_mixed_prairie)
year_2016_northern_mixed_prairie[year_2016_northern_mixed_prairie >= 65535] <- NA
year_2016_northern_mixed_prairie = crop(year_2016_northern_mixed_prairie, extent_northern_mixed_prairie_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2016_northern_mixed_prairie, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2016_northern_mixed_prairie<-crop(year_2016_northern_mixed_prairie,ex)
new<-mask(year_2016_northern_mixed_prairie,AFRI_RegionSite_Raster)
plot(new)
title("CONUS Northern Mixed-grass 2016")






# hot desert region --------------------------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#hot deserts
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 3000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 4000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
zoom_desert_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_desert_1)
extent_desert_1<- extent(-119.8845 , -99.26385  , 27.97906 ,  38.0031 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_desert_1)
plot(AFRI_RegionSite_Raster)


# desert 1986 -------------------------------------------------------------
setwd('/Users/andrewfelton/Desktop/USU/Remote_Sensing/')
year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
year_1986_desert=raster(year.1986)
plot(year_1986_desert)
year_1986_desert[year_1986_desert >= 65535] <- NA
year_1986_desert = crop(year_1986_desert, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1986_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1986_desert<-crop(year_1986_desert,ex)
new<-mask(year_1986_desert,AFRI_RegionSite_Raster)
plot(new)
title("Hot Deserts 1986")


# Desert 1996 -------------------------------------------------------------

year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
year_1996_desert=raster(year.1996)
plot(year_1996_desert)
year_1996_desert[year_1996_desert >= 65535] <- NA
year_1996_desert = crop(year_1996_desert, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1996_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1996_desert<-crop(year_1996_desert,ex)
new<-mask(year_1996_desert,AFRI_RegionSite_Raster)
plot(new)
title("Hot Deserts 1996")


# Desert 2006 -------------------------------------------------------------

year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
year_2006_desert=raster(year.2006)
plot(year_2006_desert)
year_2006_desert[year_2006_desert >= 65535] <- NA
year_2006_desert = crop(year_2006_desert, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2006_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2006_desert<-crop(year_2006_desert,ex)
new<-mask(year_2006_desert,AFRI_RegionSite_Raster)
plot(new)
title("Hot Deserts 2006")


# deserts 2016 ------------------------------------------------------------

year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
year_2016_desert=raster(year.2016)
plot(year_2016_desert)
year_2016_desert[year_2016_desert >= 65535] <- NA
year_2016_desert = crop(year_2016_desert, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2016_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2016_desert<-crop(year_2016_desert,ex)
new<-mask(year_2016_desert,AFRI_RegionSite_Raster)
plot(new)
title("Hot Deserts 2016")




# cold desert region ------------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to region
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 2000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 3000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
zoom_col_desert_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_col_desert_1)
extent_cold_desert_1<- extent(-122.317, -103.774, 33.99615 ,  48.65204 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_cold_desert_1)
plot(AFRI_RegionSite_Raster)

#cold desert 1986


# cold deserts 1986 -------------------------------------------------------


year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
year_1986_cold_desert=raster(year.1986)
plot(year_1986_cold_desert)
year_1986_cold_desert[year_1986_cold_desert >= 65535] <- NA
year_1986_cold_desert = crop(year_1986_cold_desert, extent_cold_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1986_cold_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1986_cold_desert<-crop(year_1986_cold_desert,ex)
new<-mask(year_1986_cold_desert,AFRI_RegionSite_Raster)
plot(new)
title("Cold Deserts 1986")


# cold deserts 1996 -------------------------------------------------------

year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
year_1996_cold_desert=raster(year.1996)
plot(year_1996_cold_desert)
year_1996_cold_desert[year_1996_cold_desert >= 65535] <- NA
year_1996_cold_desert = crop(year_1996_cold_desert, extent_cold_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1996_cold_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1996_cold_desert<-crop(year_1996_cold_desert,ex)
new<-mask(year_1996_cold_desert,AFRI_RegionSite_Raster)
plot(new)
title("Cold Deserts 1996")


# cold deserts 2006 -------------------------------------------------------

year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
year_2006_cold_desert=raster(year.2006)
plot(year_2006_cold_desert)
year_2006_cold_desert[year_2006_cold_desert >= 65535] <- NA
year_2006_cold_desert = crop(year_2006_cold_desert, extent_cold_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2006_cold_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2006_cold_desert<-crop(year_2006_cold_desert,ex)
new<-mask(year_2006_cold_desert,AFRI_RegionSite_Raster)
plot(new)
title("Cold Deserts 2006")


# cold deserts 2016 -------------------------------------------------------

year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
year_2016_cold_desert=raster(year.2016)
plot(year_2016_cold_desert)
year_2016_cold_desert[year_2016_cold_desert >= 65535] <- NA
year_2016_cold_desert = crop(year_2016_cold_desert, extent_cold_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2016_cold_desert, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2016_cold_desert<-crop(year_2016_cold_desert,ex)
new<-mask(year_2016_cold_desert,AFRI_RegionSite_Raster)
plot(new)
title("Cold Deserts 2016")




# All regions -------------------------------------------------------------

dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#establish the extent
extent_all_sites<- extent(AFRI_Site_raster) 

# conus 1986 --------------------------------------------------------------

year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1986.tif' 
year_1986_conus=raster(year.1986)
plot(year_1986_conus)
year_1986_conus[year_1986_conus >= 65535] <- NA
year_1986_conus = crop(year_1986_conus, extent_all_sites) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1986_conus, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1986_conus<-crop(year_1986_conus,ex)
new<-mask(year_1986_conus,AFRI_RegionSite_Raster)
plot(new)
title("1986")


# conus 1996 --------------------------------------------------------------

year.1996<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1996.tif' 
year_1996_conus=raster(year.1996)
plot(year_1996_conus)
year_1996_conus[year_1996_conus >= 65535] <- NA
year_1996_conus = crop(year_1996_conus, extent_all_sites) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_1996_conus, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1996_conus<-crop(year_1996_conus,ex)
new<-mask(year_1996_conus,AFRI_RegionSite_Raster)
plot(new)
title("1996")


# conus 2006 --------------------------------------------------------------

year.2006<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2006.tif' 
year_2006_conus=raster(year.2006)
plot(year_2006_conus)
year_2006_conus[year_2006_conus >= 65535] <- NA
year_2006_conus = crop(year_2006_conus, extent_all_sites) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2006_conus, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2006_conus<-crop(year_2006_conus,ex)
new<-mask(year_2006_conus,AFRI_RegionSite_Raster)
plot(new)
title("2006")


# Conus 2016 --------------------------------------------------------------

year.2016<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2016.tif' 
year_2016_conus=raster(year.2016)
plot(year_2016_conus)
year_2016_conus[year_2016_conus >= 65535] <- NA
year_2016_conus = crop(year_2016_conus, extent_all_sites) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, year_2016_conus, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2016_conus<-crop(year_2016_conus,ex)
new<-mask(year_2016_conus,AFRI_RegionSite_Raster)
plot(new)
title("2016")





# 1000 m ------------------------------------------------------------------

#####1000m resolution
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to region
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 2000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 3000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
zoom_col_desert_1<-zoom(AFRI_RegionSite_Raster, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE)
extent(zoom_col_desert_1)
extent_cold_desert_1<- extent(-122.317, -103.774, 33.99615 ,  48.65204 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_cold_desert_1)
plot(AFRI_RegionSite_Raster)

# cold desert 1000 m resolution ------------------------------------------------

setwd('/Users/andrewfelton/Desktop/USU/Remote_Sensing/')
year.1986<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-1000reduced-npp//landsat-1000-npp-1986.tif' 
year_1986_desert=raster(year.1986)
plot(year_1986_desert)
year_1986_desert[year_1986_desert >= 65535] <- NA
year_1986_desert = crop(year_1986_desert, extent_cold_desert_1) 

#try to fix the slightly off extents
####issue with masking a rster witha  different resolution
r.new = resample(year_1986_desert,AFRI_RegionSite_Raster, "bilinear") #brings landsat data ti the afri resolution
ex<-extent(AFRI_RegionSite_Raster) #establishes the extent
year_1986_desert<-crop(r.new,ex)
new<-mask(r.new,AFRI_RegionSite_Raster) #masks dervided coarse landsat resolution by afri sites
plot(new)
title("cold Deserts 1986")

#summary stats
cellStats(new, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(new, stat='sd', na.rm=TRUE, asSample=TRUE)
cellStats(new, stat='min', na.rm=TRUE, asSample=TRUE)
cellStats(new, stat='max', na.rm=TRUE, asSample=TRUE)


# temporal data sgs -----------------------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 5000000] <- NA
plot (AFRI_RegionSite_Raster)
extent(AFRI_RegionSite_Raster)

#zoom 
extent_region_1_semi_arid <- extent(-106.7361 , -99.0754 , 30.70696 , 43.76846) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_1_semi_arid)
plot(AFRI_RegionSite_Raster)

#temporal data
#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack>= 65535] <- NA
filenames_stack_sgs = crop(filenames_stack, extent_region_1_semi_arid) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_sgs, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_sgs<-crop(filenames_stack_sgs,ex)
new_all_years_sgs<-mask(filenames_stack_sgs,AFRI_RegionSite_Raster)
plot(new_all_years_sgs)
title("CONUS Semi-arid Steppe 1986")

#summary stats
time_series_mean_sgs<-cellStats(new_all_years_sgs, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_sgs<-cellStats(new_all_years_sgs, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_sgs<-as.data.frame(time_series_mean_sgs)
#temporal variability
sd(data_frame_time_series_mean_sgs$time_series_mean) #454.7334
#long-term mean
mean(data_frame_time_series_mean_sgs$time_series_mean) #2132.732
min(data_frame_time_series_mean_sgs$time_series_mean) #1225.721
max(data_frame_time_series_mean_sgs$time_series_mean) #2984.161

#relative pulse
(2984.161-2132.732)/2132.732 #0.4
#relative decline
(2132.732-1225.721)/2132.732 #0.43

#sd - spatial variability in npp
data_frame_time_series_sgs_sd<-as.data.frame(time_series_sd_sgs)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_sd$time_series_sd_sgs) #691.0563 #average spatial variability


# temporal data california ------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)

#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to just california grassland
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 2000000] <- NA
plot (AFRI_RegionSite_Raster)
#extent(AFRI_RegionSite_Raster)

#define the extent
extent_region_cali <- extent(-123.6578 , -115.8161, 32.39994 , 41.22494 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_region_cali ) 

#stacked
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_cali = crop(filenames_stack,extent_region_cali) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_cali, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_cali<-crop(filenames_stack_cali,ex)
new_all_years_cali<-mask(filenames_stack_cali,AFRI_RegionSite_Raster)
plot(new_all_years_cali)

######## summary stats
time_series_mean_cali<-cellStats(new_all_years_cali, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_cali<-cellStats(new_all_years_cali, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_cali<-as.data.frame(time_series_mean_cali) #mean regional npp for each year
#long-term mean npp
mean(data_frame_time_series_mean_cali$time_series_mean) #3473.923
#year with minimum npp
min(data_frame_time_series_mean_cali$time_series_mean) #2890.078
#year with maximum npp
max(data_frame_time_series_mean_cali$time_series_mean) #4121.861
#year to year variation
sd(data_frame_time_series_mean_cali$time_series_mean) #348.4559

#relative pulse
(4121.861-3473.923)/3473.923 #0.19
#relative decline
(3473.923-2890.078)/3473.923 #0.17

#sd - spatial variability in npp
data_frame_time_series_sgs_cali<-as.data.frame(time_series_sd_cali)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_cali$time_series_sd_cali) #1163.254 #average spatial variability


# temporal data hot deserts -------------------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)

#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#hot deserts
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 3000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 4000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_desert_1<- extent(-119.8845 , -99.26385  , 27.97906 ,  38.0031 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_desert_1)
plot(AFRI_RegionSite_Raster)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack>= 65535] <- NA
filenames_stack_hot_deserts = crop(filenames_stack, extent_desert_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_hot_deserts, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_hot_deserts<-crop(filenames_stack_hot_deserts,ex)
new_all_years_hot_deserts<-mask(filenames_stack_hot_deserts,AFRI_RegionSite_Raster)
plot(new_all_years_hot_deserts)

#summary stats
time_series_mean_hot_deserts<-cellStats(new_all_years_hot_deserts, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_hot_deserts<-cellStats(new_all_years_hot_deserts, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_hot_deserts<-as.data.frame(time_series_mean_hot_deserts)
# year to year variation
sd(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #146.0075
#long-term mean
mean(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #505.499
#minimum
min(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #243.3728
#maximum
max(data_frame_time_series_mean_hot_deserts$time_series_mean_hot_deserts) #779.5133

#relative pulse
(779.5133-505.499)/505.499 #0.54
#relative decline
(505.499-243.3728)/505.499 #0.52

#sd - spatial variability in npp
data_frame_time_series_sgs_sd<-as.data.frame(time_series_sd_hot_deserts)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
mean(data_frame_time_series_sgs_sd$time_series_sd_hot_deserts) #495.5031


# temporal data cold deserts ----------------------------------------------

#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

#subset to region
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 2000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 3000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_cold_desert<- extent(-122.317, -103.774, 33.99615 ,  48.65204 ) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_cold_desert)
plot(AFRI_RegionSite_Raster)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)

filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_cold_deserts = crop(filenames_stack, extent_cold_desert) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_cold_deserts , "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_cold_deserts <-crop(filenames_stack_cold_deserts ,ex)
new_all_years_cold_deserts<-mask(filenames_stack_cold_deserts ,AFRI_RegionSite_Raster)
plot(new_all_years_cold_deserts)

#summary stats
time_series_mean_cold_deserts<-cellStats(new_all_years_cold_deserts, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_cold_deserts<-cellStats(new_all_years_cold_deserts, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_cold_deserts<-as.data.frame(time_series_mean_cold_deserts)
# year to year variation
sd(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #154.8888
#long-term mean
mean(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #993.7573
#minimum
min(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #727.2936
#maximum
max(data_frame_time_series_mean_cold_deserts$time_series_mean_cold_deserts) #1287.623

#relative pulse
(1287.623-993.7573)/993.7573 #0.30
#relative decline
(993.7573-727.2936)/993.7573 #0.27

#sd - spatial variability in npp
data_frame_time_series_cold_deserts_sd<-as.data.frame(time_series_sd_cold_deserts)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
#man spatial variability
mean(data_frame_time_series_cold_deserts_sd$time_series_sd_cold_deserts) #682.0946

# temporal data northern mixed grass --------------------------------------
#Specify directory where you have the raster
dir.AFRI <- "/Users/andrewfelton/Desktop/USU/Remote_Sensing"
#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "RasterbySiteID3.tif"))

#look at raster strcture
AFRI_RegionSite_Raster
#plot it
plot(AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster   
#Create a raster for the different regions
AFRI_Region_raster <- round(AFRI_RegionSite_Raster/1000000)
plot(AFRI_Region_raster)
?round
#Create a raster for the different regions
AFRI_Site_raster <- AFRI_RegionSite_Raster - AFRI_Region_raster*1000000
plot(AFRI_Site_raster)

###
#subset to just semi-arid grasslands...
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster < 4000000] <- NA
plot (AFRI_RegionSite_Raster)
AFRI_RegionSite_Raster[AFRI_RegionSite_Raster >= 5000000] <- NA
extent(AFRI_RegionSite_Raster)

#zoom 
extent_northern_mixed_prairie_1<- extent(-116, -97.48181 , 38 , 52.0) 
AFRI_RegionSite_Raster= crop(AFRI_RegionSite_Raster, extent_northern_mixed_prairie_1)

#set your working directory
setwd('/Users/andrewfelton/Desktop/USU/')
filenames <- dir("/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp/", full.names = T)
#filenames
filenames_stack <-stack(filenames)
filenames_stack[filenames_stack >= 65535] <- NA
filenames_stack_northern_mixed = crop(filenames_stack, extent_northern_mixed_prairie_1) 

#try to fix the slightly off extents
r.new = resample(AFRI_RegionSite_Raster, filenames_stack_northern_mixed , "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
filenames_stack_northern_mixed <-crop(filenames_stack_northern_mixed ,ex)
new_all_years_northern_mixed<-mask(filenames_stack_northern_mixed,AFRI_RegionSite_Raster)
plot(new_all_years_northern_mixed)

#summary stats
time_series_mean_northern_mixed<-cellStats(new_all_years_northern_mixed, stat='mean', na.rm=TRUE, asSample=TRUE)
time_series_sd_northern_mixed<-cellStats(new_all_years_northern_mixed, stat='sd', na.rm=TRUE, asSample=TRUE)

#turn summary stats into data frame
#mean
data_frame_time_series_mean_northern_mixed<-as.data.frame(time_series_mean_northern_mixed)
# year to year variation
sd(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #271.4897
#long-term mean
mean(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #2401.067
#minimum
min(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #1802.697
#maximum
max(data_frame_time_series_mean_northern_mixed$time_series_mean_northern_mixed) #2813.676

#relative pulse
(2813.676-2401.067)/2401.0673 #0.17
#relative decline
(2401.067-1802.697)/2401.067 #0.25

#sd - spatial variability in npp
data_frame_time_series_northern_mixed_sd<-as.data.frame(time_series_sd_northern_mixed)
#hist(data_frame_time_series_sgs_sd$time_series_sd_sgs)
#man spatial variability
mean(data_frame_time_series_northern_mixed_sd$time_series_sd_northern_mixed) #889.0735

# turning into derived data frame -----------------------------------------

site<-c("Northern Mixed grass","Cold Deserts","Hot deserts","California annuals","Semi-arid steppe")
mean_npp <-c("2401.067","993.7573","505.499","2890.078","2132.732")
temporal_sd <-c("271.4897","154.8888","146.0075","348.4559","454.7334")
npp_pulse<-c("0.17","0.30","0.54","0.19","0.4")
npp_minimum<-c("0.25","0.27","0.52","0.17","0.43")
spatial_var<-c("889.0735","682.0946","495.5031","1163.254","691.0563")

spatial_sumamries<-data.frame(site,mean_npp,temporal_sd,npp_pulse,npp_minimum,spatial_var)
str(spatial_sumamries)
spatial_sumamries$mean_npp <- as.numeric(as.character(spatial_sumamries$mean_npp))
spatial_sumamries$temporal_sd  <- as.numeric(as.character(spatial_sumamries$temporal_sd))
spatial_sumamries$npp_pulse  <- as.numeric(as.character(spatial_sumamries$npp_pulse))
spatial_sumamries$npp_minimum  <- as.numeric(as.character(spatial_sumamries$npp_minimum))
spatial_sumamries$spatial_var <- as.numeric(as.character(spatial_sumamries$spatial_var))
# high versus low npp -----------------------------------------------------

#semiarid low npp
year.2011<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-2011.tif' 
year_2011_semi_arid=raster(year.2011)
year_2011_semi_arid[year_2011_semi_arid>= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, year_2011_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_2011_semi_arid<-crop(year_2011_semi_arid,ex)
new_2011<-mask(year_2011_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("Lowest regional NPP: Semi-arid Steppe (2011)")

#semi-arid highest npp

year.1992<-'/Users/andrewfelton/Desktop/USU/Remote_Sensing/landsat-6000reduced-npp//landsat-6000-npp-1992.tif' 
year_1992_semi_arid=raster(year.1992)
year_1992_semi_arid[year_1992_semi_arid >= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, year_1992_semi_arid, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
year_1992_semi_arid<-crop(year_1992_semi_arid,ex)
new_1992<-mask(year_1992_semi_arid,AFRI_RegionSite_Raster)
plot(new)
title("Highest regional NPP: Semi-arid Steppe (1992)")

stacked<-stack(year.2011,year.1992)
raster(stacked)
plot(stacked)
stacked[stacked>= 65535] <- NA
r.new = resample(AFRI_RegionSite_Raster, stacked, "bilinear")
ex<-extent(AFRI_RegionSite_Raster)
stacked<-crop(stacked,ex)
new<-mask(stacked,AFRI_RegionSite_Raster)
plot(new)
title("Lowest regional NPP: Semi-arid Steppe (2011)")


# ggplot ------------------------------------------------------------------



ggplot() +
  ggspatial::geom_spatial(data = new, mapping = aes(colour = STREAMTYPE)) +
  coord_map()

plot(new, breaks=seq(min(minValue( new )),max(maxValue(new))))
plot( new, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(0,1) )
length.out
library(rasterVis)
rng = (range(ge)0

gplot(new_1992) + 
  geom_tile(aes(fill=value)) +
  ggtitle("Semi-arid Steppe High NPP:1992") +
  scale_fill_gradient2(low="red", mid="orange", high="darkgreen", 
                       midpoint=2000,    # Value that gets the middle color (default is zero)
                       limits=c(floor(0), ceiling(8100))) 

gplot(new_2011) + 
  geom_tile(aes(fill=value)) +
  ggtitle("Semi-arid Steppe low NPP:2011") +
  scale_fill_gradient2(low="red", mid="orange", high="darkgreen", 
                       midpoint=2000,    # Value that gets the middle color (default is zero)
                       limits=c(floor(0), ceiling(8100))) 
