
#pckages needed
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
