#code from to john to work with his climate datasets

library(raster)


regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")



#Specify directory where you have the raster
sites <- "/Users/A02296270/Desktop/CONUS_AFRI/CONUS/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(raster_sites))
names(rastvals) <- "RegionSite"

#View just the values associated with each cell
#Note that the value in the raster includes both the region (in the millions digit; 1 to 5), and the siteID (in the other digits, range 1 to ~20,000 within each region)
values(raster_sites)

#Plot the raster
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- round(raster_sites/1000000)
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- raster_sites - raster_sites*1000000
plot(raster_sites)


# annual precipitation ----------------------------------------------------

#Load a dataset and plot - showing precipitation as an example
dir.AFRI_Historical <- "/Users/A02296270/Desktop/CONUS_AFRI/"
load(file.path(dir.AFRI_Historical, "annualprecip19152015")) #loads file and name it annualprecip I guess
str(annualprecip)
rownames(annualprecip)
head(annualprecip)
#add label to dataset based on row names
annualprecip$label <- row.names(annualprecip)

sitenumENDpos = as.integer(regexpr('_', annualprecip$label) )
Site <- as.integer(substr(annualprecip$label, 1, sitenumENDpos-1) )
Regionname <- substr(annualprecip$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
annualprecip$RegionSite <- Regionnum*1000000 + Site

library(plyr)
annualprecip_joindat <- join(rastvals, annualprecip, by="RegionSite")
# dim(annualprecip_joindat)
# str(annualprecip_joindat)

#get rid of years that don't relate to NPP data
annualprecip_joindat_2<-annualprecip_joindat[,-c(2:71)]

#use joined data to populate values for a raster
precip_done <- list()
#1986
AnnualPrecip_1986<- raster_sites
values(AnnualPrecip_1986) <- annualprecip_joindat_2[,"1986"]
precip_done[["AnnualPrecip_1986"]] <-AnnualPrecip_1986
#1987
AnnualPrecip_1987<- raster_sites
values(AnnualPrecip_1987) <- annualprecip_joindat_2[,"1987"]
precip_done[["AnnualPrecip_1987"]] <-AnnualPrecip_1987
#1988
AnnualPrecip_1988<- raster_sites
values(AnnualPrecip_1988) <- annualprecip_joindat_2[,"1988"]
precip_done[["AnnualPrecip_1988"]] <-AnnualPrecip_1988
#1989
AnnualPrecip_1989<- raster_sites
values(AnnualPrecip_1989) <- annualprecip_joindat_2[,"1989"]
precip_done[["AnnualPrecip_1989"]] <-AnnualPrecip_1989
#1990
AnnualPrecip_1990<- raster_sites
values(AnnualPrecip_1990) <- annualprecip_joindat_2[,"1990"]
precip_done[["AnnualPrecip_1990"]] <-AnnualPrecip_1990
#1991
AnnualPrecip_1991<- raster_sites
values(AnnualPrecip_1991) <- annualprecip_joindat_2[,"1991"]
precip_done[["AnnualPrecip_1991"]] <-AnnualPrecip_1991
#1992
AnnualPrecip_1992<- raster_sites
values(AnnualPrecip_1992) <- annualprecip_joindat_2[,"1992"]
precip_done[["AnnualPrecip_1992"]] <-AnnualPrecip_1992
#1993
AnnualPrecip_1993<- raster_sites
values(AnnualPrecip_1993) <- annualprecip_joindat_2[,"1993"]
precip_done[["AnnualPrecip_1993"]] <-AnnualPrecip_1993
#1994
AnnualPrecip_1994<- raster_sites
values(AnnualPrecip_1994) <- annualprecip_joindat_2[,"1994"]
precip_done[["AnnualPrecip_1994"]] <-AnnualPrecip_1994
#1995
AnnualPrecip_1995<- raster_sites
values(AnnualPrecip_1995) <- annualprecip_joindat_2[,"1995"]
precip_done[["AnnualPrecip_1995"]] <-AnnualPrecip_1995
#1996
AnnualPrecip_1996<- raster_sites
values(AnnualPrecip_1996) <- annualprecip_joindat_2[,"1996"]
precip_done[["AnnualPrecip_1996"]] <-AnnualPrecip_1996
#1997
AnnualPrecip_1997<- raster_sites
values(AnnualPrecip_1997) <- annualprecip_joindat_2[,"1997"]
precip_done[["AnnualPrecip_1997"]] <-AnnualPrecip_1997
#1998
AnnualPrecip_1998<- raster_sites
values(AnnualPrecip_1998) <- annualprecip_joindat_2[,"1998"]
precip_done[["AnnualPrecip_1998"]] <-AnnualPrecip_1998
#1999
AnnualPrecip_1999<- raster_sites
values(AnnualPrecip_1999) <- annualprecip_joindat_2[,"1999"]
precip_done[["AnnualPrecip_1999"]] <-AnnualPrecip_1999
#2000
AnnualPrecip_2000<- raster_sites
values(AnnualPrecip_2000) <- annualprecip_joindat_2[,"2000"]
precip_done[["AnnualPrecip_2000"]] <-AnnualPrecip_2000
#2001
AnnualPrecip_2001<- raster_sites
values(AnnualPrecip_2001) <- annualprecip_joindat_2[,"2001"]
precip_done[["AnnualPrecip_2001"]] <-AnnualPrecip_2001
#2002
AnnualPrecip_2002<- raster_sites
values(AnnualPrecip_2002) <- annualprecip_joindat_2[,"2002"]
precip_done[["AnnualPrecip_2002"]] <-AnnualPrecip_2002
#2003
AnnualPrecip_2003<- raster_sites
values(AnnualPrecip_2003) <- annualprecip_joindat_2[,"2003"]
precip_done[["AnnualPrecip_2003"]] <-AnnualPrecip_2003
#2004
AnnualPrecip_2004<- raster_sites
values(AnnualPrecip_2004) <- annualprecip_joindat_2[,"2004"]
precip_done[["AnnualPrecip_2004"]] <-AnnualPrecip_2004
#2005
AnnualPrecip_2005<- raster_sites
values(AnnualPrecip_2005) <- annualprecip_joindat_2[,"2005"]
precip_done[["AnnualPrecip_2005"]] <-AnnualPrecip_2005
#2006
AnnualPrecip_2006<- raster_sites
values(AnnualPrecip_2006) <- annualprecip_joindat_2[,"2006"]
precip_done[["AnnualPrecip_2006"]] <-AnnualPrecip_2006
#2007
AnnualPrecip_2007<- raster_sites
values(AnnualPrecip_2007) <- annualprecip_joindat_2[,"2007"]
precip_done[["AnnualPrecip_2007"]] <-AnnualPrecip_2007
#2008
AnnualPrecip_2008<- raster_sites
values(AnnualPrecip_2008) <- annualprecip_joindat_2[,"2008"]
precip_done[["AnnualPrecip_2008"]] <-AnnualPrecip_2008
#2009
AnnualPrecip_2009<- raster_sites
values(AnnualPrecip_2009) <- annualprecip_joindat_2[,"2009"]
precip_done[["AnnualPrecip_2009"]] <-AnnualPrecip_2009
#2010
AnnualPrecip_2010<- raster_sites
values(AnnualPrecip_2010) <- annualprecip_joindat_2[,"2010"]
precip_done[["AnnualPrecip_2010"]] <-AnnualPrecip_2010
#2011
AnnualPrecip_2011<- raster_sites
values(AnnualPrecip_2011) <- annualprecip_joindat_2[,"2011"]
precip_done[["AnnualPrecip_2011"]] <-AnnualPrecip_2011
#2012
AnnualPrecip_2012<- raster_sites
values(AnnualPrecip_2012) <- annualprecip_joindat_2[,"2012"]
precip_done[["AnnualPrecip_2012"]] <-AnnualPrecip_2012
#2013
AnnualPrecip_2013<- raster_sites
values(AnnualPrecip_2013) <- annualprecip_joindat_2[,"2013"]
precip_done[["AnnualPrecip_2013"]] <-AnnualPrecip_2013
#2014
AnnualPrecip_2014<- raster_sites
values(AnnualPrecip_2014) <- annualprecip_joindat_2[,"2014"]
precip_done[["AnnualPrecip_2014"]] <-AnnualPrecip_2014
#2015
AnnualPrecip_2015<- raster_sites
values(AnnualPrecip_2015) <- annualprecip_joindat_2[,"2015"]
precip_done[["AnnualPrecip_2015"]] <-AnnualPrecip_2015

#all years stacked, ready for cropping for each site
precip_stack <-stack(precip_done)
plot(precip_stack)


# annual temperature ------------------------------------------------------

dir.AFRI_Historical <- "/Users/A02296270/Desktop/CONUS_AFRI/"
load(file.path(dir.AFRI_Historical, "annualtemp19152015")) #loads file and name it annualtemp I guess
str(annualtemp)
rownames(annualtemp)
head(annualtemp)
#add label to dataset based on row names
annualtemp$label <- row.names(annualtemp)

sitenumENDpos = as.integer(regexpr('_', annualtemp$label) )
Site <- as.integer(substr(annualtemp$label, 1, sitenumENDpos-1) )
Regionname <- substr(annualtemp$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
annualtemp$RegionSite <- Regionnum*1000000 + Site

annualtemp_joindat <- join(rastvals, annualtemp, by="RegionSite")
# dim(annualtemp_joindat)
# str(annualtemp_joindat)
#get rid of years that don't relate to NPP estimate
annualtemp_joindat_2<-annualtemp_joindat[,-c(2:71)]

#use joined data to populate values for a raster
temp_done <- list()
#1986
Annualtemp_1986<- raster_sites
values(Annualtemp_1986) <- annualtemp_joindat_2[,"1986"]
temp_done[["Annualtemp_1986"]] <-Annualtemp_1986
#1987
Annualtemp_1987<- raster_sites
values(Annualtemp_1987) <- annualtemp_joindat_2[,"1987"]
temp_done[["Annualtemp_1987"]] <-Annualtemp_1987
#1988
Annualtemp_1988<- raster_sites
values(Annualtemp_1988) <- annualtemp_joindat_2[,"1988"]
temp_done[["Annualtemp_1988"]] <-Annualtemp_1988
#1989
Annualtemp_1989<- raster_sites
values(Annualtemp_1989) <- annualtemp_joindat_2[,"1989"]
temp_done[["Annualtemp_1989"]] <-Annualtemp_1989
#1990
Annualtemp_1990<- raster_sites
values(Annualtemp_1990) <- annualtemp_joindat_2[,"1990"]
temp_done[["Annualtemp_1990"]] <-Annualtemp_1990
#1991
Annualtemp_1991<- raster_sites
values(Annualtemp_1991) <- annualtemp_joindat_2[,"1991"]
temp_done[["Annualtemp_1991"]] <-Annualtemp_1991
#1992
Annualtemp_1992<- raster_sites
values(Annualtemp_1992) <- annualtemp_joindat_2[,"1992"]
temp_done[["Annualtemp_1992"]] <-Annualtemp_1992
#1993
Annualtemp_1993<- raster_sites
values(Annualtemp_1993) <- annualtemp_joindat_2[,"1993"]
temp_done[["Annualtemp_1993"]] <-Annualtemp_1993
#1994
Annualtemp_1994<- raster_sites
values(Annualtemp_1994) <- annualtemp_joindat_2[,"1994"]
temp_done[["Annualtemp_1994"]] <-Annualtemp_1994
#1995
Annualtemp_1995<- raster_sites
values(Annualtemp_1995) <- annualtemp_joindat_2[,"1995"]
temp_done[["Annualtemp_1995"]] <-Annualtemp_1995
#1996
Annualtemp_1996<- raster_sites
values(Annualtemp_1996) <- annualtemp_joindat_2[,"1996"]
temp_done[["Annualtemp_1996"]] <-Annualtemp_1996
#1997
Annualtemp_1997<- raster_sites
values(Annualtemp_1997) <- annualtemp_joindat_2[,"1997"]
temp_done[["Annualtemp_1997"]] <-Annualtemp_1997
#1998
Annualtemp_1998<- raster_sites
values(Annualtemp_1998) <- annualtemp_joindat_2[,"1998"]
temp_done[["Annualtemp_1998"]] <-Annualtemp_1998
#1999
Annualtemp_1999<- raster_sites
values(Annualtemp_1999) <- annualtemp_joindat_2[,"1999"]
temp_done[["Annualtemp_1999"]] <-Annualtemp_1999
#2000
Annualtemp_2000<- raster_sites
values(Annualtemp_2000) <- annualtemp_joindat_2[,"2000"]
temp_done[["Annualtemp_2000"]] <-Annualtemp_2000
#2001
Annualtemp_2001<- raster_sites
values(Annualtemp_2001) <- annualtemp_joindat_2[,"2001"]
temp_done[["Annualtemp_2001"]] <-Annualtemp_2001
#2002
Annualtemp_2002<- raster_sites
values(Annualtemp_2002) <- annualtemp_joindat_2[,"2002"]
temp_done[["Annualtemp_2002"]] <-Annualtemp_2002
#2003
Annualtemp_2003<- raster_sites
values(Annualtemp_2003) <- annualtemp_joindat_2[,"2003"]
temp_done[["Annualtemp_2003"]] <-Annualtemp_2003
#2004
Annualtemp_2004<- raster_sites
values(Annualtemp_2004) <- annualtemp_joindat_2[,"2004"]
temp_done[["Annualtemp_2004"]] <-Annualtemp_2004
#2005
Annualtemp_2005<- raster_sites
values(Annualtemp_2005) <- annualtemp_joindat_2[,"2005"]
temp_done[["Annualtemp_2005"]] <-Annualtemp_2005
#2006
Annualtemp_2006<- raster_sites
values(Annualtemp_2006) <- annualtemp_joindat_2[,"2006"]
temp_done[["Annualtemp_2006"]] <-Annualtemp_2006
#2007
Annualtemp_2007<- raster_sites
values(Annualtemp_2007) <- annualtemp_joindat_2[,"2007"]
temp_done[["Annualtemp_2007"]] <-Annualtemp_2007
#2008
Annualtemp_2008<- raster_sites
values(Annualtemp_2008) <- annualtemp_joindat_2[,"2008"]
temp_done[["Annualtemp_2008"]] <-Annualtemp_2008
#2009
Annualtemp_2009<- raster_sites
values(Annualtemp_2009) <- annualtemp_joindat_2[,"2009"]
temp_done[["Annualtemp_2009"]] <-Annualtemp_2009
#2010
Annualtemp_2010<- raster_sites
values(Annualtemp_2010) <- annualtemp_joindat_2[,"2010"]
temp_done[["Annualtemp_2010"]] <-Annualtemp_2010
#2011
Annualtemp_2011<- raster_sites
values(Annualtemp_2011) <- annualtemp_joindat_2[,"2011"]
temp_done[["Annualtemp_2011"]] <-Annualtemp_2011
#2012
Annualtemp_2012<- raster_sites
values(Annualtemp_2012) <- annualtemp_joindat_2[,"2012"]
temp_done[["Annualtemp_2012"]] <-Annualtemp_2012
#2013
Annualtemp_2013<- raster_sites
values(Annualtemp_2013) <- annualtemp_joindat_2[,"2013"]
temp_done[["Annualtemp_2013"]] <-Annualtemp_2013
#2014
Annualtemp_2014<- raster_sites
values(Annualtemp_2014) <- annualtemp_joindat_2[,"2014"]
temp_done[["Annualtemp_2014"]] <-Annualtemp_2014
#2015
Annualtemp_2015<- raster_sites
values(Annualtemp_2015) <- annualtemp_joindat_2[,"2015"]
temp_done[["Annualtemp_2015"]] <-Annualtemp_2015

#all years stacked, ready for cropping for each site
temp_stack <-stack(temp_done)
plot(temp_stack)

#######transpiration
######transpiration#########
dir.AFRI_Historical <- "/Users/A02296270/Desktop/CONUS_AFRI/"
load(file.path(dir.AFRI_Historical, "annualtransp19152015")) #loads file and name it annualtransp I guess
str(annualtransp)
rownames(annualtransp)
head(annualtransp)
#add label to dataset based on row names
annualtransp$label <- row.names(annualtransp)

sitenumENDpos = as.integer(regexpr('_', annualtransp$label) )
Site <- as.integer(substr(annualtransp$label, 1, sitenumENDpos-1) )
Regionname <- substr(annualtransp$label, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
annualtransp$RegionSite <- Regionnum*1000000 + Site

annualtransp_joindat <- join(rastvals, annualtransp, by="RegionSite")
# dim(annualtransp_joindat)
# str(annualtransp_joindat)
#get rid of years that don't relate to NPP estimate
annualtransp_joindat_2<-annualtransp_joindat[,-c(2:71)]

#use joined data to populate values for a raster
transp_done <- list()
#1986
Annualtransp_1986<- raster_sites
values(Annualtransp_1986) <- annualtransp_joindat_2[,"1986"]
transp_done[["Annualtransp_1986"]] <-Annualtransp_1986
#1987
Annualtransp_1987<- raster_sites
values(Annualtransp_1987) <- annualtransp_joindat_2[,"1987"]
transp_done[["Annualtransp_1987"]] <-Annualtransp_1987
#1988
Annualtransp_1988<- raster_sites
values(Annualtransp_1988) <- annualtransp_joindat_2[,"1988"]
transp_done[["Annualtransp_1988"]] <-Annualtransp_1988
#1989
Annualtransp_1989<- raster_sites
values(Annualtransp_1989) <- annualtransp_joindat_2[,"1989"]
transp_done[["Annualtransp_1989"]] <-Annualtransp_1989
#1990
Annualtransp_1990<- raster_sites
values(Annualtransp_1990) <- annualtransp_joindat_2[,"1990"]
transp_done[["Annualtransp_1990"]] <-Annualtransp_1990
#1991
Annualtransp_1991<- raster_sites
values(Annualtransp_1991) <- annualtransp_joindat_2[,"1991"]
transp_done[["Annualtransp_1991"]] <-Annualtransp_1991
#1992
Annualtransp_1992<- raster_sites
values(Annualtransp_1992) <- annualtransp_joindat_2[,"1992"]
transp_done[["Annualtransp_1992"]] <-Annualtransp_1992
#1993
Annualtransp_1993<- raster_sites
values(Annualtransp_1993) <- annualtransp_joindat_2[,"1993"]
transp_done[["Annualtransp_1993"]] <-Annualtransp_1993
#1994
Annualtransp_1994<- raster_sites
values(Annualtransp_1994) <- annualtransp_joindat_2[,"1994"]
transp_done[["Annualtransp_1994"]] <-Annualtransp_1994
#1995
Annualtransp_1995<- raster_sites
values(Annualtransp_1995) <- annualtransp_joindat_2[,"1995"]
transp_done[["Annualtransp_1995"]] <-Annualtransp_1995
#1996
Annualtransp_1996<- raster_sites
values(Annualtransp_1996) <- annualtransp_joindat_2[,"1996"]
transp_done[["Annualtransp_1996"]] <-Annualtransp_1996
#1997
Annualtransp_1997<- raster_sites
values(Annualtransp_1997) <- annualtransp_joindat_2[,"1997"]
transp_done[["Annualtransp_1997"]] <-Annualtransp_1997
#1998
Annualtransp_1998<- raster_sites
values(Annualtransp_1998) <- annualtransp_joindat_2[,"1998"]
transp_done[["Annualtransp_1998"]] <-Annualtransp_1998
#1999
Annualtransp_1999<- raster_sites
values(Annualtransp_1999) <- annualtransp_joindat_2[,"1999"]
transp_done[["Annualtransp_1999"]] <-Annualtransp_1999
#2000
Annualtransp_2000<- raster_sites
values(Annualtransp_2000) <- annualtransp_joindat_2[,"2000"]
transp_done[["Annualtransp_2000"]] <-Annualtransp_2000
#2001
Annualtransp_2001<- raster_sites
values(Annualtransp_2001) <- annualtransp_joindat_2[,"2001"]
transp_done[["Annualtransp_2001"]] <-Annualtransp_2001
#2002
Annualtransp_2002<- raster_sites
values(Annualtransp_2002) <- annualtransp_joindat_2[,"2002"]
transp_done[["Annualtransp_2002"]] <-Annualtransp_2002
#2003
Annualtransp_2003<- raster_sites
values(Annualtransp_2003) <- annualtransp_joindat_2[,"2003"]
transp_done[["Annualtransp_2003"]] <-Annualtransp_2003
#2004
Annualtransp_2004<- raster_sites
values(Annualtransp_2004) <- annualtransp_joindat_2[,"2004"]
transp_done[["Annualtransp_2004"]] <-Annualtransp_2004
#2005
Annualtransp_2005<- raster_sites
values(Annualtransp_2005) <- annualtransp_joindat_2[,"2005"]
transp_done[["Annualtransp_2005"]] <-Annualtransp_2005
#2006
Annualtransp_2006<- raster_sites
values(Annualtransp_2006) <- annualtransp_joindat_2[,"2006"]
transp_done[["Annualtransp_2006"]] <-Annualtransp_2006
#2007
Annualtransp_2007<- raster_sites
values(Annualtransp_2007) <- annualtransp_joindat_2[,"2007"]
transp_done[["Annualtransp_2007"]] <-Annualtransp_2007
#2008
Annualtransp_2008<- raster_sites
values(Annualtransp_2008) <- annualtransp_joindat_2[,"2008"]
transp_done[["Annualtransp_2008"]] <-Annualtransp_2008
#2009
Annualtransp_2009<- raster_sites
values(Annualtransp_2009) <- annualtransp_joindat_2[,"2009"]
transp_done[["Annualtransp_2009"]] <-Annualtransp_2009
#2010
Annualtransp_2010<- raster_sites
values(Annualtransp_2010) <- annualtransp_joindat_2[,"2010"]
transp_done[["Annualtransp_2010"]] <-Annualtransp_2010
#2011
Annualtransp_2011<- raster_sites
values(Annualtransp_2011) <- annualtransp_joindat_2[,"2011"]
transp_done[["Annualtransp_2011"]] <-Annualtransp_2011
#2012
Annualtransp_2012<- raster_sites
values(Annualtransp_2012) <- annualtransp_joindat_2[,"2012"]
transp_done[["Annualtransp_2012"]] <-Annualtransp_2012
#2013
Annualtransp_2013<- raster_sites
values(Annualtransp_2013) <- annualtransp_joindat_2[,"2013"]
transp_done[["Annualtransp_2013"]] <-Annualtransp_2013
#2014
Annualtransp_2014<- raster_sites
values(Annualtransp_2014) <- annualtransp_joindat_2[,"2014"]
transp_done[["Annualtransp_2014"]] <-Annualtransp_2014
#2015
Annualtransp_2015<- raster_sites
values(Annualtransp_2015) <- annualtransp_joindat_2[,"2015"]
transp_done[["Annualtransp_2015"]] <-Annualtransp_2015

#all years stacked, ready for cropping for each site
transp_stack <-stack(transp_done)
plot(transp_stack)