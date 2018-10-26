#code from to john to work with his climate datasets

library(raster)


regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")



#Specify directory where you have the raster
dir.AFRI.john <- "/Users/jbradford/Dropbox/JohnWork/Bradford/A_CURRENT_PROJECTS/AFRI"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster_john <- raster(file.path(dir.AFRI, "RasterbySiteID3.tif"))

#Quick look at the structure of the raster
AFRI_RegionSite_Raster_john

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(AFRI_RegionSite_Raster_john))
names(rastvals) <- "RegionSite"

#View just the values associated with each cell
#Note that the value in the raster includes both the region (in the millions digit; 1 to 5), and the siteID (in the other digits, range 1 to ~20,000 within each region)
values(AFRI_RegionSite_Raster_john)

#Plot the raster
plot(AFRI_RegionSite_Raster_john)

#Create a raster for the different regions
AFRI_Region_raster_john <- round(AFRI_RegionSite_Raster_john/1000000)
plot(AFRI_Region_raster_john)

#Create a raster for the different regions
AFRI_Site_raster_john <- AFRI_RegionSite_Raster_john - AFRI_Region_raster_john*1000000
plot(AFRI_Site_raster_john)

#Load a dataset and plot - showing precipitation as an example
dir.AFRI_Historical <- "/Users/jbradford/Dropbox/JohnWork/Bradford/A_CURRENT_PROJECTS/AFRI/AFRI_HIstorical"
load(file.path(dir.AFRI_Historical, "annualprecip19152015")) #loads file annualprecip
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
?join
#get rid of years that don't relate to NPP estimate
annualprecip_joindat_2<-annualprecip_joindat[,-c(2:71)]

#use joined data to populate values for a raster
precip_done <- list()
#1986
AnnualPrecip_1986<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1986) <- annualprecip_joindat_2[,"1986"]
precip_done[["AnnualPrecip_1986"]] <-AnnualPrecip_1986
#1987
AnnualPrecip_1987<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1987) <- annualprecip_joindat_2[,"1987"]
precip_done[["AnnualPrecip_1987"]] <-AnnualPrecip_1987
#1988
AnnualPrecip_1988<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1988) <- annualprecip_joindat_2[,"1988"]
precip_done[["AnnualPrecip_1988"]] <-AnnualPrecip_1988
#1989
AnnualPrecip_1989<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1989) <- annualprecip_joindat_2[,"1989"]
precip_done[["AnnualPrecip_1989"]] <-AnnualPrecip_1989
#1990
AnnualPrecip_1990<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1990) <- annualprecip_joindat_2[,"1990"]
precip_done[["AnnualPrecip_1990"]] <-AnnualPrecip_1990
#1991
AnnualPrecip_1991<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1991) <- annualprecip_joindat_2[,"1991"]
precip_done[["AnnualPrecip_1991"]] <-AnnualPrecip_1991
#1992
AnnualPrecip_1992<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1992) <- annualprecip_joindat_2[,"1992"]
precip_done[["AnnualPrecip_1992"]] <-AnnualPrecip_1992
#1993
AnnualPrecip_1993<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1993) <- annualprecip_joindat_2[,"1993"]
precip_done[["AnnualPrecip_1993"]] <-AnnualPrecip_1993
#1994
AnnualPrecip_1994<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1994) <- annualprecip_joindat_2[,"1994"]
precip_done[["AnnualPrecip_1994"]] <-AnnualPrecip_1994
#1995
AnnualPrecip_1995<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1995) <- annualprecip_joindat_2[,"1995"]
precip_done[["AnnualPrecip_1995"]] <-AnnualPrecip_1995
#1996
AnnualPrecip_1996<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1996) <- annualprecip_joindat_2[,"1996"]
precip_done[["AnnualPrecip_1996"]] <-AnnualPrecip_1996
#1997
AnnualPrecip_1997<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1997) <- annualprecip_joindat_2[,"1997"]
precip_done[["AnnualPrecip_1997"]] <-AnnualPrecip_1997
#1998
AnnualPrecip_1998<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1998) <- annualprecip_joindat_2[,"1998"]
precip_done[["AnnualPrecip_1998"]] <-AnnualPrecip_1998
#1999
AnnualPrecip_1999<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_1999) <- annualprecip_joindat_2[,"1999"]
precip_done[["AnnualPrecip_1999"]] <-AnnualPrecip_1999
#2000
AnnualPrecip_2000<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2000) <- annualprecip_joindat_2[,"2000"]
precip_done[["AnnualPrecip_2000"]] <-AnnualPrecip_2000
#2001
AnnualPrecip_2001<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2001) <- annualprecip_joindat_2[,"2001"]
precip_done[["AnnualPrecip_2001"]] <-AnnualPrecip_2001
#2002
AnnualPrecip_2002<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2002) <- annualprecip_joindat_2[,"2002"]
precip_done[["AnnualPrecip_2002"]] <-AnnualPrecip_2002
#2003
AnnualPrecip_2003<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2003) <- annualprecip_joindat_2[,"2003"]
precip_done[["AnnualPrecip_2003"]] <-AnnualPrecip_2003
#2004
AnnualPrecip_2004<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2004) <- annualprecip_joindat_2[,"2004"]
precip_done[["AnnualPrecip_2004"]] <-AnnualPrecip_2004
#2005
AnnualPrecip_2005<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2005) <- annualprecip_joindat_2[,"2005"]
precip_done[["AnnualPrecip_2005"]] <-AnnualPrecip_2005
#2006
AnnualPrecip_2006<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2006) <- annualprecip_joindat_2[,"2006"]
precip_done[["AnnualPrecip_2006"]] <-AnnualPrecip_2006
#2007
AnnualPrecip_2007<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2007) <- annualprecip_joindat_2[,"2007"]
precip_done[["AnnualPrecip_2007"]] <-AnnualPrecip_2007
#2008
AnnualPrecip_2008<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2008) <- annualprecip_joindat_2[,"2008"]
precip_done[["AnnualPrecip_2008"]] <-AnnualPrecip_2008
#2009
AnnualPrecip_2009<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2009) <- annualprecip_joindat_2[,"2009"]
precip_done[["AnnualPrecip_2009"]] <-AnnualPrecip_2009
#2010
AnnualPrecip_2010<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2010) <- annualprecip_joindat_2[,"2010"]
precip_done[["AnnualPrecip_2010"]] <-AnnualPrecip_2010
#2011
AnnualPrecip_2011<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2011) <- annualprecip_joindat_2[,"2011"]
precip_done[["AnnualPrecip_2011"]] <-AnnualPrecip_2011
#2012
AnnualPrecip_2012<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2012) <- annualprecip_joindat_2[,"2012"]
precip_done[["AnnualPrecip_2012"]] <-AnnualPrecip_2012
#2013
AnnualPrecip_2013<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2013) <- annualprecip_joindat_2[,"2013"]
precip_done[["AnnualPrecip_2013"]] <-AnnualPrecip_2013
#2014
AnnualPrecip_2014<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2014) <- annualprecip_joindat_2[,"2014"]
precip_done[["AnnualPrecip_2014"]] <-AnnualPrecip_2014
#2015
AnnualPrecip_2015<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2015) <- annualprecip_joindat_2[,"2015"]
precip_done[["AnnualPrecip_2015"]] <-AnnualPrecip_2015

#all years stacked, ready for cropping for each site
precip_stack <-stack(precip_done)

