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

#get rid of years that don't relate to NPP estimate
annualprecip_joindat_2<-annualprecip_joindat[,-c(2:71)]
dim(annualprecip_joindat_2)[2]
annualprecip_joindat_3<- melt(annualprecip_joindat_2, 
            id.vars = c("RegionSite"),
            variable.name = "year")

AnnualPrecip<- AFRI_RegionSite_Raster_john

##trying to automate this
for (i in 2:dim(annualprecip_joindat_2)[2]) {
  
  
  values(AnnualPrecip_x)
  
}

plot(AnnualPrecip_x)
#use joined data to populate values for a raster
AnnualPrecip_2002<- AFRI_RegionSite_Raster_john
values(AnnualPrecip_2002) <- annualprecip_joindat[,"2002"]
plot(AnnualPrecip_2002)

#Examine the data, determine appropriate thresholds, and then establish color palettes.  

hist(annualprecip_joindat[, "2002"])
hist(na.omit((annualprecip_joindat[, "2002"])) )
hist(na.omit((annualprecip_joindat[, "2002"])) )
range(na.omit((annualprecip_joindat[, "2002"])) )


# colors ------------------------------------------------------------------


#customize colors
library("colorspace") 

#define breaks where colors will change
bks <- c( seq(from=0, to=170, by=10))
#or define breaks using quantiles
bks <- quantile(annualprecip_joindat[, "2002"], probs=seq(from=0, to=1, length=21), na.rm=TRUE)
#define colors based on the breaks
bkcls <- (rainbow_hcl(length(bks)-1 ) )        

#plot the result: you can use plot or image, which have different advantages
image(AnnualPrecip2002, breaks = bks, col= bkcls, asp=1) 

plot(AnnualPrecip2002, breaks = bks, col= bkcls) 
