# A quick little script to identify the historic stations that were sampled on GB in the spring since the dawn of time.

direct <- "Y:/Offshore scallop/Assessment/"
  
library(tidyverse)
load(paste0(direct,"Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata"))
# Bring in the functions we'll need
source(paste(direct,"Assessment_fns/Maps/pectinid_projector.R",sep=""))
source(paste(direct,"Assessment_fns/Maps/convert_coords.R",sep=""))
source(paste(direct,"Assessment_fns/Maps/add_alpha_function.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/combine_shapefile_layers.R",sep=""))

# Get the GB spring data
# First just the condition data
cond.locs <- cf.data$GB$CF.data
str(cond.locs)
# Then all the data, primiarily interested in the abundances.
surv.res <- surv.Live$GB
surv.res$tow <- as.character(surv.res$tow)
years <- unique(surv.res$year)
num.years <- length(years)
# Now map the locations by year

windows(11,11)
pdf(paste0(direct,"2019/Misc/GB_sampling_locations.pdf"), width = 11, height = 11,onefile = T)
par(mfrow=c(2,3))
for(i in 1:num.years)
{
  tmp.surv <- surv.res[surv.res$year == years[i],]
  tmp <- left_join(cond.locs[cond.locs$year == years[i],],tmp.surv,by="tow")
  tmp.surv2 <- tmp.surv[!tmp.surv$tow %in% tmp$tow,]
  pecjector("GB",add_EEZ = T,add_custom = paste0(direct,("Data/Maps/approved/GIS_layers/offshore/GB.shp")),add_sfas = NULL)
  text(tmp$lon.x,tmp$lat.x,labels=signif(tmp$com,digits=2),cex=0.4)
  text(tmp.surv2$lon,tmp.surv2$lat,labels=signif(tmp.surv2$com,digits=2),col = "blue",cex=0.4)
  title(years[i])
} 
dev.off()

# Let's plot our 300 series stations...
surv.2017 <- surv.res[surv.res$year == 2017,]
pecjector("GB",add_EEZ = T,add_custom = paste0(direct,("Data/Maps/approved/GIS_layers/offshore/GB.shp")),add_sfas = NULL)
text(surv.2017$lon,surv.2017$lat,labels=surv.2017$tow,cex=1)


aggregate(com~tow,FUN =min,data = surv.res)
aggregate(com~tow,FUN =mean,data = surv.res)
aggregate(com~tow,FUN =length,data = surv.res)
