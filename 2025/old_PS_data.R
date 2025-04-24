# ancient PS data

years <- list.files("Y:/Offshore/Assessment/Data/Archive/PortSampling/")
years <- 1981:2005
paths <- NULL
for(i in years){
  files <- list.files(paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i))
  if(length(files)==1) dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files)
  if(length(files)>1){
    if(any(grepl(pattern="reg.", x=files, fixed=T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="reg.", x=files, fixed=T)])
    }
    if(!any(grepl(pattern="reg.", x=files, fixed=T)) & any(grepl(pattern="all.", x=files, fixed = T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="all.", fixed=T, x=files)])
    }
    if(any(grepl(pattern="FT", x=dat))) {
      dat <- dat[-grep(pattern="FT", x=dat)]
    }
    if(any(grepl(pattern="WF", x=dat))) {
      dat <- dat[-grep(pattern="WF", x=dat)]
    }
  }
  print(i)
  print(dat)
  paths <- c(paths, dat)
}

ps <- NULL
for(i in 1:length(paths)){
  ps[[i]] <- read.csv(paths[i], header=F, sep="\t")
}
save(ps, file="C:/Users/keyserf/Documents/temp_data/old_ps.RData")
#fails: old.ps <- do.call("rbind",ps)
