# GB landings for Dvora (NOAA) #DR2024_03

require(tidyverse)

# set your directory
direct <- "Y:/Offshore/Assessment/"

# read the function from github
funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=2017:2023, get.marfis = T, export = F, direct = direct, un="keyserf", pw="Decade06")


landings <- new.log.dat %>% dplyr::group_by(year, sfa, bank) %>%
  dplyr::summarize(total_kg = sum(pro.repwt, na.rm=T),
                   n_licence = length(unique(licence)))

landings <- landings[landings$sfa %in% c("27A", "27B"),]

landings[landings$n_licence<5,]
# need to merge A and B due to rule of 5

landings$total_t <- landings$total_kg/1000

landings <- landings %>%
  dplyr::select(-total_kg) %>% 
  pivot_wider(id_cols = c("year"), names_from = c("sfa", "bank"), values_from = "total_t")

landings <- landings[landings$year>2017,]

landings$SFA27 <- landings$`27A_GBa` + landings$`27B_GBb`
landings <- landings %>% select(year, SFA27)
write.csv(landings, file = "Y:/Offshore/Data requests/2024/landings_for_NOAA/landings_GBCanada_2018-2023.csv")
