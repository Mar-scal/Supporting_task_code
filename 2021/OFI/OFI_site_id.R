# reviewing survey and fishery data to identify sites for OFI sampling. 
# To facilitate this, I just hacked into pectinid, and exported from there. This code is NOT reproducible :(

load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/GBa_figures_res_250-250.RData")
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")


bound.poly.surv <- as.PolySet(bound.surv.poly$GBa,projection ="LL")
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)

load("C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/Data/Survey_data/2020/Survey_summary_output/GBa/GBa_figures_res_250-250.RData")

str(mod.res$`FR-spatial`)

base.lvls=c(0,1,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
cols <- c(rev(viridis::plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
          rev(viridis::plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))


source("C:/Users/keyserf/Documents/Github/FK/Assessment_fns/Maps/pectinid_projector_sf.R")

pecjector(area = "GBa",plot = T,crs = st_crs(mesh$crs)[1]$epsg,
          add_inla= list(field = mod.res$`FR-spatial`,mesh = mesh, dims=c(250,250),clip = bound.poly.surv.sp,
                         scale = list(scale = "discrete",breaks = base.lvls, palette = cols))) 

pect_plot

st_write(spd, "Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBa_2019.shp")
st_write(spd, "Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBb_2019.shp")
st_write(spd, "Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBa_2020.shp")
st_write(spd, "Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBb_2020.shp")


plot(st_read("Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBb_2019.shp"))


load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2020/OSAC_summary.RData")


source("C:/Users/keyserf/Documents/GitHub/FK/Assessment_fns/Survey_and_OSAC/OSAC_fishery_figures.r")


# Get the survey boundary polygon for the bank 
bound.poly.surv <- as.PolySet(bound.surv.poly$GBa,projection ="LL")

bnk.survey.bound.poly <- bound.surv.poly$GBa

# Set the levels, might need to think a bit about these!
lvls=lvl
#Get the total removals from each 1 minute cell within the bank for the levels (10 kg to 50 tonnes!)
bnk.polys <- gridPlot(bnk.fish.dat,bnk.survey.bound.poly,lvls,border=poly.brd,FUN=fun,grid.size=grids)

cells <- fish.cells[fish.cells$bank=="GBb",]

cells <- st_as_sf(cells, remove=F, coords = c("lon", "lat"))

ggplot() + geom_sf(data=cells, aes(colour=catch))

st_write(cells, "Y:/Offshore/Assessment/2021/Supporting_tasks/OFI_GBb_fishery_2020.shp")


check <- st_read("Y:/Projects/OFI/BEcoME/Survey_Planning/share_w_DalClearwater/Survey_locations.shp")
check


