# bonus figures for Survey Summary 2025

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.RData") 
rdata <- list(SS.summary=SS.summary, merged.survey.obj=merged.survey.obj, survey.obj=survey.obj, cf.data=cf.data, surv.Clap.Rand, nickname=nickname, survey.info=survey.info)

# bank per tow

bankpertow <- get("rdata")$SS.summary$GBa %>%
  dplyr::select(bank, year, n, N, NR, NPR, N.cv, NR.cv, NPR.cv, I, IR, IPR, I.cv, IR.cv, IPR.cv) %>%
  dplyr::mutate(liner="lined")

bankpertow <- bankpertow %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Ntot = sum(N, NR, NPR),
         Itot = sum(I, IR, IPR))

N <- ggplot() + geom_line(data=bankpertow,
                          aes(year, Ntot)) +
  geom_point(data=bankpertow,
             aes(year, Ntot)) +
  #geom_errorbar(data=bankpertow, aes(year, ymax=NPR+(NPR*NPR.cv), ymin=NPR-(NPR*NPR.cv), colour=bank), width=0) +
  # geom_ribbon(data=bankpertow[bankpertow$index=="N",],
  #             aes(year, ymax=index.val+cv.val, ymin=index.val-cv.val, fill=subarea), alpha=0.3)+
  theme_bw() +
  # facet_grid(factor(index, levels=c("N", "I"))~factor(size, levels=c("PR", "R", "FR")),
  #            scales="free_y")
  theme(strip.background=element_blank()) +
  #scale_y_log10() +
  xlab("Year") +
  guides(colour = "none", fill="none", linetype="none", shape="none")+
  ylab("Total abundance (number/tow)")

I <- ggplot() + geom_line(data=bankpertow,
                          aes(year, Itot)) +
  geom_point(data=bankpertow,
             aes(year, Itot)) +
  #geom_errorbar(data=bankpertow, aes(year, ymax=NPR+(NPR*NPR.cv), ymin=NPR-(NPR*NPR.cv), colour=bank), width=0) +
  # geom_ribbon(data=bankpertow[bankpertow$index=="N",],
  #             aes(year, ymax=index.val+cv.val, ymin=index.val-cv.val, fill=subarea), alpha=0.3)+
  theme_bw() +
  # facet_grid(factor(index, levels=c("N", "I"))~factor(size, levels=c("PR", "R", "FR")),
  #            scales="free_y")
  theme(strip.background=element_blank()) +
  #scale_y_log10() +
  xlab("Year") +
  guides(colour = "none", fill="none", linetype="none", shape="none") +
  ylab("Total biomass (g/tow)")

require(patchwork)
png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBa/timeseries_total.png",
    height=5, width=8, units="in", res=400)
N + I +  plot_annotation(
  title = "GBa")
dev.off()



### Shell height frequency
shf <- as.data.frame(survey.obj$GBa$shf.dat$n.yst)

names(shf) <- paste0("V", names(shf))
shf$years <- survey.obj$GBa$bankpertow$year
shf <- shf %>% tidyr::pivot_longer(cols = starts_with("V"))
shf$name <- gsub(x=shf$name, pattern="V", replacement="")
if(!any(shf$name==200)) shf$name <- as.numeric(shf$name)*5

shf <- shf[!is.na(shf$years),]

years <- shf %>%
  dplyr::group_by(years) %>%
  dplyr::summarize(total=sum(value))

shf <- left_join(shf, years)
shf$density <- shf$value/shf$total
shf$name <- as.numeric(shf$name)

bins <- data.frame(shf=rep(seq(0,199.9,0.1), length(unique(shf$years))), name=rep(rep(seq(5,200,5), each=50), length(unique(shf$years))), years=rep(unique(shf$years), each=2000))

shf <- full_join(shf, bins)

shf <- left_join(data.frame(years = min(shf$years, na.rm=T):max(shf$years, na.rm=T)), shf)
shf$bank <- "GBa"
shf$CS <- survey.obj[["GBa"]]$model.dat$CS[survey.obj[["GBa"]]$model.dat$year==max(survey.obj[["GBa"]]$model.dat$year)]
shf$RS <- survey.obj[["GBa"]]$model.dat$RS[survey.obj[["GBa"]]$model.dat$year==max(survey.obj[["GBa"]]$model.dat$year)]

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBa/SHF_historic.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf, aes(x = shf, y = as.factor(years), height=value), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(0,200), breaks=seq(0,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf, aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf, aes(xintercept=CS), linetype="dashed")+
        ggtitle("Shell height frequencies")
)
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBa/SHF_historic_log.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf, aes(x = shf, y = as.factor(years), height=log(value)), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(0,200), breaks=seq(0,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf, aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf, aes(xintercept=CS), linetype="dashed") +
        ggtitle("Shell height frequencies (log scale)")
)
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBa/SHF_historic_75plus.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf[shf$name>75 | is.na(shf$name),], aes(x = shf, y = as.factor(years), height=value), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(75,200), breaks=seq(70,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf[shf$name>75 | is.na(shf$name),], aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf[shf$name>75 | is.na(shf$name),], aes(xintercept=CS), linetype="dashed") +
        ggtitle("Shell height frequencies (>75mm)")
)
dev.off()



### GBb
# bank per tow

bankpertow <- get("rdata")$SS.summary$GBb %>%
  dplyr::select(bank, year, n, N, NR, NPR, N.cv, NR.cv, NPR.cv, I, IR, IPR, I.cv, IR.cv, IPR.cv) %>%
  dplyr::mutate(liner="lined")

bankpertow <- bankpertow %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Ntot = sum(N, NR, NPR),
         Itot = sum(I, IR, IPR))

N <- ggplot() + geom_line(data=bankpertow,
                          aes(year, Ntot)) +
  geom_point(data=bankpertow,
             aes(year, Ntot)) +
  #geom_errorbar(data=bankpertow, aes(year, ymax=NPR+(NPR*NPR.cv), ymin=NPR-(NPR*NPR.cv), colour=bank), width=0) +
  # geom_ribbon(data=bankpertow[bankpertow$index=="N",],
  #             aes(year, ymax=index.val+cv.val, ymin=index.val-cv.val, fill=subarea), alpha=0.3)+
  theme_bw() +
  # facet_grid(factor(index, levels=c("N", "I"))~factor(size, levels=c("PR", "R", "FR")),
  #            scales="free_y")
  theme(strip.background=element_blank()) +
  #scale_y_log10() +
  xlab("Year") +
  guides(colour = "none", fill="none", linetype="none", shape="none")+
  ylab("Total abundance (number/tow)")

I <- ggplot() + geom_line(data=bankpertow,
                          aes(year, Itot)) +
  geom_point(data=bankpertow,
             aes(year, Itot)) +
  #geom_errorbar(data=bankpertow, aes(year, ymax=NPR+(NPR*NPR.cv), ymin=NPR-(NPR*NPR.cv), colour=bank), width=0) +
  # geom_ribbon(data=bankpertow[bankpertow$index=="N",],
  #             aes(year, ymax=index.val+cv.val, ymin=index.val-cv.val, fill=subarea), alpha=0.3)+
  theme_bw() +
  # facet_grid(factor(index, levels=c("N", "I"))~factor(size, levels=c("PR", "R", "FR")),
  #            scales="free_y")
  theme(strip.background=element_blank()) +
  #scale_y_log10() +
  xlab("Year") +
  guides(colour = "none", fill="none", linetype="none", shape="none") +
  ylab("Total biomass (g/tow)")

require(patchwork)
png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBb/timeseries_total.png",
    height=5, width=8, units="in", res=400)
N + I +  plot_annotation(
  title = "GBb")
dev.off()



### Shell height frequency
shf <- as.data.frame(survey.obj$GBb$shf.dat$n.yst)

names(shf) <- paste0("V", names(shf))
shf$years <- survey.obj$GBb$bankpertow$year
shf <- shf %>% tidyr::pivot_longer(cols = starts_with("V"))
shf$name <- gsub(x=shf$name, pattern="V", replacement="")
if(!any(shf$name==200)) shf$name <- as.numeric(shf$name)*5

shf <- shf[!is.na(shf$years),]

years <- shf %>%
  dplyr::group_by(years) %>%
  dplyr::summarize(total=sum(value))

shf <- left_join(shf, years)
shf$density <- shf$value/shf$total
shf$name <- as.numeric(shf$name)

bins <- data.frame(shf=rep(seq(0,199.9,0.1), length(unique(shf$years))), name=rep(rep(seq(5,200,5), each=50), length(unique(shf$years))), years=rep(unique(shf$years), each=2000))

shf <- full_join(shf, bins)

shf <- left_join(data.frame(years = min(shf$years, na.rm=T):max(shf$years, na.rm=T)), shf)
shf$bank <- "GBb"
shf$CS <- survey.obj[["GBb"]]$model.dat$CS[survey.obj[["GBb"]]$model.dat$year==max(survey.obj[["GBb"]]$model.dat$year)]
shf$RS <- survey.obj[["GBb"]]$model.dat$RS[survey.obj[["GBb"]]$model.dat$year==max(survey.obj[["GBb"]]$model.dat$year)]

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBb/SHF_historic.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf, aes(x = shf, y = as.factor(years), height=value), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(0,200), breaks=seq(0,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf, aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf, aes(xintercept=CS), linetype="dashed")+
        ggtitle("Shell height frequencies")
)
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBb/SHF_historic_log.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf, aes(x = shf, y = as.factor(years), height=log(value)), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(0,200), breaks=seq(0,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf, aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf, aes(xintercept=CS), linetype="dashed") +
        ggtitle("Shell height frequencies (log scale)")
)
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/GBb/SHF_historic_75plus.png", width=6.5, height=8, units = "in", res=420)
print(ggplot() + 
        ggridges::geom_density_ridges(data=shf[shf$name>75 | is.na(shf$name),], aes(x = shf, y = as.factor(years), height=value), 
                                      stat="identity",scale=0.9) +
        theme_bw() +
        ylab("Year") + 
        xlab("Shell height (mm)") +
        scale_x_continuous(limits=c(75,200), breaks=seq(70,200,10)) +
        scale_y_discrete(expand=c(0,0.9), limits=rev) +
        geom_vline(data=shf[shf$name>75 | is.na(shf$name),], aes(xintercept=RS), linetype="dashed") +
        geom_vline(data=shf[shf$name>75 | is.na(shf$name),], aes(xintercept=CS), linetype="dashed") +
        ggtitle("Shell height frequencies (>75mm)")
)
dev.off()
