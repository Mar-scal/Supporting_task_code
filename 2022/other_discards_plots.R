
# after running estimatediscards.R

discards_report <- left_join(discards_report, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
discards_report$year <- year(ymd(discards_report$LANDING_DATE))
ann_bank_sum <- discards_report %>%
  group_by(year, bank, COMMON) %>%
  summarize(prorated_discards=sum(discards/prophooks))

unique(ann_bank_sum$COMMON[ann_bank_sum$prorated_discards>200])

ggplot() + geom_point(data=ann_bank_sum, aes(year, prorated_discards)) #+ facet_wrap(~COMMON, scales="free")
ggplot() + geom_point(data=ann_bank_sum, aes(year, prorated_discards)) + facet_grid(~bank)


halibut <- select(discards_report2, TRIP, prophooks, bank, hm, `HALIBUT(ATLANTIC)`)
halibut <- left_join(halibut, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
halibut$presence <- ifelse(halibut$`HALIBUT(ATLANTIC)`>0, "present", "absent")
a <- ggplot() + geom_point(data=halibut, aes(ymd(LANDING_DATE), `HALIBUT(ATLANTIC)`/prophooks, shape=presence)) +
  scale_shape_manual(values=c(4,1))+
  xlab("Observer trip landing date") +
  ylab("Prorated discards of Halibut (kg)") +
  theme_bw() + facet_wrap(~bank)

lump <- select(discards_report, TRIP, prophooks, bank, hm, `LUMPFISH`)
lump <- left_join(lump, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
b <- ggplot() + geom_point(data=lump, aes(ymd(LANDING_DATE), `LUMPFISH`/prophooks)) +
  xlab("Observer trip landing date") +
  ylab("Prorated discards of Lumpfish (kg)") +
  theme_minimal()+ facet_wrap(~bank)

cod <- select(discards_report, TRIP, prophooks, bank, hm, `COD(ATLANTIC)`)
cod <- left_join(cod, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
c <- ggplot() + geom_point(data=cod, aes(ymd(LANDING_DATE), `COD(ATLANTIC)`/prophooks)) +
  xlab("Observer trip landing date") +
  ylab("Prorated discards of Cod (kg)") +
  theme_minimal()+ facet_wrap(~bank)

haddock <- select(discards_report, TRIP, prophooks, bank, hm, `HADDOCK`)
haddock <- left_join(haddock, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
d <- ggplot() + geom_point(data=haddock, aes(ymd(LANDING_DATE), `HADDOCK`/prophooks)) +
  xlab("Observer trip landing date") +
  ylab("Prorated discards of Haddock (kg)") +
  theme_minimal()+ facet_wrap(~bank)

ytf <- select(discards_report, TRIP, prophooks, bank, hm, `YELLOWTAIL FLOUNDER`)
ytf <- left_join(ytf, unique(set_coord[, c("TRIP", "LANDING_DATE")]))
e <- ggplot() + geom_point(data=ytf, aes(ymd(LANDING_DATE), `YELLOWTAIL FLOUNDER`/prophooks)) +
  xlab("Observer trip landing date") +
  ylab("Prorated discards of Yellowtail Flounder (kg)") +
  theme_minimal()+ facet_wrap(~bank)

a

require(ggplot2)
hal <- ggplot() + geom_point(data=monthly[monthly$COMMON=="HALIBUT(ATLANTIC)",], aes(ymd(paste0(year, "-", month, "-01")), discardrate)) + 
  xlab("Months with observer coverage") +
  ylab("Discard rate (kg/hm)") + 
  theme_bw() +
  facet_wrap(~bank)

hal2 <- ggplot() + geom_point(data=monthly[monthly$COMMON=="HALIBUT(ATLANTIC)",], aes(ymd(paste0(year, "-", month, "-01")), totaldiscards_kg)) + 
  xlab("Months with observer coverage") +
  ylab("Fleet discards (kg)") + 
  theme_bw() +
  facet_wrap(~bank)

require(patchwork)
hal/hal2 + plot_annotation("Halibut discards from offshore scallop fishery")

ggplot() + geom_point(data=monthly[monthly$COMMON=="LUMPFISH",], aes(ymd(paste0(year, "-", month, "-01")), discardrate)) + 
  xlab("Months") +
  ylab("Discard rate (kg/hm)") + 
  theme_bw() +
  facet_wrap(~bank)

ggplot() + geom_point(data=monthly[monthly$COMMON=="LUMPFISH",], aes(ymd(paste0(year, "-", month, "-01")), totaldiscards_kg)) + 
  xlab("Months") +
  ylab("Fleet discards (kg)") + 
  theme_bw() +
  facet_wrap(~bank)

ggplot() + geom_point(data=monthly[monthly$COMMON=="COD(ATLANTIC)",], aes(ymd(paste0(year, "-", month, "-01")), discardrate)) + 
  xlab("Months") +
  ylab("Discard rate (kg/hm)") + 
  theme_bw() +
  facet_wrap(~bank)

ggplot() + geom_point(data=monthly[monthly$COMMON=="COD(ATLANTIC)",], aes(ymd(paste0(year, "-", month, "-01")), totaldiscards_kg)) + 
  xlab("Months") +
  ylab("Fleet discards (kg)") + 
  theme_bw() +
  facet_wrap(~bank)

ggplot() + geom_point(data=monthly[monthly$COMMON=="HADDOCK",], aes(ymd(paste0(year, "-", month, "-01")), discardrate)) + 
  xlab("Months") +
  ylab("Discard rate (kg/hm)") + 
  theme_bw() +
  facet_wrap(~bank)

ggplot() + geom_point(data=monthly[monthly$COMMON=="HADDOCK",], aes(ymd(paste0(year, "-", month, "-01")), totaldiscards_kg)) + 
  xlab("Months") +
  ylab("Fleet discards (kg)") + 
  theme_bw() +
  facet_wrap(~bank)


