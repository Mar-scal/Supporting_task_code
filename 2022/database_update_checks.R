load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/testing_results_historical_db.Rdata")

all.surv.dat.db <- all.surv.dat
bank.dat.db <- bank.dat
surv.Live.db <- surv.Live
surv.Rand.db <- surv.Rand
survey.obj.db <- survey.obj
merged.survey.obj.db <- merged.survey.obj
surv.dat.db <- surv.dat
mw.dat.all.db <- mw.dat.all
cf.data.db <- cf.data
CF.current.db <- CF.current

load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/Survey_all_results - 2021FINAL.Rdata")

require(tidyverse)
tows <- all.surv.dat.db %>% 
  dplyr::filter(!(bank=="GBa" & random==3)) %>%
  dplyr::filter(!(bank=="GBb" & random==3)) %>%
  dplyr::filter(!(cruise %in% c("CK03", "P454") & bank=="BBn")) %>%
  dplyr::select(year, bank, tow) %>%
  dplyr::distinct() %>%
  dplyr::arrange(year, bank, tow) %>%
  group_by(year, bank) %>%
  summarize(new=n())

tows2 <- dplyr::arrange(unique(dplyr::select(all.surv.dat, year, bank, tow)), year, bank, tow) %>%
  group_by(year, bank) %>%
  summarize(old=n())


tows <- dplyr::left_join(tows, tows2)
dim(tows)
View(tows[is.na(tows$old),])
tows[!(tows$old==tows$new) & !is.na(tows$old),]

unique(all.surv.dat.db[all.surv.dat.db$year==1994 & all.surv.dat.db$bank=="BBn", c("cruise", "tow")])


table(all.surv.dat.db[all.surv.dat.db$year==1994 & all.surv.dat.db$bank=="BBn", c("cruise", "tow")]$cruise)
table(all.surv.dat[all.surv.dat$year==1994 & all.surv.dat$bank=="BBn", c("cruise", "tow")]$cruise)


require(compareDF)


create_output_table(compare_df(tows, tows2), output_type = 'xlsx', file_name = "test_file.xlsx")
dim(tows)
dim(tows2)


dim(all.surv.dat.db)
dim(all.surv.dat)

dim(bank.dat.db$BBn)
dim(bank.dat$BBn)

dim(surv.Live.db$BBn)
dim(surv.Live$BBn)

# dim(survey.obj.db$BBn$bankpertow)
# dim(survey.obj$BBn$bankpertow)

dim(surv.dat.db$BBn)
dim(surv.dat$BBn)

dim(mw.dat.all.db$BBn)
dim(mw.dat.all$BBn)

dim(cf.data.db$BBn$CF.data)
dim(cf.data$BBn$CF.data)

dim(CF.current.db$BBn)
dim(CF.current$BBn)
