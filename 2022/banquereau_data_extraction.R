load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.RData")

Ban <- all.surv.dat[all.surv.dat$bank %in% c("Ban", "BanIce"),]
Ban <- arrange(Ban, year, cruise, date, tow)

write.csv(Ban, "Y:/Offshore/Assessment/2022/Supporting_tasks/Banquereau_survey_data.csv")

