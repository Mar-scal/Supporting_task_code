load("Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata")

towids <- ddply(.data=all.surv.dat, .(year, bank, random),
                summarize,
                mintow= min(tow),
                maxtow=max(tow))
towids$text <- paste0(towids$mintow, " - ", towids$maxtow)
towids$year <- as.numeric(as.character(towids$year))


pdf(file="Y:/Offshore scallop/Assessment/Data/Survey_data/2018/checking database tow IDs.pdf", onefile=T, height=6, width=8)
for(i in 1:length(unique(towids$bank))){
  sub <- subset(towids, bank==unique(towids$bank)[i])

  print(ggplot() + geom_text(data=sub[!sub$random==1,], aes(x=maxtow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_text(data=sub[sub$random==1,], aes(x=mintow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_segment(data=sub, aes(x=mintow, xend=maxtow, y=year, yend=year, colour=as.factor(random)), lwd=1.5) + 
          theme_bw() + 
          scale_color_viridis_d(name="Tow Type ID") +
          scale_x_continuous(expand=c(0.2,0))+
          xlab("Tow number range") +
          ylab("Year") +
          ggtitle(unique(towids$bank)[i]))
}
dev.off()
