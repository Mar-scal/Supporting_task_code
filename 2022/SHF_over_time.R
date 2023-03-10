melted <- all.surv.dat %>% pivot_longer(cols = paste0("h", seq(5, 200, 5)))
melted$name <- as.numeric(as.character(gsub(x=melted$name, "h", "")))

medians <- melted %>%
  filter(state=="live" & bank %in% c("GBa", "GBb", "Ger", "Mid", "Sab", "BBn", "BBs", "Ban"))%>%
  group_by(bank, name) %>%
  summarize(medval = median(value),
            meanval = mean(value))

tow_agg <- melted %>%
  filter(state=="live" & bank %in% c("GBa", "GBb", "Ger", "Mid", "Sab", "BBn", "BBs", "Ban"))%>%
  group_by(bank, name, year) %>%
  summarize(total_bin= sum(value))

totals <-  melted %>%
  filter(state=="live" & bank %in% c("GBa", "GBb", "Ger", "Mid", "Sab", "BBn", "BBs", "Ban"))%>%
  group_by(bank, year) %>%
  summarize(total = sum(value))

tow_agg <- left_join(tow_agg, totals)

boxes <- melted %>%
  filter(state=="live" & bank %in% c("GBa", "GBb", "Ger", "Mid", "Sab", "BBn", "BBs", "Ban")) %>%
  group_by(bank, name, year) %>%
  dplyr::summarize(p1 = boxplot.stats(value/total)$stats[1], 
            p2 = boxplot.stats(value/total)$stats[2], 
            p3 = boxplot.stats(value/total)$stats[3],
            p4 = boxplot.stats(value/total)$stats[4], 
            p5 = boxplot.stats(value/total)$stats[5])

ggplot() + 
  geom_boxplot(data=boxes, aes(name, ymin=p1, lower=p2, middle=p3, upper=p4, ymax=p5, group=name), stat="identity") + 
  facet_wrap(~bank, scales="free_y") +
  geom_vline(data=boxes, aes(xintercept=130), lwd=1, lty=2, colour=2)


ggplot() + geom_point(data=medians, aes(name, medval)) + 
  facet_wrap(~bank, scales="free_y") +
  geom_point(data=medians, aes(name, meanval), colour="blue") + 
  geom_vline(data=medians, aes(xintercept=130)) + 
  scale_x_continuous(breaks=seq(0, 200, 10))

ggplot() + geom_point(data=tow_agg, 
                     aes(name, total_bin/total, group=year)) + 
  facet_wrap(~bank, scales="free_y") +
  geom_vline(data=boxes, aes(xintercept=130), lwd=1, lty=2, colour=2) +
  ylab("Proportion of survey catch in bin")
