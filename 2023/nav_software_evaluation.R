
# From line ~368 of olex_vs_ov_2022.Rmd

lengthcomp_long <- pivot_longer(lengthcomp, cols = names(lengthcomp)[!names(lengthcomp) %in% "tow"])

lengthcomp_long$name <- as.factor(lengthcomp_long$name)
levels(lengthcomp_long$name) <- c("Olex (raw)", "Olex (correction applied)", "Oceanvision (current method)")

png("Y:/Offshore/Assessment/2023/Supporting_tasks/Olex_transition_boxplot.png", width=6, height=4, res=400, units="in")
ggplot() + geom_boxplot(data=lengthcomp_long, aes(x= name, y=as.numeric(value))) +
  xlab("Tracking method") +
  ylab("Tow length (m)") +
  theme_bw()
dev.off()

lengthcomp_ov <- pivot_longer(lengthcomp, cols=c("length_olex", "length_olex_corr"))
lengthcomp_ov$name <- as.factor(lengthcomp_ov$name)
levels(lengthcomp_ov$name) <- c("Olex (raw)", "Olex (correction applied)")

png("Y:/Offshore/Assessment/2023/Supporting_tasks/Olex_transition_linear.png", width=6, height=4, res=400, units="in")
ggplot() + geom_point(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), alpha=0.1) +
  geom_smooth(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), method="lm") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  theme_bw() + 
  xlab("Tow length from Oceanvision (m)") +
  ylab("Tow length from Olex (m)") +
  scale_colour_viridis_d(option = "D", begin = 0.2, end=0.8, name=NULL) + 
  theme(legend.position=c(0.8, 0.2), legend.background=element_rect(colour="black", fill="transparent"))
dev.off()

dim(lengthcomp)
head(lengthcomp)
dim(tracks)

