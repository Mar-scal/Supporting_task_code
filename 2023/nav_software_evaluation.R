
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

lengthcomp_long$name2 <- lengthcomp_long$name
levels(lengthcomp_long$name2) <- c("Olex (longueur brute)", "Olex (correction appliquée)", "Oceanvision (méthode actuelle)")
png("Y:/Offshore/Assessment/2023/Supporting_tasks/Olex_transition_boxplot_fr.png", width=6, height=4, res=400, units="in")
ggplot() + geom_boxplot(data=lengthcomp_long, aes(x= name2, y=as.numeric(value))) +
  xlab("Méthode d'enregistrer") +
  ylab("Longueur de trait (m)") +
  theme_bw()
dev.off()

lengthcomp_ov <- pivot_longer(lengthcomp, cols=c("length_olex", "length_olex_corr"))
lengthcomp_ov$name <- as.factor(lengthcomp_ov$name)
levels(lengthcomp_ov$name) <- c("Olex (raw)", "Olex (correction applied)")

require(ggrepel)
labs <- data.frame(x=925, y=c(925, 962), lab=c("Olex (correction applied)", "Olex (raw)"))
png("Y:/Offshore/Assessment/2023/Supporting_tasks/Olex_transition_linear.png", width=5, height=5, res=400, units="in")
ggplot() + geom_point(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), alpha=0.5) +
  geom_smooth(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), method="lm") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_text_repel(data=labs, aes(x, y, label=lab), box.padding=5, min.segment.length = 1, arrow = arrow(length = unit(0.2, units = "in")))+
  theme_bw() + 
  xlab("Tow length from Oceanvision (m)") +
  ylab("Tow length from Olex (m)") +
  xlim(775, 1150) +
  ylim(775, 1150)+
  scale_colour_manual(values=c("black", "darkorange"), name=NULL, guide="none")+ 
  theme(legend.position=c(0.8, 0.2), legend.background=element_rect(colour="black", fill="transparent"))
dev.off()

labs <- data.frame(x=925, y=c(925, 962), lab=c("Olex (correction appliquée)", "Olex (longueur brute)"))
png("Y:/Offshore/Assessment/2023/Supporting_tasks/Olex_transition_linear_fr.png", width=5, height=5, res=400, units="in")
ggplot() + geom_point(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), alpha=0.5) +
  geom_smooth(data=lengthcomp_ov, aes(as.numeric(length_ovd), as.numeric(value), colour=name), method="lm") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_text_repel(data=labs, aes(x, y, label=lab), nudge_x=c(1, -1), box.padding=3, min.segment.length = 1, arrow = arrow(length = unit(0.2, units = "in")))+
  theme_bw() + 
  xlab("Longueur de trait d'Oceanvision (m)") +
  ylab("Longueur de trait d'Olex (m)") +
  xlim(775, 1150) +
  ylim(775, 1150)+
  scale_colour_manual(values=c("black", "darkorange"), name=NULL, guide="none")+ 
  theme(legend.position=c(0.8, 0.2), legend.background=element_rect(colour="black", fill="transparent"))
dev.off()

dim(lengthcomp)
head(lengthcomp)
dim(tracks)

