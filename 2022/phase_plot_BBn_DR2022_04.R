# DR2022_4 for Ginette to work on BBn RPs for MSC purposes


load("./Offshore/Final_model_results_BBn.RData")

# model output 
model.dat <- mod.out$BBn$BUGSoutput$median  #note will use the medians for population parameters 

#raw data
raw.dat <- mod.dat$BBn   #use g,  gR; note: g2 and gR2 only come into play for the prediction evaltuation figures (g2 and gR2 are 'realized' growth).

## Define year range from survey 
YearSurvey <- seq(1991,2019, by=1) #BBn 1991 to 2019 

#NOTE - are assuming B or R do not need to be scaled by K
B.dat <- model.dat$B
R.dat <- model.dat$R
m.dat <- model.dat$m  # this is instantaneous rate #NOTE B(2019) = m(2019) * g(2018) * (B(2018) - Catch(2019)). In words we remove 2019 catch from 2018 survey biomass estimate then grow using 2018 growth and kill that off using 2019 natural mortality.  That 2019 m, is tied to the clapper equation which uses that weighted average of 2018 and 2019 clapper numbers, the m input information in 2019 has information from 2018 and 2019.  
mR.dat <- model.dat$mR  # this is instantaneous rate from RECRUITED scallops

mu.dat <- model.dat$mu  #exploitation calculated as Catch(2019) / (Biomass(2019) + Catch(2019)) so you can get an estimate every year  - assuming you don't need to shift it. 
#mu <- rbind(mu, data.frame(mu.mean=NA,  mu.median = NA))
C <- raw.dat$catch[raw.dat$year >= 1991] #BBN 1992 Catch would be catch from June 1991 - May 1992
g <- raw.dat$g[raw.dat$year >= 1991] 
gR <- raw.dat$gR[raw.dat$year >= 1991] 

length(YearSurvey)
length(B.dat)
length(R.dat)
length(m.dat)
length(mR.dat)
length(mu.dat)  
length(C)
length(g)
length(gR)

#NOte inshore BoF mu is calculated different from offshore; e.g. from SSModel -- mu[t] <- C[t]/(B[t + 1] + C[t])
#since calculate biomass difference from year t to t+1 and associate that difference with t in the dataframe, C[t] should match up with [t] in dataframe for BoF. 

#Merge data  
dat <- as.data.frame(cbind(YearSurvey=YearSurvey,  B = B.dat, R = R.dat, m = m.dat, mR = mR.dat, g=g, gR=gR, mu = mu.dat, C = C ))

#Survival of commerical biomass 
dat$survival.rate <- exp(-dat$m) 
#Survival of recruit biomass 
dat$survival.rate.recruits <- exp(-dat$mR) 

#convert natural mortality rate to proportion so can combine with exploitation
dat$Mprop <- 1-exp(-dat$m)
dat$MR_prop <- 1-exp(-dat$mR)

#total mortality 
dat$Z <- (dat$Mprop + dat$mu)  #assuming here that m and mu line up correctly for each year  - which I think they do 
summary(dat$Z )


#Percent change in commercial biomass
#Offset/match up change in biomass from year to year 
dat$BiomassTplus1 <- NA
dat$BiomassTplus1[1:(dim(dat)[1]-1)]  <- dat$B[2:dim(dat)[1]]
dat$ChangeinBiomass_TtoTplus1 <- dat$BiomassTplus1 - dat$B
dat$PercentChangeinBiomass_TtoTplus1 <- 100*(dat$ChangeinBiomass_TtoTplus1/dat$B) #note this is the change from 2014 to 2015 lined up with YearSurvey 2014; it's lined up also with catch which recall is the Sept 2013 to Aug 2014. 

# To get actual surplus production need to account for catch as part of that surplus.. SPt = Bt+1 - Bt + Ct  (see Walters et al 2008 CJFAS)
## Surplus Production Calc 
# Net change in total biomass of population in mt - this is Surplus Production in mt 
#for first row of dat - have B[1987]-B[1986] + C[1986]  where C[1986] is from Sept 1985 to Aug 1986 -- so this catch isn't lined up for how we want to do the SP calculation 
#Need B[1987]-B[1986] + C  - where C in this case would be the catch that come between 1986 and 1987 - which is labelled C[1987].. 
dat$C2 <- NA
dat$C2[1:(dim(dat)[1]-1)]  <- dat$C[2:dim(dat)[1]]
dat$net.pop.B.change.mt <- dat$ChangeinBiomass_TtoTplus1 + dat$C2  

#Net change in total biomass of population in %
dat$SP.percent <- (dat$net.pop.B.change.mt / dat$B)*100 
#Time series summary
summary(dat$SP.percent )
#SP in last 3 year summary: 
summary(dat$SP.percent[dat$YearSurvey >  (max(dat$YearSurvey)-3)] )
#SP in last 5 year summary: 
summary(dat$SP.percent[dat$YearSurvey >  (max(dat$YearSurvey)-5)] )


# Surplus Production Rate (surplus production divided by biomass in first year B[t]) 
dat$SP.rate <- dat$net.pop.B.change.mt/dat$B

dat$AREA <- "BBn"


#Note I don't know actual PA number so just testing these for now.. 
LRP <- 0.3*mean(dat$B[dat$YearSurvey>=1991&dat$YearSurvey<=2010]) #From 2012 res doc - proposal was 30% mean biomass from 1991-2010; 
USR <- 0.8*mean(dat$B[dat$YearSurvey>=1991&dat$YearSurvey<=2010]) #From 2012 res doc - proposal was 80% mean biomass from 1991-2010; 
#rr <- 0.1  #Removal reference From 2012 res doc - defined as exploitation level that results in no change in biomass - was 0.1 see Fig 6 
rr <- 0.15 #from below analysis 

dat.rr <- data.frame(biomass=seq(USR,(max(dat$B)+100),by=100), rr = rep(rr,length(seq(USR,(max(dat$B)+100),by=100)))) 

png(paste0("figures/BB/phase.plot.PA_",area,".png"),width=8,height=8,units = "in",res=920)

ggplot(data= dat) + 
  annotate(geom="rect",xmin=-Inf,xmax=LRP,ymin=-Inf,ymax=Inf, fill = "red", alpha=0.5)+
  annotate(geom="rect",xmin=LRP,xmax=USR,ymin=-Inf,ymax=Inf, fill="gold1", alpha = 0.5) +
  annotate(geom="rect",xmin=USR,xmax=Inf,ymin=-Inf,ymax=Inf, fill="green", alpha = 0.6) +
  geom_text(aes(y= mu, label=YearSurvey, x= B )) + 
  geom_path( aes(y= mu, x= B )) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Biomass (mt)") + ylab ("Exploitation") + 
  geom_line(data = dat.rr, aes(x=biomass, y=rr) , linetype="dashed",   color = "blue", size=0.5) +
  scale_x_continuous(breaks = seq(0,signif(max(dat$B),digits=2),by=5000),limits = c(700,NA)) +
  scale_y_continuous(breaks = seq(0,1,by=0.05),limits = c(0,ceiling(max(dat$mu)*10)/10)) 

dev.off()


phase <- data.frame(Year = dat$YearSurvey, Exploitation=dat$mu, Biomass=dat$B)

require(openxlsx)
write.xlsx(x=phase, file = "phase_plot_BBn.xlsx")

