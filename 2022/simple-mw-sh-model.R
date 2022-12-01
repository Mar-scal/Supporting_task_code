# Here's a quick example of how a MW-SH model and a plot of what it looks like...

# Bring in the Data we provided you
library(tidyverse)
library(sf)
library(mgcv)
mwsh <- read.csv("Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_GB_1982-2009.csv")

# FK:
head(mwsh)
str(mwsh)


# Take a look at your data, this might be slow to render because it is a lot of data
ggplot(mwsh,aes(y=wmw,x=sh)) + geom_point()

# Here's how you can break it out by month
ggplot(mwsh,aes(y=wmw,x=sh)) + geom_point()+ facet_wrap(~month)

# You can also make a 'spatial object' that easily plots the data for you in a projected coordinate system (i.e. R does GIS)
mwsh.sf <- st_as_sf(mwsh,coords = c("X","Y"),remove =F,crs = "4326")
# So here's a spatial plot showing all the points that have been sampled on the bank over the time period in question.  
# Note the clusters.
ggplot(mwsh.sf) + geom_sf()

# Can plot this all sorts of fun ways, here's one to show the year of each tow.
ggplot(mwsh.sf) + geom_sf_text(aes(label = substr(year,3,4)),size=3)

# Do lots more data exploration, look at other variables in the data and other combinations of the data.

# FK: other useful data exploration methods
summary(mwsh)
table(mwsh$year)
table(mwsh$month, mwsh$year)
ggplot() + geom_bar(data=mwsh, aes(month, fill=as.factor(year))) + facet_wrap(~dataset)
ggplot() + geom_sf(data=mwsh.sf, aes(colour=as.factor(month))) + facet_wrap(~dataset)

# FK: make a date column
require(lubridate)
mwsh$date <- ymd(paste0(mwsh$year, "-", mwsh$month, "-01"))
ggplot() + geom_bar(data=mwsh, aes(date, fill=dataset))

ggplot() + geom_bar(data=mwsh, aes(month, fill=dataset)) + facet_wrap(~year)
# FK: you can look at specific years
ggplot() + geom_sf(data=mwsh.sf[mwsh.sf$year==1988:1989,], aes(colour=as.factor(year)))

# Once you understand the data you can start to make simple models...

# FK: this is a linear model y=mx+b --> wmw = m(sh) + b
mod.lm <- lm(wmw~sh,data = mwsh)
# FK: look at the output... m = 0.5492, b = -39.5708
mod.lm
# Can see the relationship is significant... (see the asterisks!)
summary(mod.lm)
# But a linear model doesn't fit the data very well... 
# FK: you can plot this two ways. First, with ggplot:
ggplot(mwsh,aes(y=wmw,x=sh)) + geom_point() + geom_smooth(method = 'lm')
# FK: second, by predicting data using the model
preds <- data.frame(sh=seq(min(mwsh$sh), max(mwsh$sh), by=1))
preds$wmw.pred <- predict(object = mod.lm, newdata=preds)
ggplot() + geom_point(data=mwsh, aes(y=wmw, x=sh)) + 
  geom_smooth(data=mwsh, aes(y=wmw, x=sh), method="lm") + 
  geom_line(data=preds, aes(y=wmw.pred, x=sh), colour="yellow", linetype="dashed")

# This can also be seen in the model residuals which are terrible...

par(mfrow = c(2,2)) # FK: optional (lets you see all 4 figures at once)
plot(mod.lm)
# FK: we want Q-Q line to be straight (1:1) and residuals to be patternless ("starry night")

# Reset the plotting window
par(mfrow = c(1,1))
# So we need a better model... now there are many many ways one could approach this, the most 'traditional' way would be 
# realizing that we have a power law relationship MW = a(SH)^c would be to log transform the MW and SH 
# which if you do the math leads to a linearization of the power law
# log(MW = log(a) + c(log(SH)))
# FK recall equations from your draft! MW = a(SH)^c
mwsh$log.mw <- log10(mwsh$wmw)
mwsh$log.sh <- log10(mwsh$sh)

# Then do a linear regression on these two...
mod.log.lm <- lm(log.mw~log.sh,data = mwsh)
summary(mod.log.lm)
# FK it is significant... but...
# But a linear model doesn't fit the data very well...
ggplot(mwsh,aes(y=log.mw,x=log.sh)) + geom_point() + geom_smooth(method = 'lm')
# This can also be seen in the model residuals which are now way better but still are problematic
par(mfrow = c(2,2))
plot(mod.log.lm)

#dev.off()

# There are many other ways you can develop these models, one more example is using the nls package which avoids the need to transform the data (a good thing!)
# but is much less flexible than something like the lm() above (a sad thing!), for example you need to give it good 'starting values' for your
# parameter estimates.
mod.nls <- nls(wmw ~ a*sh^b, start = list(a=0.00001,b=3),data = mwsh)
# These are similar to the mod.log.lm results but not identical.
res.nls <- summary(mod.nls)
# You can look at the residuals from this model by adding them to the mwsh dataframe (the data are in the same order)
mwsh$resid.nls <- residuals(mod.nls)
# Then you can look at the Residuals to see if they are problematic
# Are they normally distributed? Nope, but does that matter for NLS, is there an assumption that the error is ~ N(0,var)
qqnorm(mwsh$resid.nls)
qqline(mwsh$resid.nls)
# Also can look to see if residuals patterns with the covariate... see that trumpet shape in the residuals, usually is bad news!
ggplot(mwsh, aes(x=sh,y=resid.nls)) +geom_point() + geom_smooth(method = 'gam')
# So another class of model we might want to explore is a Generalized Linear Model which can help when we have data where the variance increases with the mean 
# (i.e. as your prediction from the model increases so does the variance)

mod.glm <- glm(wmw ~ sh, family = Gamma(link = "log"),data = mwsh)
summary(mod.glm)

mwsh$resid.glm <- residuals(mod.glm)
# SO this model does have better residual patterns than the other models explored so far
ggplot(mwsh, aes(x=sh,y=resid.glm)) +geom_point() + geom_smooth(method = 'gam')

# A final option I'll throw out there is a Generalized Additive Model, these are very flexible models
mod.gam <- gam(wmw ~ s(sh),data = mwsh)
summary(mod.gam)

mwsh$resid.gam <- residuals(mod.gam)
# So again we see that increase in variance as SH increases
ggplot(mwsh, aes(x=sh,y=resid.gam)) +geom_point() + geom_smooth(method = 'gam')
# One last thing that's 'fun' is extracting predictions from your models, very handy skill to have... so for example...
pred.dat <- data.frame(sh = 60:150) # This makes a data frame you can use for predictions...
pred.dat$mn.gam <- predict(mod.gam,newdata = pred.dat)
pred.dat$se.gam <- predict(mod.gam,newdata = pred.dat,se.fit=T)$se.fit
# Then you can build Confidence intervals around this
pred.dat$LCI <- pred.dat$mn.gam - 1.96*pred.dat$se.gam 
pred.dat$UCI <- pred.dat$mn.gam + 1.96*pred.dat$se.gam 

# Then you can make a very fancy plot ussing ggplot
ggplot(pred.dat) + geom_line(aes(x=sh,y=mn.gam),color='blue') + 
                   geom_ribbon(aes(x=sh,ymin=LCI,ymax=UCI),color='red',fill='red',alpha=0.2) + # This is in theory the 95% Confidence interval, it is very narrow because we have soo much data and this issue of the variance increasing as the SH gets larger
                   geom_point(data= mwsh, aes(x=sh,y=wmw),size=1,alpha=0.1) + xlab('Shell Height (mm)') + ylab("Meat Weight (grams)") # Here we overlay the data points on our curve.

# Continuing along, here's a simply example of adding month to the above model, getting a 'condition' estimate from the predicted data and predicting condition by month.  

# Convert month to a factor instead of a continuous variable, not necessary but I think a good idea (please ask if you don't know why I think that!)
mwsh$month <- as.factor(mwsh$month)
# Here is a monthly model using a GAM, month included as a factor and no interaction with the mw-sh model
mod.gam.month <- gam(wmw ~ s(sh) + month ,data = mwsh)
summary(mod.gam.month)
# You should check the diagnostics on this model of course...

# So now if we predict on a 100 mm scallop, this can be considered our "condition' term, for each month we are getting the predicted scallop condition at 100 mm.
# We could pick any sh we wanted here, but 100 has historically been the number chosen so we'll stick with that!

pred.dat <- data.frame(sh = 100,month = as.factor(1:12)) # This makes a data frame you can use for predictions...
pred.dat$mn.gam.mon <- predict(mod.gam.month,newdata = pred.dat)
pred.dat$se.gam.mon <- predict(mod.gam.month,newdata = pred.dat,se.fit=T)$se.fit
# Then you can build Confidence intervals around this
pred.dat$LCI <- pred.dat$mn.gam.mon - 1.96*pred.dat$se.gam.mon 
pred.dat$UCI <- pred.dat$mn.gam.mon + 1.96*pred.dat$se.gam.mon 

# And now we can plot condition by month!
ggplot(pred.dat) + geom_point(aes(x=month,y=mn.gam.mon)) + 
  geom_errorbar(aes(x=month,ymin=LCI,ymax=UCI),width=0) + # This is in theory the 95% Confidence interval, it is very narrow because we have soo much data and this issue of the variance increasing as the SH gets larger
  xlab('Month') + ylab("Condition (MW of 100 mm scallop)") + scale_y_continuous(limits=c(10,18))

# FK:
summary(mod.gam.month)
qq.gam(mod.gam.month)
mwsh$resid.gam.month <- residuals(mod.gam.month)
# So again we see that increase in variance as SH increases
ggplot(mwsh, aes(x=sh,y=resid.gam.month)) +geom_point() + geom_smooth(method = 'gam')

