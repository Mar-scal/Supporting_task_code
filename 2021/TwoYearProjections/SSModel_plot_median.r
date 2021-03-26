########### This function was created by DK in 2016, but not documented.  This function produces the plots that are produced in the SSModel package, but instead
########### it used the median rather than the mean.  At some point this should probably be folded into the SSModel package.  Generally this is used to produce
##########  the model summary figures...
## Updates
##  Nov 2017:  DK updated to add some more functionality to the model.

###############################################################################################################
# Arguments
#1: x:                The model summary from the SSModel call, this is looking for the sims.matrix data from within the data.
#2: type:             There are several plot options here.
#     a:              "Model.est".  This is the default and produces the "biomass" plot of commercial and recruits
#     b:              "Survey.est"  This produces the survey time series for commercial and recruits
#     c:              "Prior.Post"  This produces the prior-posterior plots for a subset of the parameters.  You can't change which parameters you plot at the moment.
#     d:              "Exploit"     This produces the time series of exploitation and natural mortality.
#3: cred.lim:         This says what credible limits to show on the figure, Default = 0.05 = 95% BCI (i.e. 0.05/2 is used)
#4: ref.pts:          The reference points for the area, expects c(LRP, USR).  Default = NULL.
#5: log.R:            How do you want to show the recruits in the figure.  Three options, default is log.R=T
#     a:              NULL doesn't make a recruit plot
#     b:              T makes the plot on log scale, makes it easier to see when large recruit pulses exist, generally not an ideal way to show trends 
#                     for industry presentations though.
#     c:              F makes the plot on typical scale, can mask trends when there is 1 year with very high recruit numbers.
#6: Catch.next.year:  The interim TAC for next year.  Default = NULL
#7: pred.lim:         The range given for the prediction boxplot
#8: g:                Growth parameter for commercial sized scallop used in the the prediction of the biomass next year.  Only used when type = "Model.est".  
#                     Default = Null which triggers a warning and sets g = 1.15
#9: gR:               Growth parameter for recruit sized scallop used in the the prediction of the biomass next year.  Only used when type = "Model.est"
#                     Default = Null which triggers a warning and sets g = 1.7
#10:RP.labels         The location you want to put the labels for the reference points in terms of of the x axis + the size of the label.
#                     The default is NULL which doesn't add them.  
#                     The format for these is data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2010,2010,2010),size = c(1,1,1))
#                     This makes a data from with the RP region, where you'd like them on the x-axis, and what size to make them (using cex)
#                     They are only created if ref.pts is not NULL
#11: cex              Size of the text in the document (other than the RP.labels), default = 1
#12: BM_scale          For biomass related plots do you want the biomass in t (tonnes) or Kt (Kilo-tonnes).  Default = t

# Start the function
SSModel.plot.median <- function (x, type = "Model.est", cred.lim = 0.05, ref.pts = NULL, log.R = T, cex = 1,
          Catch.next.year = NULL, pred.lim = NULL, g=NULL,gR = NULL, RP.labels = NULL,BM_scale = "t", proj.years=1, proj.obj = NULL,...) 
{
  
  old.par <- par(no.readonly = TRUE)
  # if we haven't called for one of these plot exit the function
  if (any(type == c("Model.est", "Prior.Post", "Exploit", "Survey.est")) == 
      FALSE) return("Not a valid plot type")
  
  # First up the Posterior plots.
  if (type == "Prior.Post") 
  {
    # These are the priors/posteriors that we'll plot
    prior.nodes <- c("K", "q", "S", "sigma", "kappa.tau")
    # Grab the appropriate posteriors from the model sims.matrix.
    node.pos <- is.element(dimnames(x$sims.matrix)[[2]], prior.nodes)
    # These gets the number of nodes
    node.num <- sum(node.pos)
    # Now pull out the posteriors we want...
    post.matrix <- x$sims.matrix[, node.pos]
    # Set up the plot window.
    par(mfrow = c(ceiling(node.num/2), 2), mar = c(2, 2, 
                                                   0, 0), omi = c(0.5, 0.75, 0.5, 1), plt = c(0.2, 0.9, 
                                                                                              0.25, 0.9))
    # Now using the SSModle posterior plot make the histogram   with prior for each parameter of interest.
    # Note that we have the prior's hardcoded in here, if we ever changed the model priors these would need adjusted.
    SSModel:::plot.postprior(post.matrix[, "K"], SSModel:::Prior.LN(seq(1, 5000, 
                                                    by = 10), x$priors$logK.a, 1/sqrt(x$priors$logK.b)), 
                   0.1, 5000, "K")
    SSModel:::plot.postprior(post.matrix[, "q"], SSModel:::Prior.Beta(seq(0.1, 
                                                      1, by = 0.01), x$priors$q.a, x$priors$q.b), 10, 1, 
                   "q")
    SSModel:::plot.postprior(post.matrix[, "S"], SSModel:::Prior.Unif(seq(x$priors$S.a, 
                                                      x$priors$S.b, by = 0.01), x$priors$S.a, x$priors$S.b), 
                   10, 1, "S")
    SSModel:::plot.postprior(post.matrix[, "sigma"], SSModel:::Prior.Unif(seq(0, 
                                                          10, by = 0.1), 0, 10), 3, 10, expression(sigma))
    SSModel:::plot.postprior(post.matrix[, "kappa.tau"], SSModel:::Prior.Unif(seq(0, 
                                                              10, by = 0.1), 0, 10), x$priors$kappa.tau.a, x$priors$kappa.tau.b, 
                   expression(kappa))
    # Once done return the old plot parameters.
    on.exit(par(old.par))
    return()
  } # end  if (type == "Prior.Post") 
  
  # Next function is the exploitation time series.
  if (type == "Exploit") 
  {
    # Set up the plot window
    par(mar = c(5, 4, 4, 5) + 0.1)
    # Grab the exploitation rate
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 3), "mu[")]
    # Plot the data, note that we offset the data here, the first year of data we 
    # don't have explotation.  Also, we don't plot anything here, just set up the plot frame
    plot(x$Years, c(NA, apply(post.matrix, 2, median)), type = "n", 
         pch = 1, ylim = c(0, 1), ylab = "Exploitation rate", 
         las = 1, cex.axis = cex)
    # Here we add the exploitation rate, again notices the stick handling for the plot, the 
    # mu for 2017 in the model frame is plotted as the 2016 mu.
    lines(x$Years[-1], apply(post.matrix, 2, median), type = "b", 
          pch = 16)
    # Calculate and plot the upper and lower credible limits
    bounds <- apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 1 - cred.lim/2))
    lines(x$Years[-1], bounds[1, ], lty = 2)
    lines(x$Years[-1], bounds[2, ], lty = 2)
    # Now we add in the natural mortality (which we show as a survival term)
    par(new = T)
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 2), "m[")]
    # No offset here, the model year Natural mortaility is the same as the year shown here.
    plot(x$Years, apply(exp(-post.matrix), 2, median), type = "n", 
         pch = 1, ylim = c(0, 1), xlab = "", ylab = "", las = 1, 
         axes = F)
    lines(x$Years, apply(exp(-post.matrix), 2, median), type = "b", 
          pch = 16, col = "gray")
    # Add in the credible limits.
    bounds <- (apply(exp(-post.matrix), 2, quantile, probs = c(cred.lim/2, 1 - cred.lim/2)))
    lines(x$Years, bounds[1, ], lty = 2, col = "gray")
    lines(x$Years, bounds[2, ], lty = 4, col = "gray")
    # Add the axes...
    axis(side = 4, las = 1, cex.axis = cex)
    mtext(side = 4, line = 2.2, "Survival rate")
    return()
  } # end if (type == "Exploit") 
  
  # Here is the figure to produce the Biomass and Recruitment summary.
  if (type == "Model.est") 
  {
    # Print a warning and set the value of g to be 1.15
    if(is.null(g))  
    {
      g <- 1.15
      cat("Heads up dude!!, since you didn't provide a growth paramter value you are using commercial growth parameter of g = 1.15 for model predictions \n")
    }# end if(is.null(g))  
    
    # Print a warning and set the value of gR to be 1.7
    if(is.null(gR))
    {
      gR <- 1.7 
      cat("Heads up dude!!, since you didn't provide a recruit growth paramter value you are using for your recruits of gR = 1.7 for model predictions \n")
    } #end if(is.null(gR))  
    # Set up the figre parameters
    
    # Set up the plot window depending on whether you are plotting the Recruits or not.
    if(!is.null(log.R)) par(mfrow = c(2, 1), mar = c(2, 2, 0, 0), omi = c(0.95, 0.95, 0.1, 0.1), plt = c(0.05, 0.95, 0.025, 1))
    if(is.null(log.R)) par(mfrow = c(1, 1), mar = c(2, 2, 0, 0), omi = c(0.95, 0.95, 0.1, 0.1), plt = c(0.05, 0.95, 0.025, 1))
    # Get the biomass posteriors for every year
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 1, 2), "B[")]
    # And then grab the credbile limets.
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))
    if(BM_scale == "Kt") bounds <- bounds/1000
    # Set up the y boundaries.
    ylim <- c(0, max(bounds[2, ]))
    
    #are we doing a 1-year or 2-year projection? If 2 years, we need to make sure the axis accommodates it.
    if(proj.years>1)  Years <- c(x$Years, max(x$Years)+proj.years)
    if(proj.years==1) Years <- x$Years
    
    # If we don't provide the catch for next year we 
    if (is.null(Catch.next.year)) 
    {
      # Make the plot using the median biomass for each year
      BM.med <- apply(post.matrix, 2, median)
      if(BM_scale == "Kt") BM.med <- BM.med/1000
      plot(x$Years, BM.med, type = "b", 
           pch = 16, ylim = ylim, xlab = "", ylab = "", 
           las = 1, cex.lab = cex, cex.axis = cex, xaxt = "n")
      if(is.null(log.R)) axis(1,cex.axis=cex) # If not plotting the recruits add the x-axis
      # Add label to axis + the credible limits
      if(BM_scale == "Kt") mtext(text = "Commercial biomass (meats,Kt)", side = 2, cex = cex, line = 4)
      if(BM_scale == "t") mtext(text = "Commercial biomass (meats,tonnes)", side = 2, cex = cex, line = 4)
      lines(x$Years, bounds[1, ], lty = 2)
      lines(x$Years, bounds[2, ], lty = 2)
      # If the reference points are provided we color in the appropriate part of the figure.
      if (!is.null(ref.pts)) 
      {
        # Draw lines at the LRP and USR values
        if(BM_scale == "Kt") ref.pts <- ref.pts/1000
        abline(h = ref.pts[1], col = rgb(1, 0, 0, 0.2))
        abline(h = ref.pts[2], col = rgb(1, 1, 0, 0.2))
        # Color code the respective piecies of the figure.
        rect(min(x$Years) - 5, -ylim[2]/5, max(x$Years) + 
               5, ref.pts[1], border = NA, col = rgb(1, 
                                                          0, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[1], max(x$Years) + 
               5, ref.pts[2], border = NA, col = rgb(1, 
                                                          1, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[2], max(x$Years) + 
               5, ylim[2] * 1.2, border = NA, col = rgb(0, 
                                                        1, 0, 0.2))
        # Now add the Ref Point label to the figure....
        text(x=RP.labels$x.pos[RP.labels$region == "Healthy"],y=0.9*ylim[2],labels="HEALTHY",cex = RP.labels$size[RP.labels$region == "Healthy"],col='chartreuse2')
        text(x=RP.labels$x.pos[RP.labels$region == "Cautious"],y=ref.pts[1]*1.01,labels = "CAUTIOUS",cex=RP.labels$size[RP.labels$region == "Cautious"],col='goldenrod1')
        text(x=RP.labels$x.pos[RP.labels$region == "Critical"],y=ref.pts[1]*0.1,labels = "CRITICAL",cex=RP.labels$size[RP.labels$region == "Critical"],col='firebrick1')
      } # end if (!is.null(ref.pts)) 
    } # end if (is.null(Catch.next.year)) 
    # If we supply the catch for next year we need to add in our projection for next year.
    else 
    {
      Biomass.next.year <- predict(x, Catch.next.year, g.parm = g, gr.parm = gR)$B.next
      if(BM_scale == "Kt") Biomass.next.year <- Biomass.next.year/1000
      if (ylim[2] < max(Biomass.next.year)) 
      {
        if (is.null(pred.lim)) ylim[2] <- max(pretty(Biomass.next.year))
        else ylim[2] <- max(ylim[2],max(pretty(quantile(Biomass.next.year, 
                                            probs = c(pred.lim/2, 1 - pred.lim/2)))))
      } # end if (ylim[2] < max(Biomass.next.year)) 
      BM.med <- apply(post.matrix, 2, median)
      if(BM_scale == "Kt") BM.med <- BM.med/1000
      if(proj.years==1) {
        plot(x$Years, BM.med , type = "b", 
           pch = 16, ylim = ylim, xlim = c(min(x$Years), 
                                           max(x$Years) + 1), ylab = "", 
           las = 1, cex.lab = cex, cex.axis = cex, xaxt = "n")
      }
      if(proj.years>1) {
        plot(x$Years, BM.med , type = "b", 
             pch = 16, ylim = ylim, xlim = c(min(Years), 
                                             max(Years)), ylab = "", 
             las = 1, cex.lab = cex, cex.axis = cex, xaxt = "n")
      }
      if(is.null(log.R)) axis(1,cex.axis=cex) # If not plotting the recruits add the x-axis
      if(BM_scale == "Kt") mtext(text = "Commercial biomass (meats,Kt)", side = 2, cex = cex, line = 4)
      if(BM_scale == "t") mtext(text = "Commercial biomass (meats,tonnes)", side = 2, cex = cex, line = 4)
      if (is.null(pred.lim)) 
        boxplot(Biomass.next.year, add = T, at = max(x$Years) + 
                  1, range = 0, yaxt = "n", xaxt = "n", xlab = "")
      else 
      {
        temp.box <- boxplot(Biomass.next.year, plot = F)
        temp.box$stats[c(1, 5)] <- quantile(Biomass.next.year, 
                                            probs = c(pred.lim/2, 1 - pred.lim/2))
        temp.box$out <- temp.box$group <- numeric(0)
        temp.box$names <- ""
        bxp(temp.box, add = T, at = max(x$Years) + 1, 
            range = 0, yaxt = "n", xaxt = "n")
      } # end second else
      
      if(!is.null(proj.obj) & proj.years==2) boxplot(proj.obj, add = T, at=max(x$Years) + 2, range=0, yaxt = "n", xaxt = "n", xlab = "", col = "transparent")
      
      abline(h = ref.pts[1], col = "red")
      abline(h = ref.pts[2], col = "Orange")
      lines(x$Years, bounds[1, ], lty = 2)
      lines(x$Years, bounds[2, ], lty = 2)
      # If we are provided the reference points we color code things, see above for details...
      if (!is.null(ref.pts)) 
      {
        if(BM_scale == "Kt") ref.pts <- ref.pts/1000
        abline(h = ref.pts[1], col = rgb(1, 0, 0, 0.2))
        abline(h = ref.pts[2], col = rgb(1, 1, 0, 0.2))
        rect(min(x$Years) - 5, -ylim[2]/5, max(x$Years) + 
               5, ref.pts[1], border = NA, col = rgb(1, 
                                                          0, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[1], max(x$Years) + 
               5, ref.pts[2], border = NA, col = rgb(1, 
                                                          1, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[2], max(x$Years) + 
               5, ylim[2] * 1.2, border = NA, col = rgb(0, 
                                                        1, 0, 0.2))
        # Now add the Ref Point label to the figure....
        text(x=RP.labels$x.pos[RP.labels$region == "Healthy"],y=0.99*ylim[2],labels="HEALTHY",cex = RP.labels$size[RP.labels$region == "Healthy"],col='chartreuse2')
        text(x=RP.labels$x.pos[RP.labels$region == "Cautious"],y=ref.pts[1]*1.2,labels = "CAUTIOUS",cex=RP.labels$size[RP.labels$region == "Cautious"],col='goldenrod1')
        text(x=RP.labels$x.pos[RP.labels$region == "Critical"],y=ref.pts[1]*0.1,labels = "CRITICAL",cex=RP.labels$size[RP.labels$region == "Critical"],col='firebrick1')
      } # end if (!is.null(ref.pts)) 
    } # end the else statement that tells us we have specificed next years catch.
  
    # Now we add in the recruits, but only if log.r is set to T/F, if it is NULL we don't make this figure
    if(!is.null(log.R))
    {
      post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                       1, 2), "R[")]
      bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                           1 - cred.lim/2)))
      if(BM_scale == "Kt") bounds <- bounds/1000
      ylim <- c(min(bounds[1, ]), max((bounds[2, ])))
      if (!is.null(Catch.next.year)) 
        xlim.r <- c(min(x$Years), max(x$Years) + 1)
      else xlim.r <- range(x$Years)
      BM.med <- apply(post.matrix, 2, median)
      if(BM_scale == "Kt") BM.med <- BM.med/1000
      if(log.R == T)
      {
        plot(x$Years,BM.med, type = "b", 
           pch = 16, ylim = ylim, xlim = xlim.r, xlab = "", log="y",
           ylab = "", las = 1, cex.lab = cex, cex.axis = cex)
      }
      if(log.R == F)
      {
        plot(x$Years, BM.med, type = "b", 
             pch = 16, ylim = ylim, xlim = xlim.r, xlab = "",
             ylab = "", las = 1, cex.lab = cex, cex.axis = cex)
      }
      if(BM_scale == "Kt") mtext(text = "Recruit biomass (meats,Kt)", side = 2, cex = cex, line = 4)
      if(BM_scale == "t") mtext(text = "Recruit biomass (meats,tonnes)", side = 2, cex = cex, line = 4)
      lines(x$Years, bounds[1, ], lty = 2)
      lines(x$Years, bounds[2, ], lty = 2)
    } # end if(!is.null(log.r))
    mtext("Year", side = 1, line = 2, outer = FALSE, cex = cex)

    on.exit(par(old.par))
    return()
  }
  
  
  if (type == "Survey.est") {
    par(mfrow = c(2, 1), mar = c(2, 2, 0, 0), omi = c(0.95, 
                                                      0.95, 0.1, 0.1), plt = c(0.05, 0.95, 0.025, 1))
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 5), "Irep[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))
    if(BM_scale == "Kt") bounds <- bounds/1000
    ylim <- c(0, max(pretty(bounds[2, ])))
    BM.med <- apply(post.matrix, 2, median)
    if(BM_scale == "Kt") BM.med <- BM.med/1000
    plot(x$Years, BM.med, type = "l", 
         pch = 16, ylim = ylim, xlab = "", ylab = "", las = 1, 
         cex.lab = cex, cex.axis = cex, xaxt = "n")
    if(BM_scale == "t")  mtext(text = "Commercial biomass (meats,tonnes)", side = 2, cex = cex, line = 4)
    if(BM_scale == "Kt") mtext(text = "Commercial biomass (meats,Kt)", side = 2, cex = cex, line = 4)
    BM.surv <-x$data$I
    if(BM_scale == "Kt") BM.surv <- BM.surv/1000
    points(x$Years, BM.surv, col = "red", pch = 16)
    lines(x$Years, bounds[1, ], lty = 2)
    lines(x$Years, bounds[2, ], lty = 2)
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 6), "IRrep[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))
    if(BM_scale == "Kt") bounds <- bounds/1000
    ylim <- c(0, max(pretty(bounds[2, ])))
    BM.med <- apply(post.matrix, 2, median)
    if(BM_scale == "Kt") BM.med <- BM.med/1000
    plot(x$Years,BM.med , type = "l", 
         pch = 16, ylim = ylim, xlab = "", ylab = "", las = 1, 
         cex.lab = cex, cex.axis = cex)
    if(BM_scale == "Kt") mtext(text = "Recruit biomass (meats,Kt)", side = 2, cex = cex, line = 4)
    if(BM_scale == "t") mtext(text = "Recruit biomass (meats,tonnes)", side = 2, cex = cex, line = 4)
    BM.surv <-x$data$IR
    if(BM_scale == "Kt") BM.surv <- BM.surv/1000
    points(x$Years, BM.surv, col = "red", pch = 16)
    lines(x$Years, bounds[1, ], lty = 2)
    lines(x$Years, bounds[2, ], lty = 2)
    mtext("Year", side = 1, line = 2, outer = FALSE, cex = 0.8)
    on.exit(par(old.par))
    return()
  }
}