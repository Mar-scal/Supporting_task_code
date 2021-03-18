# This function performs 2 year projections by looping through the projection process twice; recycling the one year projection and using it for a second year's projection. 
# object: model Rdata object (e.g. Spa4.2019 or model.obj)
# surplus: surplus production value as a proportion (0, 0.15, 0.25 etc). If NULL (default) the normal model will run (no assumptions made.)

process_2y_proj <- function(object, area, mu=c(NA, NA), surplus=NULL, lastyear=F, decisiontable=T, LRP, USR){
  
  if(lastyear==F) year.start <- length(object$Years)-9
  if(lastyear==T) year.start <- length(object$Years)
  
  B.next0 <- NULL
  B.next1 <- NULL
  B.next2 <- NULL
  
  for(i in year.start:length(object$Years)) {
    
    pB0 <- B.change <- mu.p <- B.p <- vector(mode = "list", 
                                             length = length(object$labels))
    names(B.p) <- names(mu.p) <- names(B.change) <- names(pB0) <- object$labels
    H <- object$data$H
    test.col <- array("", c(7, H))
    for (h in 1:H) {
      test.col[1, h] <- paste("Bh[", i, ",", 
                              h, "]", sep = "")
      test.col[2, h] <- paste("BBh[", i - 
                                1, ",", h, "]", sep = "")
      test.col[3, h] <- paste("Ph[", i, ",", 
                              h, "]", sep = "")
      test.col[4, h] <- paste("Rh[", i, ",", 
                              h, "]", sep = "")
      test.col[5, h] <- paste("m[", i, ",", 
                              h, "]", sep = "")
      test.col[6, h] <- paste("Kh[", h, "]", sep = "")
      test.col[7, h] <- paste("sigma[", h, "]", 
                              sep = "")
    }
    Bh <- object$sims.matrix[, test.col[1, ]]
    BBh <- object$sims.matrix[, test.col[2, ]]
    Ph <- object$sims.matrix[, test.col[3, ]]
    Rh <- object$sims.matrix[, test.col[4, ]]
    m <- object$sims.matrix[, test.col[5, ]]
    Kh <- object$sims.matrix[, test.col[6, ]]
    sigma <- object$sims.matrix[, test.col[7, ]]
    
    m.avg = 5
    if (m.avg > 1) {
      for (j in 1:(m.avg - 1)) {
        for (h in 1:H) {
          test.col[5, h] <- paste("m[", object$data$N - 
                                    j, ",", h, "]", sep = "")
        }
        m <- m + object$sims.matrix[, test.col[5, ]]
      }
      m <- m/m.avg
    }
    
    Catchm <- matrix(NA, dim(Bh)[1], dim(Bh)[2])
    Bptot <- rep(0, dim(Bh)[1])
    SDM.num <- length(object$labels)
    mn.Bh <- apply(Bh + Rh * Kh, 2, mean)
    
    
    Proj.exploit <- function (Bh, Dh, exploit, e.parms, area) 
    {
      SDM.num <- length(Bh)
      if (exploit != 0) {
        FI <- e.parms[1:SDM.num] + e.parms[SDM.num + 1] * Dh/sum(Bh)
        tot.eff <- (e.parms[SDM.num + 2] * exploit)/FI[SDM.num]
        exploit <- tot.eff * FI/e.parms[SDM.num + 2]
        return(list(exploit = exploit, Effort = tot.eff))
      }
      else return(list(exploit = rep(exploit, SDM.num), Effort = 0))
    }
    
    
    c.exploit <- list(exploit = sweep(sweep(sweep(Bh, 
                                                  2, object$data$Area, "/")/apply(Bh, 1, sum), 
                                            1, object$e.parms[H + 1], "*"), 2, object$e.parms[1:H], 
                                      "+"))
    PCatch <- mn.Bh * c.exploit$exploit
    
    for(h in 1:H){
      ############### current year ##################################
      totalcatch <- object$data$C[i-1]
      PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                     "/")/Kh
      catch <- PCatch.h[,h] * Kh[,h]
      
      B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = Bh[,h], 
                            year =  object$Years[i], 
                            catch = catch,
                            totalcatch = object$data$C[i-1], 
                            mu = NA,
                            proj=0,
                            B.change=NA,
                            pB0_increase=NA,
                            strata=object$labels[h],
                            modelyear = object$Years[i])
      
      
      ############## 1 y projection ############################
      # calculate the standardized Biomass (P) for y 1
      # catch here should be the catch for the 1y projection
      # Process model for offshore:
      # Pmed[t] <- log(max(exp(-m[t]) * g[t-1] * (P[t-1] - C[t] / K) + exp(-mR[t])*gR[t-1] * r[t-1], 0.001))
      # so for 1 year projection, index each one by +1
      # here's the line from projections.r:
      # year 1 projection.  First the process equation.
      # Pmed.p[[i]]<- log(exp(-d$m[,d$NY])*(d$g[d$NY])*(d$P[,d$NY]-C.p[i]/d$K)+exp(-d$mR[,d$NY])*(d$gR[d$NY])*d$r[,d$NY])
      # mu[t] <- C[t]/(B[t]+C[t])
      # # Instantaneous fishing mortality
      # Fmort[t] <- -log(max(1 - mu[t], 0.0001))	
      
      if(is.na(mu[1])){
        
        totalcatch <- object$data$C[i]
        PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                       "/")/Kh
        catch <- PCatch.h[,h] * Kh[,h]
        
        # process equation for 29W
        Pmed.p <- exp(-m[,h]) * (object$data$g[i,h] * (Ph[,h] + Rh[,h]))
        # remove catch
        #Pcatch <- Pmed.p * exploit
        Pmed <- Pmed.p - PCatch.h[,h]
        
        Pmed[Pmed < 1e-05] <- 1e-04
        Pmed <- log(Pmed)
        Max.P <- 6
        P.out <- SSModeltest:::gen.lnorm(Pmed, sigma[,h], Max.P)
        
        
        B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = P.out * Kh[,h], 
                              year =  object$Years[i]+1, 
                              catch = catch, 
                              totalcatch=totalcatch,
                              mu = mu[1], #catch/((P.out*Kh[,h])+ catch),
                              proj=1,
                              strata=object$labels[h],
                              modelyear = object$Years[i])
      }
      
      # For offshore, don't remove removals using mu. Use CATCH! This is just here for kicks.... 
      if(!is.na(mu[1])){
        
        # only need this if specifying mu in arguments
        mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                   mu[1], object$e.parms, object$data$Area)
        
        # instantaneous vs. finite mortality
        # proportion survived = exp(-mu)
        # process equation for 29W
        Pmed <- exp(-m[,h]) * (object$data$g[i,h] * (Ph[,h] + Rh[,h]))
        PCatch.h <- Pmed * mu.exploit$exploit[h]
        Pmed <- Pmed - PCatch.h
        catch <- PCatch.h * Kh[,h]
        
        
        Pmed[Pmed < 1e-05] <- 1e-04
        Pmed <- log(Pmed)
        Max.P <- 6
        set.seed(1)
        P.out <- SSModeltest:::gen.lnorm(Pmed, sigma[,h], Max.P)
        
        B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = P.out * Kh[,h] * (1-mu[1]),
                              year =  object$Years[i]+1,
                              catch = catch,
                              totalcatch = NA,
                              mu = mu[1],
                              proj=1,
                              strata=object$labels[h],
                              modelyear = object$Years[i])
      }
      
      
      # for decision table
      # This is the projected biomass change (%) from this year to next.  > 0 = increase.
      B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$B.change <- (B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass  - B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass) / B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass * 100
      # Probability of biomass decline.  What runs are less than 0.
      B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$pB0_increase <- 0 < (B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass-B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass)
      
      
      ################## now for 2y projection. #################
      
      # if no surplus value supplied:
      if(is.null(surplus)) {
        # include K for the catch option (scale catch by carrying capacity?)
        if(is.na(mu[2])){
          
          totalcatch <- object$data$C[i+1]
          PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                         "/")/Kh
          catch <- PCatch.h[,h] * Kh[,h]
          
          # process equation for 29W
          Pmed2.p <- exp(-m[,h]) * (object$data$g[i,h] * (P.out + Rh[,h]))
          # remove catch
          #Pcatch <- Pmed.p * exploit
          Pmed2 <- Pmed2.p - PCatch.h[,h]
          
          Pmed2[Pmed2 < 1e-05] <- 1e-04
          Pmed2 <- log(Pmed2)
          Max.P <- 6
          set.seed(1)
          P.out2 <- SSModeltest:::gen.lnorm(Pmed2, sigma[,h], Max.P)
          
          B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = (P.out2 * Kh[,h]), 
                                year =  object$Years[i]+2, 
                                catch = catch,
                                totalcatch=totalcatch,
                                mu = mu[2], #catch/(P.out2*Kh[,h] + catch),
                                proj = 2,
                                strata=object$labels[h],
                                modelyear = object$Years[i])
          
        }
        
        if(!is.na(mu[2])){
          # instantaneous vs. finite mortality
          # proportion survived = exp(-mu)
          
          # only need this if specifying mu in arguments
          mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                     mu[2], object$e.parms, object$data$Area)
          
          # process equation for 29W
          Pmed2.p <- exp(-m[,h]) * (object$data$g[i] * (P.out + Rh[,h]))
          # remove catch
          PCatch.h <- Pmed2.p * mu.exploit$exploit[h]
          Pmed <- Pmed2.p - PCatch.h
          catch <- PCatch.h * Kh[,h]
          
          Pmed2.p[Pmed2.p < 1e-05] <- 1e-04
          Pmed2.p <- log(Pmed2.p)
          Max.P <- 6
          set.seed(1)
          P.out2 <- SSModeltest:::gen.lnorm(Pmed2.p, sigma[,h], Max.P)
          
          B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = (P.out2 * Kh[,h] * (1-mu[2])), 
                                year =  object$Years[i]+2, 
                                catch = catch,
                                totalcatch = NA,
                                mu = mu[2],
                                proj = 2,
                                strata=object$labels[h],
                                modelyear = object$Years[i])
        }
        
        
      }
      
      # with a defined surplus production rate 
      if(!is.null(surplus)){
        
        surplus_multiplier <- 1 + surplus
        
        if(is.na(mu[2])){
          
          totalcatch <- object$data$C[i+1]
          PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                         "/")/Kh
          catch <- PCatch.h[,h] * Kh[,h]
          
          # process equation for 29W
          Pmed2.p <- (P.out + Rh[,h]) * surplus_multiplier
          # remove catch
          #Pcatch <- Pmed.p * exploit
          Pmed2 <- Pmed2.p - PCatch.h[,h]
          
          Pmed2[Pmed2 < 1e-05] <- 1e-04
          Pmed2 <- log(Pmed2)
          Max.P <- 6
          set.seed(1)
          P.out2 <- SSModeltest:::gen.lnorm(Pmed2, sigma[,h], Max.P)
          
          B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]]<- data.frame(Biomass = (P.out2 * Kh[,h]), 
                               year =  object$Years[i]+2, 
                               catch = catch,
                               totalcatch=totalcatch,
                               mu = mu[2], #catch/(P.out2*Kh[,h] + catch),
                               proj = 2,
                               strata=object$labels[h],
                               modelyear = object$Years[i])
        }
        if(!is.na(mu[2])){
          # instantaneous vs. finite mortality
          # proportion survived = exp(-mu)
          
          # only need this if specifying mu in arguments
          mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                     mu[2], object$e.parms, object$data$Area)
          
          # process equation for 29W
          Pmed2.p <- (P.out + Rh[,h]) * surplus_multiplier
          # remove catch
          PCatch.h <- Pmed2.p * mu.exploit$exploit[h]
          Pmed <- Pmed2.p - PCatch
          catch <- PCatch.h * Kh[,h]
          
          # process equation for 29W
          Pmed2.p <- (P.out + Rh[,h]) * surplus_multiplier
          # remove catch
          #Pcatch <- Pmed.p * exploit
          Pmed2.p[Pmed2.p < 1e-05] <- 1e-04
          Pmed2.p <- log(Pmed2.p)
          Max.P <- 6
          set.seed(1)
          P.out2 <- SSModeltest:::gen.lnorm(Pmed2.p, sigma[,h], Max.P)
          
          B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- data.frame(Biomass = (P.out2 * Kh[,h] * (1-mu[2])), 
                                year =  object$Years[i]+2, 
                                catch = catch,
                                totalcatch=NA,
                                mu = mu[2],
                                proj = 2,
                                strata=object$labels[h],
                                modelyear = object$Years[i])
        }
      }
      
      # for decision table
      # This is the projected biomass change (%) from this year to next.  > 0 = increase.
      B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$B.change <- (B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass  - B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass) / B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass * 100
      # Probability of biomass increase.  What runs are less than 0.
      B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$pB0_increase <- 0 < (B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass-B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]]$Biomass)
      
      if(decisiontable==T){
        
        B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- B.next0[[paste0(object$Years[i])]][[paste0(object$labels[h])]] %>%
          dplyr::group_by(year, modelyear, proj, strata) %>%
          dplyr::summarize(biomass=median(Biomass),
                           catch=median(catch),
                           mu=median(mu),
                           B.change=median(B.change),
                           pB0_increase = sum(pB0_increase)/length(pB0_increase),
                           p.LRP = round(sum(Biomass > LRP)/length(Biomass), 3),
                           p.USR = round(sum(Biomass > USR)/length(Biomass), 3))
        
        B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- B.next1[[paste0(object$Years[i])]][[paste0(object$labels[h])]] %>%
          dplyr::group_by(year, modelyear, proj, strata) %>%
          dplyr::summarize(biomass=median(Biomass),
                           catch=median(catch),
                           mu=median(mu),
                           B.change=median(B.change),
                           pB0_increase = sum(pB0_increase)/length(pB0_increase),
                           p.LRP = round(sum(Biomass > LRP)/length(Biomass), 3),
                           p.USR = round(sum(Biomass > USR)/length(Biomass), 3))
        
        B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]] <- B.next2[[paste0(object$Years[i])]][[paste0(object$labels[h])]] %>%
          dplyr::group_by(year, modelyear, proj, strata) %>%
          dplyr::summarize(biomass=median(Biomass),
                           catch=median(catch),
                           mu=median(mu),
                           B.change=median(B.change),
                           pB0_increase = sum(pB0_increase)/length(pB0_increase),
                           p.LRP = round(sum(Biomass > LRP)/length(Biomass), 3),
                           p.USR = round(sum(Biomass > USR)/length(Biomass), 3))
      }
    }
  }
  
  return(output=list(B.next0 = B.next0, B.next1=B.next1, B.next2=B.next2))
  
}
