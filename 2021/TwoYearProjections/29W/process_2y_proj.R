# This function performs 2 year projections by looping through the projection process twice; recycling the one year projection and using it for a second year's projection. 
# object: model Rdata object (e.g. Spa4.2019 or model.obj)
# surplus: surplus production value as a proportion (0, 0.15, 0.25 etc). If NULL (default) the normal model will run (no assumptions made.)

process_2y_proj <- function(object, area, mu=c(NA, NA), surplus=NULL, decisiontable=F){
 
  if(decisiontable==F) year.start <- length(object$Years)-9
  if(decisiontable==T) year.start <- length(object$Years)
    
  if(area == "29A") strata <- which(object$labels == "Medium")
  if(area %in% c("29B", "29C", "29D")) strata <- which(object$labels == "High")
  
  B.next0 <- NULL
  B.next1 <- NULL
  B.next2 <- NULL
  for (i in year.start:length(object$Years)){
    
    # keep r, K, and sigma the same regardless of year
    q <- object$sims.matrix[,paste0("q")]
    
    K <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 1), "K")][, strata]
    
    sigma <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                       1, 5), "sigma")]
    
    # if we're projecting for the first time, we just take the data from the model output. 
    # If it's the second time, we use the values from the previous projection (which is done at the end of the 1st iteration of the loop)
    B.last <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                        1, 5), paste0("Bh[", i))][, strata]
    R <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                  1, 5), paste0("Rh[", i))][, strata]
    P <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 5), paste0("Ph[", i))][, strata]
    m <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 4), paste0("m[", i))][, strata]
    
    

    # extra stuff for 29W
    Bh <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                        1, 5), paste0("Bh[", i))]
    Rh <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 5), paste0("Rh[", i))]
    Kh <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "Kh")]
    
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
    
    # need this for current year and potentially other years
    c.exploit <- list(exploit = sweep(sweep(sweep(Bh, 
                                                  2, object$data$Area, "/")/apply(Bh, 1, sum), 
                                            1, object$e.parms[strata + 1], "*"), 2, object$e.parms[1:strata], 
                                      "+"))
    PCatch <- mn.Bh * c.exploit$exploit
    
    
############### current year ##################################
    totalcatch <- object$data$C[i-1]
    PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                 "/")/Kh
    catch <- PCatch.h[,strata] * K
    
    B.next0[[paste0(object$Years[i])]] <- data.frame(Biomass = B.last, 
                                                     year =  object$Years[i], 
                                                     catch = catch,
                                                     totalcatch = object$data$C[i-1], 
                                                     mu = NA,
                                                     proj=0,
                                                     B.change=NA,
                                                     pB0_increase=NA)

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
      catch <- PCatch.h[,strata] * K
      
      # process equation for 29W
      Pmed.p <- exp(-m) * (object$data$g[i] * (P + R))
      # remove catch
      #Pcatch <- Pmed.p * exploit
      Pmed <- Pmed.p - PCatch.h[,strata]
      
      Pmed[Pmed < 1e-05] <- 1e-04
      Pmed <- log(Pmed)
      Max.P <- 6
      P.out <- SSModeltest:::gen.lnorm(Pmed, sigma, Max.P)
      
      
      B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = P.out * K, 
                                                       year =  object$Years[i]+1, 
                                                       catch = catch, 
                                                       totalcatch=totalcatch,
                                                       mu = catch/((P.out*K)+ catch),
                                                       proj=1)
    }
    
    # For offshore, don't remove removals using mu. Use CATCH! This is just here for kicks.... 
    if(!is.na(mu[1])){
      
      # only need this if specifying mu in arguments
      mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                 mu[1], object$e.parms, object$data$Area)
      
      # instantaneous vs. finite mortality
      # proportion survived = exp(-mu)
      # process equation for 29W
      Pmed <- exp(-m) * (object$data$g[i] * (P + R))
      PCatch.h <- Pmed * mu.exploit$exploit[strata]
      Pmed <- Pmed - PCatch.h
      catch <- PCatch.h * K
      
      
      Pmed[Pmed < 1e-05] <- 1e-04
      Pmed <- log(Pmed)
      Max.P <- 6
      set.seed(1)
      P.out <- SSModeltest:::gen.lnorm(Pmed, sigma, Max.P)
      
      B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = P.out * K * (1-mu[1]),
                                                       year =  object$Years[i]+1,
                                                       catch = catch,
                                                       totalcatch = NA,
                                                       mu = mu[1],
                                                       proj=1)
    }
   
    
    # for decision table
    # This is the projected biomass change (%) from this year to next.  > 0 = increase.
    B.next1[[paste0(object$Years[i])]]$B.change <- (B.next1[[paste0(object$Years[i])]]$Biomass  - B.next0[[paste0(object$Years[i])]]$Biomass) / B.next0[[paste0(object$Years[i])]]$Biomass * 100
    # Probability of biomass decline.  What runs are less than 0.
    B.next1[[paste0(object$Years[i])]]$pB0_increase <- 0 > (B.next1[[paste0(object$Years[i])]]$Biomass-B.next0[[paste0(object$Years[i])]]$Biomass)
    
    
################## now for 2y projection. #################
    
    # if no surplus value supplied:
    if(is.null(surplus)) {
      # include K for the catch option (scale catch by carrying capacity?)
      if(is.na(mu[2])){
        
        totalcatch <- object$data$C[i+1]
        PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                     "/")/Kh
        catch <- PCatch.h[,strata] * K
        
        # process equation for 29W
        Pmed2.p <- exp(-m) * (object$data$g[i] * (P.out + R))
        # remove catch
        #Pcatch <- Pmed.p * exploit
        Pmed2 <- Pmed2.p - PCatch.h[,strata]
        
        Pmed2[Pmed2 < 1e-05] <- 1e-04
        Pmed2 <- log(Pmed2)
        Max.P <- 6
        set.seed(1)
        P.out2 <- SSModeltest:::gen.lnorm(Pmed2, sigma, Max.P)
        
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         totalcatch=totalcatch,
                                                         mu = catch/(P.out2*K + catch),
                                                         proj = 2)
        
      }
        
      if(!is.na(mu[2])){
        # instantaneous vs. finite mortality
        # proportion survived = exp(-mu)
        
        # only need this if specifying mu in arguments
        mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                   mu[2], object$e.parms, object$data$Area)
        
        # process equation for 29W
        Pmed2.p <- exp(-m) * (object$data$g[i] * (P.out + R))
        # remove catch
        PCatch.h <- Pmed2.p * mu.exploit$exploit[strata]
        Pmed <- Pmed2.p - PCatch.h
        catch <- PCatch.h * K
        
        Pmed2.p[Pmed2.p < 1e-05] <- 1e-04
        Pmed2.p <- log(Pmed2.p)
        Max.P <- 6
        set.seed(1)
        P.out2 <- SSModeltest:::gen.lnorm(Pmed2.p, sigma, Max.P)
        
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K * (1-mu[2])), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         totalcatch = NA,
                                                         mu = mu[2],
                                                         proj = 2)
      }
      
     
    }
    
    # with a defined surplus production rate 
    if(!is.null(surplus)){
      
      surplus_multiplier <- 1 + surplus

      if(is.na(mu[2])){
        
        totalcatch <- object$data$C[i+1]
        PCatch.h <- totalcatch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
                                     "/")/Kh
        catch <- PCatch.h[,strata] * K
        
        # process equation for 29W
        Pmed2.p <- (P.out + R) * surplus_multiplier
        # remove catch
        #Pcatch <- Pmed.p * exploit
        Pmed2 <- Pmed2.p - PCatch.h[,strata]
        
        Pmed2[Pmed2 < 1e-05] <- 1e-04
        Pmed2 <- log(Pmed2)
        Max.P <- 6
        set.seed(1)
        P.out2 <- SSModeltest:::gen.lnorm(Pmed2, sigma, Max.P)
        catch <- object$data$C[i+1]
        
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         totalcatch=totalcatch,
                                                         mu = mu[2],
                                                         Fmort = catch/(P.out2*K + catch),
                                                         proj = 2)
      }
      if(!is.na(mu[2])){
        # instantaneous vs. finite mortality
        # proportion survived = exp(-mu)
        
        # only need this if specifying mu in arguments
        mu.exploit <- Proj.exploit(mn.Bh, mn.Bh/object$data$Area, 
                                   mu[2], object$e.parms, object$data$Area)
        
        # process equation for 29W
        Pmed2.p <- (P.out + R) * surplus_multiplier
        # remove catch
        PCatch <- Pmed2.p * mu.exploit$exploit[strata]
        Pmed <- Pmed2.p - PCatch
        catch <- PCatch * K
        
        # process equation for 29W
        Pmed2.p <- (P.out + R) * surplus_multiplier
        # remove catch
        #Pcatch <- Pmed.p * exploit
        Pmed2.p[Pmed2.p < 1e-05] <- 1e-04
        Pmed2.p <- log(Pmed2.p)
        Max.P <- 6
        set.seed(1)
        P.out2 <- SSModeltest:::gen.lnorm(Pmed2.p, sigma, Max.P)
        catch <- P.out2 * K * mu[2]
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K * (1-mu[2])), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         totalcatch=NA,
                                                         mu = mu[2],
                                                         Fmort = catch/((P.out2*K * (1-mu[2]))+ catch),
                                                         proj = 2)
      }
    }
    
    # for decision table
    # This is the projected biomass change (%) from this year to next.  > 0 = increase.
    B.next2[[paste0(object$Years[i])]]$B.change <- (B.next2[[paste0(object$Years[i])]]$Biomass  - B.next1[[paste0(object$Years[i])]]$Biomass) / B.next1[[paste0(object$Years[i])]]$Biomass * 100
    # Probability of biomass increase.  What runs are less than 0.
    B.next2[[paste0(object$Years[i])]]$pB0_increase <- 0 < (B.next2[[paste0(object$Years[i])]]$Biomass-B.next1[[paste0(object$Years[i])]]$Biomass)
  }
  return(output=list(B.next0 = B.next0, B.next1=B.next1, B.next2=B.next2))
}
