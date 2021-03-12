# This function performs 2 year projections by looping through the projection process twice; recycling the one year projection and using it for a second year's projection. 
# object: model Rdata object (e.g. Spa4.2019 or model.obj)
# surplus: surplus production value as a proportion (0, 0.15, 0.25 etc). If NULL (default) the normal model will run (no assumptions made.)

process_2y_proj <- function(object, area, mu=c(NA, NA), surplus=NULL, decisiontable=F){
 
  if(decisiontable==F) year.start <- object$data$NY-9
  if(decisiontable==T) year.start <- object$data$NY
    
  B.next0 <- NULL
  B.next1 <- NULL
  B.next2 <- NULL
  for (i in year.start:length(object$Years)){
    browser()
    # keep r, K, and sigma the same regardless of year
    q <- object$sims.matrix[,paste0("q")]
    r <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "r[")][, i]
    K <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 1), "K")]
    sigma <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                       1, 5), "sigma")]
    
    # if we're projecting for the first time, we just take the data from the model output. 
    # If it's the second time, we use the values from the previous projection (which is done at the end of the 1st iteration of the loop)
    B.last <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                        1, 2), "B[")][, i]
    R <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                  1, 2), "R[")][, i]
    P <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "P[")][, i]
    m <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "m[")][, i]

    
############### current year ##################################
    B.next0[[paste0(object$Years[i])]] <- data.frame(Biomass = object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 1, 2), "B[")][, i], 
                                                     year =  object$Years[i], 
                                                     catch=object$data$C[i-1], 
                                                     mu = NA,
                                                     Fmort = NA,
                                                     proj=0,
                                                     B.change=NA,
                                                     pB0=NA)

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
      Pmed <- exp(-m) * object$data$g[i] * (P - object$data$C[i]/K) + exp(-m) * object$data$gR[i] * r
      catch <- object$data$C[i]
      Pmed <- ifelse(Pmed > 0.001, Pmed, 0.001)
      Pmed <- log(Pmed)
      #offshore doesn't use SSModel gen.lnorm! (inshore does!)
      Max.P <- 8
      set.seed(1)
      P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
      
      B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = P.out * K, 
                                                       year =  object$Years[i]+1, 
                                                       catch = catch, 
                                                       mu = mu[1],
                                                       Fmort = catch/((P.out*K)+ catch),
                                                       proj=1)
    }
    
    # For offshore, don't remove removals using mu. Use CATCH! This is just here for kicks.... 
    if(!is.na(mu[1])){
      # instantaneous vs. finite mortality
      # proportion survived = exp(-mu)
      Pmed <- exp(-m) * object$data$g[i] * P + exp(-m) * object$data$gR[i] * r
      Pmed <- ifelse(Pmed > 0.001, Pmed, 0.001)
      Pmed <- log(Pmed)
      #offshore doesn't use SSModel gen.lnorm! (inshore does!)
      Max.P <- 8
      set.seed(1)
      P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
      catch <- P.out * K * mu[1]

      B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = P.out * K * (1-mu[1]),
                                                       year =  object$Years[i]+1,
                                                       catch = catch,
                                                       mu = mu[1],
                                                       Fmort = catch/((P.out*K*(1-mu[1]))+ catch),
                                                       proj=1)
    }
   
    
    # for decision table
    # This is the projected biomass change (%) from this year to next.  > 0 = increase.
    B.next1[[paste0(object$Years[i])]]$B.change <- (B.next1[[paste0(object$Years[i])]]$Biomass  - B.next0[[paste0(object$Years[i])]]$Biomass) / B.next0[[paste0(object$Years[i])]]$Biomass * 100
    # Probability of biomass decline.  What runs are less than 0.
    B.next1[[paste0(object$Years[i])]]$pB0 <- 0 > (B.next1[[paste0(object$Years[i])]]$Biomass-B.next0[[paste0(object$Years[i])]]$Biomass)
    
    
################## now for 2y projection. #################
    
    # if no surplus value supplied:
    if(is.null(surplus)) {
      # include K for the catch option (scale catch by carrying capacity?)
      if(is.na(mu[2])){
        Pmed2 <- exp(-m) * object$data$g[i] * (P.out - object$data$C[i+1]/K) + exp(-m) * object$data$gR[i] * r
        catch <- object$data$C[i+1]
        # add process noise
        Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
        Pmed2 <- log(Pmed2)
        
        Max.P <- 8
        set.seed(1)
        P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
        
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         mu = mu[2],
                                                         Fmort = catch/(P.out2*K + catch),
                                                         proj = 2)
      }
      if(!is.na(mu[2])){
        # instantaneous vs. finite mortality
        # proportion survived = exp(-mu)
        Pmed2 <- exp(-m) * object$data$g[i] * (P.out) + exp(-m) * object$data$gR[i] * r
        # add process noise
        Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
        Pmed2 <- log(Pmed2)
        Max.P <- 8
        set.seed(1)
        P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
        catch <- P.out2 * K * mu[2]
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K * (1-mu[2])), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         mu = mu[2],
                                                         Fmort = catch/(P.out2*K*(1-mu[2]) + catch),
                                                         proj = 2)
      }
      
     
    }
    
    # with a defined surplus production rate 
    if(!is.null(surplus)){
      
      surplus_multiplier <- 1 + surplus

      if(is.na(mu[2])){
        Pmed2 <- (P.out - object$data$C[i+1]/K) * surplus_multiplier # could put the multiplier on P.out directly for offshore because of the 
                                                                    #timing of growth relative to the timing of the majority of catch, 
                                                                    #but doing this to be consistent across areas (and it's more conservative)
        catch <- object$data$C[i+1]
        # apply process error
        Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
        Pmed2 <- log(Pmed2)
        
        Max.P <- 8
        set.seed(1)
        P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
        
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         mu = mu[2],
                                                         Fmort = catch/(P.out2*K + catch),
                                                         proj = 2)
      }
      if(!is.na(mu[2])){
        # instantaneous vs. finite mortality
        # proportion survived = exp(-mu)
        Pmed2 <- P.out * surplus_multiplier
        
        # apply process error
        Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
        Pmed2 <- log(Pmed2)
      
        Max.P <- 8
        set.seed(1)
        P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
        
        catch <- P.out2 * K * mu[2]
        B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K * (1-mu[2])), 
                                                         year =  object$Years[i]+2, 
                                                         catch = catch,
                                                         mu = mu[2],
                                                         Fmort = catch/((P.out2*K * (1-mu[2]))+ catch),
                                                         proj = 2)
      }
    }
    
    # for decision table
    # This is the projected biomass change (%) from this year to next.  > 0 = increase.
    B.next2[[paste0(object$Years[i])]]$B.change <- (B.next2[[paste0(object$Years[i])]]$Biomass  - B.next1[[paste0(object$Years[i])]]$Biomass) / B.next1[[paste0(object$Years[i])]]$Biomass * 100
    # Probability of biomass decline.  What runs are less than 0.
    B.next2[[paste0(object$Years[i])]]$pB0 <- 0 > (B.next2[[paste0(object$Years[i])]]$Biomass-B.next1[[paste0(object$Years[i])]]$Biomass)
  }
  return(output=list(B.next0 = B.next0, B.next1=B.next1, B.next2=B.next2))
}
