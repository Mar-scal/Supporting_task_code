# This function performs 2 year projections by looping through the projection process twice; recycling the one year projection and using it for a second year's projection. 
# the "full" scenario means that process error from the first year projection will be carried through and applied in the second year projection
# the "zero surplus production" scenario ("0_surplus") does not factor in mortality, recruitment, growth etc. and simply removes the catch or applies the exploitation rate to the 1st year's estimate.
# But this can be done 2 ways - one where we don't carry process error through ("0_surplus_simple") and one where we do ("0_surplus_process")

# object: model Rdata object (e.g. Spa4.2019 or model.obj)
# scenario: "full" or "0_surplus" as described above
# type: "catch" means that you remove the catch; "exp" means you apply 1-the exploitation rate. 
# rate: NULL is default. Leave as is if you use type="catch". 
#       If type="exp", this is a vector of exploitation rates to apply. It should be the length of object$Years

process_2y_proj <- function(object, area, scenario, type, rate=NULL){
 
  # extra catch values for 2yr projections
  #length(object$Years) == length(object$data$C)
  object$data$C <- c(object$data$C, object$data$C[length(object$data$C)])

  year.start <- object$data$maxY-9
  
  B.next0 <- NULL
  B.next1 <- NULL
  B.next2 <- NULL
  sub.obj <- NULL
  Max.P <- NULL
  m.avg <- 5
  for (i in year.start:length(object$Years)){
    sub.obj$sims.matrix <- cbind(object$sims.matrix[,paste0("B[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("R[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("P[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("m[", 1:i, "]")], 
                                 object$sims.matrix[,paste0("r[", 1:i, "]")])
    
    if(area %in% c("GBa", "BBn")) sub.obj$sims.matrix  <- cbind(sub.obj$sims.matrix, object$sims.matrix[,paste0("mR[", 1:i, "]")])
    browser()
    sub.obj$Years <- object$Years[1:i]
    
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
    
    # keep all m for avg calc
    m <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "m[")]
    m.avg <- 5    
    m <- apply(m[, i - (0:(m.avg - 1))], 1, mean)
    
    if(area %in% c("GBa", "BBn")) {
      mR <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]],
                                                      1, 3), "mR[")]
      mR.avg <- 5
      mR <- apply(mR[, i - (0:(m.avg - 1))], 1, mean)
    }
  
    # current year
    B.next0[[paste0(object$Years[i])]] <- data.frame(Biomass = object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 1, 2), "B[")][, i], 
                                                     year =  object$Years[i], catch=object$data$C[i-1], exp=NA, proj=0)
    
    # calculate the standardized Biomass (P) for y 1
    # catch here should be the catch for the 1y projection
    if(!grepl(x=area, "29")){
      # Process model for BoF and offshore
      if(!area %in% c("GBa", "BBn")) Pmed <- exp(-m) * object$data$g[i] * (P - object$data$C[i]/K) + exp(-m) * 
        object$data$gR[i] * r
      if(area %in% c("GBa", "BBn")) Pmed <- exp(-m) * object$data$g[i] * (P - object$data$C[i]/K) + exp(-mR) * 
          object$data$gR[i] * r
      Pmed <- ifelse(Pmed > 0.001, Pmed, 0.001)
      Pmed <- log(Pmed)
      Max.P <- 8
      set.seed(1)
      P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
    }
    
    if(grepl(x=area, "29")){
      # process equation for 29W
      Pmed.p <- exp(-m) * (object$data$g[i] * (P + R))
      # remove catch
      #Pcatch <- Pmed.p * exploit
      Pmed.p <- Pmed.p - object$data$C[i]/K
      
      Pmed.p[Pmed.p < 1e-05] <- 1e-04
      Pmed.p <- log(Pmed.p)
      Max.P <- 6
      P.out <- SSModeltest:::gen.lnorm(Pmed.p, sigma, Max.P)
    }
    
    # B.next is the 1 y projection
    B.next1[[paste0(object$Years[i])]] <- data.frame(Biomass = P.out * K, 
                                                    year =  object$Years[i]+1, 
                                                    catch = object$data$C[i], 
                                                    exp=NA, 
                                                    proj=1)
    
    # now for 2y projection. 
    if(scenario=="full") {
      # include K for the catch option (scale catch by carrying capacity?)
      if(type=="catch") {
        if(!grepl(x=area, "29") & !area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out - object$data$C[i+1]/K) + exp(-m) * object$data$gR[i] * r
        if(area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out - object$data$C[i+1]/K) + exp(-mR) * object$data$gR[i] * r
        if(grepl(x=area, "29")) {
          Pmed2 <- exp(-m) * (object$data$g[i] * (P.out + R))
          Pmed2 <- Pmed2 - object$data$C[i+1]/K
          }
        }  
      # don't include K for the exploitation rate option (carrying capacity is accounted for within the exploitation rate?). Note that indexing is different than for catch because using output from simple projections!
      if(type=="exp") {
        if(!grepl(x=area, "29") & !area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out * (1-rate[i-year.start+1])) + exp(-m) * object$data$gR[i] * r
        if(area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out * (1-rate[i-year.start+1])) + exp(-mR) * object$data$gR[i] * r
        if(grepl(x=area, "29")) {
          Pmed2 <- exp(-m) * (object$data$g[i] * (P.out + R))
          Pmed2 <- Pmed2 * (1-rate[i-year.start+1])
        }
      }
      
      # add process noise
      Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
      Pmed2 <- log(Pmed2)
      P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
      
      B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                       year =  object$Years[i]+2, 
                                                       catch = object$data$C[i+1],
                                                       exp = rate[i-year.start+1],
                                                       proj = 2)
    }
    
    # 0 surplus, without process error... just take last year's biomass and subtract catch!
    if(scenario=="0_surplus_simple"){
      if(type=="catch") B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out * K) - object$data$C[i+1], 
                                                                         year =  object$Years[i]+2, 
                                                                         catch = object$data$C[i+1],
                                                                         exp = NA, 
                                                                         proj = 2)
      if(type=="exp") {
        if(!is.null(rate)) {
          exp.catch <- median(P.out * K) * rate[i+1]
          B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out * K) * (1-rate[i-year.start+1]), 
                                                           year =  object$Years[i]+2, 
                                                           catch = exp.catch,
                                                           exp = rate[i-year.start+1], 
                                                           proj = 2)
        }
      }
    }
    
    # 0 surplus, with process error... 
    if(scenario=="0_surplus_process"){
      
      # set m, r, and g for 0_surplus version
      # include K for the catch option (scale catch by carrying capacity?)
      m <- 0
      object$data$g[i] <- 1
      object$data$gR[i] <- 1
      r <- 0
      if(area %in% c("GBa", "BBn")) mR <- 0
      if(type=="catch") {
        if(!grepl(x=area, "29") & !area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out - object$data$C[i+1]/K) + exp(-m) * object$data$gR[i] * r
        if(area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out - object$data$C[i+1]/K) + exp(-mR) * object$data$gR[i] * r
        if(grepl(x=area, "29")) {
          Pmed2 <- exp(-m) * (object$data$g[i] * (P.out + R))
          Pmed2 <- Pmed2 - object$data$C[i+1]/K
        }
      }
      # don't include K for the exploitation rate option (carrying capacity is accounted for within the exploitation rate?). Note that indexing is different than for catch because using output from simple projections!
      if(type=="exp") {
        if(!grepl(x=area, "29") & !area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out * (1-rate[i-year.start+1])) + exp(-m) * object$data$gR[i] * r
        if(area %in% c("GBa", "BBn")) Pmed2 <- exp(-m) * object$data$g[i] * (P.out * (1-rate[i-year.start+1])) + exp(-mR) * object$data$gR[i] * r
        if(grepl(x=area, "29")) {
          Pmed2 <- exp(-m) * (object$data$g[i] * (P.out + R))
          Pmed2 <- Pmed2 * (1-rate[i-year.start+1])
        }
        
      }
      # apply process error
      Pmed2 <- ifelse(Pmed2 > 0.001, Pmed2, 0.001)
      Pmed2 <- log(Pmed2)
      P.out2 <- SSModel:::gen.lnorm(Pmed2, sigma, Max.P)
      
      B.next2[[paste0(object$Years[i])]] <- data.frame(Biomass = (P.out2 * K), 
                                                       year =  object$Years[i]+2, 
                                                       catch = object$data$C[i+1],
                                                       exp = rate[i-year.start+1], 
                                                       proj = 2)
      
    }
    
    #print(object$Years[i])
    
  }
  return(output=list(B.next0 = B.next0, B.next1=B.next1, B.next2=B.next2))
}
