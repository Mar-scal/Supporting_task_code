####################################################################
## Function to run 2 year projections for our models. It is adapted from SSModel predict function
###################################################################
# Update history
# June 2020:  File created
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
## Two_year_projections.Rmd
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
## SSModel::gen.lnorm
###############################################################################################################


###############################################################################################################
# Arguments

## object: usually mod.res. This is the model output containing the sims matrix. 
## Catch: the different levels of catch you want to project for. Usually seq(0,500,10)
## g.parm: the growth parameter for fully-recruited scallop. If 1 value, it's used for both years, if 2 values c(a, b), it's one per year 
## gr.parm: the growth parameter for recruit scallop. If 1 value, it's used for both years, if 2 values c(a, b), it's one per year 
## m.avg: the number of mortality values to average across
## Max.P: the maximum P value possible. Used in gen.lnorm function. Where P is the standardized biomass and usually hovers around 1.
## two.year: logical. If T, run the predictions for two years. If F, stick with one year.
## r.multiplier: default is 1 (untouched), otherwise, multiply values of r by this value
## m.multiplier: default is 1 (untouched), otherwise, multiply values of r by this value
###############################################################################################################


predict.SSModel.2y <- function (object, area, Catch, catch2=NULL, g.parm, gr.parm, m.avg = 5, Max.P = 8, two.year=F, r.multiplier=NULL, r.2=NULL, m.multiplier=NULL, m.2=NULL, scenario=NULL,
                                ...) 
{
  predict.call <- match.call()
  Cur.year <- length(object$Years)
  
  if(two.year==T) years <- c(Cur.year, Cur.year+1)
  if(two.year==F) years <- Cur.year
  
  if(length(g.parm) == 1 & two.year==T) g.parm <- rep(g.parm, 2)
  if(length(gr.parm) == 1 & two.year==T) gr.parm <- rep(gr.parm, 2)
  
  out <- NULL
  
  # we're going to loop through the following code in case we're doing a 2-year projection.
  for(i in 1:length(years)){
    
    # if we're projecting for the first time, we just take the data from the model output. If it's the second time, we use the values from the previous projection (which is done at the end of the 1st iteration of the loop)
    if(i==1) {
      B.last <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                          1, 2), "B[")][, years[i]]
      P <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                     1, 2), "P[")][, Cur.year]
      if(grepl(x=area, "29")) R <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                     1, 2), "R[")][, Cur.year]
    }
    
    # keep r, K, m, and sigma the same regardless of year
    r <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "r[")][, Cur.year]
    
    if(!is.null(r.multiplier) &  is.null(r.2) & i > 1) r <- r * r.multiplier
    if(is.null(r.multiplier) &  !is.null(r.2) & i > 1) r <- r.2
    
  
    K <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 1), "K")]
    
    m <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                   1, 2), "m[")]
    
    if(!is.null(m.multiplier) & is.null(m.2) & i > 1) m <- m * m.multiplier
    if(is.null(m.multiplier) & !is.null(m.2) & i > 1) {
      m <- m.2
      m.avg <- NULL
    }
  
    if(any(is.element(substring(dimnames(object$sims.matrix)[[2]], 1, 3), "mR["))) {
      mR <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                      1, 3), "mR[")]
      
      if(!is.null(m.multiplier) & is.null(m.2) & i > 1) mR <- mR * m.multiplier
      if(is.null(m.multiplier) & !is.null(m.2) & i > 1) {
        mR <- m.2
        m.avg <- NULL
      }
    }
    
    sigma <- object$sims.matrix[, is.element(substring(dimnames(object$sims.matrix)[[2]], 
                                                       1, 5), "sigma")]
    
    if (m.avg == 1 && !is.null(m.avg)) m <- m[, Cur.year]
    if (m.avg > 1 && !is.null(m.avg)) m <- apply(m[, Cur.year - (0:(m.avg - 1))], 1, mean)
    
    #for offshore only
    if(exists("mR")){
      if (m.avg == 1 && !is.null(m.avg)) mR <- mR[, Cur.year]
      if (m.avg > 1 && !is.null(m.avg)) mR <- apply(mR[, Cur.year - (0:(m.avg - 1))], 1, mean)
    }
    
    # calculate the standardized Biomass (P)
    if (length(Catch) == 1) {
      if(!grepl(x=area, "29")) {
        if(!exists("mR")) Pmed <- exp(-m) * g.parm[i] * (P - Catch/K) + exp(-m) * 
            gr.parm[i] * r
        if(exists("mR")) Pmed <- exp(-m) * g.parm[i] * (P - Catch/K) + exp(-mR) * 
            gr.parm[i] * r
      }
      if(grepl(x=area, "29")) {
        Pmed <- exp(-m) * (g.parm[i] * (P + R))
        PCatch <- Catch * object$data$Area[object$data$H]/sum(object$data$Area)
        Pmed <- Pmed - PCatch/K
      }
      Pmed <- ifelse(Pmed > 0.001, Pmed, 0.001)
      Pmed <- log(Pmed)
    }
    
    if (!length(Catch) == 1) {
      if(!grepl(x=area, "29")){
        if(!exists("mR")) Pmed <- exp(-m) * g.parm[i] * (P - sweep(matrix(Catch, length(K), 
                                                                          length(Catch), byrow = T), 1, K, "/")) + exp(-m) * gr.parm[i] * r
        if(exists("mR")) Pmed <- exp(-m) * g.parm[i] * (P - sweep(matrix(Catch, length(K), 
                                                                         length(Catch), byrow = T), 1, K, "/")) + exp(-mR) * gr.parm[i] * r
      }
      if(grepl(x=area, "29")) {
        Pmed <- exp(-m) * (g.parm[i] * (P + R))
        # assuming catch comes out equally across strata... unlikely!!
        PCatch <- Catch * object$data$Area[object$data$H]/sum(object$data$Area)
      
        # from SSModeltest_predict
        # # this is the exploitation for a particular strata
        # temp.exploit <- list(exploit = 
        #                        sweep(
        #                          sweep(
        #                            sweep(Bh,2, object$data$Area, "/")/ # biomass density 
        #                              apply(Bh, 1, sum), # proportional to entire area biomass #### THIS PART MEANS YOU HAVE TO DO ALL STRATA TO GET TOTAL BIOMASS!
        #                            1, object$e.parms[H + 1], "*"), # by slope
        #                          2, object$e.parms[1:H], "+")) # plus intercept
        # 
        # PCatch <- mn.Bh * temp.exploit$exploit
        # PCatch <- Catch * sweep(PCatch, 1, apply(PCatch, 1, sum), 
        #                         "/")/Kh
        
        Pmed <- Pmed - sweep(matrix(PCatch, length(K), 
                                    length(PCatch), byrow = T), 1, K, "/")
      }
      Pmed <- apply(Pmed, 2, function(x) {
        ifelse(x > 0.001, x, 0.001)
      })
      Pmed <- log(Pmed)
    }
    # Here's the process error step:
    # use the gen.lnorm function from SSModel to project the new standardized biomass.
    
    if(!is.null(scenario)){
      if(as.character(scenario)=="0_surplus") {
        # apply process error for 1-year-out projection
        if(i==1) P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
        # do not apply process error for 2-year-out projection
        if(i>1) P.out <- exp(Pmed)
      }
      if(as.character(scenario)=="0_surplus_JS") {
        # apply process error for 1-year-out projection
        if(i==1) P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
        # do not apply process error for 2-year-out projection
        # if(i>1) P.out <- P.out (so do nothing)
      }
    }
    if(is.null(scenario)) {
      P.out <- SSModel:::gen.lnorm(Pmed, sigma, Max.P)
    }
    
    # then unstandardize it by multiplying by K
    if (is.matrix(P.out)) {
      B.next <- sweep(P.out, 1, K, "*")
    }
    else {
      B.next <- P.out * K
    }
    
    # store these results in the out list for the i year
    if(!grepl(x=area, "29")) {
      out[[i]] <- list(B.next = B.next, B.cur = B.last, Catch = Catch, 
                     g = g.parm[i], gr = gr.parm[i], m = m, r = r, predict.call = predict.call)
    }
    
    if(grepl(x=area, "29")) {
      # these are the areas for the selected strata. medium for A, high for the rest.
      Area <- object$data$Area[object$data$H]
      out[[i]] <- list(B.next = B.next, B.cur = B.last, Catch = Catch, PCatch=PCatch,
                       g = g.parm[i], gr = gr.parm[i], m = m, r = r, Area = Area, predict.call = predict.call)
    }

    class(out[[i]]) <- "predict.ssmodel"
    
    # if you're in the 1st iteration, and there's going to be another one, store these values for the next year. 
    if(length(years)>1 & i==1) {
      if(is.null(catch2)) {
        if(is.matrix(P.out)){
          P <- P.out[,which(Catch == round(object$data$C, -1)[Cur.year])]
          B.last <- B.next[,which(Catch == round(object$data$C, -1)[Cur.year])]
        }
        if(!is.matrix(P.out)){
          P <- P.out
          B.last <- B.next
        }
      }
      if(!is.null(catch2)) {
        if(length(catch2)==1){
          if(is.matrix(P.out)){
            P <- P.out[,which(Catch == catch2)]
            B.last <- B.next[,which(Catch == catch2)]
          }
          if(!is.matrix(P.out)){
            P <- P.out
            B.last <- B.next
          }
        }
        if(length(catch2)==2){
          if(is.matrix(P.out)){
            P <- P.out[,which(Catch == catch2[1])]
            B.last <- B.next[,which(Catch == catch2[1])]
          }
          if(!is.matrix(P.out)){
            P <- P.out
            B.last <- B.next
          }
        }
        
      }
    }
  }
  #if(i==1) browser() 
  return(out)
}